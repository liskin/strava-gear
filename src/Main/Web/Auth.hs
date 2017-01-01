{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main.Web.Auth
    ( Auth(..)
    , CfgAuth
    , CookieAuthProtect
    , LoginApi
    , cookieAuthHandler
    , loginApi
    , serveStriveLogin
    , withAuthCookies
    , withStrive
    )
  where

import Protolude hiding (handle)

import Data.String (String)

import Control.Monad.Catch (handle)
import Crypto.Random (drgNew)
import Data.Default (def)
import Data.Serialize (Serialize)
import Data.Serialize.Text ()
import Network.URI (URI)
import Network.Wai (Request)
import Servant
    ( (:>)
    , AuthProtect
    , HasLink
    , Header
    , Headers
    , IsElem
    , MkLink
    , PlainText
    , QueryParam
    , Server
    , addHeader
    , err403
    , safeLink
    , throwError
    )
import Servant.Server.Experimental.Auth
    ( AuthHandler
    , mkAuthHandler
    )
import Servant.Server.Experimental.Auth.Cookie
    ( AuthCookieData
    , AuthCookieException
    , AuthCookieSettings
    , RandomSource
    , ServerKey
    , addSession
    , getSession
    , mkRandomSource
    , mkServerKey
    )
import qualified Strive as S

import Main.Web.Utils


type CfgStravaAppId = ?stravaAppId :: S.ApplicationId
type CfgStravaAppSecret = ?stravaAppSecret :: S.ApplicationSecret
type CfgAuthCookieSettings = ?authCookieSettings :: AuthCookieSettings
type CfgRandomSource = ?randomSource :: RandomSource
type CfgServerKey = ?serverKey :: ServerKey

type CfgStrive = (CfgStravaAppId, CfgStravaAppSecret)

withStrive
    :: S.ApplicationId -> S.ApplicationSecret
    -> (CfgStrive => IO a) -> IO a
withStrive appId appSecret f = do
    let ?stravaAppId = appId
    let ?stravaAppSecret = appSecret
    f

type CfgAuthCookies = (CfgAuthCookieSettings, CfgRandomSource, CfgServerKey)

withAuthCookies :: (CfgAuthCookies => IO a) -> IO a
withAuthCookies f = do
    randomSource <- mkRandomSource drgNew 1000
    let ?randomSource = randomSource
    serverKey <- mkServerKey 16 Nothing
    let ?serverKey = serverKey
    let ?authCookieSettings = def
    f

type CfgAuth = (CfgStrive, CfgBaseUri, CfgAuthCookies)

data Auth = Auth
    { authToken :: !Text
    , authAthlete :: !Integer
    }
  deriving (Show, Eq, Generic, Serialize)

type instance AuthCookieData = Auth

type LoginApi
    = "login"
    :> QueryParam "code" String
    :> GetFound '[PlainText] (Headers '[Header "set-cookie" ByteString] String)

loginApi :: Proxy LoginApi
loginApi = Proxy

type Link api l t = (IsElem l api, HasLink l, MkLink l ~ t)

serveStriveLogin
    :: (CfgAuth, Link api redirectApi URI, Link api LoginApi _t1)
    => Proxy api -> Proxy redirectApi -> Server LoginApi
serveStriveLogin api redirectApi = \case
    Nothing ->
        addHeader "" <$> redirect authorizeUri
    Just code -> do
        Right res <- liftIO $ S.exchangeToken ?stravaAppId ?stravaAppSecret code
        let auth = Auth
                { authToken = S.get S.accessToken res
                , authAthlete = S.get (S.athlete . S.id) res
                }
        addSession' auth =<< redirect redirectUri
  where
    authorizeUri = S.buildAuthorizeUrl ?stravaAppId loginUri opts
    loginUri = showAbsoluteUri $ safeLink api (Proxy :: Proxy LoginApi) Nothing
    opts = S.set S.privateScope True $ def
    redirectUri = showAbsoluteUri $ safeLink api redirectApi
    addSession' = addSession ?authCookieSettings ?randomSource ?serverKey

type CookieAuthProtect = AuthProtect "cookie-auth"

cookieAuthHandler :: CfgAuthCookies => AuthHandler Request Auth
cookieAuthHandler = mkAuthHandler $ \request -> do
    msession <- handle (\(_ :: AuthCookieException) -> notAuth) $
        liftIO (getSession ?authCookieSettings ?serverKey request)
    maybe notAuth return msession
  where
    notAuth = throwError err403
