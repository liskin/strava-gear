{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main.Web (main) where

import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import System.Environment (getArgs)

import Network.Wai.Handler.Warp (run)
import Servant
    ( (:<|>)(..)
    , (:>)
    , Application
    , Context((:.), EmptyContext)
    , Get
    , Server
    , serveWithContext
    )
import Servant.HTML.Blaze (HTML)
--import qualified Strive as S
import Text.Blaze.Html5 (Markup)
import qualified Text.Blaze.Html5 as H

import Config as Config (clientId, clientSecret, url)
import Main.Web.Auth
import Main.Web.Utils


main :: IO ()
main = do
    [read -> port] <- getArgs
    withStrive Config.clientId Config.clientSecret $
        withBaseUri Config.url $
        withAuthCookies $
        run port app

app :: (CfgAuth) => Application
app = serveWithContext api ctx serve
  where
    ctx = cookieAuthHandler :. EmptyContext

type Api
    = HomeApi
    :<|> LoginApi
    :<|> "test" :> TestApi

api :: Proxy Api
api = Proxy

--apiLink :: (IsElem e Api, HasLink e) => Proxy e -> MkLink e
--apiLink = safeLink api

serve :: (CfgAuth) => Server Api
serve = serveHome :<|> serveLogin :<|> serveTest
  where
    serveLogin = serveStriveLogin api homeApi

type HomeApi = Get '[HTML] Markup

homeApi :: Proxy HomeApi
homeApi = Proxy

serveHome :: Server HomeApi
serveHome = pure . H.docTypeHtml $ do
    H.head (H.title "home")
    H.body $ do
        H.p "TODO"

type TestApi = CookieAuthProtect :> Get '[HTML] Markup

serveTest :: Server TestApi
serveTest Auth{..} = pure . H.docTypeHtml $ do
    H.head (H.title "test")
    H.body $ do
        H.p $ "token: " <> H.text authToken
        H.p $ "athlete: " <> H.string (show authAthlete)
