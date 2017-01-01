{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main.Web (main) where

import Protolude hiding ((<>), link)

import Data.Monoid ((<>))
import Data.String (String)

import Database.Persist.Sql (SqlPersistM, runMigration)
import Database.Persist.Sqlite (runSqlite)
import Network.Wai.Application.Static
    ( defaultWebAppSettings
    , staticApp
    )
import Network.Wai.Handler.Warp (run)
import Servant
    ( (:<|>)(..)
    , (:>)
    , Application
    , Context((:.), EmptyContext)
    , Get
    , Handler
    , HasLink
    , IsElem
    , JSON
    , MkLink
    , NoContent(NoContent)
    , PlainText
    , Post
    , PostNoContent
    , QueryFlag
    , Raw
    , ReqBody
    , Server
    , err400
    , errBody
    , safeLink
    , serveWithContext
    )
import Servant.HTML.Blaze (HTML)
import qualified Strive as S (Client, buildClient)
import System.FilePath (addTrailingPathSeparator)
import Text.Blaze.Html5 (Markup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Html as H2
import qualified Text.Tabular as Tab (Table)
import qualified Text.Tabular.Html as Tab (render)

import Config as Config (clientId, clientSecret, url)
import Main.Web.Auth
import Main.Web.Utils
import StravaGear.Config (Conf, parseConf)
import StravaGear.Config.Sample (sampleConfig)
import StravaGear.Database.Schema (migrateAll)
import StravaGear.Report (componentReport, bikesReport)
import StravaGear.Sync (SyncStravaRes, syncConfig, syncStrava)


main :: IO ()
main = do
    [readMaybe -> Just port] <- getArgs
    withStrive Config.clientId Config.clientSecret $
        withBaseUri Config.url $
        withAuthCookies $
        run port app

app :: (CfgAuth) => Application
app = serveWithContext api ctx serve
  where
    ctx = cookieAuthHandler :. EmptyContext

type Api
    =    HomeApi
    :<|> StaticApi
    :<|> LoginApi
    :<|> "test" :> TestApi
    :<|> "api" :> CookieAuthProtect :> ApiApi

api :: Proxy Api
api = Proxy

apiLink :: (IsElem e Api, HasLink e) => Proxy e -> MkLink e
apiLink = safeLink api

serve :: (CfgAuth) => Server Api
serve
    =    serveHome
    :<|> serveStatic
    :<|> serveLogin
    :<|> serveTest
    :<|> serveApi
  where
    serveLogin = serveStriveLogin api homeApi

type HomeApi = Get '[HTML] Markup

homeApi :: Proxy HomeApi
homeApi = Proxy

serveHome :: CfgBaseUri => Server HomeApi
serveHome = pure homePage

homePage :: CfgBaseUri => Markup
homePage = html (H.title "strava-gear: Home") $ do
    H.section H.! HA.class_ "header" $ do
        H.h1 "strava-gear"
        H.p "Lorem ipsum..."
        loginButton

loginButton :: CfgBaseUri => Markup
loginButton = H.a H.! HA.href link $ imgConnect
  where
    link = H.stringValue . showAbsoluteUri $ apiLink loginApi Nothing

stravaLink :: Markup
stravaLink = H.a H.! HA.href link $ imgLogo
  where
    link = "https://strava.github.io/api/"

githubLink :: Markup
githubLink = H.a H.! HA.href link $ "github" -- TODO
  where
    link = "https://github.com/liskin/strava-gear"

imgLogo, imgConnect :: Markup
imgLogo = H.img H.! HA.src "static/api_logo_pwrdBy_strava_horiz_light.png"
imgConnect = H.img H.! HA.src "static/btn_strava_connectwith_orange.png"

type StaticApi = "static" :> Raw

serveStatic :: Server StaticApi
serveStatic = serveDirectory "static"

serveDirectory :: FilePath -> Server Raw
serveDirectory =
    staticApp . defaultWebAppSettings . addTrailingPathSeparator

type TestApi = CookieAuthProtect :> Get '[HTML] Markup

serveTest :: Server TestApi
serveTest Auth{..} = pure . H.docTypeHtml $ do
    H.head (H.title "test")
    H.body $ do
        H.p $ "token: " <> H.text authToken
        H.p $ "athlete: " <> H.string (show authAthlete)

type ApiApi
    =    "config" :> ConfigApi
    :<|> "sync" :> SyncApi
    :<|> "report" :> ReportApi

serveApi :: Auth -> Server ApiApi
serveApi auth
    =    serveConfigApi auth
    :<|> serveSyncApi auth
    :<|> serveReportApi auth

type ConfigApi =
         ConfigPostApi
    :<|> "check" :> ConfigCheckApi
    :<|> "sample" :> ConfigSampleApi

serveConfigApi :: Auth -> Server ConfigApi
serveConfigApi auth
    =    serveConfigPostApi auth
    :<|> serveConfigCheckApi auth
    :<|> serveConfigSampleApi auth

-- TODO: get stored config
type ConfigPostApi
    =  ReqBody '[PlainText] Text
    :> PostNoContent '[PlainText] NoContent

serveConfigPostApi :: Auth -> Server ConfigPostApi
serveConfigPostApi auth conf =
    withConfig conf $ \cfg ->
        withClient auth $ \_client ->
            -- TODO: store config somewhere
            pure NoContent <* syncConfig cfg

type ConfigCheckApi
    =  ReqBody '[PlainText] Text
    :> PostNoContent '[PlainText] NoContent

serveConfigCheckApi :: Auth -> Server ConfigCheckApi
serveConfigCheckApi _auth conf =
    withConfig conf $ const $ pure NoContent

withConfig :: Text -> ([Conf] -> Handler a) -> Handler a
withConfig c f = either e f $ parseConf c
  where
    e msg = throwError err400{ errBody = toS msg }

type ConfigSampleApi = Get '[PlainText] Text

serveConfigSampleApi :: Auth -> Server ConfigSampleApi
serveConfigSampleApi auth =
    withClient auth $ const sampleConfig

type SyncApi
    =  QueryFlag "full"
    :> Post '[JSON] SyncStravaRes

serveSyncApi :: Auth -> Server SyncApi
serveSyncApi auth = withClient auth . syncStrava
    -- TODO: store last sync time and perhaps perform full sync once a week
    -- or something?

type ReportApi
    =    "components" :> ReportComponentsApi
    :<|> "bikes" :> ReportBikesApi

serveReportApi :: Auth -> Server ReportApi
serveReportApi auth
    =    serveReportComponentsApi auth
    :<|> serveReportBikesApi auth

-- TODO: additional content types
type ReportComponentsApi = Get '[HTML] Markup

serveReportComponentsApi :: Auth -> Server ReportComponentsApi
serveReportComponentsApi auth =
    tablePage "Components report" <$> withClient auth (const componentReport)

-- TODO: additional content types
type ReportBikesApi = Get '[HTML] Markup

serveReportBikesApi :: Auth -> Server ReportBikesApi
serveReportBikesApi auth =
    tablePage "Bikes report" <$> withClient auth (const bikesReport)

tableToMarkup :: Tab.Table String String String -> Markup
tableToMarkup = H.preEscapedToMarkup . renderHtml
    . Tab.render H2.stringToHtml H2.stringToHtml H2.stringToHtml
  where
    renderHtml h = foldr (.) identity (render h) "\n"
    render = map (H2.renderHtml' 0) . H2.getHtmlElements

tablePage :: Text -> Tab.Table String String String -> Markup
tablePage title tab = html (H.title $ H.text $ "strava-gear: " <> title) $ do
    H.section H.! HA.class_ "header" $ do
        H.h1 $ H.text title

    tableToMarkup tab

html :: Markup -> Markup -> Markup
html h container = H.docTypeHtml $ do
    H.head $ do
        h
        H.base H.! HA.href (H.stringValue Config.url)
        H.meta H.! HA.name "viewport" H.! HA.content "width=device-width, initial-scale=1"
        H.link H.! HA.rel "stylesheet" H.! HA.href "static/normalize.css"
        H.link H.! HA.rel "stylesheet" H.! HA.href "static/skeleton.css"
        H.link H.! HA.rel "stylesheet" H.! HA.href "static/custom.css"

    H.body $ H.div H.! HA.class_ "container" $ do
        container

        H.hr
        H.div H.! HA.class_ "u-pull-left" $ stravaLink
        H.div H.! HA.class_ "u-pull-right" $ githubLink

withClient :: Auth -> (S.Client -> SqlPersistM a) -> Handler a
withClient Auth{..} f =
    liftIO . runSqlite ("athlete_" <> show authAthlete <> ".sqlite") $ do
        runMigration migrateAll
        liftIO (S.buildClient (Just authToken)) >>= f
