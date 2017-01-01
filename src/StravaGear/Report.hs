module StravaGear.Report
    ( bikesReport
    , componentReport
    )
  where

import Protolude hiding (from, on)

import Data.Maybe (fromMaybe)
import Data.String (String)
import Text.Printf (printf)

import Data.Time (defaultTimeLocale, formatTime)
import Database.Esqueleto
    ( InnerJoin(InnerJoin)
    , SqlExpr
    , Value(Value)
    , (^.), (+.), (==.)
    , asc
    , from
    , groupBy
    , just
    , max_
    , min_
    , on
    , orderBy
    , select
    , sub_select
    , sum_
    , where_
    )
import Database.Persist (Entity(Entity), PersistField)
import Database.Persist.Sql (SqlPersistM)
import qualified Text.Tabular as Tab
    ( Header(Group, Header)
    , Properties(DoubleLine, NoLine, SingleLine)
    , Table(Table)
    )

import StravaGear.Database.Schema
import StravaGear.Database.Utils (castToPersistValue)
import StravaGear.Types (fromComponent, fromRole)


componentReport :: SqlPersistM (Tab.Table String String String)
componentReport = do
    res <- select $
        from $ \(c `InnerJoin` ac `InnerJoin` a `InnerJoin` r) -> do
            on (c ^. ComponentId ==. ac ^. ActivityComponentComponent)
            on (ac ^. ActivityComponentActivity ==. a ^. ActivityId)
            on (ac ^. ActivityComponentRole ==. r ^. ComponentRoleId)
            groupBy (c ^. ComponentId)
            orderBy
                [ asc $ r ^. ComponentRoleName
                , asc $ c ^. ComponentUniqueId ]
            let iniTime = just $ c ^. ComponentInitialSeconds
                iniDist = just $ c ^. ComponentInitialMeters
                sumMovingTime = sum_ $ a ^. ActivityMovingTime
                sumDist = sum_ $ a ^. ActivityDistance
                firstUsage = min_ $ a ^. ActivityStartTime
                lastUsage = max_ $ a ^. ActivityStartTime
            return
                ( r, c
                , iniTime +. sumMovingTime
                , iniDist +. sumDist
                , firstUsage, lastUsage )
    let ids = [ toS $ fromRole $ componentRoleName r
              | (Entity _ r, _, _, _, _, _) <- res ]
        rh = Tab.Group Tab.NoLine (map Tab.Header ids)
        ch = Tab.Group Tab.DoubleLine
            [ Tab.Group Tab.SingleLine [Tab.Header "id", Tab.Header "name"]
            , Tab.Group Tab.SingleLine [Tab.Header "first", Tab.Header "last"]
            , Tab.Group Tab.SingleLine [Tab.Header "time", Tab.Header "distance"]
            ]
        timeFormat = formatTime defaultTimeLocale "%F"
        tab = [ [ toS $ fromComponent $ componentUniqueId c
                , toS $ componentName c
                , maybe "" timeFormat firstUsage, maybe "" timeFormat lastUsage
                , niceTime, niceDist ]
              | ( _, Entity _ c
                , Value time', Value dist'
                , Value firstUsage, Value lastUsage
                ) <- res
              , let time = fromMaybe 0 time'
              , let dist = fromMaybe 0 dist'
              , let niceTime = printf "%.1f" ((fromIntegral time :: Double) / 3600) ++ " hours"
              , let niceDist = printf "%.0f" ((dist :: Double) / 1000) ++ " km" ]
    return $ Tab.Table rh ch tab

bikesReport :: SqlPersistM (Tab.Table String String String)
bikesReport = do
    res <- select $ from $ \(b `InnerJoin` lt `InnerJoin` r `InnerJoin` c) -> do
        on $ b ^. BikeId ==. lt ^. LongtermBikeComponentBike
        on $ lt ^. LongtermBikeComponentRole ==. r ^. ComponentRoleId
        on $ lt ^. LongtermBikeComponentComponent ==. c ^. ComponentId
        groupBy (b ^. BikeId, r ^. ComponentRoleId)
        orderBy
            [ asc $ b ^. BikeName
            , asc $ r ^. ComponentRoleName
            , asc $ c ^. ComponentUniqueId ]
        let iniTime = just $ c ^. ComponentInitialSeconds
            iniDist = just $ c ^. ComponentInitialMeters
        let sub :: (PersistField a) => (SqlExpr (Entity Activity) -> SqlExpr (Value a)) -> SqlExpr (Value a)
            sub f = sub_select $ from $ \(a `InnerJoin` ac) -> do
                on $ ac ^. ActivityComponentActivity ==. a ^. ActivityId
                where_ $ c ^. ComponentId ==. ac ^. ActivityComponentComponent
                return $ f a
            sumMovingTime = sub $ \a -> sum_ $ a ^. ActivityMovingTime
            sumDist = sub $ \a -> sum_ $ a ^. ActivityDistance
        return
            ( castToPersistValue (max_ $ lt ^. LongtermBikeComponentStartTime)
            , b, r, c
            , iniTime +. sumMovingTime
            , iniDist +. sumDist
            )
    let rh = Tab.Group Tab.NoLine $ map Tab.Header $
            [ toS $ bikeName b
            | (_, Entity _ b, _, _, _, _) <- res ]
        ch = Tab.Group Tab.DoubleLine
            [ Tab.Group Tab.SingleLine [Tab.Header "role", Tab.Header "id", Tab.Header "name"]
            , Tab.Group Tab.SingleLine [Tab.Header "time", Tab.Header "distance"]
            ]
        tab = [ [ toS $ fromRole $ componentRoleName r
                , toS $ fromComponent $ componentUniqueId c
                , toS $ componentName c, niceTime, niceDist ]
              | (_, _, Entity _ r, Entity _ c, Value time', Value dist') <- res
              , let time = fromMaybe 0 time'
              , let dist = fromMaybe 0 dist'
              , let niceTime = printf "%.1f" ((fromIntegral time :: Double) / 3600) ++ " hours"
              , let niceDist = printf "%.0f" ((dist :: Double) / 1000) ++ " km" ]
    return $ Tab.Table rh ch tab
