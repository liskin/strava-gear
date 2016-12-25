{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module StravaGear.Database.Schema where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Unique)
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistUpperCase
    , share
    , sqlSettings
    )


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    Bike
        name Text
        stravaId Text
        StravaBikeId stravaId
        deriving Eq Ord Show

    Component
        uniqueId Text
        UniqueId uniqueId
        name Text
        initialSeconds Int -- seconds
        initialMeters Double -- meters
        deriving Eq Ord Show

    ComponentRole
        name Text
        UniqueName name
        deriving Eq Ord Show

    LongtermBikeComponent
        component ComponentId
        bike BikeId
        role ComponentRoleId
        startTime UTCTime
        endTime UTCTime Maybe
        UniqueLongtermBikeComponent component bike role startTime
        deriving Eq Ord Show

    Activity
        stravaId Int
        StravaActivityId stravaId
        name Text
        startTime UTCTime
        movingTime Int -- seconds
        distance Double -- meters
        gearId Text Maybe
        deriving Eq Ord Show

    ActivityComponent
        activity ActivityId
        component ComponentId
        role ComponentRoleId
        deriving Eq Ord Show

    HashTagBikeComponent
        tag HashTagId
        component ComponentId
        role ComponentRoleId
        startTime UTCTime
        endTime UTCTime Maybe
        UniqueHashTagBikeComponent tag component role startTime
        deriving Eq Ord Show

    HashTag
        name Text
        UniqueHashTag name
        deriving Eq Ord Show

    ActivityHashTag
        activity ActivityId
        tag HashTagId
        UniqueActivityHashTag activity tag
        deriving Eq Ord Show
|]

deriving instance Eq (Unique Bike)
deriving instance Eq (Unique Component)
deriving instance Eq (Unique ComponentRole)
deriving instance Eq (Unique Activity)
deriving instance Eq (Unique LongtermBikeComponent)
deriving instance Eq (Unique HashTagBikeComponent)
deriving instance Eq (Unique HashTag)
