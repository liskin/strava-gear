{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module StravaGear.Database.Schema where

import Protolude

import Data.Time (UTCTime)
import Database.Persist (Unique)
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistUpperCase
    , share
    , sqlSettings
    )

import StravaGear.Types
    ( BikeText
    , ComponentText
    , HashTagText
    , RoleText
    )


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
    Bike
        name Text
        stravaId BikeText
        UniqueBike stravaId
        deriving Eq Ord Show

    Component
        uniqueId ComponentText
        UniqueComponent uniqueId
        name Text
        initialSeconds Int -- seconds
        initialMeters Double -- meters
        deriving Eq Ord Show

    ComponentRole
        name RoleText
        UniqueRole name
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
        UniqueActivity stravaId
        name Text
        startTime UTCTime
        movingTime Int -- seconds
        distance Double -- meters
        gearId BikeText Maybe
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
        name HashTagText
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
