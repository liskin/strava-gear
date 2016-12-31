{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StravaGear.Types
    ( BikeText(..)
    , ComponentText(..)
    , HashTagText(..)
    , RoleText(..)
    )
  where

import Protolude

import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)


newtype BikeText = BikeText { fromBike :: Text }
  deriving (Eq, Ord, Show, IsString, PersistField, PersistFieldSql)

newtype ComponentText = ComponentText { fromComponent :: Text }
  deriving (Eq, Ord, Show, IsString, PersistField, PersistFieldSql)

newtype HashTagText = HashTagText { fromHashTag :: Text }
  deriving (Eq, Ord, Show, IsString, PersistField, PersistFieldSql)

newtype RoleText = RoleText { fromRole :: Text }
  deriving (Eq, Ord, Show, IsString, PersistField, PersistFieldSql)
