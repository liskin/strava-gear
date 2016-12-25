{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module StravaGear.Database.Utils
    ( UpsertResult(..)
    , castToPersistValue
    , changedEntities
    , keptEntities
    , syncEntities
    , syncEntitiesDel
    , wipeInsertMany
    )
  where

import Data.List ((\\))

import Data.List.Split (chunksOf)
import Database.Esqueleto
    ( SqlExpr
    , Value
    , (^.)
    , delete
    , deleteKey
    , from
    , in_
    , valList
    , where_
    )
import Database.Esqueleto.Internal.Sql
    (veryUnsafeCoerceSqlExprValue)
import Database.Persist
    ( Entity(Entity)
    , Key
    , PersistEntity
    , PersistEntityBackend
    , PersistValue
    , Unique
    , entityKey
    , entityVal
    , insertBy
    , insertMany_
    , persistIdField
    , replaceUnique
    , selectKeysList
    )
import Database.Persist.Sql
    ( SqlBackend
    , SqlPersistM
    )


-- | Wipe all (old = Nothing) or some (old = Just some) records and insert
-- new ones.
wipeInsertMany :: forall rec.
                  (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
               => Maybe [Key rec] -> [rec] -> SqlPersistM ()
wipeInsertMany old recs = do
    delete $ from $ \row -> case old of
        Nothing -> return ()
        Just olds -> where_ $ row ^. persistIdField `in_` valList olds
    -- FIXME: https://github.com/yesodweb/persistent/issues/527
    mapM_ insertMany_ (chunksOf 100 recs)

-- | Avoids the performance penalty of parsing into UTCTime
castToPersistValue :: SqlExpr (Value a) -> SqlExpr (Value PersistValue)
castToPersistValue = veryUnsafeCoerceSqlExprValue

data UpsertResult rec
    = UpsertAdded (Entity rec)
    | UpsertDeleted (Key rec)
    | UpsertUpdated (Entity rec)
    | UpsertNoop (Entity rec)

uprepsert :: (Eq rec, Eq (Unique rec),
              PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
          => rec -> SqlPersistM (UpsertResult rec)
uprepsert rec =
    insertBy rec >>= \case
        Left dup -> do
            if entityVal dup /= rec
                then do
                    Nothing <- replaceUnique (entityKey dup) rec
                    return $ UpsertUpdated dup
                else
                    return $ UpsertNoop dup
        Right key ->
            return $ UpsertAdded $ Entity key rec

delEntities :: (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
            => [UpsertResult rec] -> SqlPersistM [UpsertResult rec]
delEntities res = do
    allKeys <- selectKeysList [] []
    let delKeys = allKeys \\ map entityKey (keptEntities res)
    mapM (\k -> deleteKey k >> return (UpsertDeleted k)) delKeys

keptEntities :: [UpsertResult rec] -> [Entity rec]
keptEntities = concatMap $ \case
    UpsertAdded   e -> [e]
    UpsertDeleted _ -> []
    UpsertUpdated e -> [e]
    UpsertNoop    e -> [e]

changedEntities :: [UpsertResult rec] -> [Key rec]
changedEntities = concatMap $ \case
    UpsertAdded   e -> [entityKey e]
    UpsertDeleted k -> [k]
    UpsertUpdated e -> [entityKey e]
    UpsertNoop    _ -> []

syncEntities :: (Eq rec, Eq (Unique rec),
                 PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
             => [rec] -> SqlPersistM [UpsertResult rec]
syncEntities = mapM uprepsert

syncEntitiesDel :: (Eq rec, Eq (Unique rec),
                    PersistEntity rec, PersistEntityBackend rec ~ SqlBackend)
                => [rec] -> SqlPersistM [UpsertResult rec]
syncEntitiesDel recs = do
    syncRes <- syncEntities recs
    delRes <- delEntities syncRes
    return $ syncRes ++ delRes
