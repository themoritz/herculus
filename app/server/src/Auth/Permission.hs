{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Auth.Permission where

import           Lib.Prelude

import           Control.Lens
import           Database.MongoDB     ((=:))

import           Lib.Model            (Entity)
import           Lib.Model.Auth       (User)
import           Lib.Model.Column     (Column, columnTableId)
import           Lib.Model.Project    (Project)
import           Lib.Model.Row
import           Lib.Model.Table      (Table, tableProjectId)
import           Lib.Types            (Id, toObjectId)
import           Monads               (AppError (ErrForbidden), MonadHexl,
                                       getById', getOneByQuery)

permissionProject' :: MonadHexl m => Id User -> Id Project -> m Bool
permissionProject' userId projectId = do
  let query =
        [ "owner" =: toObjectId userId
        , "_id"   =: toObjectId projectId
        ]
  getOneByQuery query >>= \case
    Left _ -> pure False
    Right (_ :: Entity Project) -> pure True

permissionProject :: MonadHexl m => Id User -> Id Project -> m ()
permissionProject userId projectId =
  permissionProject' userId projectId >>= \case
    False -> throwError $ ErrForbidden "You don't have access to this project."
    True -> pure ()

permissionTable :: MonadHexl m => Id User -> Id Table -> m ()
permissionTable userId tableId = do
  table <- getById' tableId
  permissionProject userId $ table ^. tableProjectId

permissionColumn :: MonadHexl m => Id User -> Id Column -> m ()
permissionColumn userId columnId = do
  column <- getById' columnId
  permissionTable userId $ column ^. columnTableId

permissionRow :: MonadHexl m => Id User -> Id Row -> m ()
permissionRow userId rowId = do
  row <- getById' rowId
  permissionTable userId $ row ^. rowTableId
