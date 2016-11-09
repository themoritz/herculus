{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Auth.Permission where

import           Control.Lens
import           Control.Monad.Except (throwError)
import           Database.MongoDB     ((=:))

import           Lib.Model            (Entity)
import           Lib.Model.Auth       (User)
import           Lib.Model.Column     (Column, columnTableId)
import           Lib.Model.Project    (Project)
import           Lib.Model.Record     (Record (..))
import           Lib.Model.Table      (Table, tableProjectId)
import           Lib.Types            (Id, toObjectId)
import           Monads               (AppError (ErrForbidden), MonadHexl,
                                       getById', getOneByQuery)

permissionProject :: MonadHexl m => Id User -> Id Project -> m ()
permissionProject userId projectId = do
  let query =
        [ "owner" =: toObjectId userId
        , "_id"   =: toObjectId projectId
        ]
  getOneByQuery query >>= \case
    Left _ -> throwError $ ErrForbidden "You don't have access to this project."
    Right (_ :: Entity Project) -> pure ()

permissionTable :: MonadHexl m => Id User -> Id Table -> m ()
permissionTable userId tableId = do
  table <- getById' tableId
  permissionProject userId $ table ^. tableProjectId

permissionColumn :: MonadHexl m => Id User -> Id Column -> m ()
permissionColumn userId columnId = do
  column <- getById' columnId
  permissionTable userId $ column ^. columnTableId

permissionRecord :: MonadHexl m => Id User -> Id Record -> m ()
permissionRecord userId recordId = do
  record <- getById' recordId
  permissionTable userId $ recordTableId record
