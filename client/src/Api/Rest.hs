{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.Rest where

import Control.Monad

import Data.Proxy
import Data.Text
import Data.Monoid

import Reflex.Dom

import Servant.API
import Servant.Reflex

import Lib.Types as Lib
import Lib.Model
import Lib.Model.Types
import Lib.Model.Column
import Lib.Model.Cell
import Lib.Api.Rest

type Arg t a = Behavior t (Either String a)
type Res t m a = Event t () -> m (Event t (ReqResult a))

data RestApi t m = MonadWidget t m => RestApi
  { projectCreate :: Arg t Project -> Res t m (Id Project)
  , projectList   :: Res t m [Entity Project]
  , tableCreate   :: Arg t Table -> Res t m (Id Table)
  , tableList     :: Arg t (Id Project) -> Res t m [Entity Table]
  , tableData     :: Arg t (Id Table) -> Res t m [(Id Column, Id Record, CellContent)]
  , columnCreate  :: Arg t (Id Table) -> Res t m (Id Column)
  , columnSetName :: Arg t (Id Column) -> Arg t Text -> Res t m ()
  , columnSetDataType :: Arg t (Id Column) -> Arg t DataType -> Res t m ()
  , columnSetInputType :: Arg t (Id Column) -> Arg t InputType -> Res t m ()
  , columnSetSourceCode :: Arg t (Id Column) -> Arg t Text -> Res t m (Maybe CompiledCode)
  , columnList    :: Arg t (Id Table) -> Res t m [Entity Column]
  , recordCreate  :: Arg t (Id Table) -> Res t m (Id Record)
  , recordList    :: Arg t (Id Table) -> Res t m [Entity Record]
  , cellSet       :: Arg t (Id Column) -> Arg t (Id Record) -> Arg t Lib.Value -> Res t m ()
  }

api :: forall t m. MonadWidget t m => RestApi t m
api =
  let (project :<|> table :<|> column :<|> record :<|> cell) =
            client (Proxy :: Proxy Routes)
                   (Proxy :: Proxy m)
                   (constDyn (BasePath "/"))
      (projectC :<|> projectL) = project
      (tableC :<|> tableL :<|> tableD) = table
      (columnC :<|> columnSN :<|> columnSDT :<|> columnSIT :<|> columnSSC :<|> columnL) = column
      (recordC :<|> recordL) = record
      cellS = cell
  in RestApi
       { projectCreate = projectC
       , projectList   = projectL
       , tableCreate   = tableC
       , tableList     = tableL
       , tableData     = tableD
       , columnCreate  = columnC
       , columnSetName = columnSN
       , columnSetDataType = columnSDT
       , columnSetInputType = columnSIT
       , columnSetSourceCode = columnSSC
       , columnList    = columnL
       , recordCreate  = recordC
       , recordList    = recordL
       , cellSet       = cellS
       }

loader :: MonadWidget t m => Res t m a -> Event t () -> m (Event t a)
loader call trigger = el "div" $ do
  trigger' <- delay 0.001 trigger
  result <- call trigger'
  spin <- holdDyn False $ leftmost
    [ False <$ result
    , True <$ trigger'
    ]
  spinAttrs <- flip mapDyn spin $ \v ->
    "class" =: "spinner"
    <> "style" =: (if v then "display:inherit" else "display:none")
  elDynAttr "div" spinAttrs $ pure ()
  let success = fmapMaybe reqSuccess result
  dynText =<< holdDyn "" (leftmost
                [ fmapMaybe reqFailure result
                , "" <$ success
                ])
  pure success

loader' :: MonadWidget t m => Res t m a -> Event t () -> m ()
loader' call trigger = void $ loader call trigger
