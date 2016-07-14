{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.Rest where

import Data.Proxy
import Data.Text
import Data.Monoid

import Reflex.Dom

import Servant.API
import Servant.Reflex

import Lib
import Lib.Api.Rest

type Arg t a = Behavior t (Either String a)
type Res t m a = Event t () -> m (Event t (ReqResult a))

data RestApi t m = MonadWidget t m => RestApi
  { projectCreate :: Arg t Text -> Res t m (Id Project)
  , projectList   :: Res t m [Project]
  , tableCreate   :: Arg t TableCreate -> Res t m (Id Table)
  , tableList     :: Arg t (Id Project) -> Res t m [Table]
  , tableData     :: Arg t (Id Table) -> Res t m [(Id Record, [(Id Column, Text)])]
  , columnCreate  :: Arg t ColumnCreate -> Res t m (Id Column)
  , columnList    :: Arg t (Id Table) -> Res t m [(Id Column, Text, ColumnType)]
  , recordCreate  :: Arg t RecordCreate -> Res t m (Id Record)
  , cellSet       :: Arg t CellSet -> Res t m ()
  }

api :: forall t m. MonadWidget t m => RestApi t m
api =
  let (project :<|> table :<|> column :<|> record :<|> cell) =
            client (Proxy :: Proxy Routes)
                   (Proxy :: Proxy m)
                   (constDyn (BasePath "/"))
      (projectC :<|> projectL) = project
      (tableC :<|> tableL :<|> tableD) = table
      (columnC :<|> columnL) = column
      recordC = record
      cellS = cell
  in RestApi
       { projectCreate = projectC
       , projectList   = projectL
       , tableCreate   = tableC
       , tableList     = tableL
       , tableData     = tableD
       , columnCreate  = columnC
       , columnList    = columnL
       , recordCreate  = recordC
       , cellSet       = cellS
       }

loader :: MonadWidget t m => Res t m a -> Event t () -> m (Event t a)
loader call trigger = el "div" $ do
  result <- call trigger
  spin <- holdDyn False $ leftmost
    [ False <$ result
    , True <$ trigger
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
