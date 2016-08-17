{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.TableList
  ( TableList (..)
  , tableList
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map

import Reflex.Dom

import Lib.Types
import Lib.Model
import Lib.Model.Types

import Misc

import Api.Rest (loader, api)
import qualified Api.Rest as Api

data State = State
  { tables :: Map (Id Table) Text
  }

data Action
  = New (Entity Table)
  | Set [Entity Table]

update :: Id Project -> NonEmpty Action -> State -> State
update projId actions state = foldl (flip go) state actions
  where
    go (New (Entity i (Table pri name))) (State tbls)
      | pri == projId = State (Map.insert i name tbls)
      | otherwise     = State tbls
    go (Set tbls) _   = State (Map.fromList $
        map (\(Entity i (Table _ name)) -> (i, name)) $
        filter (\(Entity _ t) -> tableProjectId t == projId) tbls)

data TableList t = TableList
  { _tableList_selectTable :: Event t (Id Table)
  }

tableList :: MonadWidget t m
          => Id Project -> Event t (Entity Table)
          -> m (TableList t)
tableList projId newTable = divClass "container" $ mdo
  el "h5" $ text "Tables"
  createTable <- divClass "row" $ do
    name <- _textInput_value <$> textInput def
    create <- button "Create"
    let tableArg = Right <$> (Table <$> pure projId <*> name)
    newTbl <- loader (Api.tableCreate api tableArg) create
    pure $ attachWith (\n i -> Entity i (Table projId n)) (current name) newTbl
  postBuild <- getPostBuild
  listResult <- loader (Api.tableList api (constDyn $ Right projId)) postBuild
  state <- foldDyn (update projId) (State Map.empty) $ mergeList
    [ New <$> newTable
    , New <$> createTable
    , Set <$> listResult
    ]
  tableSelect <- divClass "row" $
    el "ul" $ list (tables <$> state) $ \name ->
      el "li" $ do
        (tbl, _) <- elAttr' "a" ("href" =: "#") $
          dynText name
        pure $ domEvent Click tbl
  pure $ TableList $ fst <$> dynMapEvents tableSelect
