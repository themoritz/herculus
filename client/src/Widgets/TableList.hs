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
  , projectId :: Maybe (Id Project)
  }

data Action
  = New (Entity Table)
  | SetProject (Id Project)
  | Set [Entity Table]

update :: NonEmpty Action -> State -> State
update actions state = foldl (flip go) state actions
  where
    go (New (Entity i (Table pri name))) (State tbls prjId)
      | Just pri == prjId = State (Map.insert i name tbls) prjId
      | otherwise         = State tbls prjId
    go (SetProject prjId) (State tbls _) = State tbls (Just prjId)
    go (Set tbls) (State _ prjId)        = State (Map.fromList $
        map (\(Entity i (Table _ name)) -> (i, name)) $
        filter (\(Entity _ t) -> Just (tableProjectId t) == prjId) tbls) prjId

data TableList t = TableList
  { _tableList_selectTable :: Event t (Id Table)
  }

tableList :: MonadWidget t m
          => Event t (Entity Table) -> Event t (Id Project)
          -> m (TableList t)
tableList newTable loadProject = divClass "container" $ mdo
  el "h5" $ text "Tables"
  createTable <- divClass "row" $ do
    name <- _textInput_value <$> textInput def
    create <- button "Create"
    let tableArg = do
          (State _ mProjId) <- state
          case mProjId of
            Just projId -> Right <$> (Table <$> pure projId <*> name)
            Nothing     -> pure $ Left ""
    newTbl <- loader (Api.tableCreate api tableArg) create
    pure $ fmapMaybe id $ attachPromptlyDynWith (\crTbl i -> case crTbl of
               Left _ -> Nothing
               Right (Table pri n) -> Just (Entity i (Table pri n))) tableArg newTbl
  projectArg <- holdDyn (Left "") (Right <$> loadProject)
  listResult <- loader (Api.tableList api projectArg) (() <$ loadProject)
  state <- foldDyn update (State Map.empty Nothing) $ mergeList
    [ New <$> newTable
    , New <$> createTable
    , SetProject <$> loadProject
    , Set <$> listResult
    ]
  tableSelect <- divClass "row" $
    el "ul" $ list (tables <$> state) $ \name ->
      el "li" $ do
        (tbl, _) <- elAttr' "a" ("href" =: "#") $
          dynText name
        pure $ domEvent Click tbl
  pure $ TableList $ fst <$> dynMapEvents tableSelect
