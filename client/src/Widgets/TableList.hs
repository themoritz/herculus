{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.TableList
  ( TableList (..)
  , tableList
  ) where

import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map

import Reflex.Dom

import Lib
import Lib.Api.Rest

import Api.Rest (loader, api)
import qualified Api.Rest as Api

data State = State
  { tables :: Map (Id Table) Text
  , projectId :: Maybe (Id Project)
  }

data Action
  = New Table
  | SetProject (Id Project)
  | Set [Table]

update :: NonEmpty Action -> State -> State
update actions state = foldl (flip go) state actions
  where
    go (New (Table i pri name)) (State tbls prjId)
      | Just pri == prjId = State (Map.insert i name tbls) prjId
      | otherwise         = State tbls prjId
    go (SetProject prjId) (State tbls _) = State tbls (Just prjId)
    go (Set tbls) (State _ prjId)        = State (Map.fromList $
        map (\(Table i _ name) -> (i, name)) $
        filter (\t -> Just (tableProjectId t) == prjId) tbls) prjId

data TableList t = TableList
  { _tableList_selectTable :: Event t (Id Table)
  }

tableList :: MonadWidget t m
          => Event t Table -> Event t (Id Project)
          -> m (TableList t)
tableList newTable loadProject = divClass "container" $ mdo
  el "h5" $ text "Tables"
  createTable <- divClass "row" $ do
    name <- (fmap pack . current . _textInput_value) <$> textInput def
    create <- button "Create"
    let tableArg = do
          (State _ mProjId) <- current state
          case mProjId of
            Just projId -> Right <$> (TableCreate <$> pure projId <*> name)
            Nothing     -> pure $ Left ""
    newTbl <- loader (Api.tableCreate api tableArg) create
    pure $ fmapMaybe id $ attachWith (\crTbl i -> case crTbl of
               Left _ -> Nothing
               Right (TableCreate pri n) -> Just (Table i pri n)) tableArg newTbl
  projectArg <- hold (Left "") (Right <$> loadProject)
  listResult <- loader (Api.tableList api projectArg) (() <$ loadProject)
  state <- foldDyn update (State Map.empty Nothing) $ mergeList
    [ New <$> newTable
    , New <$> createTable
    , SetProject <$> loadProject
    , Set <$> listResult
    ]
  tbls <- mapDyn tables state
  tableSelect <- divClass "row" $
    el "ul" $ list tbls $ \name ->
      el "li" $ do
        (tbl, _) <- elAttr' "a" ("href" =: "#") $
          dynText =<< mapDyn unpack name
        pure $ domEvent Click tbl
  TableList . switchPromptlyDyn <$> mapDyn (leftmost . map (\(k, e) -> k <$ e) . Map.toList) tableSelect
