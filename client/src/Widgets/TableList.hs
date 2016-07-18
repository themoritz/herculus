{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.TableList
  ( TableListConfig
  , tableListConfig_newTable
  , tableListConfig_loadProject
  , TableList
  , tableList_selectTable
  , tableList
  ) where

import Control.Lens

import Data.Default
import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map

import Reflex.Dom

import Lib
import Lib.Api.Rest

import Api.Rest (loader, api)
import qualified Api.Rest as Api


data TableListConfig t = TableListConfig
  { _tableListConfig_newTable :: Event t Table
  , _tableListConfig_loadProject :: Event t (Id Project)
  }

makeLenses ''TableListConfig

instance Reflex t => Default (TableListConfig t) where
  def = TableListConfig
    { _tableListConfig_newTable = never
    , _tableListConfig_loadProject = never
    }

data TableList t = TableList
  { _tableList_selectTable :: Event t (Id Table)
  }

makeLenses ''TableList

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

tableList :: MonadWidget t m => TableListConfig t -> m (TableList t)
tableList (TableListConfig newTable loadProject) = elClass "div" "container" $ mdo
  el "h5" $ text "Tables"
  createTable <- elClass "div" "row" $ do
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
               Right (TableCreate pri name) -> Just (Table i pri name)) tableArg newTbl
  projectArg <- hold (Left "") (Right <$> loadProject)
  listResult <- loader (Api.tableList api projectArg) (() <$ loadProject)
  state <- foldDyn update (State Map.empty Nothing) $ mergeList
    [ New <$> newTable
    , New <$> createTable
    , SetProject <$> loadProject
    , Set <$> listResult
    ]
  tbls <- mapDyn tables state
  tableSelect <- elClass "div" "row" $
    el "ul" $ list tbls $ \name ->
      el "li" $ do
        (tbl, _) <- elAttr' "a" ("href" =: "#") $
          dynText =<< mapDyn unpack name
        pure $ domEvent Click tbl
  TableList . switchPromptlyDyn <$> mapDyn (leftmost . map (\(k, e) -> k <$ e) . Map.toList) tableSelect
