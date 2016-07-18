{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.Table
  ( TableConfig (..)
  , table
  ) where

import Data.Monoid
import Data.Default
import           Data.Map        (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Reflex.Dom

import Lib

import Api.Rest (loader, api)
import qualified Api.Rest as Api
import Misc

import Widgets.Column

data TableConfig t = TableConfig
  { _tableConfig_loadTable :: Event t (Id Table)
  }

instance Reflex t => Default (TableConfig t) where
  def = TableConfig
    { _tableConfig_loadTable = never
    }

type State = Map (Id Column) (Text, ColumnType)

data Action
  = Set [(Id Column, Text, ColumnType)]
  | Add (Id Column) Text ColumnType

update :: Action -> State -> State
update (Set cs) _ = Map.fromList $ map (\(i, n, t) -> (i, (n, t))) cs
update (Add i n t) m = Map.insert i (n, t) m

table :: MonadWidget t m => TableConfig t -> m ()
table (TableConfig loadTable) = el "div" $ mdo
  tableIdArg <- hold (Left "") (Right <$> loadTable)
  columnsRes <- loader (Api.columnList api tableIdArg) $
                       () <$ loadTable
  state <- foldDyn update Map.empty $ leftmost
    [ Set <$> columnsRes
    , (\i -> Add i "" (ColumnInput DataString)) <$> newColId
    ]
  _ <- el "table" $ el "tbody" $
    -- TODO: Use `listWithKey` without holding values for elements
    listWithKeyNoHold state $ \columnId initial valE ->
      el "tr" $ el "td" $
        column columnId $ ColumnConfig
          { _columnConfig_setType = snd <$> valE
          , _columnConfig_setName = fst <$> valE
          , _columnConfig_initialType = snd initial
          , _columnConfig_initialName = fst initial
          }
  add <- button "Add"
  newColId <- loader (Api.columnCreate api tableIdArg) add
  pure ()
