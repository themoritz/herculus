{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Views.Table where

import           Control.Lens        hiding (view)

import           Control.Monad       (when)
import           Data.Foldable       (for_)
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IntMap
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Monoid         ((<>))

import           React.Flux
import           React.Flux.Internal (toJSString)

import           Lib.Api.Rest        (Command (..))
import           Lib.Model
import           Lib.Model.Auth      (SessionKey)
import           Lib.Model.Cell      (CellContent)
import           Lib.Model.Column
import           Lib.Model.Project   (ProjectClient)
import           Lib.Model.Row       (Row)
import           Lib.Model.Table
import           Lib.Types

import qualified Project
import           Store               (dispatchProject, dispatchProjectCommand)
import           Views.Cell
import           Views.Column
import           Views.Combinators
import           Views.Foreign
import           Views.ReportCell
import           Views.Row

data TableGridProps = TableGridProps
  { _cells            :: Map Project.Coords CellContent
  , _colByIndex       :: IntMap (Id Column, Column)
  , _rowByIndex       :: IntMap (Id Row, Row)
  , _tableId          :: Id Table
  , _projectId        :: Id ProjectClient
  , _showNewColDialog :: Bool
  , _sKey             :: SessionKey
  , _tables           :: TableNames
  }

makeLenses ''TableGridProps

posdiv_ :: String -> Int -> Int -> Int -> Int
        -> ReactElementM eh a -> ReactElementM eh a
posdiv_ key left top width height = viewWithSKey posdiv (toJSString key) ()
  where
    posdiv = defineView "posdiv" $ \() ->
      div_ [ "className" $= "Grid__cell"
           , style
             [ ("left",   toJSString $ show left <> "px")
             , ("top",    toJSString $ show top <> "px")
             , ("width",  toJSString $ show width <> "px")
             , ("height", toJSString $ show height <> "px")
             ]
           ] childrenPassedToView

tableGrid_ :: TableGridProps -> ReactElementM eh ()
tableGrid_ !props = view tableGrid props mempty

tableGrid :: ReactView TableGridProps
tableGrid = defineView "tableGrid" $ \props -> do
  let renderCell :: Id Column -> Column -> Id Row -> ReactElementM eh ()
      renderCell c col r = cldiv_ "cell" $ case col ^. columnKind of
        ColumnReport rep ->
          reportCell_ $ ReportCellProps (props ^. sKey) c r rep
        ColumnData dat ->
          case Map.lookup (Project.Coords c r) $ props ^. cells of
            Just content -> dataCell_ $ DataCellProps c r dat content
            Nothing      -> mempty

  cldiv_ "Grid" $ do
    let headHeight = 54 :: Int
        cellHeight = 37 :: Int
        addRowHeight = 37 :: Int
        delRowWidth = 37 :: Int
        cellWidth = 230 :: Int
        addColWidth = 37 :: Int
    -- Origin
    posdiv_ "-" 0 0 delRowWidth headHeight $ cldiv_ "origin" mempty
    -- Delete rows
    let rows = IntMap.toList (props ^. rowByIndex)
        cols = IntMap.toList (props ^. colByIndex)
    for_ rows $ \(y, (rowId, row')) ->
      posdiv_ (show rowId)
              (0 :: Int) (y * cellHeight + headHeight)
              delRowWidth cellHeight $ cldiv_ "record" $
                row_ (Entity rowId row')
    -- Columns
    for_ cols $ \(x, (columnId, column')) -> do
      -- Head
      posdiv_ (show columnId)
              (x * cellWidth + delRowWidth) 0
              cellWidth headHeight $
                column_ (props ^. projectId) (props ^. sKey)
                        (props ^. tables) (Entity columnId column')
      -- Cells
      for_ rows $ \(y, (rowId, _)) ->
        posdiv_ (show columnId <> show rowId)
                (x * cellWidth + delRowWidth) (y * cellHeight + headHeight)
                cellWidth cellHeight $ renderCell columnId column' rowId
    -- Add row
    posdiv_ "addrow"
            0 (length rows * cellHeight + headHeight)
            delRowWidth addRowHeight $ cldiv_ "record-new" $
              faButton_ "plus-circle" $ dispatchProjectCommand $
                                        CmdRowCreate (props ^. tableId)
    -- Add col
    posdiv_ "addcol"
           (length cols * cellWidth + delRowWidth) 0
           addColWidth headHeight $ cldiv_ "column-new" $ do
             let isOpen = props ^. showNewColDialog
                 toggle = dispatchProject Project.TableToggleNewColumnDialog
             faButton_ "plus-circle" (if isOpen then [] else toggle)
             when isOpen $ onClickOutside_ (\(_ :: String) -> toggle) $
               cldiv_ "small-menu" $ do
                 menuItem_ "columns" "New data column" $
                   toggle <> dispatchProjectCommand (CmdDataColCreate (props ^. tableId))
                 menuItem_ "file-text" "New report column" $
                   toggle <> dispatchProjectCommand (CmdReportColCreate (props ^. tableId))
