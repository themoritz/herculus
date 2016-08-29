{-# LANGUAGE FlexibleContexts #-}

module Views.Foreign where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Data.Typeable

import           React.Flux

-- react-virtualized: Grid

data GridRenderArgs = GridRenderArgs Int Int Bool -- col row scrolling

instance FromJSON GridRenderArgs where
  parseJSON (Object o) = GridRenderArgs <$> o .: "columnIndex"
                                        <*> o .: "rowIndex"
                                        <*> o .: "isScrolling"
  parseJSON _ = mempty

data GridProps = GridProps
  { gridCellRenderer :: ReactView GridRenderArgs
  , gridColumnWidth  :: Int
  , gridColumnCount  :: Int
  , gridRowHeight    :: Int
  , gridRowCount     :: Int
  }

grid_ :: GridProps -> ReactElementM ViewEventHandler ()
grid_ !props = view grid props mempty

grid :: ReactView GridProps
grid = defineView "grid" $ \props -> do
  let getArgs :: Value -> ReturnProps GridRenderArgs
      getArgs v = let (Just args) = parseMaybe parseJSON v in ReturnProps args
  foreign_ "Grid"
    [ callbackViewWithProps "cellRenderer" (gridCellRenderer props) getArgs
    , "columnWidth" &= gridColumnWidth props
    , "columnCount" &= gridColumnCount props
    , "rowHeight" &= gridRowHeight props
    , "rowCount" &= gridRowCount props
    ] mempty

-- react-ace

data CodemirrorProps = CodemirrorProps
  { codemirrorMode     :: Text
  , codemirrorTheme    :: Text
  , codemirrorValue    :: Text
  , codemirrorOnChange :: Text -> [SomeStoreAction]
  }

codemirror_ :: CodemirrorProps -> ReactElementM ViewEventHandler ()
codemirror_ !props = view codemirror props mempty

codemirror :: ReactView CodemirrorProps
codemirror = defineView "codemirror" $ \props ->
  foreign_ "Codemirror"
    [ nestedProperty "options"
      [ "mode" &= codemirrorMode props
      , "theme" &= codemirrorTheme props
      ]
    , "value" &= codemirrorValue props
    , callback "onChange" $ codemirrorOnChange props
    ] mempty

-- own: OnLoad

onDidMount_ :: (CallbackFunction ViewEventHandler func, Typeable func)
        => func -> ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
onDidMount_ !f = view onDidMount f

onDidMount :: (CallbackFunction ViewEventHandler func, Typeable func)
       => ReactView func
onDidMount = defineView "onDidMount" $ \f ->
  foreign_ "OnDidMount"
    [ callback "onDidMount" f
    ] childrenPassedToView
