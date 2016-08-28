{-# LANGUAGE FlexibleContexts #-}

module Views.Foreign where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Data.Typeable

import           React.Flux

-- react-virtualized: AutoSizer

data AutoSizerRenderArgs = AutoSizerRenderArgs Int Int -- width height

instance FromJSON AutoSizerRenderArgs where
  parseJSON (Object o) = AutoSizerRenderArgs <$> o .: "width"
                                             <*> o .: "height"
  parseJSON _ = mempty

autoSizer_ :: (AutoSizerRenderArgs -> ReactElementM ViewEventHandler ())
           -> ReactElementM ViewEventHandler ()
autoSizer_ renderer =
  let rendererView = defineView "rendererView" $ renderer
  in view autoSizer rendererView mempty

autoSizer :: ReactView (ReactView AutoSizerRenderArgs)
autoSizer = defineView "autoSizer" $ \renderer -> do
  let getArgs :: Value -> ReturnProps AutoSizerRenderArgs
      getArgs v = let (Just args) = parseMaybe parseJSON v in ReturnProps args
  foreign_ "AutoSizer"
    [ callbackViewWithProps "renderChildren" renderer getArgs
    ] mempty

-- react-virtualized: Grid

data GridRenderArgs = GridRenderArgs Int Int Bool -- col row scrolling

instance FromJSON GridRenderArgs where
  parseJSON (Object o) = GridRenderArgs <$> o .: "columnIndex"
                                        <*> o .: "rowIndex"
                                        <*> o .: "isScrolling"
  parseJSON _ = mempty

data GridProps = GridProps
  { gridCellRenderer :: ReactView GridRenderArgs
  , gridWidth        :: Int
  , gridHeight       :: Int
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
    , "width" &= gridWidth props
    , "height" &= gridHeight props
    , "columnWidth" &= gridColumnWidth props
    , "columnCount" &= gridColumnCount props
    , "rowHeight" &= gridRowHeight props
    , "rowCount" &= gridRowCount props
    ] mempty

-- react-ace

data AceProps func = AceProps
  { aceName     :: Text
  , aceMode     :: Text
  , aceTheme    :: Text
  , aceWidth    :: Text
  , aceHeight   :: Text
  , aceOnChange :: Text -> func
  }

ace_ :: (CallbackFunction ViewEventHandler func, Typeable func)
     => AceProps func -> ReactElementM ViewEventHandler ()
ace_ !props = view ace props mempty

ace :: (CallbackFunction ViewEventHandler func, Typeable func)
    => ReactView (AceProps func)
ace = defineView "ace" $ \props ->
  foreign_ "AceEditor"
    [ "name" &= aceName props
    , "mode" &= aceMode props
    , "theme" &= aceTheme props
    , "width" &= aceWidth props
    , "height" &= aceHeight props
    , callback "onChange" $ aceOnChange props
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
