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
  , gridColumnWidths :: [Int]
  , gridColumnCount  :: Int
  , gridRowHeights   :: [Int]
  , gridRowCount     :: Int
  }

grid_ :: GridProps -> ReactElementM eh ()
grid_ !props =  do
  let getArgs :: Value -> ReturnProps GridRenderArgs
      getArgs v = let (Just args) = parseMaybe parseJSON v in ReturnProps args
  foreign_ "Grid"
    [ callbackViewWithProps "cellRenderer" (gridCellRenderer props) getArgs
    , "columnWidths" &= gridColumnWidths props
    , "columnCount" &= gridColumnCount props
    , "rowHeights" &= gridRowHeights props
    , "rowCount" &= gridRowCount props
    ] mempty

-- react-codemirror

data CodemirrorProps func = CodemirrorProps
  { codemirrorMode     :: Text
  , codemirrorTheme    :: Text
  , codemirrorValue    :: Text
  , codemirrorOnChange :: func -- JSString -> a
  }

codemirror_ :: (CallbackFunction eh func, Typeable func)
            => CodemirrorProps func -> ReactElementM eh ()
codemirror_ !props =
  foreign_ "Codemirror"
    [ nestedProperty "options"
      [ "mode" &= codemirrorMode props
      , "theme" &= codemirrorTheme props
      , "lineWrapping" &= True
      , "lineNumbers" &= True
      ]
    , "value" &= codemirrorValue props
    , callback "onChange" $ codemirrorOnChange props
    ] mempty

-- react-datepicker

data DatePickerProps func = DatePickerProps
  { datePickerSelected :: Text
  , datePickerPlaceholderText :: Text
  , datePickerDateFormat :: Text
  , datePickerOnChange :: func -- JSString -> a
  , datePickerClassNames :: [(Text, Bool)]
  }

datePicker_ :: (CallbackFunction eh func, Typeable func)
            => DatePickerProps func -> ReactElementM eh ()
datePicker_ !props =
  foreign_ "DatePicker"
    [ "selected" &= datePickerSelected props
    , "placeholderText" &= datePickerPlaceholderText props
    , "dateFormat" &= datePickerDateFormat props
    , callback "onChange" $ datePickerOnChange props
    , classNames $ datePickerClassNames props
    ] mempty

-- own: OnLoad

onDidMount_ :: (CallbackFunction eh func, Typeable func)
            => func -> ReactElementM eh a -> ReactElementM eh a
onDidMount_ !f =
  foreign_ "OnDidMount"
    [ callback "onDidMount" f
    ]
