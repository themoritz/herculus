{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Views.Cell
  ( cell_
  , CellProps (..)
  ) where

import Control.Monad (when)
import Control.Lens hiding (view)
import Control.DeepSeq

import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack, pack, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Foldable

import GHC.Generics

import Text.Read (readMaybe)

import React.Flux

import Lib.Model.Column
import Lib.Model.Cell
import Lib.Model.Types
import Lib.Types

import Store
import Views.Foreign
import Views.Common
import Views.Combinators

type CellCallback a = a -> [SomeStoreAction]

data CellProps = CellProps
  { cellColId   :: !(Id Column)
  , cellRecId   :: !(Id Record)
  , cellColumn  :: !Column
  , cellContent :: !CellContent
  }

data CellState = CellState (Map (Id Column, Id Record) Bool)

data CellAction = Toggle (Id Column, Id Record)
  deriving (Generic, NFData)

cellStore :: ReactStore CellState
cellStore = mkStore (CellState Map.empty)

instance StoreData CellState where
  type StoreAction CellState = CellAction
  transform (Toggle (c, r)) (CellState m) = pure $
    CellState $ m & at (c, r) . non False %~ not

cell_ :: CellProps -> ReactElementM eh ()
cell_ !props = view cell props mempty

cell :: ReactView CellProps
cell = defineControllerView "cell" cellStore $ \(CellState m) CellProps{..} ->
  case cellContent of
    CellError msg ->
      elemText $ "Error: " <> msg
    CellValue val ->
      let open = fromMaybe False $ Map.lookup (cellColId, cellRecId) m
          inpType = columnInputType cellColumn
          datType = columnDataType cellColumn
          needsEx = needsExpand (datType, inpType)
          inline = value_ (if needsEx then Compact else Full)
                          inpType
                          datType
                          val
                          (\v -> dispatch $ CellSetValue cellColId cellRecId v)
      in
      case needsEx of
        True -> cldiv_ "compactWrapper" $ do
          cldiv_ "compact" inline
          cldiv_ "expand" $ do
            faButton_ "expand" [SomeStoreAction cellStore $ Toggle (cellColId, cellRecId)]
            when open $ cldiv_ "expanded" $ cldiv_ "body" $
              value_ Full
                     inpType
                     datType
                     val
                     (\v -> dispatch $ CellSetValue cellColId cellRecId v)
        False -> inline

needsExpand :: (DataType, InputType) -> Bool
needsExpand = \case
  (DataBool,     _)             -> False
  (DataString,   _)             -> False
  (DataNumber,   _)             -> False
  (DataTime,     _)             -> False
  (DataRecord _, ColumnInput)   -> False
  (DataRecord _, ColumnDerived) -> True
  (DataList _,   _)             -> True
  (DataMaybe _,  _)             -> False

data Mode
  = Compact
  | Full

--

value_ :: Mode -> InputType -> DataType -> Value -> CellCallback Value
       -> ReactElementM eh ()
value_ !mode !inpType !datType !val !cb =
  view value (mode, inpType, datType, val, cb) mempty

value :: ReactView (Mode, InputType, DataType, Value, CellCallback Value)
value = defineView "value" $ \(mode, inpType, datType, val, cb) -> case datType of
  DataBool -> case val of
    VBool b -> cellBool_ mode inpType b (cb . VBool)
    _ -> mempty
  DataString -> case val of
    VString s -> cellString_ mode inpType s (cb . VString)
    _ -> mempty
  DataNumber -> case val of
    VNumber n -> cellNumber_ mode inpType n (cb . VNumber)
    _ -> mempty
  DataTime -> case val of
    VTime t -> cellTime_ mode inpType t (cb . VTime)
    _ -> mempty
  DataRecord t -> case val of
    VRecord mr -> cellRecord_ mode inpType mr t (cb . VRecord)
    _ -> mempty
  DataList t -> case val of
    VList vs -> cellList_ mode inpType t vs (cb . VList)
    _ -> mempty
  DataMaybe t -> case val of
    VMaybe v -> cellMaybe_ mode inpType t v (cb . VMaybe)
    _ -> mempty

cellBool_ :: Mode -> InputType -> Bool -> CellCallback Bool
          -> ReactElementM ViewEventHandler ()
cellBool_ !mode !inpType !b !cb =
  view cellBool (mode, inpType, b, cb) mempty

cellBool :: ReactView (Mode, InputType, Bool, CellCallback Bool)
cellBool = defineView "cellBool" $ \(mode, inpType, b, cb) ->
  case (mode, inpType) of
    (Full, ColumnInput) ->
      input_
        [ "type" $= "checkbox"
        , "checked" @= b
        , onChange $ \_ -> cb (not b)
        ]
    _ ->
      elemText $ if b then "True" else "False"

cellString_ :: Mode -> InputType -> Text -> CellCallback Text
            -> ReactElementM ViewEventHandler ()
cellString_ !mode !inpType !s !cb =
  view cellString (mode, inpType, s, cb) mempty

cellString :: ReactView (Mode, InputType, Text, CellCallback Text)
cellString = defineView "cellString" $ \(mode, inpType, s, cb) ->
  case (mode, inpType) of
    (Full, ColumnInput) ->
      editBox_ EditBoxProps
        { editBoxValue = s
        , editBoxPlaceholder = ""
        , editBoxClassName = "string"
        , editBoxShow = id
        , editBoxValidator = Just
        , editBoxOnSave = cb
        }
    _ ->
      elemText s

cellNumber_ :: Mode -> InputType -> Number -> CellCallback Number
            -> ReactElementM ViewEventHandler ()
cellNumber_ !mode !inpType !n !cb =
  view cellNumber (mode, inpType, n, cb) mempty

cellNumber :: ReactView (Mode, InputType, Number, CellCallback Number)
cellNumber = defineView "cellNumber" $ \(mode, inpType, n, cb) ->
  case (mode, inpType) of
    (Full, ColumnInput) ->
      editBox_ EditBoxProps
        { editBoxValue = n
        , editBoxPlaceholder = "0"
        , editBoxClassName = "number"
        , editBoxShow = pack . show
        , editBoxValidator = \s -> Number <$> (readMaybe $ unpack s)
        , editBoxOnSave = cb
        }
    _ ->
      elemString $ show n

cellTime_ :: Mode -> InputType -> Time -> CellCallback Time
          -> ReactElementM ViewEventHandler ()
cellTime_ !mode !inpType !t !cb =
  view cellTime (mode, inpType, t, cb) mempty

cellTime :: ReactView (Mode, InputType, Time, CellCallback Time)
cellTime = defineStatefulView "cellTime" Nothing $
  \invalidTmp (mode, inpType, t, cb) -> case (mode, inpType) of
    (Full, ColumnInput) -> do
      let curT = fromMaybe (formatTime "%F" t) invalidTmp
          handleChange :: Text -> StatefulViewEventHandler (Maybe Text)
          handleChange x _ = case parseTime "%F" x of
            Nothing -> ([], Just $ Just x)
            Just t' -> (cb t', Just Nothing)
      datePicker_ $ DatePickerProps
        { datePickerSelected = curT
        , datePickerPlaceholderText = "Please select a day"
        , datePickerDateFormat = "YYYY-MM-DD"
        , datePickerOnChange = handleChange
        , datePickerClassNames = [("invalid", isJust invalidTmp)]
        }
    _ ->
      elemText $ formatTime "%F" t

cellRecord_ :: Mode -> InputType -> Maybe (Id Record) -> Id Table
            -> CellCallback (Maybe (Id Record))
            -> ReactElementM ViewEventHandler ()
cellRecord_ !mode !inpType !mr !t !cb =
  view cellRecord (mode, inpType, mr, t, cb) mempty

cellRecord :: ReactView ( Mode, InputType, Maybe (Id Record), Id Table
                        , CellCallback (Maybe (Id Record))
                        )
cellRecord = defineControllerView "cellRecord" store $
  \st (mode, inpType, mr, t, cb) -> do
    onDidMount_ (dispatch $ CacheRecordsGet t) mempty
    let records = fromMaybe Map.empty $ st ^. stateCacheRecords . at t
        showPairs = intercalate ", " . map (\(n, v) -> n <> ": " <> (pack . show) v) . Map.elems
    case mode of
      Compact -> case mr of
        Nothing -> "impossible: invalid record in derived cell"
        Just r -> case Map.lookup r records of
          Nothing -> mempty
          Just fields -> elemText $ showPairs fields
      Full -> case inpType of
        ColumnInput ->
          select_
            [ "defaultValue" &= case mr of
                Nothing -> ""
                Just r  -> show r
            , classNames [("invalid", isNothing mr)]
            , onChange $ \evt ->
                let val = target evt "value"
                in if val == ""
                      then cb Nothing
                      else case readMaybe val of
                             Nothing -> []
                             Just newId -> cb $ Just newId
            ] $ do when (isNothing mr) $ option_
                     [ "value" $= ""
                     ] $ ""
                   for_ (Map.toList records) $ \(i, record) -> option_
                     [ "value" &= show i
                     ] $ elemText $ showPairs record
        ColumnDerived -> case mr of
          Nothing -> "impossible: invalid record in derived cell"
          Just r -> case Map.lookup r records of
            Nothing -> mempty
            Just fields ->
              ul_ $ for_ fields $ \(k, content) -> li_ $ do
                elemText $ k <> ": " <> (pack . show) content


cellList_ :: Mode -> InputType -> DataType -> [Value] -> CellCallback [Value]
          -> ReactElementM ViewEventHandler ()
cellList_ !mode !inpType !datType !vs !cb =
  view cellList (mode, inpType, datType, vs, cb) mempty

cellList :: ReactView (Mode, InputType, DataType, [Value], CellCallback [Value])
cellList = defineView "cellList" $ \(mode, inpType, datType, vs, cb) ->
  case mode of
    Compact -> do
      elemShow $ length vs
      " elems: "
      ul_ ["className" $= "compact"] $ for_ (take 4 vs) $ \v ->
        li_ $ value_ mode inpType datType v (const [])
    Full -> case inpType of
      ColumnInput -> do
        button_
          [ onClick $ \_ _ ->
              let CellValue newV = defaultContentPure datType
              in  cb (newV : vs)
          ] "New"
        ul_ $ for_ (zip [0..] vs) $ \(i, v) -> li_ $ do
          let listMod ind x xs = let (h, t) = splitAt ind xs in h <> (x : drop 1 t)
              listDel ind xs   = let (h, t) = splitAt ind xs in h <> drop 1 t
          value_ mode inpType datType v (\nv -> cb (listMod i nv vs))
          button_
            [ onClick $ \_ _ -> cb (listDel i vs)
            ] "Del"
      ColumnDerived ->
        ul_ $ for_ vs $ \v -> li_ $ value_ mode inpType datType v (const [])

cellMaybe_ :: Mode -> InputType -> DataType -> Maybe Value
           -> CellCallback (Maybe Value)
           -> ReactElementM ViewEventHandler ()
cellMaybe_ !mode !inpType !datType !mVal !cb =
  view cellMaybe (mode, inpType, datType, mVal, cb) mempty

cellMaybe :: ReactView (Mode, InputType, DataType, Maybe Value, CellCallback (Maybe Value))
cellMaybe = defineView "cellMaybe" $ \(mode, inpType, datType, mVal, cb) ->
  case (mode, inpType) of
    (Full, ColumnInput) -> case mVal of
      Nothing -> cldiv_ "maybe nothing" $ do
        let CellValue new = defaultContentPure datType
        cldiv_ "button" $ button_
          [ onClick $ \_ _ -> cb $ Just new
          ] $ faIcon_ "plus-circle"
        cldiv_ "content" "Nothing"
      Just val -> cldiv_ "maybe just" $ do
        cldiv_ "button" $ button_
          [ onClick $ \_ _ -> cb Nothing
          ] $ faIcon_ "minus-circle"
        cldiv_ "content" $
          value_ mode inpType datType val (cb . Just)
    _ -> case mVal of
      Nothing -> "Nothing"
      Just val -> value_ mode inpType datType val (cb . Just)
