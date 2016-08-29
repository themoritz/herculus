{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Cell
  ( cell_
  , CellProps (..)
  ) where

import Control.DeepSeq
import Control.Monad (when)
import Control.Lens hiding (view)

import Data.Proxy
import Data.Maybe
import Data.Monoid
import Data.Text (Text, unpack, pack, intercalate)
import Data.Typeable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Foldable

import GHC.Generics

import Text.Read (readMaybe)

import React.Flux
import React.Flux.Addons.Servant

import Lib.Api.Rest
import Lib.Model.Column
import Lib.Model.Cell
import Lib.Model.Types
import Lib.Types

import Store
import Views.Foreign

type CellCallback a = a -> [SomeStoreAction]
type CellEventHandler = ViewEventHandler

data CellProps = CellProps
  { cpColId :: !(Id Column)
  , cpRecId :: !(Id Record)
  , cpColumn :: !Column
  , cpContent :: !CellContent
  }

data CellState = CellState
  { _csTmpValues :: Map (Id Column, Id Record) Value
  , _csRecordCache :: Map (Id Table) (Map (Id Record) [(Text, CellContent)])
  }

makeLenses ''CellState

data CellAction
  = SetTmpValue (Id Column) (Id Record) Value
  | UnsetTmpValue (Id Column) (Id Record)
  | GetRecords (Id Table)
  | SetRecords (Id Table) [(Id Record, [(Text, CellContent)])]
  deriving (Typeable, Generic, NFData)

instance StoreData CellState where
  type StoreAction CellState = CellAction
  transform action st = case action of

    SetTmpValue c r v -> pure $
      st & csTmpValues . at (c, r) .~ Just v

    UnsetTmpValue c r -> pure $
      st & csTmpValues . at (c, r) .~ Nothing

    GetRecords t -> case st ^. csRecordCache . at t of
      Just _  -> pure st
      Nothing -> do
        request api (Proxy :: Proxy RecordListWithData) t $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right res -> pure $ dispatchCell $ SetRecords t res
        pure st

    SetRecords t recs -> pure $
      st & csRecordCache . at t .~ Just (Map.fromList recs)

cellStore :: ReactStore CellState
cellStore = mkStore $ CellState Map.empty Map.empty

dispatchCell :: CellAction -> [SomeStoreAction]
dispatchCell a = [SomeStoreAction cellStore a]

cell_ :: CellProps -> ReactElementM eh ()
cell_ !props = view cell props mempty

cell :: ReactView CellProps
cell = defineControllerView "cell" cellStore $ \st props ->
  case cpContent props of
    CellError msg ->
      elemText $ "Error: " <> msg
    CellValue val -> do
      let col = cpColumn props
          c = cpColId props
          r = cpRecId props
          mTmpVal = st ^. csTmpValues . at (c, r)
      value_ (columnInputType col)
             (columnDataType col)
             (fromMaybe val mTmpVal)
             (\v -> [SomeStoreAction cellStore $ SetTmpValue c r v])
      when (isJust mTmpVal) $ do
        button_
          [ onClick $ \_ _ -> case mTmpVal of
              Nothing -> []
              Just tmpVal ->
                [ SomeStoreAction store $ CellSetValue c r tmpVal
                , SomeStoreAction cellStore $ UnsetTmpValue c r
                ]
          ] "Ok"
        button_
          [ onClick $ \_ _ ->
              [ SomeStoreAction cellStore $ UnsetTmpValue c r
              ]
          ] "Cancel"

--

value_ :: InputType -> DataType -> Value -> CellCallback Value
       -> ReactElementM CellEventHandler ()
value_ !inpType !datType !val !cb = view value (inpType, datType, val, cb) mempty

value :: ReactView (InputType, DataType, Value, CellCallback Value)
value = defineView "value" $ \(inpType, datType, val, cb) -> case datType of
  DataBool -> case val of
    VBool b -> cellBool_ inpType b (cb . VBool)
    _ -> mempty
  DataString -> case val of
    VString s -> cellString_ inpType s (cb . VString)
    _ -> mempty
  DataNumber -> case val of
    VNumber n -> cellNumber_ inpType n (cb . VNumber)
    _ -> mempty
  DataTime -> case val of
    VTime t -> cellTime_ inpType t (cb . VTime)
    _ -> mempty
  DataRecord t -> case val of
    VRecord r -> cellRecord_ inpType r t (cb . VRecord)
    _ -> mempty
  DataList t -> case val of
    VList vs -> cellList_ inpType t vs (cb . VList)
    _ -> mempty
  DataMaybe t -> case val of
    VMaybe v -> cellMaybe_ inpType t v (cb . VMaybe)
    _ -> mempty

cellBool_ :: InputType -> Bool -> CellCallback Bool
          -> ReactElementM CellEventHandler ()
cellBool_ !inpType !b !cb = view cellBool (inpType, b, cb) mempty

cellBool :: ReactView (InputType, Bool, CellCallback Bool)
cellBool = defineView "cellBool" $ \(inpType, b, cb) -> case inpType of
  ColumnInput ->
    input_
      [ "type" $= "checkbox"
      , "checked" @= b
      , onChange $ \_ -> cb (not b)
      ]
  ColumnDerived ->
    elemText $ if b then "True" else "False"

cellString_ :: InputType -> Text -> CellCallback Text
            -> ReactElementM CellEventHandler ()
cellString_ !inpType !s !cb = view cellString (inpType, s, cb) mempty

cellString :: ReactView (InputType, Text, CellCallback Text)
cellString = defineView "cellString" $ \(inpType, s, cb) -> case inpType of
  ColumnInput ->
    input_
      [ "value" &= s
      , onChange $ \evt -> cb (target evt "value")
      ]
  ColumnDerived ->
    elemText s

cellNumber_ :: InputType -> Number -> CellCallback Number
            -> ReactElementM CellEventHandler ()
cellNumber_ !inpType !n !cb = view cellNumber (inpType, n, cb) mempty

cellNumber :: ReactView (InputType, Number, CellCallback Number)
cellNumber = defineStatefulView "cellNumber" True $ \valid (inpType, n, cb) ->
  case inpType of
    ColumnInput -> do
      let parseNumber s = Number <$> (readMaybe $ unpack s)
      input_
        [ "value" &= show n
        , classNames [ ("invalid", not valid) ]
        , onChange $ \evt _ -> case parseNumber $ target evt "value" of
            Nothing -> ([], Just False)
            Just n' -> (cb n', Just True)
        ]
    ColumnDerived ->
      elemString $ show n

cellTime_ :: InputType -> Time -> CellCallback Time
          -> ReactElementM CellEventHandler ()
cellTime_ !inpType !t !cb = view cellTime (inpType, t, cb) mempty

cellTime :: ReactView (InputType, Time, CellCallback Time)
cellTime = defineView "cellTime" $ \(inpType, t, cb) -> case inpType of
  ColumnInput ->
    input_
      [ "value" &= formatTime "%F" t
      , onChange $ \evt -> case parseTime "%F" $ target evt "value" of
          Nothing -> []
          Just t' -> cb t'
      ]
  ColumnDerived ->
    elemString $ show t

cellRecord_ :: InputType -> Id Record -> Id Table -> CellCallback (Id Record)
            -> ReactElementM CellEventHandler ()
cellRecord_ !inpType !r !t !cb = view cellRecord (inpType, r, t, cb) mempty

cellRecord :: ReactView (InputType, Id Record, Id Table, CellCallback (Id Record))
cellRecord = defineControllerView "cellRecord" cellStore $ \st (inpType, r, t, cb) -> do
  onDidMount_ (dispatchCell $ GetRecords t) mempty
  let records = fromMaybe Map.empty $ st ^. csRecordCache . at t
      showPairs = intercalate ", " . map (\(n, v) -> n <> ": " <> (pack . show) v)
  case inpType of
    ColumnInput ->
      select_
        [ "defaultValue" &= show r
        , onChange $ \evt -> case readMaybe $ target evt "value" of
            Nothing -> []
            Just newId -> cb newId
        ] $ for_ (Map.toList records) $ \(i, record) -> option_
              [ "value" &= show r
              ] $ elemText $ showPairs record
    ColumnDerived -> case Map.lookup r records of
      Nothing -> mempty
      Just fields ->
        ul_ $ for_ fields $ \(k, content) -> li_ $ do
          elemText $ k <> ": " <> (pack . show) content


cellList_ :: InputType -> DataType -> [Value] -> CellCallback [Value]
          -> ReactElementM CellEventHandler ()
cellList_ !inpType !datType !vs !cb = view cellList (inpType, datType, vs, cb) mempty

cellList :: ReactView (InputType, DataType, [Value], CellCallback [Value])
cellList = defineView "cellList" $ \(inpType, datType, vs, cb) -> case inpType of
  ColumnInput -> do
    button_
      [ onClick $ \_ _ ->
          let CellValue newV = defaultContent datType
          in cb (newV : vs)
      ] "New"
    ul_ $ for_ (zip [0..] vs) $ \(i, v) -> li_ $ do
      let listMod ind x xs = let (h, t) = splitAt ind xs in h <> (x : drop 1 t)
          listDel ind xs   = let (h, t) = splitAt ind xs in h <> drop 1 t
      value_ inpType datType v (\nv -> cb (listMod i nv vs))
      button_
        [ onClick $ \_ _ -> cb (listDel i vs)
        ] "Del"
  ColumnDerived ->
    ul_ $ for_ vs $ \v -> li_ $ value_ inpType datType v (const [])

cellMaybe_ :: InputType -> DataType -> Maybe Value -> CellCallback (Maybe Value)
           -> ReactElementM CellEventHandler ()
cellMaybe_ !inpType !datType !mVal !cb = view cellMaybe (inpType, datType, mVal, cb) mempty

cellMaybe :: ReactView (InputType, DataType, Maybe Value, CellCallback (Maybe Value))
cellMaybe = defineView "cellMaybe" $ \(inpType, datType, mVal, cb) -> case inpType of
  ColumnInput -> case mVal of
    Nothing -> do
      let CellValue new = defaultContent datType
      button_
        [ onClick $ \_ _ -> cb (Just new)
        ] "Add"
    Just val -> do
      button_
        [ onClick $ \_ _ -> cb Nothing
        ] "Del"
      value_ inpType datType val (cb . Just)
  ColumnDerived -> case mVal of
    Nothing -> "Nothing"
    Just val -> value_ inpType datType val (cb . Just)
