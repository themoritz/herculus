{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Column where

import           Control.Lens hiding (view)
import           React.Flux

import           Control.Monad       (forM_, when)
import           Control.Applicative ((<|>))
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Data.Maybe          (fromMaybe)
import           Data.Proxy
import           Data.Text           (Text, pack)
import           Data.Tuple          (swap)
import qualified Data.Text           as Text
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)
import           Control.DeepSeq     (NFData)
import           Text.Read           (readMaybe)

import           React.Flux.Addons.Servant (request)
import           Lib.Api.Rest (TableListGlobal)
import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Types (Table (..))
import           Lib.Types

import           Store
import           Views.Foreign

type TableCache = Map (Id Table) Text

data ColumnState = ColumnState
  { _csTmpDataType  :: Map (Id Column) DataType
  , _csTmpInputType :: Map (Id Column) InputType
  , _csTmpSource    :: Map (Id Column) Text
  , _csTableCache   :: TableCache
  }

makeLenses ''ColumnState

data ColumnAction
  = ColumnSetTmpDataType (Id Column) DataType
  | ColumnUnsetTmpDataType (Id Column)
  | ColumnSetTmpInputType (Id Column) InputType
  | ColumnUnsetTmpInputType (Id Column)
  | ColumnSetTmpSource (Id Column) Text
  | ColumnUnsetTmpSource (Id Column)
  | ColumnGetTableCache
  | ColumnSetTableCache (Map (Id Table) Text)
  deriving (Typeable, Generic, NFData)

instance StoreData ColumnState where
  type StoreAction ColumnState = ColumnAction
  transform action st = case action of
    ColumnSetTmpDataType i dt ->
      pure $ st & csTmpDataType . at i .~ Just dt
    ColumnUnsetTmpDataType i ->
      pure $ st & csTmpDataType . at i .~ Nothing
    ColumnSetTmpInputType i it ->
      pure $ st & csTmpInputType . at i .~ Just it
    ColumnUnsetTmpInputType i ->
      pure $ st & csTmpInputType . at i .~ Nothing
    ColumnSetTmpSource i s ->
      pure $ st & csTmpSource . at i .~ Just s
    ColumnUnsetTmpSource i ->
      pure $ st & csTmpSource . at i .~ Nothing

    ColumnGetTableCache -> do
      when (Map.null $ st ^. csTableCache) $
        request api (Proxy :: Proxy TableListGlobal) $ pure . \case
          Left (_, e) -> dispatch $ GlobalSetError $ pack e
          Right ts    -> dispatchColumn $ ColumnSetTableCache $ toTableMap ts
      pure st
    ColumnSetTableCache m ->
      pure $ st & csTableCache .~ m

columnStore :: ReactStore ColumnState
columnStore = mkStore $ ColumnState Map.empty Map.empty Map.empty Map.empty

dispatchColumn :: ColumnAction -> [SomeStoreAction]
dispatchColumn x = [SomeStoreAction columnStore x]

toTableMap :: [Entity Table] -> TableCache
toTableMap = Map.fromList . map (\(Entity i Table{..}) -> (i, tableName))

type SelBranchCallback = DataType -> [SomeStoreAction]
type SelTableCallback  = Id Table -> [SomeStoreAction]
type SelDtEventHandler = StatefulViewEventHandler (Maybe Branch)

data Branch
  = BBool
  | BString
  | BNumber
  | BTime
  | BRecord
  | BList
  | BMaybe
  deriving (Eq, Ord, Generic, NFData)

toBranch :: DataType -> Branch
toBranch = \case
  DataBool     -> BBool
  DataString   -> BString
  DataNumber   -> BNumber
  DataTime     -> BTime
  DataRecord _ -> BRecord
  DataList   _ -> BList
  DataMaybe  _ -> BMaybe

branches :: Map Text Branch
branches = Map.fromList
  [ ("Bool"  , BBool  )
  , ("String", BString)
  , ("Number", BNumber)
  , ("Time"  , BTime  )
  , ("Record", BRecord)
  , ("List"  , BList  )
  , ("Maybe" , BMaybe )
  ]

subType :: DataType -> Maybe DataType
subType (DataList  a) = Just a
subType (DataMaybe a) = Just a
subType _             = Nothing

recordTableId :: DataType -> Maybe (Id Table)
recordTableId (DataRecord tId) = Just tId
recordTableId _                = Nothing

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineControllerView "column" columnStore $ \st c@(Entity i _) -> do
  columnTitle_ c
  let tables = st ^. csTableCache
  selDatatype_ c tables
  let mDt = st ^. csTmpDataType . at i
  button_
      [ onClick $ \_ _ -> case mDt of
          Nothing -> []
          Just dt -> [ SomeStoreAction store $ ColumnSetDt i dt
                     , SomeStoreAction columnStore $ ColumnUnsetTmpDataType i
                     ]
      ] "OK"
  selInputType_ c

columnTitle_ :: Entity Column -> ReactElementM eh ()
columnTitle_ !c = view columnTitle c mempty

columnTitle :: ReactView (Entity Column)
columnTitle = defineStatefulView "columnTitle" Nothing $ \curText (Entity i col) -> do
  input_
    [ "placeholder" &= ("Column name" :: Text)
    , "value" &= fromMaybe (columnName col) curText
    , onChange $ \evt _ -> ([], Just $ Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        let txt = fromMaybe "" curState
        in  if keyCode evt == 13 && not (Text.null txt) -- 13 = Enter
              then (dispatch $ ColumnRename i txt, Just curState)
              else ([], Nothing)
    ]
  button_
    [ onClick $ \_ _ _ -> (dispatch $ TableDeleteColumn i, Nothing)
    ] $ "-"
  br_ []
  span_ "Data type "

selDatatype_ :: Entity Column -> TableCache -> ReactElementM eh ()
selDatatype_  c m = view selDatatype (c, m) mempty

selDatatype :: ReactView (Entity Column, TableCache)
selDatatype = defineView "selectDataType" $ \(Entity i Column{..}, tables) ->
    selBranch_ (Just columnDataType) tables $
      \dt -> [SomeStoreAction columnStore $ ColumnSetTmpDataType i dt]

selBranch_ :: Maybe DataType -> TableCache -> SelBranchCallback -> ReactElementM eh ()
selBranch_ mDt tables cb = view selBranch (mDt, tables, cb) mempty

selBranch :: ReactView (Maybe DataType, TableCache, SelBranchCallback)
selBranch = defineStatefulView "selBranch" Nothing $ \curBranch (mDt, tables, cb) -> do
  let defDt = DataNumber
      Just selectedBranch = curBranch <|> toBranch <$> mDt <|> Just (toBranch defDt)
  select_
    [ "defaultValue" &= fromMaybe "" (inverseLookup selectedBranch branches)
    , onInput $ \evt _ -> case Map.lookup (target evt "value") branches of
        Just BBool   -> (cb DataBool  , Just $ Just BBool  )
        Just BString -> (cb DataString, Just $ Just BString)
        Just BNumber -> (cb DataNumber, Just $ Just BNumber)
        Just BTime   -> (cb DataTime  , Just $ Just BTime  )
        Just BMaybe  -> (cb $ DataMaybe defDt, Just $ Just BMaybe)
        Just BList   -> (cb $ DataList  defDt, Just $ Just BList)
        Just BRecord -> ([], Just $ Just BRecord)
        Nothing      -> error "selBranch: unexpected: Nothing"
    ] $ forM_ (Map.toList branches) $ \(label, _) ->
          option_ [ "value" &= label ] $ elemText label
  case selectedBranch of
    BMaybe  -> selBranch_ (subType =<< mDt) tables (\dt -> cb (DataMaybe dt))
    BList   -> selBranch_ (subType =<< mDt) tables (\dt -> cb (DataList  dt))
    BRecord -> selTable_  (recordTableId =<< mDt) tables (\i -> cb (DataRecord i))
    _       -> pure ()

selTable_ :: Maybe (Id Table) -> TableCache -> SelTableCallback -> ReactElementM eh ()
selTable_ mTableId tables cb = view selTable (mTableId, tables, cb) mempty

selTable :: ReactView (Maybe (Id Table), TableCache, SelTableCallback)
selTable = defineView "selBranch" $ \(mTableId, tables, cb) -> do
  onDidMount_ (dispatchColumn ColumnGetTableCache) mempty
  select_
    [ "defaultValue" &= fromMaybe "" mTableId
    , onChange $ \evt -> maybe [] cb $ readMaybe $ target evt "value"
    ] $ do option_ ""
           forM_ (Map.toList tables) $ \(tId, name) ->
             option_ [ "value" &= show tId ] $ elemText name

inverseLookup :: Ord v => v -> Map k v -> Maybe k
inverseLookup x m = Map.lookup x (Map.fromList $ map swap $ Map.toList m)

--

selInputType_ :: Entity Column -> ReactElementM eh ()
selInputType_ !c = view selInputType c mempty


selInputType :: ReactView (Entity Column)
selInputType = defineControllerView "selInputType" columnStore $
  \st (Entity i Column{..}) -> do
    let inpTyp = fromMaybe columnInputType $ st ^. csTmpInputType . at i
        src = fromMaybe columnSourceCode $ st ^. csTmpSource . at i
    select_
      [ "defaultValue" &= show inpTyp
      , onChange $ \evt ->
          [ SomeStoreAction columnStore $
              ColumnSetTmpInputType i $ read $ target evt "value"
          ]
      ] $ do option_ [ "value" &= show ColumnInput ] "Input"
             option_ [ "value" &= show ColumnDerived ] "Derived"
    button_
      [ onClick $ \_ _ ->
          [ SomeStoreAction store $ ColumnSetInput i (inpTyp, src)
          , SomeStoreAction columnStore $ ColumnUnsetTmpInputType i
          , SomeStoreAction columnStore $ ColumnUnsetTmpSource i
          ]
      ] "Ok"
    when (inpTyp == ColumnDerived) $
      codemirror_ $ CodemirrorProps
        { codemirrorMode = "text/x-ocaml"
        , codemirrorTheme = "neat"
        , codemirrorValue = src
        , codemirrorOnChange = \v ->
            [ SomeStoreAction columnStore $ ColumnSetTmpSource i v ]
        }
    case columnCompileResult of
      CompileResultError msg -> elemText msg
      _                      -> mempty
