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
import qualified Data.Set            as Set
import           Data.Set            (Set)
import           Data.Maybe          (fromMaybe)
import           Data.Proxy
import           Data.Text           (Text)
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
import           Views.Common (editBox_, EditBoxProps (..))
import           Views.Foreign

-- state of column controller view

type TableCache = Map (Id Table) Text

data ColConfState = ColConfState
  { _ccsTmpDataType  :: Map (Id Column) DataType
  , _ccsTmpIsFormula :: Map (Id Column) InputType
  , _ccsTmpFormula   :: Map (Id Column) Text
  , _ccsVisible      :: Set (Id Column) -- not in set === false
  , _ccsTableCache   :: TableCache
  }

makeLenses ''ColConfState

data ColumnAction
  = ColumnSetTmpDataType    (Id Column) DataType
  | ColumnUnsetTmpDataType  (Id Column)
  | ColumnSetTmpIsFormula   (Id Column) InputType
  | ColumnUnsetTmpIsFormula (Id Column)
  | ColumnSetTmpFormula     (Id Column) Text
  | ColumnUnsetTmpFormula   (Id Column)
  | ColumnSetVisibility     (Id Column) Bool
  | ColumnGetTableCache
  | ColumnSetTableCache     (Map (Id Table) Text)
  deriving (Typeable, Generic, NFData)

instance StoreData ColConfState where
  type StoreAction ColConfState = ColumnAction
  transform action st = case action of
    ColumnSetTmpDataType i dt ->
      pure $ st & ccsTmpDataType . at i .~ Just dt
    ColumnUnsetTmpDataType i ->
      pure $ st & ccsTmpDataType . at i .~ Nothing
    ColumnSetTmpIsFormula i it ->
      pure $ st & ccsTmpIsFormula . at i .~ Just it
    ColumnUnsetTmpIsFormula i ->
      pure $ st & ccsTmpIsFormula . at i .~ Nothing
    ColumnSetTmpFormula i s ->
      pure $ st & ccsTmpFormula . at i .~ Just s
    ColumnUnsetTmpFormula i ->
      pure $ st & ccsTmpFormula . at i .~ Nothing

    ColumnSetVisibility i b -> pure $ case b of
      True  -> st & ccsVisible %~ Set.insert i
      False -> st & ccsVisible %~ Set.delete i

    ColumnGetTableCache -> do
      when (Map.null $ st ^. ccsTableCache) $
        request api (Proxy :: Proxy TableListGlobal) $ pure . \case
          Left (_, e) -> dispatch $ GlobalSetError $ Text.pack e
          Right ts    -> [SomeStoreAction colConfStore $ ColumnSetTableCache $ toTableMap ts]
      pure st
    ColumnSetTableCache m ->
      pure $ st & ccsTableCache .~ m

colConfStore :: ReactStore ColConfState
colConfStore = mkStore $ ColConfState Map.empty Map.empty Map.empty Set.empty Map.empty

-- helper

toTableMap :: [Entity Table] -> TableCache
toTableMap = Map.fromList . map (\(Entity i Table{..}) -> (i, tableName))

--

-- branch selection types

--

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

--

-- views

--

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineView "column" $ \c@(Entity i col) -> do
  editBox_ EditBoxProps
    { editBoxValue = columnName col
    , editBoxPlaceholder = "Column name..."
    , editBoxClassName = "columnName"
    , editBoxOnSave = dispatch . ColumnRename i
    }
  -- TODO: summary of datatype and isFormula
  columnConfig_ c

-- configure column

columnConfig_ :: Entity Column -> ReactElementM eh ()
columnConfig_ !c = view columnConfig c mempty

columnConfig :: ReactView (Entity Column)
columnConfig = defineControllerView "column configuration" colConfStore $
  \state c@(Entity i Column{..}) -> do
    cldiv_ "columnConfigOpen" $ do
      case columnCompileResult of
        CompileResultError msg -> div_
                                    [ "className" &= ("columnConfigError" :: Text)
                                    , "title" &= msg
                                    ] $ elemText "Err"
        _                      -> pure ()
      cldiv_ "columnConfigOpenIcon" $ elemText "Conf"
    when (Set.member i $ state ^. ccsVisible) $ do
      cldiv_ "columnConfigDialog" $ do
        cldiv_ "columnConfigDatatype" $
          selDatatype_ c (state ^. ccsTableCache)
        cldiv_ "columnConfigFormula" $ do
          checkIsFormula_ c Nothing
          -- input field for formula
          case state ^. ccsTmpIsFormula . at i ?: columnInputType of
            ColumnDerived -> inputFormula_ c Nothing
            ColumnInput   -> pure ()
      cldiv_ "columnConfigButtons" $ do
        a_
          [ onClick $ \_ _ ->
              [ SomeStoreAction colConfStore $ ColumnSetVisibility i False
              , SomeStoreAction colConfStore $ ColumnUnsetTmpDataType i
              , SomeStoreAction colConfStore $ ColumnUnsetTmpFormula i
              , SomeStoreAction colConfStore $ ColumnUnsetTmpIsFormula i
              ]
          ] "cancel"
        -- TODO: figure out what changed before initiating ajax
        -- in the Nothing case: nothing has changed
        let inpTyp = (state ^. ccsTmpIsFormula . at i) ?: columnInputType
            formula = (state ^. ccsTmpFormula . at i)  ?: columnSourceCode
            dataTypeActions = case state ^. ccsTmpDataType . at i of
              Nothing -> []
              Just dt -> [ SomeStoreAction store        $ ColumnSetDt i dt
                         , SomeStoreAction colConfStore $ ColumnUnsetTmpDataType i
                         ]
            saveActions =
              -- hide dialog
              [ SomeStoreAction colConfStore $ ColumnSetVisibility i False
              -- is formula and formula
              , SomeStoreAction store        $ ColumnSetFormula i (inpTyp, formula)
              , SomeStoreAction colConfStore $ ColumnUnsetTmpIsFormula i
              , SomeStoreAction colConfStore $ ColumnUnsetTmpFormula i
              -- datatype selection
              ] ++ dataTypeActions
        button_
          [ onClick $ \_ _ -> saveActions
          ] "OK"

-- select datatype

selDatatype_ :: Entity Column -> TableCache -> ReactElementM eh ()
selDatatype_  c m = view selDatatype (c, m) mempty

selDatatype :: ReactView (Entity Column, TableCache)
selDatatype = defineView "selectDataType" $ \(Entity i Column{..}, tables) ->
    selBranch_ (Just columnDataType) tables $
      \dt -> [SomeStoreAction colConfStore $ ColumnSetTmpDataType i dt]

selBranch_ :: Maybe DataType -> TableCache -> SelBranchCallback -> ReactElementM eh ()
selBranch_ mDt tables cb = view selBranch (mDt, tables, cb) mempty

selBranch :: ReactView (Maybe DataType, TableCache, SelBranchCallback)
selBranch = defineStatefulView "selBranch" Nothing $ \curBranch (mDt, tables, cb) -> do
  let defDt = DataNumber
      selectedBranch = curBranch <|> toBranch <$> mDt ?: toBranch defDt
  cldiv_ "selBranch" $ select_
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
  onDidMount_ ([SomeStoreAction colConfStore ColumnGetTableCache]) mempty
  select_
    [ "defaultValue" &= fromMaybe "" (show <$> mTableId)
    , onChange $ \evt -> maybe [] cb $ readMaybe $ target evt "value"
    ] $ do option_ ""
           forM_ (Map.toList tables) $ \(tId, name) ->
             option_ [ "value" &= show tId ] $ elemText name

-- checkbox for "is formula"

checkIsFormula_ :: Entity Column -> Maybe InputType -> ReactElementM eh ()
checkIsFormula_ !c !i = view checkIsFormula (c, i) mempty


checkIsFormula :: ReactView (Entity Column, Maybe InputType)
checkIsFormula = defineView "checkIsFormula" $ \(Entity i Column{..}, mIsFormula) ->
    input_
      [ "type"    &= ("checkbox"    :: Text)
      , "value"   &= ("use formula" :: Text)
      , "checked" &= case mIsFormula ?: columnInputType of
          ColumnDerived -> True
          ColumnInput   -> False
      , onClick $ \evt _ ->
          [ SomeStoreAction colConfStore $ ColumnSetTmpIsFormula i $ read $ target evt "value"
          ]
      ]

-- input field for formula (i.a.)

inputFormula_ :: Entity Column -> Maybe Text -> ReactElementM eh ()
inputFormula_ !c !f = view inputFormula (c, f) mempty

inputFormula :: ReactView (Entity Column, Maybe Text)
inputFormula = defineView "input formula" $ \(Entity i Column{..}, mFormula) -> do
      codemirror_ $ CodemirrorProps
        { codemirrorMode = "text/x-ocaml"
        , codemirrorTheme = "neat"
        , codemirrorValue = mFormula ?: columnSourceCode
        , codemirrorOnChange = \v ->
            [ SomeStoreAction colConfStore $ ColumnSetTmpFormula i v ]
        }

--

-- util

--

inverseLookup :: Ord v => v -> Map k v -> Maybe k
inverseLookup x m = Map.lookup x (Map.fromList $ map swap $ Map.toList m)

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

infixl 3 ?:
