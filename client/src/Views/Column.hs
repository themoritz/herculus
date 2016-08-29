{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Column where

import Control.Lens hiding (view)
import           React.Flux

import           Control.Monad    (forM_, when)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text, pack)
import           Data.Tuple (swap)
import qualified Data.Text        as Text
import Data.Monoid ((<>))
import           Data.Typeable    (Typeable)
import           GHC.Generics     (Generic)
import           Control.DeepSeq  (NFData)

import           Lib.Model
import           Lib.Model.Column
import           Lib.Types

import           Store
import           Views.Foreign

data ColumnState = ColumnState
  { _csTmpDataType :: Map (Id Column) DataType
  , _csTmpInputType :: Map (Id Column) InputType
  , _csTmpSource :: Map (Id Column) Text
  }

makeLenses ''ColumnState

data ColumnAction
  = ColumnSetTmpDataType (Id Column) DataType
  | ColumnUnsetTmpDataType (Id Column)
  | ColumnSetTmpInputType (Id Column) InputType
  | ColumnUnsetTmpInputType (Id Column)
  | ColumnSetTmpSource (Id Column) Text
  | ColumnUnsetTmpSource (Id Column)
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

columnStore :: ReactStore ColumnState
columnStore = mkStore $ ColumnState Map.empty Map.empty Map.empty

type SelBranchCallback = DataType -> [SomeStoreAction]
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
branches = [ ("Bool"  , BBool  )
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

column_ :: Entity Column -> ReactElementM eh ()
column_ !c = view column c mempty

column :: ReactView (Entity Column)
column = defineControllerView "column" columnStore $ \st c@(Entity i _) -> do
  columnTitle_ c
  selDatatype_ c
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

selDatatype_ :: Entity Column -> ReactElementM eh ()
selDatatype_  c = view selDatatype c mempty

selDatatype :: ReactView (Entity Column)
selDatatype = defineView "selectDataType" $ \(Entity i Column{..}) ->
    selBranch_ (Just columnDataType) $
      \dt -> [SomeStoreAction columnStore $ ColumnSetTmpDataType i dt]

selBranch_ :: Maybe DataType -> SelBranchCallback -> ReactElementM eh ()
selBranch_ mDt cb = view selBranch (mDt, cb) mempty

selBranch :: ReactView (Maybe DataType, SelBranchCallback)
selBranch = defineStatefulView "branch" Nothing $ \curBranch (mDt, cb) -> do
  let defDt = DataNumber
      selectedBranch = fromMaybe (fromMaybe (toBranch defDt) $ toBranch <$> mDt) curBranch
  select_
    [ "defaultValue" &= fromMaybe "" (inverseLookup selectedBranch branches)
    , onChange $ \evt _ -> case Map.lookup (target evt "value") branches of
        Just BBool   -> (cb DataBool  , Just $ Just BBool  )
        Just BString -> (cb DataString, Just $ Just BString)
        Just BNumber -> (cb DataNumber, Just $ Just BNumber)
        Just BTime   -> (cb DataTime  , Just $ Just BTime  )
        Just BMaybe  -> (cb $ DataMaybe defDt, Just $ Just BMaybe)
        Just BList   -> (cb $ DataList  defDt, Just $ Just BList)
        Just BRecord -> undefined -- TODO
        Nothing      -> error "Nothing" -- TODO
    ] $ forM_ (Map.toList branches) $ \(label, branch) -> do
          option_ $ elemText label
  case selectedBranch of
    BMaybe  -> selBranch_ (subType =<< mDt) (\dt -> cb (DataMaybe dt))
    BList   -> selBranch_ (subType =<< mDt) (\dt -> cb (DataList  dt))
    BRecord -> undefined -- TODO
    _       -> pure ()

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
