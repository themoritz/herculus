{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.Cell
  where

import qualified Data.Map as Map
import Data.Text (Text, pack, unpack, append)
import Data.Monoid

import Text.Read (readMaybe)

import Reflex.Dom hiding (Value, value, append)

import Lib.Types
import Lib.Model
import Lib.Model.Types
import Lib.Model.Cell
import Lib.Model.Column

import Api.Rest (loader, api)
import qualified Api.Rest as Api
import Misc

data CellConfig t = CellConfig
  { _cellConfig_content :: Dynamic t CellContent
  , _cellConfig_column :: Column
  }

cell :: MonadWidget t m
     => Id Column -> Id Record
     -> CellConfig t -> m (Event t Value)
cell colId recId (CellConfig content column) = el "div" $
  switchEvent $ dynWidget content $ \case
    CellError msg -> do
      text $ "Error: " <> msg
      pure never
    CellValue val -> value (columnInputType column)
                           (columnDataType column) val

value :: MonadWidget t m => InputType -> DataType -> Value -> m (Event t Value)
value inpType datType val = case datType of
  DataBool -> case val of
    VBool b -> do
      inp <- cellBool inpType b
      pure (VBool <$> inp)
    _ -> pure never
  DataString -> case val of
    VString s -> do
      inp <- cellString inpType s
      pure (VString <$> inp)
    _ -> pure never
  DataNumber -> case val of
    VNumber n -> do
      inp <- cellNumber inpType n
      pure (VNumber <$> inp)
    _ -> pure never
  DataRecord t -> case val of
    VRecord r -> do
      inp <- cellRecord inpType r t
      pure (VRecord <$> inp)
    _ -> pure never
  DataList t -> case val of
    VList xs -> do
      inp <- cellList inpType t xs
      pure (VList <$> inp)
    _ -> pure never
  DataMaybe t -> case val of
    VMaybe x -> do
      inp <- cellMaybe inpType t x
      pure (VMaybe <$> inp)
    _ -> pure never

cellBool :: MonadWidget t m => InputType -> Bool -> m (Event t Bool)
cellBool inpType b = case inpType of
  ColumnInput -> do
    inp <- checkbox b def
    pure $ _checkbox_change inp
  ColumnDerived -> do
    text $ if b then "true" else "false"
    pure never

cellString :: MonadWidget t m => InputType -> Text -> m (Event t Text)
cellString inpType s = case inpType of
  ColumnInput -> do
    new <- _textInput_value <$> textInput def
             { _textInputConfig_initialValue = s
             }
    set <- button "Set"
    pure $ tagPromptlyDyn new set
  ColumnDerived -> do
    text s
    pure never

cellNumber :: forall t m. MonadWidget t m => InputType -> Number -> m (Event t Number)
cellNumber inpType n = case inpType of
  ColumnInput -> mdo
    let parseNumber s = Number <$> (readMaybe $ unpack s)
    mNew <- (fmap parseNumber . _textInput_value) <$> textInput (def :: TextInputConfig t)
              { _textInputConfig_initialValue = pack $ show n
              , _textInputConfig_attributes = attrs
              }
    let attrs = ffor mNew $ \case
          Nothing -> "style" =: "border-color: red"
          Just _  -> "style" =: "border-color: auto"
    set <- button "Set"
    pure $ fmapMaybe id $ tagPromptlyDyn mNew set
  ColumnDerived -> do
    text $ pack $ show n
    pure never

cellRecord :: MonadWidget t m => InputType -> Id Record -> Id Table -> m (Event t (Id Record))
cellRecord inpType r t = case inpType of
  ColumnInput -> do
    text "not implemented"
    pure never
  ColumnDerived -> do
    postBuild <- getPostBuild
    dat <- loader (Api.recordData api $ constDyn $ Right r) postBuild
    res <- holdDyn Map.empty (Map.fromList . map (\(Entity i c, content) -> (i,(c,content)))<$> dat)
    _ <- el "ul" $ listWithKey res $ \i v -> el "li" $ do
      dynText (flip append ": " . columnName . fst <$> v)
      dyn $ ffor v $ \(col, content) -> case content of
        CellError e -> do
          text e
          pure never
        CellValue val -> value inpType (columnDataType col) val
    pure never

--

cellList :: MonadWidget t m => InputType -> DataType -> [Value] -> m (Event t [Value])
cellList inpType datType vs = case inpType of
  ColumnInput -> do
    new <- button "New"
    let l = constDyn $ Map.fromList $ zip [0..] vs
    updates <- fmap dynMapEvents $ el "ul" $ list l $ \dynV -> do
      switchEvent $ dyn $ ffor dynV $ \v -> el "li" $ do
        del <- button "Del"
        upd <- value inpType datType v
        pure $ leftmost [ Left <$> del, Right <$> upd ]
    let (CellValue newV) = defaultContent datType
    pure $ leftmost
      [ (newV:vs) <$ new
      , (\(i, e) -> either (\_ -> let (h, t) = splitAt i vs in h <> drop 1 t)
                           (\v -> let (h, t) = splitAt i vs in h <> (v:drop 1 t)) e
        ) <$> updates
      ]
  ColumnDerived -> do
    _ <- el "ul" $ simpleList (constDyn vs) $ \dynV ->
      dyn $ ffor dynV $ \v -> el "li" $
        value inpType datType v
    pure never
--

cellMaybe :: MonadWidget t m => InputType -> DataType -> Maybe Value -> m (Event t (Maybe Value))
cellMaybe inpType datType mVal = case inpType of
  ColumnInput -> case mVal of
    Nothing -> do
      add <- button "Add"
      pure $ (let (CellValue new) = defaultContent datType in Just new) <$ add
    Just val -> do
      del <- button "Del"
      edit <- value inpType datType val
      pure $ leftmost [ Just <$> edit, Nothing <$ del ]
  ColumnDerived -> case mVal of
    Nothing -> do
      text "Nothing"
      pure never
    Just val -> do
      _ <- value inpType datType val
      pure never
