module Views.Table where


data RendererArgs = RendererArgs Int Bool

instance FromJSON RendererArgs where
  parseJSON (Object o) = RendererArgs <$> o .: "index"
                                      <*> o .: "isScrolling"
  parseJSON _ = mempty

test :: ReactView ()
test = defineControllerView "test" store $ \(State _ ps) () -> do
  h1_ "Test"
  button_ [ onClick $ \_ _ -> dispatch Load ] "Load"
  button_ [ onClick $ \_ _ -> dispatch $ SendWebSocket $ WsUpGreet "foo" ] "Send"
  let huge = join $ replicate 10000 ps
      toProps :: Value -> ReturnProps (Project, Bool)
      toProps v = case parseMaybe parseJSON v of
        Nothing -> ReturnProps (entityVal $ huge !! 0, False)
        Just (RendererArgs index scrolling) -> ReturnProps (entityVal $ huge !! index, scrolling)
  foreign_ "VirtualScroll"
    [ "width" &= (300 :: Int)
    , "height" &= (300 :: Int)
    , "rowCount" &= length huge
    , "rowHeight" &= (60 :: Int)
    , callbackViewWithProps "rowRenderer" project toProps
    ] mempty

project :: ReactView (Project, Bool)
project = defineView "project" $ \(p, b) -> do
  if b then "scrolling" else ""
  elemText (projectName p)

toCellGrid :: State -> Map Coords CellInfo
toCellGrid (State _ cells columns records) =
  let indexedCols = map (\((i, c), p) -> (i, (c, p))) $ zip (Map.toList columns) [0..]
      indexedRows = zip (Map.keys records) [0..]
      colMap = Map.fromList indexedCols
      rowMap = Map.fromList indexedRows
      go m (Coords colId recId) val =
        let may = do
              (col, x) <- Map.lookup colId colMap
              y <- Map.lookup recId rowMap
              pure (Position x y, col)
        in case may of
          Just (pos, col) -> Map.insert (Coords colId recId) (CellInfo pos col val) m
          Nothing         -> m
  in Map.foldlWithKey' go Map.empty cells

toColumns :: State -> Map (Id Column) (Int, Column, Maybe (Id Table))
toColumns (State tblId _ columns _) =
  let indexedCols = zip (Map.toList columns) [0..]
  in Map.fromList $ map (\((colId, col), i) -> (colId, (i, col, tblId))) indexedCols

toRecords :: State -> Map (Id Record) (Int, Record)
toRecords (State _ _ _ records) =
  let indexedRecs = zip (Map.toList records) [0..]
  in Map.fromList $ map (\((recId, reco), i) -> (recId, (i, reco))) indexedRecs

toCellUpdate :: Entity Cell -> (Id Column, Id Record, CellContent)
toCellUpdate (Entity _ (Cell content (Aspects _ c r))) = (c, r, content)
