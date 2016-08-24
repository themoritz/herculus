module Views.Cell where

import React.Flux

import Lib.Model.Cell
import Lib.Types

cell_ :: CellContent -> ReactElementM eh ()
cell_ !c = view cell c mempty

cell :: ReactView CellContent
cell = defineView "cell" $ \c -> do
  "cell"
