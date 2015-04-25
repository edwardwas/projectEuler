{-# LANGUAGE RecordWildCards, TemplateHaskell  #-}

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Lens
import Control.Lens.TH
import Data.Maybe
import qualified Data.Map as M

type Pos = (Int,Int)

inf :: Num a => a
inf = 99999999999

data Grid a = Grid {
      _grid :: M.Map Pos a
    , _xSize :: Int
    , _ySize :: Int
}
makeLenses ''Grid

movePositions :: Grid a -> Pos -> Pos -> Maybe Pos
movePositions Grid{..} (sx,sy) (dx,dy)
    | nx < 0 || ny < 0 || nx >= _xSize || ny >= _ySize = Nothing
    | otherwise = Just (nx,ny)
    where nx = dx + sx
          ny = dy + sy

getUpDownLeft :: Grid a -> Pos -> [Pos]
getUpDownLeft g pos = catMaybes $ movePositions g pos <$> [(0,-1),(0,1),(-1,0)]

--Djikstar's ALgo-----

data Node a = Node{
      _tenVal :: a
    , _visited :: Bool
    , _value :: a 
}
makeLenses ''Node

type NodeState a = State (Grid (Node a))

createNodeGrid :: Num a => Grid a -> NodeState a ()
createNodeGrid = put . over grid (fmap (Node inf False))

updateTenVal v p = do
    let oldV = (grid . at p . traversed . tenVal) g
    assign (grid . at p . traversed . tenVal) . fromJust oldV
    



doNode :: (Int,Int) -> NodeState a ()
doNode (sx,sy) = do
    neighbours <- flip getUpDownLeft (sx,sy) <$> get
    return ()

    

