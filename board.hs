-- | module containing methods and structures allowing us to make
--   and manipulate board state in the game
module GameData
    ( Board
    , Player(..)
    , swapPlayer
    , initBoard
    , hasWall
    , completedBoxes
    -- , updateBoard
    ) where

import Data.Array
import Data.List (transpose)

data Orientation = Horizontal | Vertical deriving (Eq, Show, Read)

type Move = (Orientation, (Int, Int))

data Player = X | O | NeitherPlayer deriving (Eq)

instance Show Player where
    show X = "X "
    show O = "O "
    show NeitherPlayer = "//"

data Board = Board { horzLines :: Array (Int, Int) Bool
                   , vertLines :: Array (Int, Int) Bool
                   , boxes     :: Array (Int, Int) Player
                   , height    :: Int
                   , width     :: Int
                   }

instance Show Board where
    show b = displayBoard b

displayBoard :: Board -> String
displayBoard (Board {horzLines = hl, vertLines = vl, boxes = b, height = h, width = w}) =
    unlines $ interweave horzRows vertRows
  where
    -- horzRows =
    --     let
    --         horzBorders = fmap (\x -> if x then "--" else "  ") hl
    --         corners = chunk w $ take (h * w) $ repeat "+"
    --     in
    --         map concat $ transpose $ map (interweave (take (w-1) (repeat "+"))) $ chunk (w - 1) $ elems horzBorders
    strHl = fmap (\x -> if x then "--" else "  ") hl
    horzRows = map (\x -> x ++ "+") $ map concat [["+" ++ (strHl ! (x, y)) | x <- [0..(w-2)]] | y <- [0..(h-1)]]
    vertRows =
        let
            vertBorders = chunk w $ elems $ fmap (\x -> if x then "|" else "_") vl
            boxValues = transpose $ chunk (w - 1) $ elems $ fmap show b
        in
            map concat $ zipWith (zipWith (++)) vertBorders boxValues

-- | splits a list into multiple sublists every n elements
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = first : (chunk n rest)
  where
    (first, rest) = splitAt n l

-- | interweaves 2 lists together
interweave :: [a] -> [a] -> [a]
interweave [] ys = ys
interweave xs [] = xs
interweave (x:xs) (y:ys) = x:y:(interweave xs ys)

-- | swaps the player from X -> O and vice versa, leaving the empty player the sameo
swapPlayer :: Player -> Player
swapPlayer X = O
swapPlayer O = X
swapPlayer _ = NeitherPlayer

-- | initialize the board with the given height and width
initBoard :: Int -> Int -> Board
initBoard h w =
    Board {horzLines = hl, vertLines = vl, boxes = b, height = h, width = w}
  where
    hl = listArray ((0,0),(w-2, h-1)) (repeat False)          -- The ranges are all funny because we're 0-indexing
    vl = listArray ((0,0),(w-1, h-2)) (repeat False)
    b = listArray ((0,0),(w-2, h-2)) (repeat NeitherPlayer)

-- | Is there a wall at the given move?
hasWall :: Board -> Move -> Bool
hasWall b (Horizontal, loc) = (horzLines b) ! loc
hasWall b (Vertical, loc) = (vertLines b) ! loc

maybeToList :: Maybe a -> [a]
maybeToList (Just v)  = [v]
maybeToList Nothing = [] 

-- | Return a list of box locations that would be closed if the given move was played
--   If no boxes are closed, returns the empty list
completedBoxes :: Board -> Move -> [(Int, Int)]
completedBoxes b (Vertical, loc) = lb ++ rb
  where
    lb = maybeToList $ completedLeftBox b loc
    rb = maybeToList $ completedRightBox b loc
completedBoxes b (Horizontal, loc) = tb ++ bb
  where
    tb = maybeToList $ completedTopBox b loc
    bb = maybeToList $ completedBottomBox b loc

-- | Will making this move complete the box to the left of the given Vertical wall
completedLeftBox :: Board -> (Int, Int) -> Maybe (Int, Int)
completedLeftBox b (x, y) =
    if top && bottom && left && right
    then Just (x - 1, y)
    else Nothing
  where
    left   = (vertLines b) ! (x-1, y)
    top    = (horzLines b) ! (x-1, y)
    right  = True
    bottom = (horzLines b) ! (x-1, y+1)

-- | Will making this move complete the box to the right of the given Vertical wall
completedRightBox :: Board -> (Int, Int) -> Maybe (Int, Int)
completedRightBox b (x, y) =
    if top && bottom && left && right
    then Just (x, y)
    else Nothing
  where
    left   = True
    top    = (horzLines b) ! (x, y)
    right  = (vertLines b) ! (x+1, y)
    bottom = (horzLines b) ! (x, y+1)

-- | Will making the given move complete the box above the Horizonal wall
completedTopBox :: Board -> (Int, Int) -> Maybe (Int, Int)
completedTopBox b (x, y) =
    if top && bottom && left && right
    then Just (x, y)
    else Nothing
  where
    left   = (vertLines b) ! (x, y-1)
    top    = (horzLines b) ! (x, y-1)
    right  = (vertLines b) ! (x+1, y-1)
    bottom = True

-- | Will making the given move complete the box below the Horizonal wall
completedBottomBox :: Board -> (Int, Int) -> Maybe (Int, Int)
completedBottomBox b (x, y) =
    if top && bottom && left && right
    then Just (x, y)
    else Nothing
  where
    left   = (vertLines b) ! (x, y)
    top    = True
    right  = (vertLines b) ! (x+1, y)
    bottom = (horzLines b) ! (x, y+1)
