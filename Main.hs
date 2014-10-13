-- This program finds all of the solutions to the 8-queens problem,
-- which can be stated briefly as "place 8 queens on a chess board in
-- such a way that no queen is threatening another".
import Data.Bits ((.&.), clearBit, complement, setBit, testBit)
import Data.List (nub)
import Data.Word (Word64)

-- == Data ==
-- We represent a move as an integer 0-63. This is
-- distinct from a BoardPosition, which is a tuple (a, b), where a and b
-- are between 0 and 7, inclusive. We can convert back and forth with
-- moveToBoardPosition and boardPositionToMove
type Move = Int
type BoardPosition = (Int, Int)

-- Turn a number [0..63] into a board location, 0-indexed rows and
-- columns
moveToBoardPosition :: Move -> BoardPosition
moveToBoardPosition n = (n `quot` 8, n `mod` 8)

-- Reverse of moveToBoardPosition
boardPositionToMove :: BoardPosition -> Move
boardPositionToMove (row, col) = row * 8 + col

-- We represent threatened squares on a board with a Mask. This is 64
-- bits, where 0 represents threatened and 1 represents unthreatened.
-- Why represent in this way? We will soon see.
type Mask = Word64

-- The empty board state, where nothing is threatened
emptyMask :: Mask
emptyMask = (complement 0)

-- == Utility functions ==
-- Given a set of moves, we can represent this with a Word64, with 0
-- representing a move location and 1 representing an empty space. Note
-- that this doesn't preserve the order of the moves, but we don't care
-- about that.
movesToWord64 :: [Move] -> Word64
movesToWord64 = foldr (flip clearBit) emptyMask

word64ToMoves :: Word64 -> [Move]
word64ToMoves mask = filter (not . testBit mask) [0..63]

-- == Deduplication ==
-- One of the tasks we'll need to tackle is deduplication. That is, if
-- we have a bunch of possible solutions ([[Move]]), we want to
-- eliminate any that are the "same" solution. By "same", of course, I
-- mean "the same up to some symmetry".
deduplicate :: [[Move]] -> [[Move]]
deduplicate = map word64ToMoves . nub . map canonical

-- Get the "canonical" move, for deduplication purposes. In this case,
-- "canonical" means "apply all symmetries, convert to Word64s, and take
-- the smallest".
canonical :: [Move] -> Mask
canonical moves = minimum getSymmetries
    where
        getSymmetries = map (\f -> movesToWord64 . map (boardPositionToMove . f . moveToBoardPosition) $ moves) squareSymmetries

-- All symmetries of the square
squareSymmetries :: [((Int, Int) -> (Int, Int))]
squareSymmetries = [id, r1, r2, r3, m1, m2, m3, m4]
    where
        r1 (a, b) = (7 - b, a)
        r2 (a, b) = (7 - a, 7 - b)
        r3 (a, b) = (b, 7 - a)
        m1 (a, b) = (7 - a, b)
        m2 (a, b) = (a, 7 - b)
        m3 (a, b) = (b, a)
        m4 (a, b) = (7 - b, 7 - a)

-- == Core Algorithm ==
-- Get all possible sets of 8 mutually-nonthreating moves
queensMoves :: [[Move]]
queensMoves = deduplicate $ qMImpl emptyMask [] 0

-- qMImpl is a simple recurse, storing some things like the Mask of
-- which squares are currently threatened, which moves have been taken,
-- and how many moves have been taken. Technically, we could easily
-- recreate the threatened squares and the number of moves from the list
-- of moves, but it is more efficient (not to mention clearer) to keep
-- the values than to recalculate them.
qMImpl :: Mask -> [Move] -> Int -> [[Move]]
qMImpl mask moves numMoves
    | numMoves < 0 = []
    | numMoves > 8 = []
    | numMoves == 8 = [moves]
    | otherwise = concat . map recurse $ openMovesInRow mask numMoves
    where
        recurse move = qMImpl (doMove mask move) (move:moves) (numMoves + 1)

-- Get all the unthreatened moves for a mask in the given row
openMovesInRow :: Mask -> Int -> [Move]
openMovesInRow mask n = filter (testBit mask) $ take 8 [8*n..]

-- Given a move and a mask of threatened spaces, return a new mask of
-- threatened spaces
doMove :: Mask -> Move -> Mask
doMove mask move = (getMask move) .&. mask

-- For a given move, get the corresponding mask of threatened spaces
getMask :: Move -> Mask
getMask move = foldr (flip clearBit) emptyMask threatened
    where
        threatened = filter (isThreatened move) [0..63]

-- Given two moves, determine if they are threatening each other
isThreatened :: Move -> Move -> Bool
isThreatened a b =
    (cola == colb) ||
    (rowa == rowb) ||
    (cola + rowa == colb + rowb) ||
    (cola - rowa == colb - rowb)
    where
        (rowa, cola) = moveToBoardPosition a
        (rowb, colb) = moveToBoardPosition b


showMask :: Mask -> IO ()
showMask mask = mapM_ putStrLn $ map displayRow [0..7]
    where
        displayRow :: Int -> String
        displayRow row = map (getChar row) [0..7]
        getChar :: Int -> Int -> Char
        getChar row col = if testBit mask $ boardPositionToMove (row, col)
            then '1'
            else '0'

showMoves :: [Move] -> IO ()
showMoves = showMask . movesToWord64

main :: IO ()
main = mapM_ showOne (map movesToWord64 queensMoves)
    where
        showOne mask = do
            showMask $ mask
            putStrLn ""

