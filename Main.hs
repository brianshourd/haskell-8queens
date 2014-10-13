-- == 8 Queens Solution ==
-- Brian Shourd
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

-- We represent threatened squares on a board with a Word64, where 0
-- represents threatened and 1 represents unthreatened. Why represent in
-- this way? We will soon see.
type ThreatMask = Word64

-- We also represent a bunch of moves as a Word64, with 1 as an empty
-- space and 0 as a space with a move in it. We convert back and forth
-- from a list of moves to a MoveMask as well (though order is not
-- preserved).
type MoveMask = Word64

-- The empty board state, where nothing is threatened, and no moves are
-- made. Can serve as both a MoveMask and a ThreatMask
emptyMask :: Word64
emptyMask = (complement 0)

movesToMoveMask :: [Move] -> MoveMask
movesToMoveMask = foldr (flip clearBit) emptyMask

moveMaskToMoves :: MoveMask -> [Move]
moveMaskToMoves mmask = filter (not . testBit mmask) [0..63]

-- == Utility functions ==
-- == Deduplication ==
-- One of the tasks we'll need to tackle is deduplication. That is, if
-- we have a bunch of possible solutions ([MoveMask]), we want to
-- eliminate any that are the "same" solution. By "same", of course, I
-- mean "the same up to some symmetry".
deduplicate :: [MoveMask] -> [MoveMask]
deduplicate = nub . map canonical

-- Get the "canonical" move, for deduplication purposes. In this case,
-- "canonical" means "apply all symmetries and take the smallest".
canonical :: MoveMask -> MoveMask
canonical moves = minimum . map ($moves) $ squareSymmetries

-- All symmetries of the square
squareSymmetries :: [(MoveMask -> MoveMask)]
squareSymmetries = map applyOnMasks [id, r1, r2, r3, m1, m2, m3, m4]
    where
        r1 (a, b) = (7 - b, a)
        r2 (a, b) = (7 - a, 7 - b)
        r3 (a, b) = (b, 7 - a)
        m1 (a, b) = (7 - a, b)
        m2 (a, b) = (a, 7 - b)
        m3 (a, b) = (b, a)
        m4 (a, b) = (7 - b, 7 - a)
        applyOnMasks :: (BoardPosition -> BoardPosition) -> MoveMask -> MoveMask
        applyOnMasks f = movesToMoveMask . map (boardPositionToMove . f . moveToBoardPosition) . moveMaskToMoves

-- == Core Algorithm ==
-- Get all possible sets of 8 mutually-nonthreating moves
queensMoves :: [MoveMask]
queensMoves = deduplicate $ qMImpl emptyMask emptyMask 0

-- qMImpl is a simple recurse, storing some things like the ThreatMask
-- of which squares are currently threatened, which moves have been
-- taken, and how many moves have been taken. Technically, we could
-- easily recreate the threatened squares and the number of moves from
-- the list of moves, but it is more efficient (not to mention clearer)
-- to keep the values than to recalculate them.
qMImpl :: ThreatMask -> MoveMask -> Int -> [MoveMask]
qMImpl tmask mmask numMoves
    | numMoves < 0 = []
    | numMoves > 8 = []
    | numMoves == 8 = [mmask]
    | otherwise = concat . map recurse $ openMovesInRow tmask numMoves
    where
        recurse :: Move -> [MoveMask]
        recurse move = qMImpl (doMove tmask move) (clearBit mmask move) (numMoves + 1)

-- Get all the unthreatened moves for a mask in the given row
openMovesInRow :: ThreatMask -> Int -> [Move]
openMovesInRow tmask n = filter (testBit tmask) $ take 8 [8*n..]

-- Given a move and a mask of threatened spaces, return a new mask of
-- threatened spaces
doMove :: ThreatMask -> Move -> ThreatMask
doMove tmask move = (getThreatMask move) .&. tmask

-- For a given move, get the corresponding mask of threatened spaces
getThreatMask :: Move -> ThreatMask
getThreatMask move = foldr (flip clearBit) emptyMask threatened
    where
        threatened = filter (isThreatened move) [0..63]

-- Determine if two moves threaten each other
isThreatened :: Move -> Move -> Bool
isThreatened a b =
    (cola == colb) ||
    (rowa == rowb) ||
    (cola + rowa == colb + rowb) ||
    (cola - rowa == colb - rowb)
    where
        (rowa, cola) = moveToBoardPosition a
        (rowb, colb) = moveToBoardPosition b

main :: IO ()
main = mapM_ (putStrLn . show . map moveToBoardPosition . moveMaskToMoves) queensMoves
