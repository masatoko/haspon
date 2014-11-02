import System.IO (BufferMode (..), stdout, stdin, hPutStrLn, hFlush, hSetBuffering)
import System.Environment (getArgs)
import System.Random (StdGen, randomRs, randomR, mkStdGen)
import Control.Monad (when)
import Control.Arrow
import Control.Concurrent (threadDelay)
import Data.List.Split (chunksOf)
import Data.List (intercalate, group, groupBy, transpose, foldl', nub)
import Text.Printf (printf)


data Panel = PA | PB | PC | PD | PE deriving (Eq, Enum, Show)
data Cell = Null | Box Panel deriving (Show, Eq)
data Cursor = CNone | CLeft | CRight

type Board = [Col]
type Col = [Cell]

type RowInd = Int
type ColInd = Int

type Score = Int

-- [w][a][s][d] : move cursor
-- [j] : swap panels
-- [k] : vanish
-- [l] : lift row

main :: IO ()
main = do
  hSetBuffering stdout $ BlockBuffering (Just 1)
  hSetBuffering stdin NoBuffering
  (seed:_) <- getArgs
  let
    sd = read seed
    gen = mkStdGen $ sd + 1
    bd = mkBoard sd numCol numRow
  loopGame 0 gen 0 0 0 bd
 where
  numCol = 6  :: ColInd
  numRow = 10 :: RowInd
  loopGame :: Int -> StdGen -> Score -> ColInd -> RowInd -> Board -> IO ()
  loopGame i g score c r bd = do
    -- print information
    hPutStrLn stdout $ "score " ++ show score
    hPutStrLn stdout $ show . evalBoard $ bd
    hPutStrLn stdout $ showBoard c r bd
    hFlush stdout
    -- operation
    chr <- getChar
    hPutStrLn stdout ""
    case parseCommand chr of
      Nothing  -> loopGame i g score c r bd
      Just cmd -> do
        (g', c', r', bd', mScore) <- runCommand numCol numRow g c r bd cmd
        let
          s = maybe 0 evalScores mScore
        hPutStrLn stdout $ maybe "" scoresToString mScore
        bd'' <- dropLoop True numRow bd'
        loopGame (i + 1) g' (score + s) c' r' bd''

----- Convert data to String -----
showPanel :: Panel -> String
showPanel PA = "@"
showPanel PB = "&"
showPanel PC = "#"
showPanel PD = "O"
showPanel PE = "v"

showCell :: Cell -> String
showCell Null    = "."
showCell (Box p) = showPanel p

showCell' :: Cursor -> Cell -> String
showCell' CNone  cell = " " ++ showCell cell ++ " "
showCell' CLeft  cell = "[" ++ showCell cell ++ "."
showCell' CRight cell = "." ++ showCell cell ++ "]"

showBoard' :: Board -> String
showBoard' = unlines . map (foldl' (++) [] . map (showCell' CNone)) . reverse . transpose

showBoard :: Int -> Int -> Board -> String
showBoard c r bd = rowStr
 where
  rs = transpose bd
  (rsD, row, rsU) = extract r rs
  (csL, cellL, cellR, csR) = extract' c row
  row' = intercalate "" $ map (showCell' CNone) csL ++ showCell' CLeft cellL : showCell' CRight cellR : map (showCell' CNone) csR
  rowStr = unlines . reverse $ map showNormalRow rsD ++ row' : map showNormalRow rsU

  showNormalRow = intercalate "" . map (showCell' CNone)
----------


----- Command -----
data Command
  = Move ColInd RowInd
  | Swap
  | Vanish
  | Lift

parseCommand :: Char -> Maybe Command
parseCommand 'w' = Just $ Move 0 1
parseCommand 's' = Just $ Move 0 (-1)
parseCommand 'a' = Just $ Move (-1) 0
parseCommand 'd' = Just $ Move 1 0
parseCommand 'j' = Just Swap
parseCommand 'k' = Just Vanish
parseCommand 'l' = Just Lift
parseCommand _   = Nothing

runCommand :: ColInd -> RowInd -> StdGen -> ColInd -> RowInd -> Board -> Command -> IO (StdGen, Int, Int, Board, Maybe [Score])
runCommand nc nr g c r bd (Move dc dr) = return (g, c', r', bd, Nothing)
 where
  norm n = min n . max 0
  c' = norm (nc - 2) (c + dc)
  r' = norm (nr - 1) (r + dr)

runCommand _ _ g c r bd Swap = return (g, c, r, swapBoard c r bd, Nothing)
runCommand _ nr g c r bd Vanish = do
  (bd', scores) <- vanishAndDrop nr bd
  return (g, c, r, bd', Just scores)

runCommand _ _ g c r bd Lift = return (g', c, r, bd', Nothing)
 where
  (bd', g') = liftUpBoard g bd
----------

----- Evaluation -----

-- eval score for chain iteration
evalAt :: Int -> Score -> Score
evalAt i s = i * i * s

evalScores :: [Score] -> Score
evalScores = sum . zipWith evalAt [1..]

scoresToString :: [Score] -> String
scoresToString = evalScores &&& conv >>> integ
 where
  integ (t, s) = printf "get score %d: %s" t s
  conv = intercalate " -> " . zipWith str [1..]
  str i s = printf "%d (= %d^2 * %d)" (evalAt i s) i s

evalBoard :: Board -> Score
evalBoard cs = evalBlocks . concat $ evCs ++ evRs
 where
  evCs = map groupSizeForLine cs
  evRs = map groupSizeForLine $ transpose cs

evalBlocks :: [Int] -> Score
evalBlocks = total &&& length >>> uncurry (*)
 where
  total = sum . map evalBlock

-- evaluation for a group of continuous panels
evalBlock :: Int -> Score
evalBlock x
  | x < 3     = 0
  | otherwise = (x - 3) * 2 + 1
----------


----- Control board -----
mkBoard :: Int -> Int -> Int -> Board
mkBoard i nc nr = board
 where
  board = take center cs ++ nullLine : drop (center + 1) cs
  center = nc `div` 2
  nullLine = take nr $ repeat Null
  cs = take nc . map mkFullColumn . chunksOf (nr - ceilSpace) . map (Box . toEnum) . randomRs (0,4) . mkStdGen $ i
  mkFullColumn xs = take nr $ xs ++ repeat Null
  ceilSpace = 3

swapBoard :: Int -> Int -> Board -> Board
swapBoard c r bd = bd'
 where
  (rs, ls) = splitAt c $ bd
  (lc:rc:_) = take 2 ls
  (lc', rc') = swap lc rc r
  bd' = rs ++ lc':rc':(drop 2 ls)

liftUpBoard :: StdGen -> Board -> (Board, StdGen)
liftUpBoard g bd
  = if any ((/= Null) . last) bd
      then (bd, g)
      else liftUpBoard' g bd

liftUpBoard' :: StdGen -> Board -> (Board, StdGen)
liftUpBoard' g [] = ([], g)
liftUpBoard' g (c:cs) = (c':xs, g'')
 where
  (a, g') = randomR (0,4) g
  c' = (Box $ toEnum a) : init c
  (xs, g'') = liftUpBoard' g' cs

vanishAndDrop :: Int -> Board -> IO (Board, [Score])
vanishAndDrop numRow bd = do
  case vanish bd of
    Nothing -> return (bd, [])
    Just (bd', score) -> do
      hPutStrLn stdout $ "+" ++ show score ++ "!"
      hPutStrLn stdout $ showBoard' bd'
      threadDelay $ 1000 * 300
      bd'' <- dropLoop False numRow bd'
      (board, scores) <- vanishAndDrop numRow bd''
      return (board, score:scores)

dropLoop :: Bool -> Int -> Board -> IO Board
dropLoop pStop numRow bd = do
  let bd' = dropOne numRow bd
  if bd' == bd
    then return bd
    else do
      when pStop $ do
        hPutStrLn stdout $ showBoard' bd
        threadDelay $ 1000 * 200  

      hPutStrLn stdout $ showBoard' bd'
      threadDelay $ 1000 * 100
      dropLoop False numRow bd'

vanish :: Board -> Maybe (Board, Score)
vanish bd = if null ps then Nothing else Just (bd', score)
 where
  ps = posForBoard bd
  score = evalBoard bd
  bd' = map (\(i, col) -> map (\(j, cell) -> if (i,j) `elem` ps then Null else cell) $ zip [0..] col) $ zip [0..] bd

dropOne :: Int -> Board -> Board
dropOne numRow bd = map (dropColumn numRow) bd

dropColumn :: Int -> Col -> Col
dropColumn numRow cells = fillNull . drp $ cells
 where
  fillNull xs = take numRow $ xs ++ repeat Null
  drp = concat . map dropNull . group
  dropNull cs = if nullGrp cs then tail cs else cs
  nullGrp = (== Null) . head

posForBoard :: Board -> [(ColInd, RowInd)]
posForBoard cs = nub $ pc ++ pr
 where
  rs = transpose cs
  pc = concat $ zipWith (\i c -> zip (repeat i) (indexForLine c)) [0..] cs
  pr = concat $ zipWith (\r i -> zip (indexForLine r) (repeat i)) rs [0..]

indexForLine :: [Cell] -> [Int]
indexForLine = map fst . concat . groupValuableCellsForLine

-- |
-- >>> groupSizeForLine [Box PA, Box PA, Box PA, Box PB, Box PB, Box PB, Box PB, Null, Null, Null]
-- [3,4]
groupSizeForLine :: [Cell] -> [Int]
groupSizeForLine = map length . groupValuableCellsForLine

-- |
-- >>> groupValuableCellsForLine [Box PA, Box PA, Box PA, Box PB, Box PB, Box PB, Box PB, Null, Null, Null]
-- [[(0,Box PA),(1,Box PA),(2,Box PA)],[(3,Box PB),(4,Box PB),(5,Box PB),(6,Box PB)]]
groupValuableCellsForLine :: [Cell] -> [[(Int, Cell)]]
groupValuableCellsForLine = work
 where
  work = filter valuable . filter notNull . groupBy cmp . zip [0..]
  cmp (_,x) (_,y) = x == y
  notNull = (/= Null) . snd . head
  valuable = (>= 3) . length
----------

-- i: index from bottom
swap :: [a] -> [a] -> Int -> ([a], [a])
swap xs ys i = (xs', ys')
 where
  (xd,x,xu) = extract i xs
  (yd,y,yu) = extract i ys
  xs' = xd ++ y:xu
  ys' = yd ++ x:yu

extract :: Int -> [a] -> ([a], a, [a])
extract i xs = (as, head bs, tail bs)
 where
  (as,bs) = splitAt i xs

extract' :: Int -> [a] -> ([a], a, a, [a])
extract' i xs = (as, l, r, drop 2 bs)
 where
  (as,bs) = splitAt i xs
  (l:r:_) = take 2 bs