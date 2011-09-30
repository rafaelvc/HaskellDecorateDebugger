import Data.Array
import Control.Monad.State
import Data.Maybe
import Data.List
import System.IO

type Move  = (XO, Int)

data XO = X | O
    deriving Eq

instance Show XO where
    show X = "X"
    show O = "O"

other :: XO -> XO
other X = O
other O = X

data Board = Board (Array Int (Maybe XO))

test 0 = putStrLn ""
test x = putStrLn $ (show x)

instance Show Board where
    show (Board b) = "+---+---+---+\n"
                  ++ "| " ++ (p(b!1)) ++ " | " ++ (p(b!2)) ++ " | " ++ (p(b!3)) ++ " |\n"
                  ++ "+---+---+---+\n"
                  ++ "| " ++ (p(b!4)) ++ " | " ++ (p(b!5)) ++ " | " ++ (p(b!6)) ++ " |\n"
                  ++ "+---+---+---+\n"
                  ++ "| " ++ (p(b!7)) ++ " | " ++ (p(b!8)) ++ " | " ++ (p(b!9)) ++ " |\n"
                  ++ "+---+---+---+\n"
        where
            p :: Maybe XO -> String
            p (Just x) = show x
            p Nothing  = " "

emptyBoard = Board (array (1,9) $ zip [1..9] (repeat Nothing))

winTest :: Board -> (Int, Int, Int) -> Maybe XO
winTest (Board board) (ia, ib, ic) = markWinTest (board ! ia) (board ! ib) (board ! ic)

markWinTest :: Maybe XO -> Maybe XO -> Maybe XO -> Maybe XO
markWinTest (Just a) (Just b) (Just c)
    | a == b && b == c
        = Just a
markWinTest _ _ _ = Nothing

win :: Board -> Maybe XO
win board = foldr mplus Nothing $
                map (winTest board) [(1,2,3),
                                     (4,5,6),
                                     (7,8,9),
                                     (1,4,7),
                                     (2,5,8),
                                     (3,6,9),
                                     (1,5,9),
                                     (3,5,7)]

full :: Board -> Bool
full (Board board) = and $ map (maybe False (const True)) (elems board)

move :: Move -> Board -> Maybe Board
move (pl, s) (Board board)
    | s `elem` [1..9] = maybe (Just $ Board $ board // [(s, Just pl)])
                              (const Nothing)
                              (board ! s)
    | otherwise       = Nothing


wins :: XO -> Board -> Bool
wins pl board = maybe False
                      (\winner -> winner == pl)
                      (win board)

allBoards :: XO -> Board -> [Board]
allBoards pl board =
    concatMap maybeToList
        (map (\s -> move (pl, s) board) [1..9])

data Player = AI
            | Human String

act :: Player -> XO -> Board -> IO (Maybe Board)
act player@(Human name) pl board = do
    putStr $ "Your move, " ++ name ++ " (" ++ show pl ++ "): "
    space <- readLn :: IO Int
    maybe (do putStrLn "Invalid move"
              act player pl board)
          (return . Just)
          (move (pl,space) board)

--   The AI follows   --

act AI pl board = return $ aiMakeMove pl board

data Rating = MayLose | WontLose | WillWin
    deriving (Eq, Ord, Enum)

finiteMin :: (Eq a) => (a -> a -> Bool) -> a -> a -> [a] -> a
finiteMin _ _ mx [] = mx
finiteMin less mn mx (a:as)
    | a == mn   = mn
    | otherwise = let mas = finiteMin less mn mx as in
                      if mas `less` a
                          then mas
                          else a

ratingMin :: [Rating] -> Rating
ratingMin = finiteMin (<) MayLose WillWin

ratingMax :: [Rating] -> Rating
ratingMax = finiteMin (>) WillWin MayLose


aiMove :: XO -> Board -> Rating
aiMove pl board =
    makeRating pl board $ ratingMin $ do
            oppmove <- allBoards (other pl) board
            return $ makeRating pl oppmove $ ratingMax $ do
                mymove <- allBoards pl oppmove
                return $ aiMove pl mymove

makeRating :: XO -> Board -> Rating -> Rating
makeRating pl board def
    | wins pl board         = WillWin
    | wins (other pl) board = MayLose
    | full board            = WontLose
    | otherwise             = def

maxRating :: (Board -> Rating) -> [Board] -> Board
maxRating f as = maximumBy
                    (\l r -> f l `compare` f r)
                    as

aiMakeMove :: XO -> Board -> Maybe Board
aiMakeMove pl board =
    let boards = allBoards pl board in
    if length boards == 0
        then Nothing
        else Just $ maxRating (aiMove pl) boards

--    End of AI   --


playGame :: (Player,Player) -> StateT (XO,Board) IO (Maybe XO)
playGame (cur,oth) = do
    (pl, board) <- get
    liftIO $ putStrLn $ show board
    maybe (if full board
                then return Nothing
                else (do mnextboard <- liftIO $ act cur pl board
                         maybe (return $ Just $ other pl)
                               (\newboard -> do put (other pl, newboard)
                                                playGame (oth,cur))
                               mnextboard))
          (return . Just)
          (win board)


makePlayer :: String -> Player
makePlayer "AI" = AI
makePlayer name = Human name

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr $ "Player 1: "
    p1 <- getLine
    putStr $ "Player 2: "
    p2 <- getLine
    (mwinner,(_,finalboard)) <- runStateT (playGame (makePlayer p1, makePlayer p2)) (X,emptyBoard)
    putStrLn $ maybe "No winner" (\winner -> show winner ++ " wins!") mwinner
    putStrLn $ show finalboard
