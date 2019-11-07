import Test.QuickCheck
import Data.Char

--1)a)
max1 :: Int -> Int -> Int -> Int
max1 x y z = if ((x >= y) && (x >= z))
             then x
             else if ((y >= x) && (y >= z))
             then y
             else z

--max1 986529 975804 690238
--986529

max2 :: Int -> Int -> Int -> Int
max2' x y
    | x >= y = x
    | otherwise = y

max2 x y z = max2' x (max2' y z)

--max2 986529 975804 690238
--986529

--1)b)
checkcorrectness :: Int -> Int -> Int -> Bool
checkcorrectness x y z =
    max1 x y z == max2 x y z

-- +++ OK, passed 100 tests.

--2)
luigi :: Float -> Float -> Float
luigi x y = fromIntegral(round((((square x * 3.142) * 0.002 + (0.6 * y)) * 1.6) * 10^2)) / 10^2

square :: Float -> Float
square x = x * x

--3)a)
counta :: [Char] -> Int
counta xs = length([x | x <- xs, isDigit x])

--b)
countb :: (a -> Bool) -> [a] -> Int
countb f xs = length(countb' f xs)

countb' :: (a -> Bool) -> [a] -> [a]
countb' f [] = []
countb' f (x:xs) = if f x then
                    x : countb' f xs
                  else
                    countb' f xs

--c)
countc :: [Char] -> Int
countc [] = 0
countc (x:xs) = if isDigit x then
                  1 + countc xs
                else
                  0 + countc xs 

--5)a)
won :: [Int] -> Bool
won piles = sum piles == 0

--b)
validMove :: [Int] -> Int -> Int -> Bool
validMove piles num coins = head(drop (num - 1)(piles)) >= coins

--c)
takeAway :: [Int] -> Int -> Int -> [Int]
takeAway [] _ _ = []
takeAway piles@(x:xs) num coins
  | num == 1  = (x - coins):xs
  | otherwise = x:takeAway xs (num - 1) coins 

--d)
getMove1 :: [Int] -> IO(Int, Int)
getMove1 piles = do putStr "Enter a pile number: "
                    x <- readLn
                    putStr "How many coins would you like to take? "
                    y <- readLn
                    return (x,y)

--e)
getMove2 :: [Int] -> IO(Int, Int)
getMove2 piles = do putStr "Enter a pile number: "
                    x <- readLn
                    putStr "How many coins would you like to take? "
                    y <- readLn
                    if validMove piles x y then
                      return (x,y)
                    else
                      getMove2 piles

--f)
displayGame :: [Int] -> IO()
displayGame [] = putStr ""
displayGame piles = do putStrLn (show piles ++ "\n")
                       displayGame' piles 1

displayGame' :: [Int] -> Int -> IO()
displayGame' [] _ = putStr ""
displayGame' piles n = do putStr (show n)
                          putStr ": "
                          redoAction (head(piles)) (putStr "*")
                          displayGame' (drop 1 (piles)) (n+1)

redoAction :: Int -> IO() -> IO()
redoAction n action
    | n <= 0    = putStrLn ""
    | otherwise = do action
                     redoAction (n-1) action

--g)
readPiles :: IO [Int]
readPiles = do putStr "Enter starting piles as [list]: "
               piles <- readLn
               if null piles then
                do
                 let piles = [5,4,3,6]
                 return piles
               else
                 return piles

nim :: IO()
nim = do putStr "Enter name for player 1: "
         p1 <- readLn
         putStr "Enter name for player 2: "
         p2 <- readLn
         piles <- readPiles
         nimaux p1 p2 piles


nimaux :: String -> String -> [Int] -> IO()
nimaux p1 p2 piles = do putStrLn ""
                        displayGame piles
                        if won piles then
                          do putStrLn ""
                             putStr p2
                             putStrLn " Wins!"
                        else
                          do putStrLn ""
                             putStr p1
                             putStrLn ": "
                             t <- getMove2 piles
                             let x = fst t
                             let y = snd t
                             nimaux p2 p1 (takeAway piles x y)

--h)
--Enter name for player 1: "Sean"
--Enter name for player 2: "Isaac"
--Enter starting piles as [list]: []

--[5,4,3,6]

--1: *****
--2: ****
--3: ***
--4: ******

--Sean:
--Enter a pile number: 1
--How many coins would you like to take? 5

--[0,4,3,6]

--1:
--2: ****
--3: ***
--4: ******

--Isaac:
--Enter a pile number:



--6)a)
stratB :: [Int] -> (Int,Int)
stratB piles = stratB' piles 0

stratB' :: [Int] -> Int -> (Int,Int)
stratB' piles n 
  | (piles!!n) == 0 = stratB' piles (n+1)
  | otherwise       = (n+1,piles!!n)

