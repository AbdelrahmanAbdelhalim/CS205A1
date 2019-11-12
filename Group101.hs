import Test.QuickCheck
import Data.Char
import Data.Bits

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
-- To find the area of the pizza we need to do pi * radius^2.
-- After finding the area of the pizza we then add prices of toppings along with
-- a multiplier for profit.
luigi :: Float -> Float -> Float
luigi diameter toppings = fromIntegral(round((((pi * (square (diameter/2))) 
  * 0.002 + (0.6 * toppings)) * 1.6) * 10^2)) / 10^2

square :: Float -> Float
square x = x * x

--Pizza Bambini (£8.02) is less expensive than Pizza Famiglia (£12.21)
-- luigi 15 6
-- = 6.33
-- luigi 32 2
-- = 12.21

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
-- won funtion sums all elements in the list to check if every pile is empty or not.
won :: [Int] -> Bool
won piles = sum piles == 0

--b)
-- validMove checks if the coins in the user's specified pile is greater than how many
-- coins they want to take away and also ensures that the user doesn't try to take away
-- 0 coins.
validMove :: [Int] -> Int -> Int -> Bool
validMove piles num coins = (head(drop (num - 1)(piles)) >= coins) && (coins /= 0)

--c)
-- takeAway function splits the piles list into the first element followed by all the 
-- other elements. It then recurses through the function until we get to the desired pile
-- to take coins away from. When we get there, the coins are taken from the pile.
takeAway :: [Int] -> Int -> Int -> [Int]
takeAway [] _ _ = []
takeAway piles@(x:xs) num coins
  | num == 1  = (x - coins):xs
  | otherwise = x:takeAway xs (num - 1) coins 

--d)
-- getMove1 is a general getMove function that receives a move from the player and returns 
-- the specified pile and coins to take away. 
getMove1 :: [Int] -> IO(Int, Int)
getMove1 piles = do putStr "Enter a pile number: "
                    x <- readLn
                    putStr "How many coins would you like to take? "
                    y <- readLn
                    return (x,y)

--e)
-- getMove2 builds on the code in getMove1 but also applies a validMove check as well. If the
-- move is not valid then we start the function again asking the user for another pile and coin value.
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
-- displayGame uses the displayGame' helper function to be able to print the correct amount of piles
-- and coins to the screen.
displayGame :: [Int] -> IO()
displayGame [] = putStr ""
displayGame piles = do putStrLn (show piles ++ "\n")
                       displayGame' piles 1

-- This displayGame' helper function prints the number of the piles first followed by a list of
-- '*'s. It uses the redoAction function which prints the '*'s that's relative to the size
-- of the pile.
displayGame' :: [Int] -> Int -> IO()
displayGame' [] _ = putStr ""
displayGame' piles n = do putStr (show n)
                          putStr ": "
                          redoAction (head(piles)) (putStr "*")
                          displayGame' (drop 1 (piles)) (n+1)

-- redoAction does what it says on the tin. It repeats the action n times. In this case it repeats a print of '*'
-- n times.
redoAction :: Int -> IO() -> IO()
redoAction n action
    | n <= 0    = putStrLn ""
    | otherwise = do action
                     redoAction (n-1) action

--g)
-- Reads an input of [Int] from the user and returns that list as a list of piles. If en empty list is entered
-- then a default list of [5,4,3,6] is returned
readPiles :: IO [Int]
readPiles = do putStr "Enter starting piles as [list]: "
               piles <- readLn
               if null piles then
                do
                 let piles = [5,4,3,6]
                 return piles
               else
                 return piles

-- This function nim takes in names for player 1 and 2 and then passes them into the function
-- nimaux for the game of nim to be played.
nim :: IO()
nim = do putStr "Enter name for player 1: "
         p1 <- readLn
         putStr "Enter name for player 2: "
         p2 <- readLn
         piles <- readPiles
         nimaux p1 p2 piles

-- nimaux is the main function for running the game of nim. It checks if the game is finished 
-- before allowing any moves to be made. After every move is made, player 1 and 2's names are
-- swapped so we can keep track of the current player and who's won the game.
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
-- stratB uses a helper function to keep track on which element we're currently looking at
stratB :: [Int] -> (Int,Int)
stratB piles = stratB' piles 0

-- stratB' then finds the first pile that contains coins and returns a pile number and number
-- of coins in that pile.
stratB' :: [Int] -> Int -> (Int,Int)
stratB' piles n 
  | (piles!!n) == 0 = stratB' piles (n+1)
  | otherwise       = (n+1,piles!!n)

--b)
-- nimAI reads a list of piles from the player and then passes the strategy and list of piles
-- into a helper function
nimAI :: ([Int] -> (Int,Int)) -> IO()
nimAI strat = do piles <- readPiles
                 nimAI' strat piles "AI"

-- nimAI runs the whole program where the player plays against an AI using a specified strategy.
-- The code works the same way where we use a determined move by a strategy for AI. We just change the player
-- name on each go to track who wins the game.
nimAI' :: ([Int] -> (Int,Int)) -> [Int] -> String -> IO()
nimAI' strat piles player = do putStrLn ""
                               displayGame piles
                               if won piles then
                                do putStrLn ""
                                   putStr player
                                   putStrLn " Win!"
                               else if player == "AI" then
                                do putStrLn "\nYour Go: "
                                   t <- getMove2 piles
                                   let x = fst t
                                   let y = snd t
                                   nimAI' strat (takeAway piles x y) "You"
                               else
                                do putStrLn "\nAI's Go: "
                                   let aiMove = strat piles
                                   let row = fst aiMove
                                   let coins = snd aiMove
                                   nimAI' strat (takeAway piles (fst aiMove) (snd aiMove)) "AI"

--c)
-- The xorCalc function uses `xor` between each element in the piles list. This effectively
-- works as a simpler way of finding a bitwise nimsum rather than converting every pile to a
-- binary value and then adding them altogether. So we use xorCalc to find out if the bitwise
-- nimsum of all elements is 0 or not. If it's not 0 then the AI has the winning strategy.
xorCalc :: [Int] -> Int
xorCalc [] = 0
xorCalc (x:[]) = x
xorCalc (x:xs) = x `xor` (head xs) `xor` xorCalc(tail xs)

-- This strat works so that if the AI has a losing strategy, where the bitwise nimsum of piles
-- at the start of their turn is 0, then we just use stratB. Otherwise we pass the piles into a
-- moveCalc function starting at pile 0 and starting with coins being a value of 1.
stratI :: [Int] -> (Int,Int)
stratI piles = if xorCalc piles == 0 then
                    stratB piles
                  else
                    moveCalc piles 0 1

-- This function basically checks if the current value of coins is greater than the value of 
-- coins in the current pile we're looking at. If so, we increment pile by 1 and set coins to
-- 1 again.
moveCalc :: [Int] -> Int -> Int -> (Int,Int)
moveCalc piles pile coins = if coins <= piles!!pile then
                              moveCalc' piles pile coins
                            else
                              moveCalc' piles (pile+1) 1

-- This function is used as a loop to check if the current proposed move will leave the piles
-- in  a state where their bitwise nimsum is 0. If not then we increment the coins by 1 and start
-- the process again at moveCalc. We do this until we find a move that leaves the piles in a state
-- where their bitwise nimsum is , leaving the player in a losing position.
moveCalc' :: [Int] -> Int -> Int -> (Int,Int)
moveCalc' piles pile coins = if xorCalc (takeAway piles (pile+1) coins) == 0 then
                              (pile+1,coins)
                             else
                              moveCalc piles pile (coins+1)
 



