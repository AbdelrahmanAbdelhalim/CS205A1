import System.IO


won :: [Int] -> Bool
won [] = True
won xs = if head(xs) == 0 then True && won (tail xs) else False

validMove ::  [Int] -> Int -> Int -> Bool
validMove xs x y =  y <= head (drop (x-1) (xs))

takeAway :: [Int] -> Int -> Int -> [Int]
takeAway [] x y = []
takeAway xs x y = if x-1 == 0 then ( if (validMove (xs) (1) (y)) then [head (xs) - y] ++ takeAway (tail xs) (x-1) (y) else [head (xs)] ++ takeAway (tail xs) (x-1) (y)) 
                  else [head (xs)] ++ takeAway (tail xs) (x-1) (y)   


getMove1 :: [Int] -> IO(Int,Int)
getMove1 xs = do putStrLn "Please enter the pile number :"
                 input1 <- getLine
                 putStrLn "Please enter the number of coins :"
                 input2 <- getLine
                 let x = (read input1 :: Int)
                 let y = (read input2 :: Int)
                 return (x,y)

getMove2 :: [Int] -> IO(Int,Int)
getMove2 xs = do putStrLn "Please enter the pile number :"
                 input1 <- getLine
                 putStrLn "Please enter the number of coins :"
                 input2 <- getLine
                 let x = (read input1 :: Int)
                 let y = (read input2 :: Int)
                 if validMove xs x y then return (x,y) else do putStrLn "Please enter a valid move"
                                                               getMove2 xs                        

displayGame :: [Int] -> IO()
displayGame [] = putStrLn ""  
displayGame xs = do 
                    if head (xs) == 0 then 
                       do putStrLn ""
                          displayGame (tail xs) 
                    else do putStr "*"
                            displayGame ([head(xs) - 1] ++ drop (1) (xs))

nim :: IO()
nim = do putStrLn "Please enter Player 1 name: "
         x <- getLine
         putStrLn "Please enter Player 2 name: "
         y <- getLine
         putStrLn "Please enter the game setup as a list: "
         input3 <- getLine
         if null input3 then 
            do let z = [5,4,3,6]
               numaux (x,y) z
         else do 
                let z = (read input3 :: [Int])
                numaux (x,y) z

numaux :: (String,String) -> [Int] -> IO()
numaux (x,y) xs = do displayGame xs
                     if won xs then 
                        do putStrLn ""
                           putStr y
                           putStrLn " Won!"
                     else do i <- getMove2 xs
                             let pile = fst i
                             let number = snd i
                             numaux (y,x) (takeAway xs (pile) (number))


stratB :: [Int] -> (Int,Int)
stratB xs = stratB' xs 1

stratB' :: [Int] -> Int ->  (Int,Int)
stratB' xs x = if head xs == 0 then stratB' (xs) (x+1) else (x,head(xs))

nimAi :: ([Int] -> (Int,Int)) -> IO()
nimAi fx  = nimAiPlay (fx) (nimAigetSetup)


nimAigetSetup :: IO [Int]
nimAigetSetup = do (putStrLn "Please enter the game Setup :"
                    input1 <- getLine
                    return input1)

 nimAiPlay :: ([Int] -> (Int,Int)) -> [Int] -> IO()
 nimAiPlay fx xs = do putStrLn "Hello World"



