module Q4 where

normaliseSpace::String -> String
normaliseSpace [] = []
normaliseSpace (' ':' ':xs) = normaliseSpace (' ':xs)
normaliseSpace (x:xs) = x:normaliseSpace xs

normaliseFront::String -> String
normaliseFront [] = []
normaliseFront (x:xs)
      | x == ' ' = normaliseFront xs
      | otherwise = x:xs

-- @assumption: string is of finite length
normaliseBack::String -> String
normaliseBack [] = []
normaliseBack x = reverse(normaliseFront(reverse x))

normalise::String -> String
normalise [] = []
normalise x = normaliseSpace(normaliseBack(normaliseFront(x)))

prefix::String -> String -> Bool
prefix [] [] = True
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = x == y && prefix xs ys

substr::String -> String -> Bool
substr [] [] = True
substr [] _ = True
substr _ [] = False
substr x y = if prefix x y then
                True
             else if substr x (tail y) then
                True
             else
                False

-- @assumption: both arguments finite
postfix::String -> String -> Bool
postfix [] [] = True
postfix [] y = True
postfix x [] = False
postfix x y = prefix (reverse x) (reverse y)

-- substitution after first match. e.g. substitude "aa" "b" "aaa" = "ba"
substitute::String -> String -> String -> String
substitute xs ys [] = []
substitute xs ys zs@(z:zz) = if prefix xs zs then
                                ys ++ substitute xs ys (drop(length xs)zs)
                             else
                                z:substitute xs ys zz
