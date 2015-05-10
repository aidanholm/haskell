{-Problem 1-}

myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

{-Problem 2-}

myButLast :: [a] -> a
myButLast (x:xs:[]) = x
myButLast (x:xs) = myButLast xs

{-Problem 3-}

elementAt :: (Integral b) => [a] -> b -> a
elementAt list index
    | index == 0 = head list
    | index  > 0 = elementAt (tail list) (index-1)

{-Problem 4-}

myLength :: (Num a) => [b] -> a
myLength [] = 0
myLength list = 1 + myLength (tail list)

{-Problem 5-}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

{-Problem 6-}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == myReverse list

{-Problem 7-}

data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List l) = foldl (++) [] $ map flatten l

{-Problem 8-}

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs:xss)
    | x == xs = compress $ x:xss
    | x /= xs = x:(compress $ xs:xss)

{-Problem 9-}

rpack :: (Eq a) => ([[a]],[a]) -> ([[a]],[a])
rpack (a,[]) = (a,[])
rpack ([],(x:xs)) = rpack ([[x]], xs)
rpack (p,(x:xs)) = if x == head (last p)
                    then rpack ((init p) ++ [(x : last p)], xs)
                    else rpack (p ++ [[x]], xs)

pack :: (Eq a) => [a] -> [[a]]
pack list = fst $ rpack ([],list)

{-Problem 10-}

{-encode :: (Num a) => [a] -> [(n,a)]-}
encode str = map (\x -> (length x, head x)) $ pack str

{-Problem 11-}

data Run a = Multiple Int a | Single a deriving (Show)
encodeModified str = map (\x -> if fst x == 1 then (Single $ snd x) else (Multiple (fst x) (snd x))) $ encode str

{-Problem 12-}

decodeModified a = foldl (++) [] $ map f a
    where f (Single a) = [a]
          f (Multiple n a) = take n $ repeat a

{-Problem 13-}

runCount :: Run a -> Int
runCount (Single _) = 1
runCount (Multiple n _) = n

runItem :: Run a -> a
runItem (Single i) = i
runItem (Multiple _ i) = i

encodeDirectHelper :: (Eq a) => ([Run a],[a]) -> ([Run a],[a])
encodeDirectHelper (a,[]) = (a,[])
encodeDirectHelper ([],(x:xs)) = encodeDirectHelper ([Single x],xs)
encodeDirectHelper (p,(x:xs)) = if x == runItem (last p)
                    then encodeDirectHelper ((init p) ++ [Multiple (1 + runCount (last p)) x], xs)
                    else encodeDirectHelper (p ++ [Single x], xs)

encodeDirect list = fst $ encodeDirectHelper ([],list)

{-Problem 14-}

dupli :: [a] -> [a]
dupli list = repli list 2

{-Problem 15-}

repli :: [a] -> Int -> [a]
repli list num = foldl (++) [] $ map (\x -> take num (repeat x)) list

{-Problem 16-}

dropEvery :: [a] -> Int -> [a]
dropEvery list num = helper list (num-1) num
    where helper [] _ _ = []
          helper list n num = if n == 0 then helper (tail list) (num-1) num else (head list) : helper (tail list) (n-1) num
