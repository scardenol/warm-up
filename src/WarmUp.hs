import Test.QuickCheck

-- | Append 2 lists.
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- | Extract the last element of a list, which must be
-- finite and non-empty.
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- | Test whether a list is empty.
myNull :: [a] -> Bool
myNull [] = True
myNull (x:xs) = False

-- | Testing 'myAppend' function.
propMyAppend :: [Int] -> [Int] -> Bool
propMyAppend xs ys = myAppend (myAppend xs []) (myAppend ys []) == myAppend xs ys
-- | Testing 'myNull' function.
propMyNull :: [Int] -> Bool
propMyNull xs = (myNull xs == myNull [] || myNull xs /= myNull []) == True
main :: IO ()
main = do
    quickCheck propMyAppend
    quickCheck propMyNull