myReverse :: [a] -> [a]
myReverse xs = revInto [] xs
    where
        revInto acc [] = acc
        revInto acc (x:xs) = revInto (x:acc) xs

data Q a = Q [a] [a] deriving (Show, Eq)

empty :: Q a -> Q a
empty a = Q [] []

isEmpty :: Q a -> Bool
isEmpty (Q [] []) = True
isEmpty _ = False

toQ :: [a] -> Q a
toQ xs = Q xs []

fixQ :: Q a -> Q a
fixQ (Q [] back) = Q (myReverse back) []
fixQ q = q

removeQ :: Q a -> (a, Q a)
removeQ (Q [] []) = error "Queue is empty"
removeQ (Q (x:xs) back) = (x, fixQ $ Q xs back)

frontQ :: Q a -> a
frontQ (Q [] []) = error "Queue is empty"
frontQ (Q (x:xs) back) = x

addQ :: Q a -> a -> Q a
addQ (Q front back) x = fixQ $ Q front (x:back)

printQ :: Show a => Q a -> IO ()
printQ q = do
    if not (isEmpty q)
        then do
            let (x, q') = removeQ q
            print x
            printQ q'
        else putStrLn "queue empty"