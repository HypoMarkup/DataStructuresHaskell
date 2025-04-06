data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = sort (filter (< x) xs) ++ [x] ++ sort (filter (>= x) xs)

sortListNoDups :: Ord a => [a] -> [a]
sortListNoDups [] = []
sortListNoDups xs = noDups $ sort xs
    where
        noDups :: Ord a => [a] -> [a]
        noDups [] = []
        noDups [x] = [x]
        noDups (x:y:zs)
            | x == y = noDups (y:zs)
            | otherwise = x : noDups (y:zs)


sortedToTree :: [a] -> Tree a
sortedToTree [] = Empty
sortedToTree [x] = Leaf x
sortedToTree [x,y] = Node y (Leaf x) Empty
sortedToTree xs = Node root leftTree rightTree
    where
    n = length xs `div` 2
    (left, root:right) = splitAt n xs
    leftTree = sortedToTree left
    rightTree = sortedToTree right

treeToSorted :: Tree a -> [a] -> [a]
treeToSorted Empty acc = acc
treeToSorted (Leaf a) acc = a:acc
treeToSorted (Node x l r) acc = (treeToSorted l []) ++ [x] ++ (treeToSorted r []) ++ acc


left :: Tree a -> Tree a
left Empty = Empty
left (Node x l r) = l

right :: Tree a -> Tree a
right Empty = Empty
right (Node x l r) = r

-- no duplicates, also currently completely broken
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Leaf x
insert x (Leaf y)
    | x < y = balance x $ Node y (Leaf x) Empty
    | x > y = balance x $ Node y Empty (Leaf x)
    | x == y = (Leaf y)
insert x (Node y left right)
    | x < y = balance x $ Node y (insert x left) right
    | y < x = balance x $ Node y left (insert x right)
    | otherwise = Node y left right

insertMany :: Ord a => [a] -> Tree a -> Tree a
insertMany [] t = t
insertMany (x:xs) t = insertMany xs $ insert x t

findLargest :: Eq a => Tree a -> a
findLargest Empty = error "atempted to find largest int in empty subtree"
findLargest (Leaf x) = x
findLargest (Node x left right)
    | right /= Empty = findLargest right
    | otherwise = x


remove :: Ord a => a -> Tree a -> Tree a
remove x Empty = Empty
remove x (Leaf y) = if x == y then Empty else Leaf y
remove x t@(Node y left right)
    | x < y = Node y (remove x left) right
    | x > y = Node y left (remove x right)
    | x == y = removalHelper x t
        where 
            removalHelper x (Node y left right)
                | left == Empty = Empty

rotateLeft :: Tree a -> Tree a
rotateLeft (Node y yl (Node x xl xr)) = Node x (Node y yl xl) xr

rotateRight :: Tree a -> Tree a
rotateRight (Node y (Node x xl xr) yr) = Node x xl (Node y xr yr)

rotateRL :: Tree a -> Tree a
rotateRL (Node y l r) = rotateLeft (Node y l (rotateRight r))

rotateLR :: Tree a -> Tree a
rotateLR (Node y l r) = rotateRight (Node y (rotateLeft l) r)

scoreBalance :: Tree a -> Int
scoreBalance (Node x l r) = getHeight r - getHeight l

-- start simple for now, can add rotations later
hardReset :: Tree a -> Tree a
hardReset t = sortedToTree (treeToSorted t [])

balance :: Ord a => a -> Tree a -> Tree a
balance key t@(Node x l r) = let score = scoreBalance t in
    if score > 1 && key > x
        then rotateLeft t
    else if score < -1 && key < x
        then rotateRight t
    else if score > 1 && key < x
        then rotateRL t
    else if score < -1 && key > x
        then rotateLR t
    else t

getHeight :: Tree t -> Int
getHeight (Leaf x) = 1
getHeight Empty = 1
getHeight (Node x l r) = 1 + max (getHeight l) (getHeight r)

ensureBalance :: Tree a -> Tree a
ensureBalance = undefined

main :: IO ()
main = do
    let tree = sortedToTree $ sortListNoDups [0,3,3,4,5,2,3,4,4,5,6,3,6,7,8,9]
    let tree' = flip insertMany tree $ take 15 $ filter even [15..30]

    printTree tree'







-- LLM helped to make ASCII string for debugging
printTree :: Show a => Tree a -> IO ()
printTree tree = putStrLn (unlines (printHelper tree "" True))
    where
        printHelper :: Show a => Tree a -> String -> Bool -> [String]
        printHelper Empty prefix isTail =
            [prefix ++ (if isTail then "└─ " else "├─ ") ++ "Empty"]
        printHelper (Leaf x) prefix isTail =
            [prefix ++ (if isTail then "└─ " else "├─ ") ++ show x]
        printHelper (Node x left right) prefix isTail =
            [prefix ++ (if isTail then "└─ " else "├─ ") ++ show x] ++
            printHelper left (prefix ++ (if isTail then "    " else "│   ")) False ++
            printHelper right (prefix ++ (if isTail then "    " else "│   ")) True
