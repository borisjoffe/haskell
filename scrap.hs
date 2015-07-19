#!/usr/bin/runhaskell

-- Chapter 3 Part 2
-------------------
import Data.List

-- Ex 9 - 12

-- Ex 9
data Point p = Point {
		  xCoord :: Int
		, yCoord :: Int
	} deriving (Show, Eq)

data Direction d = CCW | CW | Collinear
	deriving (Show)

-- Ex 10
getDirection :: Point a -> Point b -> Point c -> Direction d
getDirection (Point x_a y_a) (Point x_b y_b) (Point x_c y_c)
	| z > 0  = CCW
	| z < 0  = CW
	| z == 0 = Collinear
	where
		-- vector AB
		x_ab = x_b - x_a
		y_ab = y_b - y_a
		-- vector BC
		x_bc = x_c - x_b
		y_bc = y_c - y_b
		-- cross product
		z = (x_ab * y_bc) - (y_ab * x_bc)

-- Ex 11
nth :: Int -> [a] -> a
nth num xs = head (drop num xs)

getAllDirections :: [Point a] -> [Direction d]
getAllDirections ps =
	if length ps < 3
	then []
	else getDirection (nth 0 ps) (nth 1 ps) (nth 2 ps) : getAllDirections (tail ps)

-- Ex 12

lowestYThenLowestX :: Point a -> Point b -> Ordering
lowestYThenLowestX a b =
	if compareY == EQ
	then compare (xCoord a) (xCoord b)
	else compareY
	where
		compareY = compare (yCoord a) (yCoord b)

-- TODO: implement
uniq :: [Point a] -> [Point a]
uniq ps = ps


{-

 -      o
 -     *|
 -   *  |  y
 - *    |
-0---------------
     x

bigger y -> increases angle; bigger x -> decreases angle
-}
getRelativePolarAngleWithXAxis :: Point a -> Double
getRelativePolarAngleWithXAxis p = yCoord p / xCoord p

-- Get point with minimum y coordinate; if multiple points have the minimum, use the one with smallest x
getLowestY :: [Point a] -> Point a
getLowestY ps =
	minimumBy (lowestYThenLowestX) ps

isCcw :: PointTriple pt -> Bool
isCcw pt = getDirection' pt == CCW

{-
a, b, c, d
-> (a,b,c) (b,c,d)
-> isCcw triple -> getDirection triple == CCW
-}

makeTriples :: [a] -> [[a]]
makeTriples xs
    | length xs < 3 = [[]] -- or [xs]???
	| otherwise = [(nth 0 xs), (nth 1 xs), (nth 2 xs)] : makeTriples (tail xs)


getHullFromDirections :: [Point a] -> [Point a]
getHullFromDirections ps =
    -- when 3 points are CCW, add points to hull, if collinear, discard middle point
    -- filter points that are not ccw out
    filter (isCcw) (makeTriples ps)

grahamScan :: [Point a] -> [Point a]
grahamScan ps =
    getHullFromDirections (lowestPoint : pointsSortedByPolarAngle)
    where
        lowestPoint = getLowestY ps
        pointsSortedByPolarAngle = sortBy getRelativePolarAngleWithXAxis restOfPoints
        restOfPoints = filter (not lowestPoint) (uniq ps)


-- Ex 8
data MyTree a = MyNode a (MyTree a) (MyTree a)
            | Empty
			  deriving (Show)

treeHeight :: MyTree t -> Int
treeHeight Empty = 0
treeHeight (MyNode n leftTree rightTree) =
	1 + max (treeHeight leftTree) (treeHeight rightTree)

testTreeHeight =
	do
		assert (treeHeight Empty)                     0
		assert (treeHeight leaf)                      1
		assert (treeHeight onlyLeft)                  2
		assert (treeHeight onlyRight)                 2
		assert (treeHeight both)                      2
		assert (treeHeight (MyNode 5 onlyLeft Empty)) 3
		where
			leaf      = MyNode 5 Empty Empty
			onlyLeft  = MyNode 5 leaf Empty
			onlyRight = MyNode 5 Empty leaf
			both      = MyNode 5 leaf leaf

-- Ex 7
myIntersperse :: [a] -> [[a]] -> [a]
myIntersperse separator []       = []
myIntersperse separator [x]      = x
myIntersperse separator (x : xs) = x ++ separator ++ (myIntersperse separator xs)

testMyIntersperse =
	do
		assert (myIntersperse "," []) []
		assert (myIntersperse "," ["a"]) "a"
		assert (myIntersperse "," ["abc","def"]) "abc,def"

-- mini test framework
assert :: (Eq a, Show a) => a -> a -> IO ()
assert actual expected =
	if actual /= expected
	then putStrLn ("\nERROR: Expected: " ++ show expected ++ " but got: " ++ show actual)
	else putStr "."

test :: IO a -> IO ()
test func =
	do
		func
		putStr "\n"
-- end test framework


-- Ex 6
sortByListLength :: [[a]] -> [[a]]
sortByListLength list = sortBy compareListLength list

compareListLength :: [a] -> [a] -> Ordering
compareListLength xs ys = compare (length xs) (length ys)


-- Ex5
isPalindrome list =
	case list of
	[]       -> True
	[x]      -> True
	(x : xs) ->
		x == last xs
		&&
		isPalindrome allButFirstAndLast
		where
			allButFirstAndLast = drop (len - 1) allButLast
			allButLast         = take (len - 1) xs
			len = length xs

-- Ex4
palindrome xs = xs ++ reverse xs

-- Ex3
myMean :: [Double] -> Double
myMean xs = sum xs / fromIntegral (length xs)

-- Ex1 - 2
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs


-- Chapter 3 Part 1
-------------------
-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- exercise 1
toList (Cons x (xs)) = x : toList xs
toList Nil = []

-- exercise 2
data Tree a = Node {
		  treeVal   :: a
		, treeLeft  :: (Maybe (Tree a))
		, treeRight :: (Maybe (Tree a))
	} deriving (Eq, Show)


-- Chapter 2 Part 2
-------------------

-- type synonyms
-- NOT data constructors, just more descriptive types
type Id = Int
type Title = String
type Author = String
type CustomerId = Int
type ReviewBody = String

data Book = Book {
		  bookId      :: Id
		, bookTitle   :: Title
		, bookAuthors :: [Author]
	} deriving (Eq, Show)

myBook = Book 123 "Title1" ["Author1"]

-- normal to have type/data constructor share the same name
-- since they're stored in separate namespaces
data BookReview = BookReview Book CustomerId ReviewBody
	deriving (Show)


-- Chapter 2 Part 1
-------------------

secondToLast :: [a] -> a
secondToLast xs =
	if length xs < 2
	then error ("xs must have length of 2 or more but had length: " ++ show (length xs))
	else head (drop 1 (reverse (xs)))
