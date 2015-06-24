#!/usr/bin/runhaskell

-- Chapter 3 Part 2
-------------------
import Data.List

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
