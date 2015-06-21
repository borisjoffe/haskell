#!/usr/bin/runhaskell

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
secondToLast xs = head (drop 1 (reverse (xs)))
