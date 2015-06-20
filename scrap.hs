#!/usr/bin/runhaskell

secondToLast :: [a] -> a
secondToLast xs = head (drop 1 (reverse (xs)))

-- type synonyms
-- NOT data constructors, just more descriptive types
type Id = Int
type Title = String
type Author = String
type CustomerId = Int
type ReviewBody = String

data Book = Book Id Title [Author]
	deriving (Eq, Show)

myBook = Book 123 "Title1" ["Author1"]

-- normal to have type/data constructor share the same name
-- since they're stored in separate namespaces
data BookReview = BookReview Book CustomerId ReviewBody
	deriving (Show)
