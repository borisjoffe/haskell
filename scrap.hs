#!/usr/bin/runhaskell

secondToLast :: [a] -> a
secondToLast xs = head (drop 1 (reverse (xs)))
