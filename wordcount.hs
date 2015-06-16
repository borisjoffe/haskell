#!/usr/bin/runhaskell

main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
