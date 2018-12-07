module Phonemes where

import Types

-- TODO: Expand
consonantPhonemes :: [Phoneme]
consonantPhonemes = map Phoneme
    ["b", "d", "f", "g", "h", "j", "k", "l", "m", "n"
    ,"p", "r", "s", "t", "v", "w", "y", "z", "sh", "ch"
    ,"th"]

-- TODO: Expand
vowelPhonemes :: [Phoneme]
vowelPhonemes = map Phoneme
    ["a", "e", "i", "o", "u"]

isVowel :: Phoneme -> Bool
isVowel p = p `elem` vowelPhonemes

