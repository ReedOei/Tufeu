module Syllables where

import Control.Lens
import Control.Monad.IO.Class

import Data.List
import Data.Maybe

import Phonemes
import Types
import Util

randomSyllableComp :: MonadIO m => Phonology -> SyllableComp -> m Phoneme
randomSyllableComp phonology C = choice $ phonology^.consonants
randomSyllableComp phonology V = choice $ phonology^.vowels

randomSyllableType :: MonadIO m => Phonology -> SyllableType -> m Syllable
randomSyllableType phonology (SyllableType syllableComps) =
    Syllable <$> mapM (randomSyllableComp phonology) syllableComps

randomSyllable :: MonadIO m => Phonology -> m Syllable
randomSyllable phonology = do
    syllableType <- choice $ phonology^.syllableTypes

    randomSyllableType phonology syllableType

-- TODO: Expand this system
syllableTypesList =
    [SyllableType [C,V,C]
    ,SyllableType [C,C,V]
    ,SyllableType [V,C]
    ,SyllableType [C,V]
    ,SyllableType [C,V] -- Duplicate intentional to increase prob.
    ,SyllableType [V]]

replaceVowels :: Phoneme -> Syllable -> Syllable
replaceVowels rep (Syllable phonemes) = Syllable $ map (\p -> if isVowel p then rep else p) phonemes

-- TODO: Do this better so this sort of hack isn't necessary
vowelFrom :: Syllable -> Phoneme
vowelFrom (Syllable phonemes) = fromJust $ find isVowel phonemes

