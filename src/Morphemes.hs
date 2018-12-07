module Morphemes where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Syllables
import Types
import Util

randomMorpheme :: MonadIO m => Phonology -> m Morpheme
randomMorpheme phonology = do
    l <- randRange $ phonology^.morphemeLengthRange
    Morpheme <$> replicateM (fromIntegral l) (randomSyllable phonology)

randomLexeme :: MonadIO m => Phonology -> m Lexeme
randomLexeme phonology = do
    l <- randRange $ phonology^.lexemeLengthRange
    Lexeme <$> replicateM (fromIntegral l) (randomMorpheme phonology)

