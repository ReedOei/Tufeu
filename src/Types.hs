{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Control.Lens ((^.))
import qualified Control.Lens as Lens
import Control.Monad.IO.Class
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Prelude hiding (Word)

import System.Random

import Util

class PrettyPrint a where
    prettyPrint :: a -> String

data SyllableComp = C | V -- Consonant or vowel
    deriving (Show, Eq, Ord, Read)
data SyllableType = SyllableType [SyllableComp]
    deriving (Show, Eq, Ord, Read)

data NumberInf = Singular
               | Plural
               | NumberInf Integer
    deriving (Show, Eq, Ord, Read)

data Phoneme = Phoneme String
    deriving (Show, Eq, Ord, Read)
data Syllable = Syllable [Phoneme]
    deriving (Show, Eq, Ord, Read)
data Morpheme = Morpheme [Syllable]
    deriving (Show, Eq, Ord, Read)
data Lexeme = Lexeme [Morpheme]
    deriving (Show, Eq, Ord, Read)
data Word = Word [Lexeme] -- TODO: Add other things to words such as case, tense, number, gender, etc.
    deriving (Show, Eq, Ord, Read)
data Sentence = Sentence [Word]
    deriving (Show, Eq, Read)

instance PrettyPrint Phoneme where
    prettyPrint (Phoneme s) = s

instance PrettyPrint Syllable where
    prettyPrint (Syllable phonemes) = concatMap prettyPrint phonemes

instance PrettyPrint Morpheme where
    prettyPrint (Morpheme syllables) = concatMap prettyPrint syllables

instance PrettyPrint Lexeme where
    prettyPrint (Lexeme morphemes) = concatMap prettyPrint morphemes

instance PrettyPrint Word where
    prettyPrint (Word lexemes) = concatMap prettyPrint lexemes

instance PrettyPrint Sentence where
    prettyPrint (Sentence words) = unwords $ map prettyPrint words

data PersonType = First
                | Second
                | Third
    deriving (Show, Eq, Ord, Read)

data PartOfSpeech = Noun
                  | Verb
                  | Adjective
                  | Adverb
                  | Conjunction
                  | Preposition
                  | Interjection
    deriving (Show, Eq, Ord, Read)

-- TODO: Maybe allow ergative-absolutive...
data CaseType = Nominative
              | Accusative
              | Dative
              | Genitive
              | Vocative
              | Ablative
              | Prepositional
              | Instrumental
    deriving (Show, Eq, Ord, Read)

data LexemeInfo = LexemeInfo
    { _definite :: Bool
    , _number :: NumberInf
    , _caseType :: CaseType
    , _partOfSpeech :: PartOfSpeech
    , _gender :: Lexeme
    , _person :: PersonType }
    deriving (Show, Eq, Ord, Read)
Lens.makeLenses ''LexemeInfo

instance PrettyPrint LexemeInfo where
    prettyPrint info = show (info^.definite, info^.number, info^.caseType, info^.partOfSpeech, prettyPrint (info^.gender), info^.person)

data Phonology = Phonology
    { _consonants :: [Phoneme]
    , _vowels :: [Phoneme]
    , _syllableTypes :: [SyllableType]
    , _morphemeLengthRange :: (Integer, Integer)
    , _lexemeLengthRange :: (Integer, Integer) }
    deriving (Show, Read)
Lens.makeLenses ''Phonology

data InflectionLocation = Prefix
                        | Suffix
                        | Infix Integer
                        -- TODO: Maybe this should be a separate thing
                        | ParticleSuffix
                        | ParticlePrefix
    deriving (Show, Eq, Ord, Read)

data InflectionType = Number
                    | Case
                    | Tense
                    | Gender
                    | Person
                    | Definiteness
                    | PartOfSpeechInf
    deriving (Show, Eq, Ord, Read)

data InflectionStyle = InflectPrefix
                     | InflectSuffix
                     | InflectVowel
    deriving (Show, Eq, Ord, Read)

-- TODO: Add pronouns (maybe not here though?)
data Inflected = Inflected
    { _inflectionTypes :: [InflectionType]
    , _inflectionLocations :: [InflectionLocation]
    , _style :: InflectionStyle
    , _morphemes :: [Morpheme] }
    deriving (Show, Eq, Ord, Read)
Lens.makeLenses ''Inflected

data Article = Definite Inflected
             | Indefinite Inflected
    deriving (Show, Eq, Ord, Read)

data Inflection = Inflection
    { _plurality :: Inflected
    , _articles :: [Article]
    , _inflectsFor :: [InflectionType]
    , _cases :: [CaseType]
    , _genders :: [Lexeme]
    , _numbers :: [NumberInf]
    , _inflectedPOS :: [PartOfSpeech]
    , _inflections :: Map LexemeInfo Morpheme } -- TODO: Add null morpheme
    deriving (Show, Read)
Lens.makeLenses ''Inflection

-- TODO: Actually make use of this (add a role thing to the lexeme info so that we can tell which is which)
data SentenceOrderComp = SubjectC | VerbC | ObjectC
    deriving (Show, Eq, Ord, Read)
data SentenceOrder = SentenceOrder (SentenceOrderComp, SentenceOrderComp, SentenceOrderComp)
    deriving (Show, Eq, Ord, Read)

data Translatable = Translatable [Lexeme] [LexemeInfo]
    deriving (Show, Read)

data Grammar = Grammar
    { _genderMap :: Map Lexeme Lexeme
    , _inflection :: Inflection
    , _sentenceOrder :: SentenceOrder }
    deriving (Show, Read)
Lens.makeLenses ''Grammar

data Language = Language
    { _name :: Lexeme
    , _vocab :: Map Lexeme Lexeme
    , _phonology :: Phonology
    , _grammar :: Grammar }
    deriving (Show, Read)
Lens.makeLenses ''Language

data LanguageConfig = LanguageConfig
    { _consonantNRange :: (Integer, Integer)

    , _vowelNRange :: (Integer, Integer)

    , _consonantChanceRange :: (Integer, Integer)

    , _syllableTypesNRange :: (Integer, Integer)

    , _minMorphemeLengthRange :: (Integer, Integer)
    , _maxMorphemeLengthRange :: (Integer, Integer)
    , _minLexemeLengthRange :: (Integer, Integer)
    , _maxLexemeLengthRange :: (Integer, Integer)

    , _genderNRange :: (Integer, Integer)

    , _minClusterLengthRange :: (Integer, Integer)
    , _maxClusterLengthRange :: (Integer, Integer)
    , _clusterCountNRange :: (Integer, Integer) }
    deriving Show
Lens.makeLenses ''LanguageConfig

