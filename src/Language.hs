{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Language where

import Control.Lens ((^.))
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Prelude hiding (Word)

import System.Random

import Grammar
import Morphemes
import Phonemes
import Syllables
import Types
import Util

defaultLanguageConfig :: LanguageConfig
defaultLanguageConfig = LanguageConfig
    { _consonantNRange = (5, 80)

    , _vowelNRange = (3, 30)

    , _consonantChanceRange = (1, 4)

    , _syllableTypesNRange = (2, 10)

    , _minMorphemeLengthRange = (1, 2)
    , _maxMorphemeLengthRange = (2, 3)
    , _minLexemeLengthRange = (1, 2)
    , _maxLexemeLengthRange = (2, 3)

    , _genderNRange = (1, 5)

    , _minClusterLengthRange = (1, 2)
    , _maxClusterLengthRange = (2, 5)
    , _clusterCountNRange = (0, 50) }

generateConsonantCluster :: MonadIO m => (Integer, Integer) -> [Phoneme] -> m Phoneme
generateConsonantCluster lenRange consonants = do
    l <- randRange lenRange
    ps <- map (\(Phoneme s) -> s) . take (fromIntegral l) <$> randomList consonants

    pure $ Phoneme (concat ps)

randomPhonology :: MonadIO m => LanguageConfig -> m Phonology
randomPhonology config = do
    consonantN <- randRange $ config^.consonantNRange
    vowelN <- randRange $ config^.vowelNRange

    consonants <- repM consonantN consonantPhonemes
    vowels <- repM vowelN vowelPhonemes

    clusterLenRange <- minMaxRange (config^.minClusterLengthRange) (config^.maxClusterLengthRange)

    syllableN <- randRange $ config^.syllableTypesNRange
    syllableTypes <- repM syllableN syllableTypesList

    morphemeLenRange <- minMaxRange (config^.minMorphemeLengthRange) (config^.maxMorphemeLengthRange)
    lexemeLenRange <- minMaxRange (config^.minLexemeLengthRange) (config^.maxLexemeLengthRange)

    pure $ Phonology
        { _consonants = consonants
        , _vowels = vowels
        , _syllableTypes = syllableTypes
        , _morphemeLengthRange = morphemeLenRange
        , _lexemeLengthRange = lexemeLenRange}

randomLanguage :: MonadIO m => LanguageConfig -> m Language
randomLanguage config = do
    phonology <- randomPhonology config
    grammar <- randomGrammar config phonology
    name <- randomLexeme phonology

    pure $ Language
        { _name = name
        , _vocab = Map.empty
        , _phonology = phonology
        , _grammar = grammar }

getLexeme :: (MonadIO m, MonadState Language m) => Lexeme -> m Lexeme
getLexeme lexeme@(Lexeme morphemes) = do
    phonology <- Lens.view Types.phonology <$> get
    translation <- Map.lookup lexeme . Lens.view vocab <$> get

    case translation of
        Just w -> pure w
        Nothing -> do
            l <- randRange (max 1 (length morphemes - 1), length morphemes + 1)
            newLexeme <- Lexeme <$> replicateM (fromIntegral l) (randomMorpheme phonology)
            modify $ Lens.over vocab (Map.insert lexeme newLexeme)
            pure newLexeme

translate :: (MonadIO m, MonadState Language m) => [Lexeme] -> [LexemeInfo] -> m Sentence
translate lexemes lexemeInfos = do
    inflection <- Lens.view (grammar . Types.inflection) <$> get
    translatedLexemes <- mapM getLexeme lexemes

    -- Transform genders

    sentences <- zipWithM (inflect (inflection^.plurality)) translatedLexemes lexemeInfos
    pure $ Sentence $ concatMap (\(Sentence words) -> words) sentences

sampleLanguage :: MonadIO m => LanguageConfig -> m ()
sampleLanguage config = do
    lang <- randomLanguage config

    let grammar = lang^.Types.grammar
    let phonology = lang^.Types.phonology
    let inflection = grammar^.Types.inflection

    liftIO $ putStrLn "Random sampling of lexemes, inflected for plurality: "
    lexemes <- replicateM 10 $ randomLexeme phonology
    lexemeInfo <- replicateM 10 $ randomInfo $ inflection^.genders
    (vs, newState) <- runStateT (zipWithM (\l linfo -> inflect (inflection^.plurality) l linfo) lexemes lexemeInfo) lang

    liftIO $ mapM_ (\(l, info, infL) -> putStrLn (prettyPrint l ++ " " ++ prettyPrint info ++ " - " ++ prettyPrint infL)) $ zip3 lexemes lexemeInfo vs

    let tempLexs = [Lexeme [Morpheme [Syllable [Phoneme "m", Phoneme "a", Phoneme "n"]]],
                    Lexeme [Morpheme [Syllable [Phoneme "b", Phoneme "e"]]],
                    Lexeme [Morpheme [Syllable [Phoneme "h", Phoneme "u", Phoneme "n"], Syllable [Phoneme "g", Phoneme "r", Phoneme "y"]]]]
    let tempLexInfo = [LexemeInfo True Singular Nominative Noun (Lexeme []) Third,
                       LexemeInfo False Singular Nominative Verb (Lexeme []) Third,
                       LexemeInfo False Singular Accusative Adjective (Lexeme []) Third]

    (s, _) <- runStateT (translate tempLexs tempLexInfo) lang
    liftIO $ putStrLn $ prettyPrint s

    where
        randomInfo genders = do
            def <- choice [False, True]
            number <- do
                temp <- choice [Singular, Plural, NumberInf 1]
                case temp of
                    NumberInf i -> NumberInf <$> randRange (1, 10)
                    num -> pure num
            person <- choice [First, Second, Third]
            caseType <- choice caseTypeList
            pos <- choice partOfSpeechList
            g <- choice genders

            pure $ LexemeInfo
                { _definite = def
                , _number = number
                , _person = person
                , _partOfSpeech = pos
                , _gender = g
                , _caseType = caseType }

