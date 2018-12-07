{-# LANGUAGE FlexibleContexts #-}

module Grammar where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Debug.Trace

import Morphemes
import Syllables
import Types
import Util

inflectionLocationsWeighted =
    [(8, ParticlePrefix)
    ,(8, ParticleSuffix)
    ,(4, Prefix)
    ,(4, Suffix)
    ,(1, Infix 1)] -- TODO: Expand this

inflectionTypeList =
    [Number, Case, Tense, Gender, Person
    ,Definiteness, PartOfSpeechInf]

caseTypeList =
    [Nominative, Accusative, Dative, Genitive
    ,Vocative, Ablative, Prepositional, Instrumental]

partOfSpeechList =
    [Noun, Verb, Adjective, Adverb, Conjunction
    ,Preposition, Interjection]

ifContains :: Eq a => [a] -> a -> b -> b -> b
ifContains l e ifYes ifNo
    | e `elem` l = ifYes
    | otherwise = ifNo

randomInflected :: MonadIO m => Phonology -> [InflectionType] -> m Inflected
randomInflected phonology legalInflectionTypes = do
    inflectionLocations <- replicateM 1 $ weightedChoice inflectionLocationsWeighted
    -- TODO: Do something so this is more consistent across terms for a given language
    inflectionTypes <- takeRandom (0, 3 :: Integer) legalInflectionTypes

    -- TODO: Maybe allow more than one morpheme per inflected term?
    morphemes <- replicateM 1 $ Morpheme <$> replicateM 1 (randomSyllable phonology)

    style <- choice [InflectPrefix, InflectSuffix, InflectVowel]

    pure $ Inflected
        { _inflectionTypes = inflectionTypes
        , _inflectionLocations = inflectionLocations
        , _morphemes = morphemes
        , _style = style }

makeAllInflections :: MonadIO m => Phonology ->
                                   [Lexeme] ->
                                   [InflectionType] ->
                                   [CaseType] ->
                                   [NumberInf] ->
                                   m (Map LexemeInfo Morpheme)
makeAllInflections phonology genders inflectionTypes caseTypes numberInfs =
    Map.fromList <$> sequence pairs
    where
        pairs = do
            let check = ifContains inflectionTypes

            definite <- check Definiteness [True, False] [False]
            n <- check Number numberInfs [Singular]
            c <- check Case caseTypes [Nominative]
            pos <- check PartOfSpeechInf partOfSpeechList [Noun]
            g <- check Gender genders [head genders]
            person <- check Person [First, Second, Third] [First]

            let info = LexemeInfo
                    { _definite = definite
                    , _number = n
                    , _caseType = c
                    , _partOfSpeech = pos
                    , _gender = g
                    , _person = person }

            trace ("Generating: " ++ prettyPrint info) $ pure $ (info,) <$> (Morpheme <$> replicateM 1 (randomSyllable phonology))

randomInflection :: MonadIO m => LanguageConfig -> Phonology -> m Inflection
randomInflection config phonology = do
    genderN <- randRange $ config^.genderNRange
    genders <- replicateM (fromIntegral genderN) (randomLexeme phonology)

    legalInflectionTypes <- nub <$> takeRandom (1, 9 :: Integer) inflectionTypeList

    -- All languages must have words for singular and plural
    -- Maybe can remove restriction for plural, but very hard to imagine a language without singular...
    tempNumbers <- nub <$> replicateM 10 (weightedChoice [(10, Singular), (10, Plural), (4, NumberInf 2), (1, NumberInf 3), (1, NumberInf 4)])
    legalNumbers <- nub . ([Singular, Plural] ++) <$> takeRandom (0, 4 :: Integer) tempNumbers

    hasArticles <- choice [False, True]

    articles <- if hasArticles then
                    sequence [Definite <$> randomInflected phonology legalInflectionTypes,
                              Indefinite <$> randomInflected phonology legalInflectionTypes]
                else pure []

    plurality <- randomInflected phonology legalInflectionTypes
    cases <- nub . ([Nominative] ++) <$> takeRandom (0, 10 :: Integer) caseTypeList
    allInflections <- makeAllInflections phonology genders legalInflectionTypes cases legalNumbers
    inflectedPOS <- nub <$> replicateM 4 (weightedChoice [(20, Noun), (20, Verb), (5, Adjective), (5, Adverb)])

    pure $ Inflection
        { _plurality = plurality
        , _articles = articles
        , _inflectsFor = legalInflectionTypes
        , _cases = cases
        , _numbers = legalNumbers
        , _genders = genders
        , _inflectedPOS = inflectedPOS
        , _inflections = allInflections }

randomSentenceOrder :: MonadIO m => m SentenceOrder
randomSentenceOrder = do
    f <- choice [SubjectC, VerbC, ObjectC]
    s <- choice [SubjectC, VerbC, ObjectC]
    t <- choice [SubjectC, VerbC, ObjectC]

    pure $ SentenceOrder (f, s, t)

randomGrammar :: MonadIO m => LanguageConfig -> Phonology -> m Grammar
randomGrammar config phonology = do
    inflection <- randomInflection config phonology

    sentenceOrder <- randomSentenceOrder

    pure $ Grammar
        { _genderMap = Map.empty
        , _inflection = inflection
        , _sentenceOrder = sentenceOrder }

placeAtLocation :: [Morpheme] -> InflectionLocation -> Lexeme -> Sentence
placeAtLocation morphemes ParticlePrefix lexeme = Sentence [Word [Lexeme morphemes], Word [lexeme]]
placeAtLocation morphemes ParticleSuffix lexeme = Sentence [Word [lexeme], Word [Lexeme morphemes]]
placeAtLocation morphemes Prefix lexeme = Sentence [Word [Lexeme morphemes, lexeme]]
placeAtLocation morphemes Suffix lexeme = Sentence [Word [lexeme, Lexeme morphemes]]
placeAtLocation morphemes (Infix n) (Lexeme lMorphemes) =
    let (start, end) = splitAt (fromIntegral n) lMorphemes
    in Sentence [Word [Lexeme (start ++ morphemes ++ end)]]

makeNumberLegal :: Inflection -> NumberInf -> NumberInf
makeNumberLegal inflection n
    | n `elem` (inflection^.numbers) = n
    | otherwise =
        case n of
            NumberInf n | n > 1 -> Plural
            NumberInf n | n == 1 -> Singular
            -- Should be no other cases
            _ -> n

reduceInfo :: Inflection -> [InflectionType] -> LexemeInfo -> LexemeInfo
reduceInfo inflection inflectedFor info =
    let check = ifContains inflectedFor
        checkCase = ifContains $ inflection^.cases
        def = check Definiteness (info^.definite) False
        n = makeNumberLegal inflection $ check Number (info^.number) Singular
        c = checkCase (info^.caseType) (check Case (info^.caseType) Nominative) Nominative
        pos = check PartOfSpeechInf (info^.partOfSpeech) Noun
        g = check Gender (info^.gender) (head (inflection^.genders))
        p = check Person (info^.person) First
    in LexemeInfo
        { _definite = def
        , _number = n
        , _caseType = c
        , _partOfSpeech = pos
        , _gender = g
        , _person = p }

transformGender :: (MonadIO m, MonadState Language m) => Lexeme -> LexemeInfo -> m LexemeInfo
transformGender lexeme info = do
    g <- view grammar <$> get

    newGender <- case Map.lookup lexeme $ g^.genderMap of
                    Nothing -> do
                        newGender <- choice (g^.inflection.genders)
                        modify $ over (grammar.genderMap) $ Map.insert lexeme newGender
                        pure newGender
                    Just newGender -> pure newGender
    pure $ set gender newGender info

harmonizeVowels :: Morpheme -> [Morpheme] -> [Morpheme]
harmonizeVowels (Morpheme (harmonizeWith:_)) = map go
    where go (Morpheme syllables) = Morpheme $ map (replaceVowels (vowelFrom harmonizeWith)) syllables

class Inflectable a where
    inflect :: (MonadIO m, MonadState Language m) => a -> Lexeme -> LexemeInfo -> m Sentence
    inflectedForm :: (MonadIO m, MonadState Language m) => a -> LexemeInfo -> m [Morpheme]

instance Inflectable Inflected where
    inflectedForm inflected info = do
        inflection <- view (grammar . Types.inflection) <$> get

        let newInfo = reduceInfo inflection (inflected^.inflectionTypes) info
        case Map.lookup newInfo (inflection^.inflections) of
            Just inflectedMorpheme ->
                case inflected^.style of
                    InflectPrefix -> pure $ inflectedMorpheme : inflected^.morphemes
                    InflectSuffix -> pure $ inflected^.morphemes ++ [inflectedMorpheme]
                    InflectVowel -> pure $ harmonizeVowels inflectedMorpheme $ inflected^.morphemes
            Nothing -> error $ "Cannot inflect: " ++ show newInfo

    inflect inflected lexeme info = do
        inflection <- view (grammar . Types.inflection) <$> get

        if info^.partOfSpeech `elem` inflection^.inflectedPOS then do
            location <- choice $ inflected^.inflectionLocations
            form <- inflectedForm inflected info

            pure $ placeAtLocation form location lexeme
        else
            pure $ Sentence [Word [lexeme]]

