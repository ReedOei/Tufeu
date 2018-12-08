{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module GameTypes where

import Control.Lens
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Language
import Types
import Util

data ButtonChoice = ButtonChoice
    { _sequenceNum :: Maybe Integer
    , _buttonInscription :: Translatable
    , _pressed :: Bool }
    deriving (Show, Read)
makeLenses ''ButtonChoice
data ButtonPuzzle = ButtonPuzzle { _choices :: [ButtonChoice] }
    deriving (Show, Read)
makeLenses ''ButtonPuzzle

data Notes = Notes
    { _suspectedVocab :: Map Lexeme Lexeme }
    deriving (Show, Read)
makeLenses ''Notes

data RoomExit = RoomExit
    { _puzzle :: Maybe ButtonPuzzle  -- TODO: expand the number of types of puzzles allow
    , _roomExitType :: String
    , _roomExitDirName :: String
    , _roomExitDestType :: String
    , _roomExitId :: Integer }
    deriving (Show, Read)
makeLenses ''RoomExit

data Room = Room
    { _roomId :: Integer
    , _roomType :: String
    , _roomDesc :: String
    , _roomExits :: [RoomExit] }
    deriving (Show, Read)
makeLenses ''Room

data Game = Game
    { _notes :: Notes
    , _allRooms :: Map Integer Room
    , _currentRoom :: Room
    , _language :: Language }
    deriving (Show, Read)
makeLenses ''Game

