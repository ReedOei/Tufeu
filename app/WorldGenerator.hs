{-# LANGUAGE FlexibleContexts #-}

module WorldGenerator where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import GameTypes
import Language
import Types
import Util

randomButton :: (MonadIO m, MonadState Game m) => Maybe Integer -> m ButtonChoice
randomButton sequenceNum = do
    let inscription = Translatable [Lexeme [Morpheme [Syllable [Phoneme (show sequenceNum)]]]]
                                   [LexemeInfo False Singular Nominative Adjective (Lexeme []) Third]

    pure $ ButtonChoice
        { _sequenceNum = sequenceNum
        , _buttonInscription = inscription
        , _pressed = False }

randomPuzzle :: (MonadIO m, MonadState Game m) => m ButtonPuzzle
randomPuzzle = do
    -- TODO: Have some sort of configuration for this
    buttonN <- randRange (0, 4)
    buttonNumbers <- replicateM buttonN $ maybeCall 4 $ randRange (0, 3)
    buttons <- mapM randomButton buttonNumbers

    -- Make sure that at least one button is pressable
    firstButton <- randomButton $ Just 0

    -- TODO: Make it so that this button doesn't always come first
    pure $ ButtonPuzzle $ firstButton : buttons

randomRoomExit :: (MonadIO m, MonadState Game m) => String -> Integer -> m RoomExit
randomRoomExit exitDirName exitId = do
    exitType <- choice ["door", "open field"]

    puzzle <- if exitType == "door" then maybeCall 2 randomPuzzle else pure Nothing

    exitDestType <- choice ["temple", "house", "field"]

    pure $ RoomExit
        { _puzzle = puzzle
        , _roomExitType = exitType
        , _roomExitDirName = exitDirName
        , _roomExitDestType = exitDestType
        , _roomExitId = exitId }

-- TODO: make this better (maybe make a custom type for the directions)
oppositeDir :: String -> String
oppositeDir "north" = "south"
oppositeDir "south" = "north"
oppositeDir "east" = "west"
oppositeDir "west" = "east"
oppositeDir dir = dir

randomRoom :: (MonadIO m, MonadState Game m) => Maybe (String, String, Room) -> String -> Integer -> m Room
randomRoom enterFrom rType rId = do
    maxKey <- maximum . Map.keys . view allRooms <$> get
    let exitIds = [max rId maxKey + 1..]

    let roomExitInfo = zip ["north", "south", "east", "west"] exitIds
    tempRoomExits <- mapChance (uncurry randomRoomExit) 3 roomExitInfo

    -- TODO: Refactor this out
    let roomExits = nubBy (\e1 e2 -> e1^.roomExitDirName == e2^.roomExitDirName) $ case enterFrom of
                        Nothing -> tempRoomExits
                        Just (enterFromDir, enterFromType, enterFromRoom) ->
                            let returnExit = RoomExit
                                                { _puzzle = Nothing
                                                , _roomExitType = enterFromType
                                                , _roomExitDirName = enterFromDir
                                                , _roomExitDestType = enterFromRoom^.roomType
                                                , _roomExitId = enterFromRoom^.roomId }
                            in returnExit : tempRoomExits

    pure $ Room
        { _roomId = rId
        , _roomType = rType
        , _roomDesc = "This is a " ++ rType
        , _roomExits = roomExits }

