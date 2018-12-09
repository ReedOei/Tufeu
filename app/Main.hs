{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

import GameTypes
import Language
import Types
import WorldGenerator
import Util

solvedPuzzle :: ButtonPuzzle -> Bool
solvedPuzzle (ButtonPuzzle choices) = solvedPuzzle' (-1) $ filter (^.pressed) choices
    where
        (Just maxPress) = view sequenceNum $ maximumBy (comparing (^.sequenceNum)) choices
        solvedPuzzle' i [] = i == maxPress
        solvedPuzzle' i (pressedButton:buttons)
            | pressedButton^.sequenceNum == Just (i + 1) = solvedPuzzle' (i + 1) buttons
            | otherwise = False

blankNotes :: Notes
blankNotes = Notes Map.empty

printNotes :: MonadIO m => Notes -> m ()
printNotes notes = liftIO $ print $ notes^.suspectedVocab

showButtonChoice :: (MonadIO m, MonadState Game m) => ButtonChoice -> m String
showButtonChoice buttonChoice = do
    l <- view language <$> get
    (s, newL) <- runStateT (translate (buttonChoice^.buttonInscription)) l
    modify $ set language newL
    pure $ prettyPrint s

printRoomExit :: (MonadIO m, MonadState Game m) => RoomExit -> m ()
printRoomExit exit = do
    liftIO $ putStrLn $ "To the " ++ (exit^.roomExitDirName) ++
                        " there is a " ++ (exit^.roomExitType) ++
                        " leading to " ++ (exit^.roomExitDestType)
    case exit^.puzzle of
        Nothing -> pure ()
        Just p -> do
            liftIO $ putStrLn $ "There is a puzzle over the door."
            liftIO $ putStrLn $ "There are " ++ show (length (p^.choices)) ++ " buttons, reading:"
            zipWithM_ (\i bc -> do
                buttonS <- showButtonChoice bc
                liftIO $ putStrLn $ show i ++ " (" ++ show (bc^.pressed, bc^.sequenceNum) ++ "): " ++ buttonS) [0..] $ p^.choices

printRoom :: (MonadIO m, MonadState Game m) => Room -> m ()
printRoom room = do
    liftIO $ putStrLn $ room^.roomDesc
    liftIO $ putStrLn ""
    mapM_ printRoomExit $ room^.roomExits

goToRoom :: (MonadIO m, MonadState Game m) => String -> RoomExit -> m ()
goToRoom dir exit = do
    let roomId = exit^.roomExitId
    let roomType = exit^.roomExitDestType
    curRoom <- view currentRoom <$> get
    room <- Map.lookup roomId . view allRooms <$> get

    case room of
        Nothing -> do
            newRoom <- randomRoom (Just (oppositeDir dir, exit^.roomExitType, curRoom))
                                  roomType roomId
            modify $ over allRooms (Map.insert roomId newRoom)
            modify $ set currentRoom newRoom
        Just newRoom -> modify $ set currentRoom newRoom

tryGetExit :: (MonadIO m, MonadState Game m) => String -> (RoomExit -> m RoomExit) -> m ()
tryGetExit dir call = do
    exits <- view (currentRoom.roomExits) <$> get
    curRoomId <- view (currentRoom.roomId) <$> get

    -- TODO: Refactor this
    case findIndex (\exit -> exit^.roomExitDirName == dir) exits of
        -- TODO: handle typing things like N for north and such
        --       basically, as long as we can uniquely determine the
        --       direction from the string, just roll with it
        Nothing -> liftIO $ putStrLn $ "No such exit: " ++ dir
        Just idx -> do
            newExit <- call $ exits !! idx
            -- Do this in case the call changes the current room
            modify $ set (allRooms.at curRoomId._Just.roomExits.element (fromIntegral idx)) newExit
            -- Re-sync current room
            -- TODO: Improve this system

            newId <- view (currentRoom.roomId) <$> get
            rMaybe <- view (allRooms.at newId) <$> get

            case rMaybe of
                Nothing -> error $ "Unknown room id: " ++ show newId
                Just r -> modify $ set currentRoom r

goToIfSolved :: (MonadIO m, MonadState Game m) => String -> RoomExit -> m RoomExit
goToIfSolved dir exit = do
    case exit^.puzzle of
        Nothing -> goToRoom dir exit
        Just p | solvedPuzzle p -> do
            liftIO $ putStrLn "You have solved the puzzle and may proceed."
            goToRoom dir exit
        Just p -> liftIO $ putStrLn $ "You must solve the puzzle to go " ++ dir
    pure exit -- Don't change the exit

-- TODO: Generalize this, by taking a Read a instead of an integer
--       the corresponding type class will probably need multiple parameters
pressButton :: (MonadIO m, MonadState Game m) => Int -> RoomExit -> m RoomExit
pressButton buttonIdx exit =
    case exit^.puzzle of
        Nothing -> do
            liftIO $ putStrLn "There is no puzzle in this direction."
            pure exit
        Just p ->
            if buttonIdx < length (p^.choices) then do
                pure $ over (puzzle._Just.choices.element buttonIdx.pressed) not exit
            else do
                liftIO $ putStrLn $ "There are only " ++ show (length (p^.choices)) ++ " buttons."
                pure exit

handleInput :: (MonadIO m, MonadState Game m) => String -> m ()
handleInput str = do
    case words str of
        ["go", dir] -> tryGetExit dir $ goToIfSolved dir
        ["look"] -> printRoom =<< view currentRoom <$> get
        ["press", dir, buttonNum] -> tryGetExit dir $ pressButton $ read buttonNum
        _ -> liftIO $ putStrLn $ "Unknown command: " ++ str

playGame :: (MonadIO m, MonadState Game m) => m ()
playGame = do
    printNotes =<< view notes <$> get
    printRoom =<< view currentRoom <$> get
    -- g <- view allRooms <$> get
    -- liftIO $ print g
    -- c <- view currentRoom <$> get
    -- liftIO $ print c
    input <- liftIO getLine
    handleInput input
    playGame

main :: IO ()
main = do
    lang <- randomLanguage defaultLanguageConfig

    let currentRoom = Room 0 "field" "This is a field"
                        [RoomExit Nothing "field" "north" "field" 1]
    let allRooms = Map.fromList [(currentRoom^.roomId, currentRoom)]
    runStateT playGame $ Game blankNotes allRooms currentRoom lang
    pure ()

