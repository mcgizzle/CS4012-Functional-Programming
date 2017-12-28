module Chan where

import Control.Concurrent hiding (Chan)

type Stream a = MVar (Item a)
data Item a = Item a (Stream a)
data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))

newChan :: IO (Chan a)
newChan = do
        hole <- newEmptyMVar
        readVar <- newMVar hole
        writeVar <- newMVar hole
        return $ Chan readVar writeVar

writeChan :: Chan a -> a -> IO ()
writeChan (Chan _ writeVar) val = do
        newHole <- newEmptyMVar
        oldHole <- takeMVar writeVar
        putMVar oldHole $ Item val newHole
        putMVar writeVar newHole

readChan :: Chan a -> IO a
readChan (Chan readVar _) = do
        stream <- takeMVar readVar
    -- If using multicast-channels then this must use readMVar
    -- Otherwise takeMVar stream would be fine
        Item val next <- readMVar stream
        putMVar readVar next
        return val

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan _ writeVar) = do
        hole <- readMVar writeVar
        newReadVar <- newMVar hole
        return $ Chan newReadVar writeVar

