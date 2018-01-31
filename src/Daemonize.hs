{-# LANGUAGE CPP #-}

module Daemonize
    ( daemonize
    ) where

import           Control.Monad        (void, when)
import           System.Exit          (exitSuccess)
#ifdef mingw32_HOST_OS
-- import           System.Environment
-- import           System.Process
import           Control.Concurrent   (forkIO)
#else
import           System.Posix.IO
import           System.Posix.Process (createSession, forkProcess)
#endif

import           Server               (createListenSocket, startServer)

-- | This goes against the common daemon guidelines and does not change the
-- current working directory!
--
-- We need the daemon to stay in the current directory for the GHC API to work
#ifdef mingw32_HOST_OS
-- daemonize :: Bool -> IO () -> IO ()
-- daemonize exit sock = do
--     exePath <- getExecutablePath
--     void $ createProcess $ (proc exePath ["admin", "--socket=" ++ sock, "--start-server", "-n"]) {
--         close_fds = True }
--     when exit exitSuccess
daemonize :: Bool -> IO () -> IO ()
daemonize exit program = do
    _ <- forkIO program
    when exit $ exitSuccess
#else
daemonize :: Bool -> IO () -> IO ()
daemonize exit program = do
    _ <- forkProcess child1
    when exit exitSuccess

    where
    child1 = do
        _ <- createSession
        _ <- forkProcess child2
        exitSuccess

    child2 = do
        mapM_ closeFd [stdInput, stdOutput, stdError]
        nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
        mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
        closeFd nullFd
        program
#endif
