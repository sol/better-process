{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ == 700
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
module System.Process.Better (
  CmdSpec(..)
, readProcess
) where

import           Data.String
import           System.IO
import           System.Process hiding (readProcess)
import qualified Control.Exception as C

import           Control.DeepSeq
import           Control.Monad
import           System.Exit
import           System.Process.Copied hiding (processFailedException)
import qualified System.Process.Copied as Copied

-- | construct a `ShellCommand` from a string literal
instance IsString CmdSpec where
  fromString = ShellCommand

command :: CmdSpec -> CreateProcess
command c = (proc undefined undefined) {cmdspec = c}

-- | @readProcess@ forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns the output
-- string.
--
-- If an asynchronous exception is thrown to the thread executing
-- @readProcess@. The forked process will be terminated and @readProcess@ will
-- wait (block) until the process has been terminated.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- This function throws an 'IOError' if the process 'ExitCode' is
-- anything other than 'ExitSuccess'.
--
-- Users of this function should compile with @-threaded@ if they
-- want other Haskell threads to keep running while waiting on
-- the result of readProcess.
--
-- >  > readProcess "date" [] []
-- >  "Thu Feb  7 10:03:39 PST 2008\n"
--
-- The arguments are:
--
-- * The command to run, which must be in the $PATH, or an absolute or relative path
--
-- * A list of separate command line arguments to the program
--
-- * A string to pass on standard input to the forked process.
--
readProcess
    :: CmdSpec                  -- ^ command to run

    -> String                   -- ^ standard input
    -> IO String                -- ^ standard output
readProcess cmd input = do
    let cp_opts = (command cmd) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = Inherit
                  }
    (ex, output) <- withCreateProcess_ "readProcess" cp_opts $
      \(Just inh) (Just outh) _ ph -> do

        -- fork off a thread to start consuming the output
        output  <- hGetContents outh
        withForkWait (C.evaluate $ rnf output) $ \waitOut -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          hClose outh

        -- wait on the process
        ex <- waitForProcess ph
        return (ex, output)

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> processFailedException "readProcess" cmd r

processFailedException :: String -> CmdSpec -> Int -> IO a
processFailedException fun cmd exit_code = case cmd of
  ShellCommand c -> Copied.processFailedException fun c [] exit_code
  RawCommand c args -> Copied.processFailedException fun c args exit_code
