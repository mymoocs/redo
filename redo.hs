{-# OPTIONS_GHC -fforce-recomp #-}
{-# LANGUAGE StandaloneDeriving #-} 
import Control.Monad (filterM, liftM)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import System.Directory (copyFile,renameFile, removeFile, doesFileExist)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..))


deriving instance Show CreateProcess
deriving instance Show StdStream
deriving instance Show CmdSpec

trace' arg = trace (show arg) arg


main :: IO ()
main = mapM_ redo =<< getArgs
  -- getArgs >>= mapM_ redo
  -- do getArgs >>= \args -> mapM_ redo args


redo :: String -> IO ()
redo target = do
  
  maybe printMissing redo' =<<redoPath target
  where
    tmp :: FilePath
    tmp = target ++"---redoing"
    printMissing :: IO ()
    printMissing = error "No .do file found for target"
    -- cmd path = "sh " ++ path  ++ " 0 " ++ takeBaseName target ++ " " ++ tmp  ++ " > " ++ tmp
    cmd :: FilePath -> String
    cmd path = trace' $ unwords ["sh", path, "0",takeBaseName target, tmp, ">",tmp] 
    redo' :: FilePath -> IO ()
    redo' path = do
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      --let newEnv = ("REDO_TARGET",target): (filter((/="REDO_TARGET") . fst) oldEnv)
      (_, _, _, ph) <- createProcess $ trace' $ (shell $ cmd path) {env = Just newEnv}
      exit  <- waitForProcess ph
      case trace' exit of
        ExitSuccess -> do renameFile tmp target
        ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                               removeFile tmp


redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target =  listToMaybe  `liftM` filterM doesFileExist candidates
  -- liftM safeHead $ filterM doesFileExist candidates
  -- existingCandidates <- filterM doesFileExist candidates
  -- return $ safeHead existingCandidates 
  where
      candidates = [target++".do"] ++ if hasExtension target
                                      then [replaceBaseName target "default" ++ ".do"]
                                      else  []
