{-# OPTIONS_GHC -fforce-recomp #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception (catch,IOException,  SomeException(..))
import Control.Monad (filterM, liftM, unless)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
import Data.Typeable (typeOf)
import Debug.Trace(trace)
import GHC.IO.Exception (IOErrorType(..)) 
import System.Directory (copyFile,renameFile, removeFile, doesFileExist, getDirectoryContents)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..))


trace' arg = trace (show arg) arg


main :: IO ()
main = mapM_ redo =<< getArgs


redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target
  unless upToDate' $ maybe printMissing redo' =<< redoPath target
  --if upToDate'
  --  then return()
  --  else  maybe printMissing redo' =<<redoPath target
  where
    tmp :: FilePath
    tmp = target ++"---redoing"
    printMissing :: IO ()
    printMissing = error "No .do file found for target"
    cmd :: FilePath -> String
    cmd path = trace' $ unwords ["sh", path, "0",takeBaseName target, tmp, ">",tmp] 
    redo' :: FilePath -> IO ()
    redo' path = do
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
      exit  <- waitForProcess ph
      case trace' exit of
        ExitSuccess -> do renameFile tmp target
        ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                               removeFile tmp


redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target =  listToMaybe  `liftM` filterM doesFileExist candidates
  where
      candidates = [target++".do"] ++ if hasExtension target
                                      then [replaceBaseName target "default" ++ ".do"]
                                      else  []

upToDate :: FilePath -> IO Bool
upToDate target = catch
  (do deps <- getDirectoryContents depDir
      (trace' . all id) `liftM` mapM depUpToDate deps)
  (\(e :: IOException) -> return False)
  where depDir = ".redo/" </> target
        depUpToDate :: FilePath -> IO Bool
        depUpToDate  dep = catch
          (do oldMD5 <- trace' `liftM`  readFile (depDir </> dep)
              return False)
          (\e -> return (ioeGetErrorType e == InappropriateType))

--3.
--          (\e -> do if ioeGetErrorType e == InappropriateType
--                       then return True
--                       else return False)
        
--2.
--          (\e -> do hPutStrLn stderr $ show $ ioeGetErrorType e
--                    return False)

--1.
--          (\(SomeException e) -> do hPutStrLn stderr $ show $ typeOf e
--                                  return False)
