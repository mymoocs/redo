{-# OPTIONS_GHC -fforce-recomp #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception (catch,catchJust, throw, IOException,  SomeException(..))
import Control.Monad (filterM, liftM, unless, guard)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 (md5)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
import Data.Typeable (typeOf)
import Debug.Trace(trace)
import GHC.IO.Exception (IOErrorType(..)) 
import System.Directory (copyFile,renameFile, removeFile, doesFileExist, getDirectoryContents, removeDirectoryRecursive, createDirectoryIfMissing)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName, (</>))
import System.IO (hPutStrLn, stderr, hGetLine, withFile, IOMode(..))
import System.IO.Error (ioeGetErrorType, isDoesNotExistError)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..), StdStream(..), CmdSpec(..))


trace' arg = trace (show arg) arg


main :: IO ()
main = mapM_ redo =<< getArgs


redo :: String -> IO ()
redo target = do
  upToDate' <- upToDate target metaDepsDir
  unless upToDate' $ maybe printMissing redo' =<< redoPath target
  --if upToDate'
  --  then return()
  --  else  maybe printMissing redo' =<<redoPath target
  where
    redo' :: FilePath -> IO ()
    redo' path = do
      catchJust (\e -> guard $ isDoesNotExistError e)
                (removeDirectoryRecursive metaDepsDir)
                (\_ -> return ())

      {--catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                (removeDirectoryRecursive metaDepsDir)
                (\_ -> return ())--}

     {-- catch (removeDirectoryRecursive metaDepsDir)
            (\e -> if isDoesNotExistError e
                   then return ()
                   else throw e )--}
      createDirectoryIfMissing True metaDepsDir
      writeFile (metaDepsDir </> path ) =<< md5' path
      oldEnv <- getEnvironment
      let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
      (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
      exit  <- waitForProcess ph
      case trace' exit of
        ExitSuccess -> do renameFile tmp target
        ExitFailure code -> do hPutStrLn stderr $ "Redo script exited with non-zero exit code: " ++ show code
                               removeFile tmp
  ------------------------------------------
    tmp :: FilePath
    tmp = target ++"---redoing"
    printMissing :: IO ()
    printMissing = error "No .do file found for target"
    cmd :: FilePath -> String
    cmd path = trace' $ unwords ["sh", path, "0",takeBaseName target, tmp, ">",tmp]
    metaDepsDir :: FilePath
    metaDepsDir = ".redo" </> target
                               


redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target =  listToMaybe  `liftM` filterM doesFileExist candidates
  where
      candidates = [target++".do"] ++ if hasExtension target
                                      then [replaceBaseName target "default" ++ ".do"]
                                      else []

upToDate :: String -> FilePath -> IO Bool
upToDate target metaDepsDir = catch
  (do deps <- getDirectoryContents metaDepsDir
      (trace' . all id) `liftM` mapM depUpToDate deps)
  (\(e :: IOException) -> return False)
  where depUpToDate :: FilePath -> IO Bool
        depUpToDate  dep = catch
          (do oldMD5 <- trace' `liftM`  withFile (metaDepsDir </> dep) ReadMode hGetLine
              newMD5 <- md5' dep 
              return $ oldMD5 == newMD5)
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


md5' :: FilePath -> IO String
md5' path = (show .  md5) `liftM` BL.readFile path
