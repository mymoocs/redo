{-# OPTIONS_GHC -fforce-recomp #-}
import Control.Monad (filterM, liftM)
import Data.Maybe (listToMaybe)
import System.Directory (copyFile,renameFile, removeFile, doesFileExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell)

main :: IO ()
main = mapM_ redo =<< getArgs
  -- getArgs >>= mapM_ redo
  -- do getArgs >>= \args -> mapM_ redo args

redo :: String -> IO ()
redo target = do
  
  maybe printMissing redo' =<<redoPath target
  where
    tmp = target ++"---redoing"
    printMissing = error "No .do file found for target"
    -- cmd path = "sh " ++ path  ++ " 0 " ++ takeBaseName target ++ " " ++ tmp  ++ " > " ++ tmp
    cmd path = unwords ["sh", path, "0",takeBaseName target, tmp, ">",tmp]
    redo' :: FilePath -> IO ()
    redo' path = do
      (_, _, _, ph) <- createProcess $ shell $ cmd path
      exit  <- waitForProcess ph
      case exit of
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
