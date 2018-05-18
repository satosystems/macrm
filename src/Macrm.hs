{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Macrm where

import Control.Conditional (ifM)
import Control.Monad
  ( foldM
  , unless
  , when
  )
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_macrm (version)
import System.Console.CmdArgs
  ( (&=)
  , Data
  , Typeable
  , args
  , cmdArgs
  , explicit
  , help
  , name
  , summary
  , typ
  )
import System.Exit
  ( ExitCode(ExitFailure, ExitSuccess)
  , exitWith
  )
import System.Directory
  ( doesDirectoryExist
  , doesPathExist
  , getHomeDirectory
  , pathIsSymbolicLink
  , renameDirectory
  , renameFile
  )
import System.FilePath
  ( addTrailingPathSeparator
  , normalise
  )
import System.IO
  ( hClose
  , hFlush
  , hPutStr
  , hPutStrLn
  , stderr
  , stdout
  )
import System.Path.NameManip
  ( absolute_path
  , guess_dotdot
  )
import System.Posix.Files
  ( getFileStatus
  , isNamedPipe
  , isSocket
  )
import System.Process
  ( CreateProcess(std_err, std_in, std_out)
  , StdStream(CreatePipe)
  , createProcess
  , proc
  , waitForProcess
  )


data Macrm = Macrm
  { directory :: Bool
  , force :: Bool
  , interactive :: Bool
  , plaster :: Bool
  , recursive :: Bool
  , recursive' :: Bool
  , verbose :: Bool
  , whiteouts :: Bool
  , files :: [FilePath]
  } deriving (Data, Show, Typeable)


macrm :: Macrm
macrm = Macrm
  { directory = False
  &= help "Attempt to remove directories as well as other types of files."

  , force = False
  &= help
    (  "Attempt to remove the files without prompting for confirmation, "
    ++ "regardless of the file's permissions. If the file does not exist, "
    ++ "do not display a diagnostic message or modify the exit status to reflect an error. "
    ++ "The -f option overrides any previous -i options."
    )

  , interactive = False
  &= help
    (  "Request confirmation before attempting to remove each file, "
    ++ "regardless of the file's permissions, "
    ++ "or whether or not the standard input device is a terminal. "
    ++ "The -i option overrides any previous -f options."
    )

  , plaster = False
  &= name "P"
  &= help
    (  "Overwrite regular files before deleting them. "
    ++ "Files are overwritten three times, first with the byte pattern 0xff, "
    ++ "then 0x00, and then 0xff again, before they are deleted. "
    ++ "This flag is ignored under macrm."
    )

  , recursive = False
  &= name "R"
  &= help
    (  "Attempt to remove the file hierarchy rooted in each file argument. "
    ++ "The -R option implies the -d option. If the -i option is specified, "
    ++ "the user is prompted for confirmation before each directory's "
    ++ "contents are processed (as well as before the attempt is made to remove the directory). "
    ++ "If the user does not respond affirmatively, "
    ++ "the file hierarchy rooted in that directory is skipped."
    )

  , recursive' = False
  &= name "r"
  &= explicit
  &= help "Equivalent to -R."

  , verbose = False
  &= help "Be verbose when deleting files, showing them as they are removed."

  , whiteouts = False
  &= name "W"
  &= help
    (  "Attempt to undelete the named files. "
    ++ "Currently, this option can only be used to recover files covered by whiteouts. "
    ++ "This flag is ignored under macrm."
    )

  , files = []
  &= args
  &= typ "FILES/DIRS"
  } &= summary ("macrm " ++ showVersion version)


absolutize :: FilePath -> IO FilePath
absolutize ('~':cs) = do
  homePath <- getHomeDirectory
  return $ normalise $ addTrailingPathSeparator homePath ++ cs
absolutize path = fromJust . guess_dotdot <$> absolute_path path


moveToTrash :: [FilePath] -> IO ()
moveToTrash paths = do
  homePath <- getHomeDirectory
  let trashPath = homePath ++ "/.Trash/"
  pairs <- mapM (makePairs trashPath) paths
  mapM_ move pairs
 where
  makePairs :: FilePath -> FilePath -> IO (FilePath, FilePath)
  makePairs trashPath oldPath = do
    let splited = T.split (== '/') $ T.pack oldPath
    newPath <- searchNewPath 0 $ T.unpack $ last splited
    return (oldPath, newPath)
   where
    searchNewPath :: Int -> FilePath -> IO FilePath
    searchNewPath n filename = do
      let suffix = if n == 0 then "" else " - copy" ++ show n
          newPath = trashPath ++ filename ++ suffix
      ifM (doesPathExist newPath) (searchNewPath (succ n) filename) (return newPath)
  move :: (FilePath, FilePath) -> IO ()
  move (old, new) = ifM (doesDirectoryExist old) (renameDirectory old new) (renameFile old new)


rm :: Macrm -> ExitCode -> [FilePath] -> [FilePath] -> IO ExitCode
rm (Macrm False False False False False False False False []) ExitSuccess [] [] = do
  hPutStrLn stderr "usage: macrm [-f | -i] [-dPRrvW] file ...\n       unlink file"
  return $ ExitFailure 1
rm _ exitCode [] [] = return exitCode
rm _ exitCode removablePaths [] = do
  ec <- remove removablePaths
  case ec of
    ExitSuccess -> return exitCode
    _ -> return $ ExitFailure 1
rm options exitCode removablePaths (path:paths) = do
  pathExist <- doesPathExist path
  if not pathExist && not (force options)
    then do
      hPutStrLn stderr $ "macrm: " ++ path ++ ": No such file or directory"
      rm options (ExitFailure 1) removablePaths paths
    else do
      directoryExist <- doesDirectoryExist path
      if directoryExist && not (recursive options || recursive' options)
        then do
          hPutStrLn stderr $ "macrm: " ++ path ++ ": is a directory"
          rm options (ExitFailure 1) removablePaths paths
        else if pathExist
          then if interactive options
            then ifM (isAgree (if directoryExist then "examine files in directory " else "remove ") path)
              (do
                when (verbose options) $ putStrLn path
                rm options exitCode (path:removablePaths) paths)
              (rm options exitCode removablePaths paths)
            else do
              when (verbose options) $ putStrLn path
              rm options exitCode (path:removablePaths) paths
          else rm options exitCode removablePaths paths


remove :: [FilePath] -> IO ExitCode
remove paths = do
  absolutePaths <- mapM absolutize paths
  (normals, specials) <- foldM filterSpecialFiles ([], []) absolutePaths
  unless (null specials) (moveToTrash specials)
  if null normals then return ExitSuccess else executeScript $ createScript normals


executeScript :: String -> IO ExitCode
executeScript script = do
  (Just stdIn, _, _, ph) <- createProcess (proc "osascript" [])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  hPutStr stdIn script
  hFlush stdIn
  hClose stdIn
  waitForProcess ph


createScript :: [FilePath] -> String
createScript paths = concat
  [ "set l to {}\n"
  , concatMap (\path -> "set end of l to posix file \"" ++ path ++ "\" as alias\n") paths
  , "tell application \"Finder\"\n"
  , "delete l\n"
  , "end tell\n"
  , "return"
  ]


isAgree :: String -> FilePath -> IO Bool
isAgree message path = do
  putStr $ message ++ path ++ "? "
  hFlush stdout
  input <- getLine
  return $ not (null input) && toUpper (head input) == 'Y'


filterSpecialFiles :: ([FilePath], [FilePath]) -> FilePath -> IO ([FilePath], [FilePath])
filterSpecialFiles (normals, specials) path = do
  isSpecial <- isSpecialFile path
  if isSpecial
    then return (normals, path:specials)
    else return (path:normals, specials)


isSpecialFile :: FilePath -> IO Bool
isSpecialFile path = do
  status <- getFileStatus path
  isSymbolicLink <- pathIsSymbolicLink path
  return $ isSymbolicLink || isNamedPipe status || isSocket status


run :: IO ()
run = do
  options <- cmdArgs macrm
  ec <- rm options ExitSuccess [] $ files options
  exitWith ec
