{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Macrm where

import Control.Conditional (ifM)
import Control.Monad
  ( foldM
  , unless
  , when
  )
import Control.Monad.Catch
  ( SomeException(SomeException)
  , catch
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
  , hGetLine
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
  ( FileStatus
  , fileGroup
  , fileMode
  , fileOwner
  , getFileStatus
  , groupExecuteMode
  , groupReadMode
  , groupWriteMode
  , intersectFileModes
  , isDirectory
  , isNamedPipe
  , isSocket
  , ownerExecuteMode
  , ownerReadMode
  , ownerWriteMode
  , otherExecuteMode
  , otherReadMode
  , otherWriteMode
  , setGroupIDMode
  , setUserIDMode
  )
import System.Posix.Types
  ( FileMode
  , GroupID
  , UserID
  )
import System.Posix.User
  ( getRealGroupID
  , getRealUserID
  )
import System.Process
  ( CreateProcess(std_err, std_in, std_out)
  , StdStream(CreatePipe)
  , createProcess
  , proc
  , waitForProcess
  )


type File = (FilePath, FileStatus)


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


moveToTrash :: [File] -> IO ()
moveToTrash files = do
  homePath <- getHomeDirectory
  let trashPath = homePath ++ "/.Trash/"
  pairs <- mapM (makePairs trashPath) files
  mapM_ move pairs
 where
  makePairs :: FilePath -> File -> IO (FilePath, FilePath)
  makePairs trashPath (oldPath, _) = do
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


rm :: Macrm -> ExitCode -> UserID -> GroupID -> [File] -> [FilePath] -> IO ExitCode
rm (Macrm False False False False False False False False []) ExitSuccess _ _ [] [] = do
  hPutStrLn stderr "usage: macrm [-f | -i] [-dPRrvW] file ...\n       unlink file"
  return $ ExitFailure 1
rm _ exitCode _ _ [] [] = return exitCode
rm _ exitCode _ _ removablePaths [] = do
  ec <- remove removablePaths
  case ec of
    ExitSuccess -> return exitCode
    _ -> return $ ExitFailure 1
rm options exitCode uid gid removablePaths (path:paths) = do
  pathExist <- doesPathExist path
  if not pathExist && not (force options)
    then do
      hPutStrLn stderr $ "macrm: " ++ path ++ ": No such file or directory"
      rm options (ExitFailure 1) uid gid removablePaths paths
    else do
      status <- getFileStatus path
      let isDir = isDirectory status
      if isDir && not (recursive options || recursive' options)
        then do
          hPutStrLn stderr $ "macrm: " ++ path ++ ": is a directory"
          rm options (ExitFailure 1) uid gid removablePaths paths
        else if pathExist
          then if interactive options
            then ifM (isAgree (if isDir then "examine files in directory " else "remove ") path)
              (do
                when (verbose options) $ putStrLn path
                rm options exitCode uid gid ((path, status):removablePaths) paths)
              (rm options exitCode uid gid removablePaths paths)
            else do
              when (verbose options) $ putStrLn path
              rm options exitCode uid gid ((path, status):removablePaths) paths
          else rm options exitCode uid gid removablePaths paths


remove :: [File] -> IO ExitCode
remove files = do
  let (paths, statuses) = foldl (\(paths, statuses) (path, status) -> (path:paths, status:statuses)) ([], []) files
  absolutePaths <- mapM absolutize paths
  (normals, specials) <- foldM filterSpecialFiles ([], []) (zip absolutePaths statuses)
  unless (null specials) (moveToTrash specials)
  if null normals then return ExitSuccess else executeScript $ createScript $ map fst normals


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


filterSpecialFiles :: ([File], [File]) -> File -> IO ([File], [File])
filterSpecialFiles (normals, specials) file = do
  isSpecial <- isSpecialFile file
  if isSpecial
    then return (normals, file:specials)
    else return (file:normals, specials)


isSpecialFile :: File -> IO Bool
isSpecialFile (path, status) = do
  isSymbolicLink <- pathIsSymbolicLink path
  return $ isSymbolicLink || isNamedPipe status || isSocket status


makeMessage :: FileStatus -> UserID -> GroupID -> IO String
makeMessage status uid gid = do
  userAndGroup <- makeUserAndGroupString uid gid
  return $ "override " ++ makePermissionString status ++ "  " ++ userAndGroup ++ " for "


makePermissionString :: FileStatus -> String
makePermissionString status =
  [ ifm permission ownerReadMode 'r'
  , ifm permission ownerWriteMode 'w'
  , ifm permission ownerExecuteMode 'x'
  , ifm permission groupReadMode 'r'
  , ifm permission groupWriteMode 'w'
  , ifm permission groupExecuteMode 'x'
  , ifm permission otherReadMode 'r'
  , ifm permission otherWriteMode 'w'
  , ifm permission otherExecuteMode 'x'
  ]
 where
  permission :: FileMode
  permission = fileMode status
  ifm :: FileMode -> FileMode -> Char -> Char
  ifm p m a =
    let stickyBit = 0o1000
        isU = m == ownerExecuteMode && intersectFileModes p setUserIDMode == setUserIDMode
        isG = m == groupExecuteMode && intersectFileModes p setGroupIDMode == setGroupIDMode
        isO = m == otherExecuteMode && intersectFileModes p stickyBit == stickyBit
    in if intersectFileModes p m == m
      then if isU || isG then 's' else if isO then 't' else a
      else if isU || isG then 'S' else if isO then 'T' else '-'


makeUserAndGroupString :: UserID -> GroupID -> IO String
makeUserAndGroupString uid gid = do
  (_, Just stdOut, Just _, processHandle) <- createProcess (proc "id" ["-nu", show uid])
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  output <- hGetLine stdOut `catch` \(SomeException _) -> return ""
  exitCode <- waitForProcess processHandle
  let user = if exitCode == ExitSuccess then output else show uid
  contents <- readFile "/etc/group"
  let group = searchGroupName $ lines contents
  return $ user ++ "/" ++ group
 where
  searchGroupName :: [String] -> String
  searchGroupName [] = show gid
  searchGroupName (('#':_):ss) = searchGroupName ss
  searchGroupName (s:ss) = if groupId == show gid then groupName else searchGroupName ss
   where
    splitted :: [T.Text]
    splitted = T.splitOn ":" $ T.pack s
    groupId :: String
    groupId = T.unpack $ splitted !! 2
    groupName :: String
    groupName = T.unpack $ head splitted


run :: IO ()
run = do
  options <- cmdArgs macrm
  uid <- getRealUserID
  gid <- getRealGroupID
  ec <- rm options ExitSuccess uid gid [] $ files options
  exitWith ec
