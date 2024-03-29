{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Macrm where

import           Control.Conditional            ( ifM )
import           Control.Monad                  ( foldM
                                                , unless
                                                , when
                                                )
import           Data.Char                      ( toUpper )
import           Data.Fixed                     ( Fixed
                                                , HasResolution
                                                , Uni
                                                , showFixed
                                                )
import           Data.Int                       ( Int32 )
import           Data.List.Utils                ( replace )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import qualified Data.Text                     as T
import           Data.Time.LocalTime            ( TimeOfDay(TimeOfDay)
                                                , ZonedTime
                                                  ( zonedTimeToLocalTime
                                                  )
                                                , getZonedTime
                                                , localTimeOfDay
                                                )
import           Data.Tuple.Utils               ( fst3
                                                , snd3
                                                , thd3
                                                )
import           Data.Version                   ( showVersion )
import           Foreign.C.String               ( withCString )
import           GitHash                        ( GitInfo
                                                , giDirty
                                                , giHash
                                                , tGitInfoCwd
                                                )
import qualified Language.C.Inline             as C
import           Paths_macrm                    ( version )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , args
                                                , cmdArgs
                                                , explicit
                                                , help
                                                , name
                                                , noAtExpand
                                                , program
                                                , summary
                                                , typ
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , getHomeDirectory
                                                , listDirectory
                                                , renameDirectory
                                                , renameFile
                                                )
import           System.Exit                    ( ExitCode
                                                  ( ExitFailure
                                                  , ExitSuccess
                                                  )
                                                , exitWith
                                                )
import           System.IO                      ( hClose
                                                , hFlush
                                                , hGetContents
                                                , hPutStr
                                                , hPutStrLn
                                                , stderr
                                                , stdout
                                                )
import           System.Path.NameManip          ( absolute_path
                                                , guess_dotdot
                                                )
import           System.Posix.Files             ( FileStatus
                                                , fileGroup
                                                , fileMode
                                                , fileOwner
                                                , getSymbolicLinkStatus
                                                , groupExecuteMode
                                                , groupReadMode
                                                , groupWriteMode
                                                , intersectFileModes
                                                , isBlockDevice
                                                , isCharacterDevice
                                                , isDirectory
                                                , isNamedPipe
                                                , isSocket
                                                , isSymbolicLink
                                                , otherExecuteMode
                                                , otherReadMode
                                                , otherWriteMode
                                                , ownerExecuteMode
                                                , ownerReadMode
                                                , ownerWriteMode
                                                , setGroupIDMode
                                                , setUserIDMode
                                                )
import           System.Posix.Types             ( FileMode
                                                , GroupID
                                                , UserID
                                                )
import           System.Posix.User              ( getRealUserID )
import           System.Process                 ( CreateProcess
                                                  ( std_err
                                                  , std_in
                                                  , std_out
                                                  )
                                                , StdStream(CreatePipe)
                                                , createProcess
                                                , proc
                                                , waitForProcess
                                                )

C.include "<fcntl.h>"
C.include "<unistd.h>"
C.include "<sys/stat.h>"

data FileExists = NotExists | DeadLink | Exists deriving (Eq, Show)

type FileInfo = (FilePath, FileExists, Maybe FileStatus)

data Options = Options
  { directory   :: Bool
  , force       :: Bool
  , interactive :: Bool
  , plaster     :: Bool
  , recursive   :: Bool
  , recursive'  :: Bool
  , verbose     :: Bool
  , whiteouts   :: Bool
  , files       :: [FilePath]
  }
  deriving (Data, Show, Typeable)

getOptions :: Options
getOptions =
  Options
      { directory   = False
        &= help "Attempt to remove directories as well as other types of files."
      , force       = False &= help
        (  "Attempt to remove the files without prompting for confirmation, "
        ++ "regardless of the file's permissions. If the file does not exist, "
        ++ "do not display a diagnostic message or modify the exit status to reflect an error. "
        ++ "The -f option overrides any previous -i options."
        )
      , interactive = False &= help
        (  "Request confirmation before attempting to remove each file, "
        ++ "regardless of the file's permissions, "
        ++ "or whether or not the standard input device is a terminal. "
        ++ "The -i option overrides any previous -f options."
        )
      , plaster     = False &= name "P" &= help
        (  "Overwrite regular files before deleting them. "
        ++ "Files are overwritten three times, first with the byte pattern 0xff, "
        ++ "then 0x00, and then 0xff again, before they are deleted. "
        ++ "This flag is ignored under macrm."
        )
      , recursive   = False &= name "R" &= help
        ("Attempt to remove the file hierarchy rooted in each file argument. "
        ++ "The -R option implies the -d option. If the -i option is specified, "
        ++ "the user is prompted for confirmation before each directory's "
        ++ "contents are processed (as well as before the attempt is made to remove the directory). "
        ++ "If the user does not respond affirmatively, "
        ++ "the file hierarchy rooted in that directory is skipped."
        )
      , recursive'  = False &= name "r" &= explicit &= help "Equivalent to -R."
      , verbose     = False &= help
        "Be verbose when deleting files, showing them as they are removed."
      , whiteouts   = False &= name "W" &= help
        ("Attempt to undelete the named files. "
        ++ "Currently, this option can only be used to recover files covered by whiteouts. "
        ++ "This flag is ignored under macrm."
        )
      , files       = [] &= args &= typ "FILES/DIRS"
      }
    &= summary versionString
    &= program "macrm"
    &= noAtExpand

absolutize :: FilePath -> IO FilePath
absolutize path = fromJust . guess_dotdot <$> absolute_path path

moveToTrash :: [FileInfo] -> IO ()
moveToTrash []               = return ()
moveToTrash (fileInfo : fis) = do
  pair <- makePair fileInfo
  move pair
  moveToTrash fis
 where
  makePair :: FileInfo -> IO (FilePath, FilePath)
  makePair (path, _, _) = do
    let fileOrDirName = T.unpack . last . T.splitOn "/" . T.pack $ path
    removedPath <- getRemovedPath fileOrDirName
    return (path, removedPath)
  move :: (FilePath, FilePath) -> IO ()
  move (from, to) = ifM (doesDirectoryExist from)
                        (renameDirectory from to)
                        (renameFile from to)

getRemovedPath :: FilePath -> IO FilePath
getRemovedPath fileOrDirName = do
  dayOfTime <- getCurrentDayOfTime
  searchRemovedPath fileOrDirName $ ' ' : dayOfTime

searchRemovedPath :: FilePath -> String -> IO FilePath
searchRemovedPath fileOrDirName suffix = do
  homePath <- getHomeDirectory
  let trashPath = homePath ++ "/.Trash/"
  removed <- listDirectory trashPath
  if fileOrDirName `elem` removed
    then searchRemovedPath (fileOrDirName ++ suffix) suffix
    else return $ trashPath ++ fileOrDirName

getCurrentDayOfTime :: IO String
getCurrentDayOfTime = do
  zonedTime <- getZonedTime
  let (TimeOfDay hour minute second) =
        (localTimeOfDay . zonedTimeToLocalTime) zonedTime
      sHour    = if hour >= 10 then show hour else '0' : show hour
      sMinute  = if minute >= 10 then show minute else '0' : show minute
      uSecond  = changeResolution second :: Uni
      sSecond' = showFixed True uSecond
      sSecond  = if uSecond >= 10 then sSecond' else '0' : sSecond'
      suffix   = sHour ++ "." ++ sMinute ++ "." ++ sSecond
  return suffix

changeResolution :: (HasResolution a, HasResolution b) => Fixed a -> Fixed b
changeResolution = fromRational . toRational

rm :: Options -> ExitCode -> UserID -> [FileInfo] -> [FilePath] -> IO ExitCode
rm (Options False False False False False False False False []) ExitSuccess _ [] []
  = do
    hPutStrLn stderr
              "usage: macrm [-f | -i] [-dPRrvW] file ...\n       unlink file"
    return $ ExitFailure 1
rm _ exitCode _ []         [] = return exitCode
rm _ exitCode _ removables [] = do
  ec <- remove removables
  case ec of
    ExitSuccess -> return exitCode
    _           -> return $ ExitFailure 1
rm options exitCode uid removables (path : paths) = do
  fileInfo <- getFileInfo path
  if snd3 fileInfo == NotExists
    then if force options
      then rm options exitCode uid removables paths
      else do
        hPutStrLn stderr $ "macrm: " ++ path ++ ": No such file or directory"
        rm options (ExitFailure 1) uid removables paths
    else do
      let status        = fromJust . thd3 $ fileInfo
      let isDir         = snd3 fileInfo == Exists && isDirectory status
      let withRecursive = recursive options || recursive' options
      let withDirectory = directory options
      isNotEmpty <- if isDir
        then not . null <$> listDirectory path
        else return False
      if isDir
           && not withRecursive
           && (not withDirectory || withDirectory && isNotEmpty)
        then if withDirectory && isNotEmpty
          then do
            hPutStrLn stderr $ "macrm: " ++ path ++ ": Directory not empty"
            rm options (ExitFailure 1) uid removables paths
          else do
            hPutStrLn stderr $ "macrm: " ++ path ++ ": is a directory"
            rm options (ExitFailure 1) uid removables paths
        else if interactive options
          then do
            let message = case (isDir, withRecursive) of
                  (True, True) -> "examine files in directory "
                  _            -> "remove "
            agreement <- getAgreement message path
            if agreement
              then do
                when (verbose options) $ putStrLn path
                rm options exitCode uid (fileInfo : removables) paths
              else rm options exitCode uid removables paths
          else do
            let fileUid = fileOwner status
                fileGid = fileGroup status
            mMessage <- if uid == fileUid
              then return Nothing
              else Just <$> makeMessage fileInfo fileUid fileGid
            needRemove <- if isNothing mMessage
              then return True
              else getAgreement (fromJust mMessage) path
            if needRemove
              then do
                when (verbose options) $ putStrLn path
                rm options exitCode uid (fileInfo : removables) paths
              else rm options exitCode uid removables paths

remove :: [FileInfo] -> IO ExitCode
remove fileInfos = do
  let (paths, isDeadLinks, mStatuses) = foldl
        (\(paths', isDeadLinks', mStatuses') (path, isDeadLink, mStatus) ->
          (path : paths', isDeadLink : isDeadLinks', mStatus : mStatuses')
        )
        ([], [], [])
        fileInfos
  absolutePaths       <- mapM absolutize paths
  (normals, specials) <- foldM filterSpecialFiles ([], [])
    $ zip3 absolutePaths isDeadLinks mStatuses
  unless (null specials) $ moveToTrash . reverse $ specials
  if null normals
    then return ExitSuccess
    else executeScript . createScript . map fst3 . reverse $ normals

executeScript :: String -> IO ExitCode
executeScript script = do
  (Just stdIn, _, _, ph) <- createProcess (proc "osascript" [])
    { std_in  = CreatePipe
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
  , concatMap
    (\path ->
      "set end of l to posix file \""
        ++ replace "\"" "\\\"" path
        ++ "\" as alias\n"
    )
    paths
  , "tell application \"Finder\"\n"
  , "delete l\n"
  , "end tell\n"
  , "return"
  ]

getAgreement :: String -> FilePath -> IO Bool
getAgreement message path = do
  putStr $ message ++ path ++ "? "
  hFlush stdout
  input <- getLine
  return $ not (null input) && toUpper (head input) == 'Y'

filterSpecialFiles
  :: ([FileInfo], [FileInfo]) -> FileInfo -> IO ([FileInfo], [FileInfo])
filterSpecialFiles (normals, specials) fileInfo = if isSpecialFile fileInfo
  then return (normals, fileInfo : specials)
  else return (fileInfo : normals, specials)

getFileFlags :: FilePath -> IO (Maybe String)
getFileFlags path = do
  (_, Just stdOut, _, ph) <- createProcess (proc "/bin/ls" ["-lO", path])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  _      <- waitForProcess ph
  output <- hGetContents stdOut
  if null output
    then return Nothing
    else
      let flags = T.unpack $ (T.splitOn " " . T.pack $ output) !! 7
      in  return $ Just flags

makeMessage :: FileInfo -> UserID -> GroupID -> IO String
makeMessage (path, _, Just status) uid gid = do
  userAndGroup <- makeUserAndGroupString uid gid
  mFlags       <- getFileFlags path
  return
    $  "override "
    ++ makePermissionString status
    ++ "  "
    ++ userAndGroup
    ++ maybe "" (" " ++) mFlags
    ++ " for "
makeMessage _ _ _ = undefined -- never happen

makePermissionString :: FileStatus -> String
makePermissionString status =
  [ ifm permission ownerReadMode    'r'
  , ifm permission ownerWriteMode   'w'
  , ifm permission ownerExecuteMode 'x'
  , ifm permission groupReadMode    'r'
  , ifm permission groupWriteMode   'w'
  , ifm permission groupExecuteMode 'x'
  , ifm permission otherReadMode    'r'
  , ifm permission otherWriteMode   'w'
  , ifm permission otherExecuteMode 'x'
  ]
 where
  permission :: FileMode
  permission = fileMode status
  ifm :: FileMode -> FileMode -> Char -> Char
  ifm p m a =
    let
      stickyBit = 0o1000
      isU =
        m
          == ownerExecuteMode
          && intersectFileModes p setUserIDMode
          == setUserIDMode
      isG =
        m
          == groupExecuteMode
          && intersectFileModes p setGroupIDMode
          == setGroupIDMode
      isO =
        m == otherExecuteMode && intersectFileModes p stickyBit == stickyBit
    in
      if intersectFileModes p m == m
        then if isU || isG then 's' else if isO then 't' else a
        else if isU || isG then 'S' else if isO then 'T' else '-'

makeUserAndGroupString :: UserID -> GroupID -> IO String
makeUserAndGroupString uid gid = do
  passwdContents <- readFile "/etc/passwd"
  let user =
        searchIdName (show (fromIntegral uid :: Int32)) $ lines passwdContents
  groupContents <- readFile "/etc/group"
  let group =
        searchIdName (show (fromIntegral gid :: Int32)) $ lines groupContents
  return $ user ++ "/" ++ group
 where
  searchIdName :: String -> [String] -> String
  searchIdName uidOrGid []               = uidOrGid
  searchIdName uidOrGid (('#' : _) : ss) = searchIdName uidOrGid ss
  searchIdName uidOrGid (s         : ss) = if id' == uidOrGid
    then name'
    else searchIdName uidOrGid ss
   where
    splitted :: [T.Text]
    splitted = T.splitOn ":" . T.pack $ s
    id' :: String
    id' = T.unpack $ splitted !! 2
    name' :: String
    name' = T.unpack . head $ splitted

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
  fileExists <- isPathExists path
  if fileExists == NotExists
    then return (path, fileExists, Nothing)
    else do
      status <- getSymbolicLinkStatus path
      return (path, fileExists, Just status)

isSpecialFile :: FileInfo -> Bool
isSpecialFile (_, NotExists, _      ) = False
isSpecialFile (_, DeadLink , _      ) = True
isSpecialFile (_, Exists   , Nothing) = undefined -- never happen
isSpecialFile (_, Exists, Just status) =
  isSymbolicLink status
    || isNamedPipe status
    || isSocket status
    || isCharacterDevice status
    || isBlockDevice status

isPathExists :: FilePath -> IO FileExists
isPathExists path = do
  rc <- withCString path $ \cpath -> [C.block| int {
    struct stat lstat_info;
    int fd;
    if (lstat($(char *cpath), &lstat_info) == -1) {
      return 0; // not exists
    }
    fd = open($(char *cpath), O_RDONLY);
    if (fd == -1) {
      return 1; // dead link
    }
    close(fd);
    return 2; // exists
  } |]
  return $ case rc of
    0 -> NotExists
    1 -> DeadLink
    2 -> Exists
    _ -> undefined -- never happen

gitInfo :: GitInfo
gitInfo = $$(tGitInfoCwd)

versionString :: String
versionString = concat
  [ "macrm ver "
  , showVersion version
  , " based on Git commit "
  , giHash gitInfo
  , if giDirty gitInfo then " Dirty" else " Clean"
  ]

run :: IO ()
run = do
  options <- cmdArgs getOptions
  uid     <- getRealUserID
  ec      <- rm options ExitSuccess uid [] . files $ options
  exitWith ec
