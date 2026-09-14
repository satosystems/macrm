{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Macrm where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.Int (Int32)
import Data.List (intercalate)
import qualified Data.List as List
import Data.Maybe
  ( fromJust,
    listToMaybe,
    mapMaybe,
  )
import qualified Data.Text as T
import Data.Version (showVersion)
import Foreign.C.String (withCString)
import GitHash
  ( GitInfo,
    giDirty,
    giHash,
    tGitInfoCwd,
  )
import qualified Language.C.Inline as C
import Paths_macrm (version)
import System.Console.GetOpt
  ( ArgDescr (NoArg),
    ArgOrder (RequireOrder),
    OptDescr (Option),
    getOpt,
  )
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit
  ( ExitCode
      ( ExitFailure,
        ExitSuccess
      ),
    exitSuccess,
    exitWith,
  )
import System.FilePath ((</>))
import System.IO
  ( hClose,
    hFlush,
    hGetContents,
    hPutStr,
    hPutStrLn,
    stderr,
    stdout,
  )
import System.Path.NameManip
  ( absolute_path,
    guess_dotdot,
  )
import System.Posix.Files
  ( FileStatus,
    fileGroup,
    fileMode,
    fileOwner,
    getSymbolicLinkStatus,
    groupExecuteMode,
    groupReadMode,
    groupWriteMode,
    intersectFileModes,
    isDirectory,
    otherExecuteMode,
    otherReadMode,
    otherWriteMode,
    ownerExecuteMode,
    ownerReadMode,
    ownerWriteMode,
    setGroupIDMode,
    setUserIDMode,
  )
import System.Posix.Types
  ( FileMode,
    GroupID,
    UserID,
  )
import System.Posix.User (getRealUserID)
import System.Process
  ( CreateProcess
      ( std_err,
        std_in,
        std_out
      ),
    StdStream (CreatePipe),
    createProcess,
    proc,
    waitForProcess,
  )

C.include "<fcntl.h>"
C.include "<unistd.h>"
C.include "<sys/stat.h>"

data FileExists = NotExists | DeadLink | Exists deriving (Eq, Show)

type FileInfo = (FilePath, FileExists, Maybe FileStatus)

data Options = Options
  { directory :: Bool,
    force :: Bool,
    interactive :: Bool,
    plaster :: Bool,
    recursive :: Bool,
    recursive' :: Bool,
    verbose :: Bool,
    whiteouts :: Bool,
    files :: [FilePath]
  }
  deriving (Eq, Show)

data Flag
  = FlagDirectory
  | FlagForce
  | FlagInteractive
  | FlagPlaster
  | FlagRecursive
  | FlagRecursiveAlias
  | FlagVerbose
  | FlagWhiteouts
  | FlagHelp
  | FlagVersion
  | FlagNumericVersion
  deriving (Eq, Show)

data Command = Run Options | PrintHelp | PrintVersion | PrintNumericVersion
  deriving (Eq, Show)

emptyOptions :: Options
emptyOptions = Options False False False False False False False False []

optionDescriptions :: [OptDescr Flag]
optionDescriptions =
  [ Option ['d'] ["directory"] (NoArg FlagDirectory) "Attempt to remove directories as well as other types of files.",
    Option ['f'] ["force"] (NoArg FlagForce) "Attempt to remove the files without prompting for confirmation.",
    Option ['i'] ["interactive"] (NoArg FlagInteractive) "Request confirmation before attempting to remove each file.",
    Option ['P'] ["plaster"] (NoArg FlagPlaster) "Overwrite regular files before deleting them. This flag is ignored under macrm.",
    Option ['R'] ["recursive"] (NoArg FlagRecursive) "Attempt to remove the file hierarchy rooted in each file argument.",
    Option ['r'] [] (NoArg FlagRecursiveAlias) "Equivalent to -R.",
    Option ['v'] ["verbose"] (NoArg FlagVerbose) "Be verbose when deleting files, showing them as they are removed.",
    Option ['W'] ["whiteouts"] (NoArg FlagWhiteouts) "Attempt to undelete the named files. This flag is ignored under macrm.",
    Option ['?'] ["help"] (NoArg FlagHelp) "Display help message",
    Option ['V'] ["version"] (NoArg FlagVersion) "Print version information",
    Option [] ["numeric-version"] (NoArg FlagNumericVersion) "Print just the version number"
  ]

usageString :: String
usageString = "usage: macrm [-f | -i] [-dPRrvW] file ...\n       unlink file"

helpString :: String
helpString =
  unlines
    [ "macrm [OPTIONS] [FILES/DIRS]",
      "",
      "Common flags:",
      "  -d --directory        Attempt to remove directories as well as other types",
      "                        of files.",
      "  -f --force            Attempt to remove the files without prompting for",
      "                        confirmation, regardless of the file's permissions. If",
      "                        the file does not exist, do not display a diagnostic",
      "                        message or modify the exit status to reflect an error.",
      "                        The -f option overrides any previous -i options.",
      "  -i --interactive      Request confirmation before attempting to remove each",
      "                        file, regardless of the file's permissions, or whether",
      "                        or not the standard input device is a terminal. The -i",
      "                        option overrides any previous -f options.",
      "  -P --plaster          Overwrite regular files before deleting them. Files",
      "                        are overwritten three times, first with the byte",
      "                        pattern 0xff, then 0x00, and then 0xff again, before",
      "                        they are deleted. This flag is ignored under macrm.",
      "  -R --recursive        Attempt to remove the file hierarchy rooted in each",
      "                        file argument. The -R option implies the -d option. If",
      "                        the -i option is specified, the user is prompted for",
      "                        confirmation before each directory's contents are",
      "                        processed (as well as before the attempt is made to",
      "                        remove the directory). If the user does not respond",
      "                        affirmatively, the file hierarchy rooted in that",
      "                        directory is skipped.",
      "  -r                    Equivalent to -R.",
      "  -v --verbose          Be verbose when deleting files, showing them as they",
      "                        are removed.",
      "  -W --whiteouts        Attempt to undelete the named files. Currently, this",
      "                        option can only be used to recover files covered by",
      "                        whiteouts. This flag is ignored under macrm.",
      "  -? --help             Display help message",
      "  -V --version          Print version information",
      "     --numeric-version  Print just the version number"
    ]

parseOptions :: [String] -> Either String Command
parseOptions rawArgs =
  case getOpt RequireOrder optionDescriptions rawArgs of
    (flags, operands, []) -> Right $ flagsToCommand flags operands
    (_, _, errors) -> Left $ concat errors ++ usageString ++ "\n"

flagsToCommand :: [Flag] -> [FilePath] -> Command
flagsToCommand flags operands =
  case listToMaybe $ mapMaybe flagCommand flags of
    Just command -> command
    Nothing -> Run $ (List.foldl' applyFlag emptyOptions flags) {files = operands}

flagCommand :: Flag -> Maybe Command
flagCommand FlagHelp = Just PrintHelp
flagCommand FlagVersion = Just PrintVersion
flagCommand FlagNumericVersion = Just PrintNumericVersion
flagCommand _ = Nothing

applyFlag :: Options -> Flag -> Options
applyFlag options FlagDirectory = options {directory = True}
applyFlag options FlagForce = options {force = True, interactive = False}
applyFlag options FlagInteractive = options {force = False, interactive = True}
applyFlag options FlagPlaster = options {plaster = True}
applyFlag options FlagRecursive = options {recursive = True}
applyFlag options FlagRecursiveAlias = options {recursive' = True}
applyFlag options FlagVerbose = options {verbose = True}
applyFlag options FlagWhiteouts = options {whiteouts = True}
applyFlag options _ = options

absolutize :: FilePath -> IO FilePath
absolutize path = fromJust . guess_dotdot <$> absolute_path path

rm :: Options -> ExitCode -> UserID -> [FileInfo] -> [FilePath] -> IO ExitCode
rm (Options False False False False False False False False []) ExitSuccess _ [] [] =
  do
    hPutStrLn
      stderr
      "usage: macrm [-f | -i] [-dPRrvW] file ...\n       unlink file"
    return $ ExitFailure 1
rm _ exitCode _ removables [] = removeRemovables exitCode removables
rm options exitCode uid removables (path : paths) = do
  fileInfo <- getFileInfo path
  case fileInfo of
    (_, NotExists, _) ->
      if force options
        then rm options exitCode uid removables paths
        else do
          hPutStrLn stderr $ "macrm: " ++ path ++ ": No such file or directory"
          rm options (ExitFailure 1) uid removables paths
    (_, _, Nothing) -> do
      hPutStrLn stderr $ "macrm: " ++ path ++ ": unable to inspect file"
      rm options (ExitFailure 1) uid removables paths
    (_, fileExists, Just status) -> do
      let isDir = fileExists == Exists && isDirectory status
      let withRecursive = recursive options || recursive' options
      let withDirectory = directory options
      isNotEmpty <-
        if isDir
          then not . null <$> listDirectory path
          else return False
      if isDir
        && not withRecursive
        && (not withDirectory || withDirectory && isNotEmpty)
        then
          if withDirectory && isNotEmpty
            then do
              hPutStrLn stderr $ "macrm: " ++ path ++ ": Directory not empty"
              rm options (ExitFailure 1) uid removables paths
            else do
              hPutStrLn stderr $ "macrm: " ++ path ++ ": is a directory"
              rm options (ExitFailure 1) uid removables paths
        else
          if interactive options
            then do
              let (message, isDirWithRecursive) = case (isDir, withRecursive) of
                    (True, True) -> ("examine files in directory ", True)
                    _ -> ("remove ", False)
              agreement <- getAgreement message path
              if agreement
                then
                  if isDirWithRecursive
                    then
                      rmInteractiveRecursiveDirectory
                        options
                        exitCode
                        uid
                        removables
                        fileInfo
                        path
                        paths
                    else do
                      when (verbose options) $ putStrLn path
                      rm options exitCode uid (fileInfo : removables) paths
                else rm options exitCode uid removables paths
            else do
              let fileUid = fileOwner status
                  fileGid = fileGroup status
              needRemove <-
                if force options || uid == fileUid
                  then return True
                  else do
                    message <- makeMessage path status fileUid fileGid
                    getAgreement message path
              if needRemove
                then do
                  when (verbose options) $ putStrLn path
                  rm options exitCode uid (fileInfo : removables) paths
                else rm options exitCode uid removables paths

removeRemovables :: ExitCode -> [FileInfo] -> IO ExitCode
removeRemovables exitCode [] = return exitCode
removeRemovables exitCode removables = do
  ec <- remove removables
  case ec of
    ExitSuccess -> return exitCode
    _ -> return $ ExitFailure 1

markFailed :: ExitCode -> ExitCode
markFailed ExitSuccess = ExitFailure 1
markFailed exitCode = exitCode

rmInteractiveRecursiveDirectory ::
  Options ->
  ExitCode ->
  UserID ->
  [FileInfo] ->
  FileInfo ->
  FilePath ->
  [FilePath] ->
  IO ExitCode
rmInteractiveRecursiveDirectory options exitCode uid removables fileInfo path paths = do
  -- -iR では、親ディレクトリを削除候補に入れる前に子要素を個別に確認する。
  -- 先に指定された子パスが保留中の場合、物理的に残ったままだと再帰走査で二重に処理されるため、ここで確定済みの削除を反映する。
  exitCodeAfterPending <- removeRemovables exitCode removables
  entries <- map (path </>) <$> listDirectory path
  exitCodeAfterEntries <- rm options exitCodeAfterPending uid [] entries
  removeDirectoryAgreement <- getAgreement "remove " path
  if removeDirectoryAgreement
    then do
      afterEntries <- listDirectory path
      if null afterEntries
        then do
          when (verbose options) $ putStrLn path
          exitCodeAfterDirectory <- removeRemovables exitCodeAfterEntries [fileInfo]
          rm options exitCodeAfterDirectory uid [] paths
        else do
          -- 子要素を残したまま親を Trash に送ると、ユーザーが拒否した子まで削除される。
          -- そのため、通常の rmdir 相当として失敗扱いにして次の引数へ進む。
          hPutStrLn stderr $ "macrm: " ++ path ++ ": Directory not empty"
          rm options (markFailed exitCodeAfterEntries) uid [] paths
    else rm options exitCodeAfterEntries uid [] paths

remove :: [FileInfo] -> IO ExitCode
remove fileInfos = do
  let paths = map (\(path, _, _) -> path) fileInfos
  absolutePaths <- mapM absolutize paths
  executeScript . createScript . reverse $ absolutePaths

executeScript :: String -> IO ExitCode
executeScript script = do
  (Just stdIn, _, _, ph) <-
    createProcess
      (proc "osascript" ["-l", "JavaScript"])
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  hPutStr stdIn script
  hFlush stdIn
  hClose stdIn
  waitForProcess ph

createScript :: [FilePath] -> String
createScript paths =
  unlines
    [ "ObjC.import('Foundation');",
      "const fm = $.NSFileManager.defaultManager;",
      "const paths = [",
      intercalate ",\n" $ map (("  " ++) . quoteJavaScriptString) paths,
      "];",
      "for (const path of paths) {",
      "  const url = $.NSURL.fileURLWithPath(path);",
      "  const result = Ref();",
      "  const err = Ref();",
      "  const ok = fm.trashItemAtURLResultingItemURLError(url, result, err);",
      "  if (!ok) {",
      "    throw ObjC.unwrap(err[0].localizedDescription);",
      "  }",
      "}"
    ]

quoteJavaScriptString :: String -> String
quoteJavaScriptString value = "\"" ++ concatMap escape value ++ "\""
  where
    escape :: Char -> String
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = [c]

getAgreement :: String -> FilePath -> IO Bool
getAgreement message path = do
  putStr $ message ++ path ++ "? "
  hFlush stdout
  maybe False ((== 'Y') . toUpper) . listToMaybe <$> getLine

getFileFlags :: FilePath -> IO (Maybe String)
getFileFlags path = do
  (_, Just stdOut, _, ph) <-
    createProcess
      (proc "/bin/ls" ["-lO", path])
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }
  _ <- waitForProcess ph
  output <- hGetContents stdOut
  if null output
    then return Nothing
    else case drop 4 (words output) of
      flags : _ -> return $ Just flags
      [] -> return Nothing

makeMessage :: FilePath -> FileStatus -> UserID -> GroupID -> IO String
makeMessage path status uid gid = do
  userAndGroup <- makeUserAndGroupString uid gid
  mFlags <- getFileFlags path
  return $
    "override "
      ++ makePermissionString status
      ++ "  "
      ++ userAndGroup
      ++ maybe "" (" " ++) mFlags
      ++ " for "

makePermissionString :: FileStatus -> String
makePermissionString status =
  [ ifm permission ownerReadMode 'r',
    ifm permission ownerWriteMode 'w',
    ifm permission ownerExecuteMode 'x',
    ifm permission groupReadMode 'r',
    ifm permission groupWriteMode 'w',
    ifm permission groupExecuteMode 'x',
    ifm permission otherReadMode 'r',
    ifm permission otherWriteMode 'w',
    ifm permission otherExecuteMode 'x'
  ]
  where
    permission :: FileMode
    permission = fileMode status
    ifm :: FileMode -> FileMode -> Char -> Char
    ifm p m a =
      let stickyBit = 0o1000
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
       in if intersectFileModes p m == m
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
    searchIdName uidOrGid [] = uidOrGid
    searchIdName uidOrGid (('#' : _) : ss) = searchIdName uidOrGid ss
    searchIdName uidOrGid (s : ss) =
      if id' == uidOrGid
        then name'
        else searchIdName uidOrGid ss
      where
        id' :: String
        name' :: String
        (id', name') = case T.splitOn ":" (T.pack s) of
          nameText : _ : idText : _ ->
            (T.unpack idText, T.unpack nameText)
          _ -> ("", "")

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path = do
  fileExists <- isPathExists path
  if fileExists == NotExists
    then return (path, fileExists, Nothing)
    else do
      status <- getSymbolicLinkStatus path
      return (path, fileExists, Just status)

isPathExists :: FilePath -> IO FileExists
isPathExists path = do
  rc <- withCString path $ \cpath ->
    [C.block| int {
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
  case rc of
    0 -> return NotExists
    1 -> return DeadLink
    2 -> return Exists
    _ -> fail $ "unexpected lstat result: " ++ show rc -- never happen

gitInfo :: GitInfo
gitInfo = $$(tGitInfoCwd)

versionString :: String
versionString =
  concat
    [ "macrm ver ",
      showVersion version,
      " based on Git commit ",
      giHash gitInfo,
      if giDirty gitInfo then " Dirty" else " Clean"
    ]

run :: IO ()
run = do
  rawArgs <- getArgs
  case parseOptions rawArgs of
    Left errors -> do
      hPutStr stderr errors
      exitWith $ ExitFailure 1
    Right PrintHelp -> do
      putStr helpString
      exitSuccess
    Right PrintVersion -> do
      putStrLn versionString
      exitSuccess
    Right PrintNumericVersion -> do
      putStrLn $ showVersion version
      exitSuccess
    Right (Run options) -> do
      uid <- getRealUserID
      ec <- rm options ExitSuccess uid [] . files $ options
      exitWith ec
