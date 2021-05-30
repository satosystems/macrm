{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8         as BS8
import           Data.List.Utils                ( startswith )
import           Data.Maybe                     ( isNothing )
import           Data.UUID                      ( toString )
import           Data.UUID.V4                   ( nextRandom )
import           Data.Version                   ( showVersion )
import           Test.Hspec                     ( hspec
                                                , describe
                                                , it
                                                , shouldBe
                                                , shouldContain
                                                , shouldReturn
                                                , shouldSatisfy
                                                , Spec
                                                )
import           Test.Main                      ( withArgs
                                                , ExitCode
                                                  ( ExitSuccess
                                                  , ExitFailure
                                                  )
                                                , captureProcessResult
                                                , withStdin
                                                , ProcessResult
                                                  ( prStdout
                                                  , prStderr
                                                  , prExitCode
                                                  , prException
                                                  )
                                                )
import           System.Directory               ( createDirectory
                                                , createDirectoryIfMissing
                                                , doesPathExist
                                                , listDirectory
                                                , removeDirectoryRecursive
                                                , removeFile
                                                , renameFile
                                                )
import           System.Environment             ( getEnv )
import           System.FilePath                ( addTrailingPathSeparator )
import           System.IO                      ( hGetContents )
import           System.Posix.Files             ( createSymbolicLink )
import           System.Process                 ( createProcess
                                                , proc
                                                , CreateProcess(std_out)
                                                , StdStream(CreatePipe)
                                                )

import           Macrm                          ( absolutize
                                                , run
                                                )
import           Paths_macrm                    ( version )

data Path = Path
  { fileOrDirName :: String
  , relativePath :: FilePath
  , absolutePath :: FilePath
  }

data TestFiles = TestFiles
  { parentDir :: Path
  , normalFiles :: [Path]
  , notExistPaths :: [Path]
  , emptyDirs :: [Path]
  , notEmptyDirs :: [Path]
  , symbolicLinkFiles :: [Path]
  , symbolicLinkDirs :: [Path]
  , deadSymbolicLinks :: [Path]
  }

createTestFile :: String -> IO Path
createTestFile baseDir = do
  uuid <- nextRandom
  let name = toString uuid
  let path = addTrailingPathSeparator baseDir ++ name
  writeFile path ""
  Path name path <$> absolutize path

createNotExistTestFile :: String -> IO Path
createNotExistTestFile baseDir = do
  uuid <- nextRandom
  let name = toString uuid
  let path = addTrailingPathSeparator baseDir ++ name
  Path name path <$> absolutize path

createTestDir :: String -> IO Path
createTestDir baseDir = do
  uuid <- nextRandom
  let name = toString uuid
  let path = addTrailingPathSeparator baseDir ++ name
  createDirectoryIfMissing True path
  Path name path <$> absolutize path

createTestSymbolicLink :: FilePath -> String -> IO Path
createTestSymbolicLink baseDir source = do
  uuid <- nextRandom
  let name = toString uuid
  let path = addTrailingPathSeparator baseDir ++ name
  createSymbolicLink source path
  Path name path <$> absolutize path

createTestFiles :: IO TestFiles
createTestFiles = do
  baseDir <- createTestDir ".test"
  let baseDirPath = relativePath baseDir
  let baseDirs    = replicate 3 baseDirPath
  normalFiles'   <- mapM createTestFile baseDirs
  notExistPaths' <- mapM createNotExistTestFile baseDirs
  emptyDirs'     <- mapM createTestDir baseDirs
  notEmptyDirs'  <- mapM createTestDir baseDirs
  mapM_ (createTestFile . relativePath) notEmptyDirs'
  let createTestSymbolicLink' =
        mapM (\(Path fn _ _) -> createTestSymbolicLink baseDirPath fn)
  symbolicLinkFiles' <- createTestSymbolicLink' normalFiles'
  symbolicLinkDirs'  <- createTestSymbolicLink' emptyDirs'
  normalFiles''      <- mapM createTestFile baseDirs
  deadSymbolicLinks' <- createTestSymbolicLink' normalFiles''
  mapM_ (\(Path _ _ ap) -> removeFile ap) normalFiles''
  return $ TestFiles { parentDir         = baseDir
                     , normalFiles       = normalFiles'
                     , notExistPaths     = notExistPaths'
                     , emptyDirs         = emptyDirs'
                     , notEmptyDirs      = notEmptyDirs'
                     , symbolicLinkFiles = symbolicLinkFiles'
                     , symbolicLinkDirs  = symbolicLinkDirs'
                     , deadSymbolicLinks = deadSymbolicLinks'
                     }

removeTestFiles :: TestFiles -> IO ()
removeTestFiles = removeDirectoryRecursive . relativePath . parentDir

createSpecifiedFile :: String -> IO (Path, Path)
createSpecifiedFile name = do
  baseDir <- createTestDir ".test"
  let baseDirPath = relativePath baseDir
  let path = addTrailingPathSeparator baseDirPath ++ name
  writeFile path ""
  filePath <- Path name path <$> absolutize path
  return (baseDir, filePath)

spec :: FilePath -> Spec
spec trashPath = describe "run" $ do
  it "shows usage with no options" $ do
    pr <- withArgs [] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` BS8.unlines
      ["usage: macrm [-f | -i] [-dPRrvW] file ...", "       unlink file"]
    prExitCode pr `shouldBe` ExitFailure 1
    (isNothing . prException) pr `shouldBe` True
  it "shows version with `--version' option" $ do
    (_, Just hout, _, _) <- createProcess (proc "git" ["rev-parse", "HEAD"])
      { std_out = CreatePipe
      }
    output <- hGetContents hout
    pr     <- withArgs ["--version"] $ captureProcessResult run
    prStdout pr `shouldBe` BS8.pack
      (  "macrm ver "
      ++ showVersion version
      ++ " based on Git commit "
      ++ (head . lines) output
      ++ " Clean\n"
      )
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    pr' <- withArgs ["-V"] $ captureProcessResult run
    pr `shouldBe` pr'
  it "shows numeric version with `--numeric-version' option" $ do
    pr <- withArgs ["--numeric-version"] $ captureProcessResult run
    prStdout pr `shouldBe` BS8.pack (showVersion version ++ "\n")
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
  it "shows help with `--help' option" $ do
    let
      help = BS8.unlines
        [ "macrm [OPTIONS] [FILES/DIRS]"
        , ""
        , "Common flags:"
        , "  -d --directory        Attempt to remove directories as well as other types"
        , "                        of files."
        , "  -f --force            Attempt to remove the files without prompting for"
        , "                        confirmation, regardless of the file's permissions. If"
        , "                        the file does not exist, do not display a diagnostic"
        , "                        message or modify the exit status to reflect an error."
        , "                        The -f option overrides any previous -i options."
        , "  -i --interactive      Request confirmation before attempting to remove each"
        , "                        file, regardless of the file's permissions, or whether"
        , "                        or not the standard input device is a terminal. The -i"
        , "                        option overrides any previous -f options."
        , "  -P --plaster          Overwrite regular files before deleting them. Files"
        , "                        are overwritten three times, first with the byte"
        , "                        pattern 0xff, then 0x00, and then 0xff again, before"
        , "                        they are deleted. This flag is ignored under macrm."
        , "  -R --recursive        Attempt to remove the file hierarchy rooted in each"
        , "                        file argument. The -R option implies the -d option. If"
        , "                        the -i option is specified, the user is prompted for"
        , "                        confirmation before each directory's contents are"
        , "                        processed (as well as before the attempt is made to"
        , "                        remove the directory). If the user does not respond"
        , "                        affirmatively, the file hierarchy rooted in that"
        , "                        directory is skipped."
        , "  -r                    Equivalent to -R."
        , "  -v --verbose          Be verbose when deleting files, showing them as they"
        , "                        are removed."
        , "  -W --whiteouts        Attempt to undelete the named files. Currently, this"
        , "                        option can only be used to recover files covered by"
        , "                        whiteouts. This flag is ignored under macrm."
        , "  -? --help             Display help message"
        , "  -V --version          Print version information"
        , "     --numeric-version  Print just the version number"
        ]
    pr <- withArgs ["--help"] $ captureProcessResult run
    prStdout pr `shouldSatisfy` (help `BS8.isSuffixOf`)
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    pr' <- withArgs ["-?"] $ captureProcessResult run
    pr `shouldBe` pr'
  it "will get an error if it tries to remove a file that does not exist" $ do
    testFiles <- createTestFiles
    let filePath = (relativePath . head . notExistPaths) testFiles
    pr <- withArgs [filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` BS8.pack
      ("macrm: " ++ filePath ++ ": No such file or directory\n")
    prExitCode pr `shouldBe` ExitFailure 1
    (isNothing . prException) pr `shouldBe` True
    removeTestFiles testFiles
  it "removes a normal file" $ do
    testFiles <- createTestFiles
    let path     = head . normalFiles $ testFiles
    let fileName = fileOrDirName path
    let filePath = relativePath path
    pr <- withArgs [filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath = addTrailingPathSeparator trashPath ++ fileName
    doesPathExist removedFilePath `shouldReturn` True
    doesPathExist filePath `shouldReturn` False
    removeFile removedFilePath
    removeTestFiles testFiles
  it "removes normal files" $ do
    testFiles <- createTestFiles
    let paths     = normalFiles testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <- withArgs filePaths $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    mapM_ (\path -> doesPathExist path `shouldReturn` True) removedFilePaths
    mapM_ (\path -> doesPathExist path `shouldReturn` False) filePaths
    mapM_ removeFile removedFilePaths
    removeTestFiles testFiles
  it "removes same name files" $ do
    testFiles <- createTestFiles
    let basePath     = head . normalFiles $ testFiles
    let filePath0    = relativePath basePath
    let baseDir      = relativePath . head . notEmptyDirs $ testFiles
    let sameFileName = fileOrDirName basePath
    let filePath1    = addTrailingPathSeparator baseDir ++ sameFileName
    renameFile (relativePath . last . normalFiles $ testFiles) filePath1
    pr <- withArgs [filePath0, filePath1] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath0 = addTrailingPathSeparator trashPath ++ sameFileName
    removed <- listDirectory trashPath
    let filtered = filter (startswith $ sameFileName ++ " ") removed
    length filtered `shouldBe` 1
    doesPathExist removedFilePath0 `shouldReturn` True
    doesPathExist filePath0 `shouldReturn` False
    doesPathExist filePath1 `shouldReturn` False
    removeFile removedFilePath0
    removeFile . (addTrailingPathSeparator trashPath ++) . head $ filtered
    removeTestFiles testFiles
  it "removes same name special files" $ do
    testFiles <- createTestFiles
    let path      = head . symbolicLinkFiles $ testFiles
    let fileName  = fileOrDirName path
    let filePath0 = relativePath path
    let baseDir   = relativePath . head . notEmptyDirs $ testFiles
    let filePath1 = addTrailingPathSeparator baseDir ++ fileName
    renameFile (relativePath . last . symbolicLinkFiles $ testFiles) filePath1
    pr <- withArgs [filePath0, filePath1] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath0 = addTrailingPathSeparator trashPath ++ fileName
    removed <- listDirectory trashPath
    let filtered = filter (startswith $ fileName ++ " ") removed
    length filtered `shouldBe` 1
    (fileName `elem` removed) `shouldBe` True
    doesPathExist filePath0 `shouldReturn` False
    doesPathExist filePath1 `shouldReturn` False
    removeFile removedFilePath0
    removeFile . (addTrailingPathSeparator trashPath ++) . head $ filtered
    removeTestFiles testFiles
  it "removes same name directries" $ do
    testFiles <- createTestFiles
    let path     = head . emptyDirs $ testFiles
    let dirName  = fileOrDirName path
    let dirPath0 = relativePath path
    let baseDir  = relativePath . head . notEmptyDirs $ testFiles
    let dirPath1 = addTrailingPathSeparator baseDir ++ dirName
    createDirectory dirPath1
    pr <- withArgs ["-d", dirPath0, dirPath1] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath0 = addTrailingPathSeparator trashPath ++ dirName
    removed <- listDirectory trashPath
    let filtered = filter (startswith $ dirName ++ " ") removed
    length filtered `shouldBe` 1
    doesPathExist removedDirPath0 `shouldReturn` True
    doesPathExist dirPath0 `shouldReturn` False
    doesPathExist dirPath1 `shouldReturn` False
    removeDirectoryRecursive removedDirPath0
    removeDirectoryRecursive
      . (addTrailingPathSeparator trashPath ++)
      . head
      $ filtered
    removeTestFiles testFiles
  it "fails to remove directory" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs [dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr
      `shouldBe` BS8.pack ("macrm: " ++ dirPath ++ ": is a directory\n")
    prExitCode pr `shouldBe` ExitFailure 1
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` False
    doesPathExist dirPath `shouldReturn` True
    removeTestFiles testFiles
  it "removes empty directory with `--directory' option" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["--directory", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes empty directory with `-d' option" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["-d", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "fails to remove non empty directory with `--directory' option" $ do
    testFiles <- createTestFiles
    let path    = head . notEmptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["--directory", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr
      `shouldBe` BS8.pack ("macrm: " ++ dirPath ++ ": Directory not empty\n")
    prExitCode pr `shouldBe` ExitFailure 1
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` False
    doesPathExist dirPath `shouldReturn` True
    removeTestFiles testFiles
  it "fails to remove non empty directory with `-d' option" $ do
    testFiles <- createTestFiles
    let path    = head . notEmptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["-d", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr
      `shouldBe` BS8.pack ("macrm: " ++ dirPath ++ ": Directory not empty\n")
    prExitCode pr `shouldBe` ExitFailure 1
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` False
    doesPathExist dirPath `shouldReturn` True
    removeTestFiles testFiles
  it "does nothing when specified non-existance file with `--force' option" $ do
    testFiles <- createTestFiles
    let path     = head . notExistPaths $ testFiles
    let filePath = relativePath path
    pr <- withArgs ["--force", filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    removeTestFiles testFiles
  it "does nothing when specified non-existance file with `-f' option" $ do
    testFiles <- createTestFiles
    let path     = head . notExistPaths $ testFiles
    let filePath = relativePath path
    pr <- withArgs ["-f", filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    removeTestFiles testFiles
  it "removes or not with `--interactive' option" $ do
    testFiles <- createTestFiles
    let paths     = normalFiles testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <-
      withStdin "Y\nn\nyes\n"
      . withArgs ("--interactive" : filePaths)
      $ captureProcessResult run
    let prompt =
          foldl (\acc cur -> acc ++ "remove " ++ cur ++ "? ") "" filePaths
    prStdout pr `shouldBe` BS8.pack prompt
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    doesPathExist (head removedFilePaths) `shouldReturn` True
    doesPathExist (removedFilePaths !! 1) `shouldReturn` False
    doesPathExist (last removedFilePaths) `shouldReturn` True
    doesPathExist (head filePaths) `shouldReturn` False
    doesPathExist (filePaths !! 1) `shouldReturn` True
    doesPathExist (last filePaths) `shouldReturn` False
    removeFile $ head removedFilePaths
    removeFile $ last removedFilePaths
    removeTestFiles testFiles
  it "removes or not with `-i' option" $ do
    testFiles <- createTestFiles
    let paths     = normalFiles testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <-
      withStdin "Y\nn\nyes\n"
      . withArgs ("-i" : filePaths)
      $ captureProcessResult run
    let prompt =
          foldl (\acc cur -> acc ++ "remove " ++ cur ++ "? ") "" filePaths
    prStdout pr `shouldBe` BS8.pack prompt
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    doesPathExist (head removedFilePaths) `shouldReturn` True
    doesPathExist (removedFilePaths !! 1) `shouldReturn` False
    doesPathExist (last removedFilePaths) `shouldReturn` True
    doesPathExist (head filePaths) `shouldReturn` False
    doesPathExist (filePaths !! 1) `shouldReturn` True
    doesPathExist (last filePaths) `shouldReturn` False
    removeFile $ head removedFilePaths
    removeFile $ last removedFilePaths
    removeTestFiles testFiles
  it "ignores `--plaster' option" $ do
    testFiles <- createTestFiles
    let path     = head . normalFiles $ testFiles
    let fileName = fileOrDirName path
    let filePath = relativePath path
    pr <- withArgs ["--plaster", filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath = addTrailingPathSeparator trashPath ++ fileName
    doesPathExist removedFilePath `shouldReturn` True
    doesPathExist filePath `shouldReturn` False
    removeFile removedFilePath
    removeTestFiles testFiles
  it "ignores `-P' option" $ do
    testFiles <- createTestFiles
    let path     = head . normalFiles $ testFiles
    let fileName = fileOrDirName path
    let filePath = relativePath path
    pr <- withArgs ["-P", filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath = addTrailingPathSeparator trashPath ++ fileName
    doesPathExist removedFilePath `shouldReturn` True
    doesPathExist filePath `shouldReturn` False
    removeFile removedFilePath
    removeTestFiles testFiles
  it "removes non empty directory with `--recursive' option" $ do
    testFiles <- createTestFiles
    let path    = head . notEmptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["--recursive", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes non empty directory with `-R' option" $ do
    testFiles <- createTestFiles
    let path    = head . notEmptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["-R", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes non empty directory with `-r' option" $ do
    testFiles <- createTestFiles
    let path    = head . notEmptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withArgs ["-r", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes specified files and shows verbose with `--verbose' option" $ do
    testFiles <- createTestFiles
    let paths     = normalFiles testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <- withArgs ("--verbose" : filePaths) $ captureProcessResult run
    prStdout pr `shouldBe` (BS8.pack . unlines) filePaths
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    mapM_ (\path -> doesPathExist path `shouldReturn` True) removedFilePaths
    mapM_ (\path -> doesPathExist path `shouldReturn` False) filePaths
    mapM_ removeFile removedFilePaths
    removeTestFiles testFiles
  it "removes specified files and shows verbose with `-v' option" $ do
    testFiles <- createTestFiles
    let paths     = normalFiles testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <- withArgs ("-v" : filePaths) $ captureProcessResult run
    prStdout pr `shouldBe` (BS8.pack . unlines) filePaths
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    mapM_ (\path -> doesPathExist path `shouldReturn` True) removedFilePaths
    mapM_ (\path -> doesPathExist path `shouldReturn` False) filePaths
    mapM_ removeFile removedFilePaths
    removeTestFiles testFiles
  it "ignores `--whiteouts' option" $ do
    testFiles <- createTestFiles
    let path     = head . normalFiles $ testFiles
    let fileName = fileOrDirName path
    let filePath = relativePath path
    pr <- withArgs ["--whiteouts", filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath = addTrailingPathSeparator trashPath ++ fileName
    doesPathExist removedFilePath `shouldReturn` True
    doesPathExist filePath `shouldReturn` False
    removeFile removedFilePath
    removeTestFiles testFiles
  it "ignores `-W' option" $ do
    testFiles <- createTestFiles
    let path     = head . normalFiles $ testFiles
    let fileName = fileOrDirName path
    let filePath = relativePath path
    pr <- withArgs ["-W", filePath] $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedFilePath = addTrailingPathSeparator trashPath ++ fileName
    doesPathExist removedFilePath `shouldReturn` True
    doesPathExist filePath `shouldReturn` False
    removeFile removedFilePath
    removeTestFiles testFiles
  it "removes symbolic files" $ do
    testFiles <- createTestFiles
    let paths     = symbolicLinkFiles testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <- withArgs filePaths $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    removed <- listDirectory trashPath
    mapM_ (\fileName -> removed `shouldContain` [fileName])  fileNames -- cannot use doesPathExist for simbolic link
    mapM_ (\path -> doesPathExist path `shouldReturn` False) filePaths
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    mapM_ removeFile removedFilePaths
    removeTestFiles testFiles
  it "removes dead link files" $ do
    testFiles <- createTestFiles
    let paths     = deadSymbolicLinks testFiles
    let fileNames = map fileOrDirName paths
    let filePaths = map relativePath paths
    pr <- withArgs filePaths $ captureProcessResult run
    prStdout pr `shouldBe` ""
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    removed <- listDirectory trashPath
    mapM_ (\fileName -> removed `shouldContain` [fileName])  fileNames -- cannot use doesPathExist for simbolic link
    mapM_ (\path -> doesPathExist path `shouldReturn` False) filePaths
    let removedFilePaths =
          map (addTrailingPathSeparator trashPath ++) fileNames
    mapM_ removeFile removedFilePaths
    removeTestFiles testFiles
  it "removes an empty directory with `--interactive --directory' options" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <-
      withStdin "y\n"
      $ withArgs ["--interactive", "--directory", dirPath]
      $ captureProcessResult run
    prStdout pr `shouldBe` BS8.pack ("remove " ++ dirPath ++ "? ")
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes empty directory with `-id' options" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withStdin "y\n" $ withArgs ["-id", dirPath] $ captureProcessResult run
    prStdout pr `shouldBe` BS8.pack ("remove " ++ dirPath ++ "? ")
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes empty directory with `--interactive --recursive' options" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <-
      withStdin "y\n"
      $ withArgs ["--interactive", "--recursive", dirPath]
      $ captureProcessResult run
    prStdout pr
      `shouldBe` BS8.pack ("examine files in directory " ++ dirPath ++ "? ")
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes empty directory with `-iR' options" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withStdin "y\n" $ withArgs ["-iR", dirPath] $ captureProcessResult run
    prStdout pr
      `shouldBe` BS8.pack ("examine files in directory " ++ dirPath ++ "? ")
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "removes empty directory with `-ir' options" $ do
    testFiles <- createTestFiles
    let path    = head . emptyDirs $ testFiles
    let dirName = fileOrDirName path
    let dirPath = relativePath path
    pr <- withStdin "y\n" $ withArgs ["-ir", dirPath] $ captureProcessResult run
    prStdout pr
      `shouldBe` BS8.pack ("examine files in directory " ++ dirPath ++ "? ")
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True
    let removedDirPath = addTrailingPathSeparator trashPath ++ dirName
    doesPathExist removedDirPath `shouldReturn` True
    doesPathExist dirPath `shouldReturn` False
    removeDirectoryRecursive removedDirPath
    removeTestFiles testFiles
  it "tries to remove /bin/rm" $ do
    pr <- withStdin "n\n" $ withArgs ["/bin/rm"] $ captureProcessResult run
    prStdout pr `shouldBe` BS8.pack
      "override rwxr-xr-x  root/wheel restricted,compressed for /bin/rm? "
    prStderr pr `shouldBe` ""
    prExitCode pr `shouldBe` ExitSuccess
    (isNothing . prException) pr `shouldBe` True

bugs :: FilePath -> Spec
bugs trashPath = describe "bugs" $ do
  it "remove '\"' file" $ do
    (baseDir, path) <- createSpecifiedFile "\""
    pr <- withArgs [relativePath path] $ captureProcessResult run
    prExitCode pr `shouldBe` ExitSuccess
    let removedFilePath = addTrailingPathSeparator trashPath ++ "\""
    doesPathExist removedFilePath `shouldReturn` True
    removeDirectoryRecursive $ relativePath baseDir

main :: IO ()
main = do
  homePath <- getEnv "HOME"
  let trashPath = homePath ++ "/.Trash"
  hspec $ spec trashPath
  hspec $ bugs trashPath
