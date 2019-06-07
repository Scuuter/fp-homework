{-# LANGUAGE RankNTypes #-}

module FileSystem (
    FS
  , scan
  , cd
  , ls
  , file
  , replaceExt
  , names
  , delIfEmpty
) where


import System.Directory (
    listDirectory
  , doesDirectoryExist
  , doesFileExist
  )

import System.FilePath (
    combine
  , dropTrailingPathSeparator
  , splitPath
  , takeFileName
  , (-<.>) -- replaceExtension
  )

import Lens.Micro (
    Lens'
  , Traversal'
  , lens
  , traversed
  , filtered
  , (%~) -- over
  , (.~) -- set
  , (^.)
  , (^..)
  , (&)
  )

data FS
    = Dir
    { _name     :: FilePath  -- название папки, не полный путь
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath  -- название файла, не полный путь
    } deriving (Show)

scan :: FilePath -> IO FS
scan path = do
  isFile' <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  if isFile' then pure $ File $ takeFileName path
  else if isDirectory then do
    list <- listDirectory path
    next <- sequenceA (scan . combine path <$> list)
    pure $ Dir (dropTrailingPathSeparator $ last $ splitPath path) next
  else ioError $ userError "Argument must be a valid filepath"

isFile :: FS -> Bool
isFile (File _) = True
isFile _        = False

isDir :: FS -> Bool
isDir = not . isFile

--basic lenses
name :: Lens' FS FilePath
name = lens _name (\fs v -> fs { _name = v })

contents :: Lens' FS [FS]
contents = lens _contents (\fs v -> fs { _contents = v })

-- task 3
cd :: FilePath -> Traversal' FS FS
cd dir = contents . traversed . filtered (\x -> isDir x && x ^. name == dir)

ls :: Traversal' FS FilePath
ls = contents . traversed . name

file :: FilePath -> Traversal' FS FilePath
file name' = contents . traversed . filtered (\x -> isFile x && x ^. name == name') . name

-- ghci tests
-- *Main FileSystem Lens.Micro> lol <- scan "hw5"
-- *Main FileSystem Lens.Micro> lol ^? cd "src" . file "BaseLenses.hs"
-- Just "BaseLenses.hs"
-- *Main FileSystem Lens.Micro> lol ^? cd "src" . file "thereisnosuchfile"
-- Nothing
-- *Main FileSystem Lens.Micro> lol ^.. ls
-- ["hw5.cabal","src","README.md","test","LICENSE",".stack-work","app"]
-- *Main FileSystem Lens.Micro> lol ^.. file "src"
-- []

replaceExt :: FS -> String -> FS
replaceExt fs new = fs & contents . traversed . filtered isFile . name %~ (-<.> new)

names :: FS -> [FilePath]
names fs = fs ^.. ls ++ concatMap (\x -> if isDir x then names x else []) (fs ^. contents)

delIfEmpty :: FS -> FilePath -> FS
delIfEmpty fs dir = fs & contents .~
  (fs ^.. contents . traversed . filtered
    (\x -> isDir x && x ^. name /= dir
        || isFile x
        || not (null (x ^. contents))
    )
  )
