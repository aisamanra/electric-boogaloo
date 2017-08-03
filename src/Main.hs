{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (when)
import           Data.Char (isSpace, isAlphaNum)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Console.GetOpt as Opt
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys

data Decl = Decl
  { dName   :: Text
  , dFields :: [Field]
  } deriving (Eq, Show)

data Field = Field
  { fName    :: Text
  , fType    :: Text
  , fNull    :: Bool
  , fUniq    :: Bool
  , fDefault :: Maybe Text
  } deriving (Eq, Show)

uncomment :: Text -> Text
uncomment = fst . T.breakOn "--"

isValid :: Text -> Bool
isValid = T.all (\ x -> isAlphaNum x || x == '_')

isSpecial :: Char -> Bool
isSpecial c = c == '?' || c == '!'

parseField :: Text -> Field
parseField ln =
  let (name, tt) = T.break (== ':') (T.strip ln)
      (typ, def) = T.break (== '=') (T.strip tt)
  in Field
       { fName = T.strip name
       , fType = T.strip (T.drop 1 (T.filter (not . isSpecial) typ))
       , fNull = T.any (== '?') typ
       , fUniq = T.any (== '!') typ
       , fDefault =
           case def of
             "" -> Nothing
             rs -> Just (T.strip (T.drop 1 rs))
       }

parse :: Text -> Either String [Decl]
parse = go [] . map uncomment . T.lines
  where go ds [] = return ds
        go ds (l:ls)
          | T.length (T.takeWhile isSpace l) > 0 =
            let f = parseField l
            in case ds of
                 (d:ds') -> go (d { dFields = f : dFields d } : ds') ls
                 []      -> Left "indented line outside of table decl"
          | T.all isSpace l =
            go ds ls
          | otherwise =
            let d = Decl { dName   = T.strip l
                         , dFields = []
                         }
            in go (d:ds) ls

builtins :: [Text]
builtins = [ "null"
           , "int"
           , "integer"
           , "real"
           , "text"
           , "bool"
           , "blob"
           , "date"
           ]

check :: [Decl] -> Either String ()
check ds =
  let tables = map dName ds
      types  = builtins ++ tables
      chk Field { fName = f, fType = t }
       | not (isValid f) =
         Left $ "Invalid field name: '" ++ T.unpack f ++ "'"
       | f == "id" = Left "Field 'id' conflicts with built-in id"
       | t `elem` types = return ()
       | otherwise = Left ("Unknown type: " ++ T.unpack t)
  in do
    mapM_ (\ Decl { dName = n } ->
             if not (isValid n)
               then Left $ "Invalid table name: '" ++ T.unpack n ++ "'"
               else return ()
          ) ds
    mapM_ (mapM_ chk . dFields) ds

typeName :: Text -> Text
typeName t
  | t `elem` builtins = T.toUpper t
  | otherwise = "INTEGER"

pprint :: Decl -> Sys.Handle -> IO ()
pprint Decl { dName = n, dFields = fs } h = do
  T.hPutStr h "CREATE TABLE IF NOT EXISTS "
  T.hPutStrLn h n
  T.hPutStrLn h "  ( id INTEGER PRIMARY KEY ASC"
  mapM_ printField fs
  mapM_ printForeign fs
  T.hPutStrLn h "  );"
  where printField Field { fName = f
                         , fType = t
                         , fNull = l
                         , fUniq = u
                         , fDefault = d
                         } = do
              T.hPutStr h "  , "
              T.hPutStr h f
              T.hPutStr h " "
              T.hPutStr h (typeName t)
              when (not l) $
                T.hPutStr h " NOT NULL"
              when u $
                T.hPutStr h " UNIQUE"
              case d of
                Nothing -> return ()
                Just dv -> T.hPutStr h (" DEFAULT " <> dv)
              T.hPutStrLn h ""
        printForeign Field { fName = f, fType = t }
          | t `elem` builtins = return ()
          | otherwise = do
              T.hPutStr h "  , FOREIGN KEY("
              T.hPutStr h f
              T.hPutStr h ") REFERENCES "
              T.hPutStr h t
              T.hPutStrLn h "(id)"

rev :: [Decl] -> [Decl]
rev ds = reverse [ d { dFields = reverse (dFields d) } | d <- ds ]

-- *

data Options = Options
  { inputFile  :: Maybe FilePath
  , outputFile :: Maybe FilePath
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ let inFile (Just f) o
          | f /= "-" = o { inputFile = Just f }
        inFile _ o   = o { inputFile = Nothing }
    in Opt.Option ['i'] ["input"] (Opt.OptArg inFile "file")
         "input Electric Boogaloo schema"
  , let outFile (Just f) o
          | f /= "-" = o { outputFile = Just f }
        outFile _ o  = o { outputFile = Nothing }
    in Opt.Option ['o'] ["output"] (Opt.OptArg outFile "file")
         "output SQLite schema"
  ]

guard :: Either String a -> IO a
guard (Left err) = do
  Sys.hPutStrLn Sys.stderr err
  Sys.exitFailure
guard (Right x) = return x

header :: String
header = "Electric Boogaloo"

main :: IO ()
main = do
  args <- Sys.getArgs
  opts <- guard $ case Opt.getOpt Opt.Permute options args of
                    (optsApp, [], []) -> return optsApp
                    _ -> Left (Opt.usageInfo header options)
  let o = foldl (.) id opts (Options Nothing Nothing)
  cs <- case inputFile o of
    Nothing -> T.getContents
    Just f  -> T.readFile f
  decls <- guard (parse cs)
  ()    <- guard (check decls)
  outH  <- case outputFile o of
    Nothing -> return Sys.stdout
    Just f  -> Sys.openFile f Sys.ReadMode
  mapM_ (flip pprint outH) (rev decls)
