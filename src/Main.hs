{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (when)
import           Data.Char (isSpace, isAlphaNum)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
builtins = [ "null", "int", "integer", "real", "text", "blob", "date" ]

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

pprint :: Decl -> IO ()
pprint Decl { dName = n, dFields = fs } = do
  T.putStr "CREATE TABLE IF NOT EXISTS "
  T.putStrLn n
  T.putStrLn "  ( id INTEGER PRIMARY KEY ASC"
  mapM_ printField fs
  mapM_ printForeign fs
  T.putStrLn "  );"
  where printField Field { fName = f
                         , fType = t
                         , fNull = l
                         , fUniq = u
                         , fDefault = d
                         } = do
              T.putStr "  , "
              T.putStr f
              T.putStr " "
              T.putStr (typeName t)
              when (not l) $
                T.putStr " NOT NULL"
              when u $
                T.putStr " UNIQUE"
              case d of
                Nothing -> return ()
                Just dv -> T.putStr (" DEFAULT " <> dv)
              T.putStrLn ""
        printForeign Field { fName = f, fType = t }
          | t `elem` builtins = return ()
          | otherwise = do
              T.putStr "  , FOREIGN KEY("
              T.putStr f
              T.putStr ") REFERENCES "
              T.putStr t
              T.putStrLn "(id)"

rev :: [Decl] -> [Decl]
rev ds = reverse [ d { dFields = reverse (dFields d) } | d <- ds ]

main :: IO ()
main = do
  cs <- T.getContents
  case parse cs of
    Left err -> putStrLn err
    Right ds -> do
      case check ds of
        Left err -> putStrLn err
        Right () -> mapM_ pprint (rev ds)
