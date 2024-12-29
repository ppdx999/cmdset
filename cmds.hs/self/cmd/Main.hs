module Main where

import Shared (showMsgs)
import System.Environment (getArgs)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B

usage :: IO ()
usage = showMsgs
  [ "Usage: self <f1> <f2> ... "
  , "Version: 0.1.0"
  , "Description: Extract fields from stdin and print them to stdout."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    xs -> do
      let fields = parseFields xs
      case fields of
        Left err -> putStrLn err
        Right fs -> B.interact (mainProc fs)

data Field = Num Int
           | Range Int Int
           | All

mainProc :: [Field] -> B.ByteString -> B.ByteString
mainProc fs = B.unlines . map (procLine fs) . B.lines

procLine :: [Field] -> B.ByteString -> B.ByteString
procLine fs = joinFields . filterFields fs . toFields

readInt :: T.Text -> Either String Int
readInt = go . T.unpack
  where
    go :: String -> Either String Int
    go x
      | all isDigit x = Right $ read x
      | otherwise = Left $ "Not a number: " ++ x

parseFields :: [String] -> Either String [Field]
parseFields = mapM (go . T.pack)
  where
    go :: T.Text -> Either String Field
    go x
      | x == T.pack "0" = Right All
      | T.isInfixOf (T.pack "/") x = do
          case T.splitOn (T.pack "/") x of
            [a, b] -> do
              a' <- readInt a
              b' <- readInt b
              return $ Range a' b'
            _ -> Left $ "Invalid range: " ++ T.unpack x
      | otherwise = do
          n <- readInt x
          return $ Num n

toFields :: B.ByteString -> [B.ByteString]
toFields line = filter (/= blank) $ B.split ' ' line
              where blank = B.pack ""

filterFields :: [Field] -> [B.ByteString] -> [B.ByteString]
filterFields fields xs = map go fields
  where go (Num n) = xs !! (n - 1)
        go All = joinFields xs
        go (Range a b) =
          joinFields
          $ if b < a
            then reverse $ goRange b a xs
            else goRange a b xs

        goRange :: Int -> Int -> [B.ByteString] -> [B.ByteString]
        goRange a b = take (b - a + 1) . drop (a - 1)

joinFields :: [B.ByteString] -> B.ByteString
joinFields = B.intercalate (B.pack " ")
