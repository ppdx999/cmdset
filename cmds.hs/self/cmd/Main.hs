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

data SingleItem = SNumber Int | SNF | SAll
data RangeItem = RNumber Int | RNF
data Field = Single SingleItem | Range RangeItem RangeItem

mainProc :: [Field] -> B.ByteString -> B.ByteString
mainProc fs = B.unlines . map (procLine fs) . B.lines

procLine :: [Field] -> B.ByteString -> B.ByteString
procLine fs = joinFields . filterFields fs . toFields

readNumber :: T.Text -> Either String Int
readNumber = go . T.unpack
  where
    go :: String -> Either String Int
    go x
      | all isDigit x = Right $ read x
      | otherwise = Left $ "Not a number: " ++ x

isRange :: T.Text -> Bool
isRange = T.isInfixOf (T.pack "/")

parseFields :: [String] -> Either String [Field]
parseFields = mapM (go . T.pack)
  where
    go :: T.Text -> Either String Field
    go x
      | isRange x = case T.splitOn (T.pack "/") x of
            [a, b] -> do
                      a' <- parseRangeItem a
                      b' <- parseRangeItem b
                      return $ Range a' b'
            _      -> Left $ "Invalid range: " ++ T.unpack x
      | otherwise = do
          a <- parseSingleItem x
          return $ Single a

    parseSingleItem :: T.Text -> Either String SingleItem
    parseSingleItem x
      | x == T.pack "NF" = Right SNF
      | x == T.pack "0" = Right SAll
      | otherwise = SNumber <$> readNumber x

    parseRangeItem :: T.Text -> Either String RangeItem
    parseRangeItem x
      | x == T.pack "NF" = Right RNF
      | otherwise = RNumber <$> readNumber x

toFields :: B.ByteString -> [B.ByteString]
toFields line = filter (/= blank) $ B.split ' ' line
              where blank = B.pack ""

filterFields :: [Field] -> [B.ByteString] -> [B.ByteString]
filterFields fields xs = map go fields
    where
      go :: Field -> B.ByteString
      go (Single item) = goSingle item
      go (Range a b) = goRange a b xs

      goSingle :: SingleItem -> B.ByteString
      goSingle item = case item of
        SNumber n -> xs !! (n - 1)
        SNF -> last xs
        SAll -> joinFields xs

      goRange :: RangeItem -> RangeItem -> [B.ByteString] -> B.ByteString
      goRange a b xs' = joinFields $ case (a, b) of
        (RNumber a', RNumber b') -> if a' <= b' then goRNumber a' b' xs' else reverse $ goRNumber b' a' xs'
        (RNumber a', RNF) -> drop (a' - 1) xs'
        (RNF, RNumber b') -> reverse $ drop (b' - 1) xs'
        (RNF, RNF) -> take 1 $ reverse xs'

      goRNumber :: Int -> Int -> [B.ByteString] -> [B.ByteString]
      goRNumber a b = take (b - a + 1) . drop (a - 1)


joinFields :: [B.ByteString] -> B.ByteString
joinFields = B.intercalate (B.pack " ")
