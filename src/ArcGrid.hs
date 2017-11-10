-- | ESRI/ArcInfo ASCII Grid file format
module ArcGrid
  ( ArcGrid
  , arcGridFromFile
  , parse
  )
where

import qualified Data.Vector as V
import Data.Char
import Text.Parsec
import Text.Parsec.ByteString
import Text.ParserCombinators.Parsec.Numeric

{-
 - The ESRI ArcInfo ASCII Grid spec:
 - http://resources.esri.com/help/9.3/arcgisdesktop/com/gp_toolref/spatial_analyst_tools/esri_ascii_raster_format.htm
 -}


data ArcGrid = ArcGrid
               { ncols :: Int
               , nrows :: Int
               , xllcorner :: Maybe Float
               , yllcorner :: Maybe Float
               , xllcenter :: Maybe Float
               , yllcenter :: Maybe Float
               , cellsize :: Float
               , nodata_value :: Maybe Int
               , vat :: V.Vector (V.Vector Int) -- value attribute table
               } deriving (Show)



arcGridFromFile :: String -> IO ArcGrid
arcGridFromFile fname = do
  e <- parseFromFile parser fname
  case e of
    Left err -> error $ show err
    Right ag -> return ag


-- | Both upper- and lower-case strings
wordParser :: String -> Parser ()
wordParser str =
  let ucstr = map toUpper str
      lcstr = map toLower str
  in
    do
      string str <|> string ucstr <|> string lcstr
      return ()

-- | Spaces and tabs
pvDelimParser :: Parser ()
pvDelimParser = do
  many1 (char ' ' <|> char '\t')
  return ()


-- | Parameter-value line
pvLineParser :: String -> Parser a -> Parser a
pvLineParser param vparser = do
  wordParser param
  pvDelimParser
  val <- vparser
  endOfLine
  return val


-- | Parse (x|y)llcorner xor (x|y)llcenter block. Returns (corner, center)
cllParser :: Char -> Parser (Maybe Float, Maybe Float)
cllParser c =
  let cllcorner = c : "llcorner"
      cllcenter = c : "llcenter"
  in
    do
      -- try "?llcorner" first without consuming the output
      mbcor <- optionMaybe $ try $ pvLineParser cllcorner floating
      case mbcor of
        Just val -> return (Just val, Nothing)
        Nothing -> do
          -- read "?llcenter" then
          val <- pvLineParser cllcenter floating
          return (Nothing, Just val)


-- | Parse VAT row
vatLineParser :: Int -> Parser [Int]
vatLineParser ncols = do
  skipMany space
  as <- count ncols $ do
    a <- int
    space <|> newline
    return a

  return as


parser :: Parser ArcGrid
parser = do
  _ncols <- pvLineParser "ncols" decimal
  _nrows <- pvLineParser "nrows" decimal

  (_xllcorner, _xllcenter) <- cllParser 'x'
  (_yllcorner, _yllcenter) <- cllParser 'y'

  _cellsize <- pvLineParser "cellsize" floating
  _nodata_value <- optionMaybe $ pvLineParser "NODATA_value" int

  -- XXX
  rows <- count _nrows $ do
    row <- vatLineParser _ncols
    let rowv = V.fromList row
    return rowv
  let rowvv = V.fromList rows


  return $ ArcGrid
    { ncols = _ncols
    , nrows = _nrows
    , xllcorner = _xllcorner
    , yllcorner = _yllcorner
    , xllcenter = _xllcenter
    , yllcenter = _yllcenter
    , cellsize = _cellsize
    , nodata_value = _nodata_value
    , vat = rowvv
    }

