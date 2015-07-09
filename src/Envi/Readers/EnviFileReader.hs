module Envi.Readers.EnviFileReader where

import Control.Applicative ( (<$>) )
import Data.List ( intersperse )
import Data.Array.Repa.Repr.ByteString
import Data.Array.Repa as R
import Data.Char ( toUpper )
import Data.Maybe ( fromMaybe )
import System.FilePath.Windows ( replaceExtension )
import Text.Parsec as P
import Text.Parsec.String ( parseFromFile )
import qualified Data.ByteString as B
import qualified Data.Map as Map

import HyperData
import HyperDataProperties

type Properties = Map.Map String [String]

readHyperData :: FilePath -> IO HyperData
readHyperData path = do
  let headerFilePath = replaceExtension path ".hdr" 
  (Right headerContents) <- readHeaderFile headerFilePath 
  let properties = makeHyperDataProperties headerContents
  let numLines = fromMaybe 0 $ nlines properties
  let numSamples = fromMaybe 0 $ nsamples properties
  let numBands = fromMaybe 0 $ nbands properties
  let scale = fromMaybe 1 $ reflectanceScaleFactor properties
  let interleave' = fromMaybe "BIP" $ interleave properties
  let shape = shapeOfInterleave interleave' numSamples numLines numBands
  rawFile <- fromByteString shape <$> B.readFile path
  let rawData = R.map (/scale) $ R.map fromIntegral rawFile :: Array D DIM3 Double  
  let permutedData = permuteOnInterleave rawData interleave'
  reshapedData <- computeP $ reshape (Z:. numBands :. (numLines * numSamples) :: DIM2) permutedData
  return $ HyperCube reshapedData properties -- TODO: return cube or lib depending on file type

shapeOfInterleave :: String -> Int -> Int -> Int -> DIM3
shapeOfInterleave i s l b
  | Prelude.map toUpper i == "BIL" = (Z :. l :. b :. s :: DIM3)  
  | Prelude.map toUpper i == "BSQ" = (Z :. b :. l :. s :: DIM3)
  | otherwise = (Z :. s :. l :. b :: DIM3)

permuteOnInterleave :: Array D DIM3 e -> String -> Array D DIM3 e
permuteOnInterleave cube i
  | Prelude.map toUpper i == "BIL" = bilToBip cube
  | Prelude.map toUpper i == "BSQ" = bsqToBip cube
  | otherwise = cube

bilToBip :: (Source r e) => Array r DIM3 e -> Array D DIM3 e
bilToBip cube = backpermute e flop cube
  where
    e@(Z :. x :. y :. z) = extent cube
    flop (Z :. x :. y :. z) = (Z:. x :. z :. y)

bsqToBip :: (Source r e) => Array r DIM3 e -> Array D DIM3 e
bsqToBip cube = backpermute e flop cube
  where
    e@(Z :. x :. y :. z) = extent cube
    flop (Z :. x :. y :. z) = (Z:. z :. y :. x)

readHeaderFile :: FilePath -> IO (Either (Map.Map k a) Properties)
readHeaderFile path = do
  result <- parseFromFile enviParser path 
  return $ case result of
    Left _ -> Left Map.empty
    Right xs -> Right $ Map.fromList xs

enviParser :: Parsec String () [(String, [String])]
enviParser = do
  _ <- word 
  fields <- many $ do
    field <- ((try listFieldParser) <|> (try singleFieldParser))
    return field
  spaces
  return fields

singleFieldParser :: Parsec String () (String, [String])
singleFieldParser = do
  key <- keyParser
  _ <- char '='
  value <- singleValueParser
  return (key, [value])

listFieldParser :: Parsec String () (String,[String])
listFieldParser = do
  key <- keyParser
  _ <- char '='
  value <- listValueParser 
  return (key, value)

keyParser :: Parsec String () String
keyParser = spaces >> wordsSepBySpaces >>= return

singleValueParser :: Parsec String () String
singleValueParser = spaces >> ((many1 digit) <|> wordsSepBySpaces) >>= return

listValueParser :: Parsec String () [String]
listValueParser = between (spaces >> char '{' >> spaces) (char '}') $ val `sepBy1` valueSeparator >>= return 

fieldSeparator :: Parsec String () ()
fieldSeparator = spaces >> char '\n' >> spaces

valueSeparator :: Parsec String () () 
valueSeparator = spaces >> char ',' >> spaces 

wordsSepBySpaces :: Parsec String () String
wordsSepBySpaces = concat . intersperse " " <$> word `sepEndBy` (many $ oneOf " ")

word :: Parsec String () String
word = many1 letter

val :: Parsec String () String
val = many1 $ alphaNum <|> oneOf "[]():+-=._ \t\r\n" 

makeHyperDataProperties :: Properties -> HyperDataProperties
makeHyperDataProperties p = HyperDataProperties { description = Map.lookup "description" p
  , nsamples = read <$> concat <$> Map.lookup "samples" p
  , nlines = read <$> concat <$> Map.lookup  "lines" p
  , nbands = read <$> concat <$> Map.lookup "bands" p
  , headerOffset = read <$> concat <$> Map.lookup "header offset" p
  , fileType = concat <$> Map.lookup "file type" p
  , dataType = concat <$> Map.lookup "data type" p
  , interleave = concat <$> Map.lookup "interleave" p
  , sensorType = concat <$> Map.lookup "sensor type" p
  , byteOrder = read <$> concat <$> Map.lookup "byte order" p
  , reflectanceScaleFactor = read <$> concat <$> Map.lookup "reflectance scale factor" p
  , mapInfo = Map.lookup "map info" p
  , wavelengthUnits = concat <$> Map.lookup "wavelength units" p
  , bandNames = Map.lookup "band names" p
  , wavelengths = fmap read <$> Map.lookup "wavelength" p
  , fwhm = fmap read <$> Map.lookup "fwhm" p } 

