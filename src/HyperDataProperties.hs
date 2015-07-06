module HyperDataProperties where

data HyperDataProperties = 
  HyperDataProperties { 
    description :: Maybe [String]
    , nsamples :: Maybe Int
    , nlines :: Maybe Int
    , nbands :: Maybe Int
    , headerOffset :: Maybe Int
    , fileType :: Maybe String
    , dataType :: Maybe String
    , interleave :: Maybe String
    , sensorType :: Maybe String
    , byteOrder :: Maybe Int
    , reflectanceScaleFactor :: Maybe Double
    , mapInfo :: Maybe [String]
    , wavelengthUnits :: Maybe String
    , bandNames :: Maybe [String]
    , wavelengths :: Maybe [Double]
    , fwhm :: Maybe [Double]
    } deriving (Eq, Show)

