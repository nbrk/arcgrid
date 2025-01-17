## Description
This is a parser for ESRI/ArcInfo (ArcGrid) files. These are raster GIS files
widely used by many geographics-related software to represent elevations or
terrain features.

Only ASCII GRID (.asc) files are supported at the moment.

In `app/valley.asc` there is a test elevation model of a small valley area. It
is parsed and printed by the `arcgrid-exe` if you do `stack exec arcgrid-exe`.

## Usage
The library exports the `ArcGrid` datatype and its accessors:

``` haskell
data ArcGrid = ArcGrid
               { ncols :: Int
               , nrows :: Int
               , xllcorner :: Maybe Float
               , yllcorner :: Maybe Float
               , xllcenter :: Maybe Float
               , yllcenter :: Maybe Float
               , cellsize :: Float
               , nodata_value :: Maybe Int
               , vat :: [Int]
               }
```

The main interface for reading of grid files is `arcGridFromFile`, but the
bytestring parser (to be used with Parsec) is also exported.

``` haskell
arcGridFromFile :: String -> IO ArcGrid

asciiGridParser :: Parser ArcGrid
```

## TODO:
- unparsing
- optimizations
- support of proprietary binary formats?
