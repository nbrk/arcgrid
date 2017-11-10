## Description
This is a parser for ESRI/ArcInfo (ArcGrid) files. These are raster GIS files
widely used by many geographics-related software to represent elevations or
terrain features.

Only ASCII GRID (.asc) files are supported at the moment.

In `app/valley.asc` there is a test elevation model of a small valley area. It
is parsed and printed by the `arcgrid-exe` if you do `stack exec arcgrid-exe`.

## Usage
The library exports the `ArcGrid` datatype and `arcGridFromFile`:

``` haskell
arcGridFromFile :: String -> IO ArcGrid
```

There is also the bytestring parser to be used with parsec:

``` haskell
asciiGridParser :: Parser ArcGrid
```

## TODO:
- unparsing
- optimizations
