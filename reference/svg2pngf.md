# Convert a folder of SVG files to PNGs with varying size, color, and opacity

Convert a folder of SVG files to PNGs with varying size, color, and
opacity

## Usage

``` r
svg2pngf(
  inputfolder,
  outputfolder,
  size = 256,
  alpha = 1,
  hex = "#000000",
  recursive = FALSE,
  regex = NULL
)
```

## Arguments

- inputfolder:

  Path to the input directory containing SVGs.

- outputfolder:

  Path to the output directory.

- size:

  Numeric vector of target sizes in pixels.

- alpha:

  Numeric vector of opacity values between 0 and 1.

- hex:

  Character/Numeric vector of colors (e.g., "#000000", "red").

- recursive:

  Logical. Should sub-directories be processed while retaining folder
  structure?

- regex:

  Character. Optional regex string to filter the target SVG files.
