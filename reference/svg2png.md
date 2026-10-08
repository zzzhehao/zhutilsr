# Convert a single SVG file to PNG(s) with varying size, color, and opacity

Convert a single SVG file to PNG(s) with varying size, color, and
opacity

## Usage

``` r
svg2png(input, output_dir, size = 256, alpha = 1, hex = "#000000")
```

## Arguments

- input:

  Path to the input SVG file.

- output_dir:

  Path to the output directory.

- size:

  Numeric vector of target sizes in pixels.

- alpha:

  Numeric vector of opacity values between 0 and 1.

- hex:

  Character/Numeric vector of colors (e.g., "#000000", "red").
