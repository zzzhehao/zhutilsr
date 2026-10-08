# Alternating Brightness Adjustment for Color Palettes

Takes a vector of hex colors and artificially forces contrast between
neighboring colors by making odd-indexed colors brighter and
even-indexed colors darker.

## Usage

``` r
.adjust_palette_brightness(hex_colors, factor = 0.2)
```

## Arguments

- hex_colors:

  Character vector of hex color codes.

- factor:

  Numeric adjustment factor (0 to 1, e.g., 0.2 for a 20% shift).

## Value

Character vector of updated hex codes.
