# Turn Characters Vectors to be Safe

Turn Characters Vectors to be Safe

## Usage

``` r
.safe_name(c, replacement = "_", allow = "")
```

## Arguments

- c:

  Character vector. Input to be convert into safe characters.

- replacement:

  Character vector of length 1. Element to replace unsafe characters

- allow:

  Regex expression. To be added as `[^allow]`. Matching additional
  characters to be retained. Default to none, which only allows safest
  characters (alphabet, numbers, underscore).

## Value

Character vector of length `c`. Safe version of the input. Attribute
`lut` provides a look-up table of the conversion, which can be used to
map the original value to the safe values.
