# Map a vector's values using a lookup table

Map a vector's values using a lookup table

## Usage

``` r
map_values(input_vector, lut_df, key_col, value_col, keep_original = TRUE)
```

## Arguments

- input_vector:

  The vector of original values

- lut_df:

  The lookup data frame

- key_col:

  The unquoted column name in `lut_df` to match against.

- value_col:

  The unquoted column name in `lut_df` to get the new values from.

- keep_original:

  Keep original value if no match is found. Default is TRUE.

## Value

A new vector with the mapped values, in the same order as the input.
