# Check overwrite policy

Check overwrite policy

## Usage

``` r
.overwrite_check(
  ...,
  dir = NULL,
  file = NULL,
  overwrite = FALSE,
  suffix = FALSE,
  interactive = FALSE
)
```

## Arguments

- ...:

  This is a placeholder, it enforces whoever use this function to
  consciously specify their input correctly as a directory or file.

- dir:

  Path to directory (can be a vector)

- file:

  Path to file (can be a vector)

- overwrite:

  Logical or vector. Whether to overwrite the target.

- suffix:

  Logical or vector. Whether to use unique suffix for output. If
  `overwrite` is `TRUE`, this value is always treated as `FALSE`.

- interactive:

  Logical or vector. Whether to start an interactive instance in console
  to let user decide overwrite option. If `overwrite` is `TRUE`, this
  value is always treated as `FALSE`.

## Value

A vector of binary encoded check results. See details in attributes.
