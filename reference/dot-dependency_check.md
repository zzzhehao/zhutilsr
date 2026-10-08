# Check whether the required dependency is installed

Check whether the required dependency is installed

## Usage

``` r
.dependency_check(pkg, install.call = NULL)
```

## Arguments

- pkg:

  Character vector. Package names.

- install.call:

  A suggested function (or list of functions) for installing the package
  if missing. Default to `NULL`, no installation will be suggested.

## Value

A logical vector of the same length as `pkg` indicating if packages are
installed.
