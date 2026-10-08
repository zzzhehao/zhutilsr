# Clean Stale Cache Files

Deletes cache files that have not been modified within the specified
period.

## Usage

``` r
.cache_clean(period = 7, pkg = utils::packageName())
```

## Arguments

- period:

  Numeric value indicating the maximum age of cache files in days. Files
  older than this will be deleted. Defaults to `7`.

- pkg:

  Package name for the cache directory. Defaults to the current package
  environment.

## Value

Invisibly returns the number of files deleted.
