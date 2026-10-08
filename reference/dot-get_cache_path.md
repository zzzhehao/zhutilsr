# Get Cache File Path

Internal helper to generate a deterministic file path for cached
objects.

## Usage

``` r
.get_cache_path(name, identifier, pkg = utils::packageName())
```

## Arguments

- name:

  Character string for the object's base name.

- identifier:

  An arbitrary vector or list to hash.

- pkg:

  Package name for the cache directory. Defaults to the current package
  environment.

## Value

Character string of the absolute file path.
