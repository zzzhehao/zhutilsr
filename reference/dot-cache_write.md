# Write Object to Cache

Serializes an R object to an OS-specific cache directory as an `.rds`
file.

## Usage

``` r
.cache_write(obj, name, identifier)
```

## Arguments

- obj:

  The R object to cache.

- name:

  Character string representing the conventional name of the object.

- identifier:

  A vector or list used to generate a unique hash, distinguishing
  different versions of the object under the same `name`.

## Value

The original object `obj`, invisibly.
