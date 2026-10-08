# Read Object from Cache

Reads a previously cached `.rds` file based on its name and identifier
hash.

## Usage

``` r
.cache_read(name, identifier)
```

## Arguments

- name:

  Character string representing the conventional name of the object.

- identifier:

  A vector or list used to generate the unique hash.

## Value

The cached R object.

## Details

Throws an error if the corresponding cache file does not exist.
