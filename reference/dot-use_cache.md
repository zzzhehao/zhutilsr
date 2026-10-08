# Evaluate Expression with Caching

Attempts to retrieve an object from the cache. If it is not found, it
evaluates the provided expression, writes the result to the cache, and
returns it.

## Usage

``` r
.use_cache(name, identifier, expr)
```

## Arguments

- name:

  Character string representing the conventional name of the object.

- identifier:

  A vector or list used to generate the unique hash.

- expr:

  An expression to evaluate if the cache is missing.

## Value

The result of `expr`, either retrieved from cache or newly evaluated.
