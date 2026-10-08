# Safely remove flat or nested properties from a list

Safely remove flat or nested properties from a list

## Usage

``` r
.clean_yaml_list(lst, excludes = NULL)
```

## Arguments

- lst:

  The frontmatter list

- excludes:

  A character vector of properties to remove (e.g., c("name",
  "parent\$child"))
