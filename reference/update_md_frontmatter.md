# Update a markdown file with new YAML frontmatter

Update a markdown file with new YAML frontmatter

## Usage

``` r
update_md_frontmatter(
  fm,
  file_path,
  exclude_property = NULL,
  mode = c("modify", "rewrite")
)
```

## Arguments

- fm:

  The modified frontmatter list

- file_path:

  Path to the markdown file

- exclude_property:

  Character vector of keys to strip before writing

- mode:

  Update mode. Unambiguous abbreviations are accepted. Default to
  `modify`, which keeps the unmodified properties. Also accept
  `rewrite`, which ignores original properties and rewrite completely
  with given `fm`.
