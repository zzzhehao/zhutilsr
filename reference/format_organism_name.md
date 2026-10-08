# Format Orgnism Name like a Descent Taxonomist

Applies appropriate formatting (italics) to organism names. The genus
name is always italicized. Specific epithets are formatted unless they
are "sp.". Taxonomic qualifiers like "cf." and "aff." are left
unformatted, while the subsequent species epithets (splitted by
whitespace) are formatted.

Can output standard HTML/Markdown tags or `plotmath`-safe strings for
`ggplot2`, or using any defined opening and closing tags.

## Usage

``` r
format_organism_name(
  label,
  format = NULL,
  tag_open = "<i>",
  tag_close = "</i>",
  plotmath.safe = FALSE
)
```

## Arguments

- label:

  A character vector of organism/taxa names to format.

- format:

  An optional character string specifying a preset format. Accepts
  unambiguous partial matches of `"html"`, `"markdown"`, or
  `"plotmath"`. If provided, this overrides `tag_open`, `tag_close`, and
  `plotmath.safe`.

- tag_open:

  A character string for the opening format tag (default: `"<i>"`).
  Ignored if `format` is specified.

- tag_close:

  A character string for the closing format tag (default: `"</i>"`).
  Ignored if `format` is specified.

- plotmath.safe:

  Logical. If `TRUE`, formats the output as a valid R expression string
  for `ggplot2` parsing (e.g., replacing spaces with `~` and wrapping
  non-italicized text in single quotes). Ignored if `format` is
  specified.

## Value

A character vector of formatted organism names, the same length as
`label`.
