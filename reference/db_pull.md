# Pull Table

Pull Table

## Usage

``` r
db_pull(
  table,
  dbconn = .dbconn(),
  cleaned = T,
  formatting = T,
  format = NULL,
  silent = F
)
```

## Arguments

- table:

  Table name. Type "show_table" to display all available table names in
  database.

- cleaned:

  Clean table, rows marked in `del` column will be dropped.

- formatting:

  Format table. Utilize table-specific formatting function to format the
  table if available. Formatting functions are always named under the
  rule `format_` + table name.

- format:

  Argument to pass over to formatting functions. If available different
  formatting could be chosen.

- silent:

  Logical. Whether to silent messages from internal function call.

## Value

A data.frame of requested table.
