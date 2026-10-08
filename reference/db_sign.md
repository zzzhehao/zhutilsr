# Generate Signature for Database Change

Generate Signature for Database Change

## Usage

``` r
db_sign(
  table,
  type,
  msg,
  request = request.id,
  con = .dbconn(),
  time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  signature.table = "signature"
)
```

## Arguments

- table:

  Table name subjected to change

- type:

  Change type

- request:

  Request ID

- con:

  Connection to database as produced by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- time:

  Time of change. Default to current time.

- signature.table:

  Table name to write signature.
