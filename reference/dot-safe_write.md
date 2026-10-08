# Safely write output files with overwrite check

Write object to output paths. There are three ways to specify output
paths. Provide base file names for all objects and a output directory
for all, provide base file names and output directories for all objects,
or provide file paths for all objects. The resulted file paths must be
in the same length of the object.

## Usage

``` r
.safe_write(
  obj,
  ...,
  dir = NULL,
  file = NULL,
  files = NULL,
  write_func = NULL,
  create_dir = TRUE,
  write_func_args = NULL,
  overwrite_args = list(interactive = interactive()),
  csv_args = list(sep = ";", row.names = FALSE)
)
```

## Arguments

- obj:

  Object to write.

- ...:

  Placeholder. Nothing will be evaluated in this slot. It enforces
  whoever use this function to contiously specifying their output as
  dir, file, or files.

- dir:

  Directory to write the output files

- file:

  Mantadory if `dir` is provided. File name of the output.

- files:

  File path to write the object.

- write_func:

  Function to write object, the first argument must be object to write
  and the second argument must be the path. Default to `NULL`.

- create_dir:

  Logical. Whether to create directories if the target directories do
  not exist. If set to `TRUE`, the directories will be created
  recursively until the file can be written. Default to `TRUE`.

- write_func_args:

  Arguments to passed to `write_func`.

- overwrite_args:

  Arguments passed to
  [`.overwrite_check()`](https://zzzhehao.github.io/zhutilsr/reference/dot-overwrite_check.md)

- csv_args:

  Arguments passed to
  [`write.table()`](https://rdrr.io/r/utils/write.table.html)

## Details

Write function could be specified in `write_func`. If set to `NULL`,
some file extension will be detected and written with corresponding
functions.
