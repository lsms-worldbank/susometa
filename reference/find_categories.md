# Find paradata files

Return path(s) of categories files found target path.

## Usage

``` r
find_categories(dir, file_pattern = "\\.xlsx", recurse = TRUE)
```

## Arguments

- dir:

  Character. Directory to scan for categories files

- file_pattern:

  Character. Default value is SuSo's default file name.

- recurse:

  Boolean. Default value assumes that `dir` is parent diectory that
  contains child directories that contain the categories.

## Value

Character vector of paths to categories file(s).
