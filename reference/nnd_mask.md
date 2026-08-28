# Turn a focal / neighbour restriction into a row mask

Turn a focal / neighbour restriction into a row mask

## Usage

``` r
nnd_mask(data, values, arg)
```

## Arguments

- data:

  An aniframe.

- values:

  Named list of column to permitted values, or `NULL`.

- arg:

  Argument name, for error messages.

## Value

Logical vector, one element per row.
