# Re-declare a derived frame the way its source was declared

Detection only recognises the standard identity names, so letting
`as_aniframe()` re-detect gives a frame using its own names an invented
`keypoint` column and a replaced declaration (#47). The rest of the
source's metadata is carried over with it.

## Usage

``` r
redeclare_like(derived, source, space_cols)
```

## Arguments

- derived:

  A plain data frame derived from `source`.

- source:

  The aniframe it came from.

- space_cols:

  The spatial columns `derived` carries.

## Value

`derived` as an aniframe, declared as `source` was.
