# Re-declare a derived frame the way its source was declared

Detection only recognises the standard identity names, so letting
`as_anipoint()` re-detect gives a frame using its own names an invented
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

  The anipoint it came from.

- space_cols:

  The spatial columns `derived` carries.

## Value

`derived` as an anipoint, declared as `source` was.
