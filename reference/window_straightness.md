# Straightness over sliding windows

Straightness over sliding windows

## Usage

``` r
window_straightness(position, window_width)
```

## Arguments

- position:

  A data frame of positions, one column per axis.

- window_width:

  The window width, in rows.

## Value

Numeric vector: the distance between the positions at the ends of each
row's window, over the distance travelled between them. `NA` where the
window holds a missing position, whose steps either side are `NA`, so
that the displacement and the distance always cover the same rows.
