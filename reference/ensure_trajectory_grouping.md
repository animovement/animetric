# Is this frame grouped as one trajectory per group?

Speed comes from successive rows within a group, so a group has to hold
one position per moment. Pool several keypoints of an animal together
and the distance between keypoints is measured as movement: two
keypoints 100 apart, each drifting 1 per frame, report a speed of 2.7
instead of 1.

## Usage

``` r
ensure_trajectory_grouping(data, call = rlang::caller_env())
```

## Arguments

- data:

  An aniframe.

## Value

`TRUE`, invisibly.

## Details

Path length accumulates the same way, and the tortuosity summary takes
its last value minus its first – across concatenated trajectories, that
is a number describing nothing.

Regrouping an aniframe is allowed, and `anicore` warns that its grouping
and its declaration then disagree. This is a stronger statement about a
narrower thing: these computations have a precondition, and silently
returning a wrong number is worse than refusing.
