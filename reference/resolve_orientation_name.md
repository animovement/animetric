# The columns an orientation is written to

The columns an orientation is written to

## Usage

``` r
resolve_orientation_name(
  name,
  declared,
  roles,
  overwrite,
  call = rlang::caller_env()
)
```

## Arguments

- name:

  The `name` argument, or `NULL`.

- declared:

  The orientation already declared, role to column.

- roles:

  The roles being written: `"yaw"`, or the four quaternion roles.

- overwrite:

  Whether a declaration in other columns may be replaced.

- call:

  The calling environment, for error messages.

## Value

Character vector of column names, one per role.
