# Resolve `method` and `name` for a derived point

Resolve `method` and `name` for a derived point

## Usage

``` r
resolve_point_method(method, name, call = rlang::caller_env())
```

## Arguments

- method:

  `"centroid"`, `"median"`, `"weighted"` or a function.

- name:

  The new member's name, or `NULL` for the method's default.

- call:

  The calling environment, for error messages.

## Value

A list with `kind` (the method's name, or `"function"`), `fun` (for a
function) and `name`.
