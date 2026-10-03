# Check that arguments name members of a level

Check that arguments name members of a level

## Usage

``` r
check_members(
  x,
  members,
  level,
  arg,
  single = TRUE,
  call = rlang::caller_env()
)
```

## Arguments

- x:

  The argument's value.

- members:

  The level's members.

- level:

  The level's name, for the message.

- arg:

  The argument's name, for the message.

- single:

  Whether exactly one member is expected.

- call:

  The calling environment, for error messages.

## Value

`TRUE`, invisibly.
