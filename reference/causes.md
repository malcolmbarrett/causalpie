# Create sufficient causes

Create sufficient causes

## Usage

``` r
sc(...)

causify(..., add_u = TRUE)
```

## Arguments

- ...:

  components of the cause

- add_u:

  logical. Should component "U" be added to the sufficient cause?
  Default is `TRUE`.

## Value

`sc()`: a named vector; `causify()`: a `tibble` with sufficient causes.

## Examples

``` r
causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
#> # A tibble: 7 × 5
#>   component value label  frac cause             
#>   <chr>     <dbl> <chr> <dbl> <chr>             
#> 1 A             1 A = 1 0.25  Sufficient Cause 1
#> 2 B             0 B = 0 0.25  Sufficient Cause 1
#> 3 U1           NA U1    0.5   Sufficient Cause 1
#> 4 A             1 A = 1 0.167 Sufficient Cause 2
#> 5 E             1 E = 1 0.167 Sufficient Cause 2
#> 6 C             0 C = 0 0.167 Sufficient Cause 2
#> 7 U2           NA U2    0.5   Sufficient Cause 2

causify(sc(F = 1, Q = 0), sc(A = 1), add_u = FALSE)
#> # A tibble: 3 × 5
#>   component value label  frac cause             
#>   <chr>     <dbl> <chr> <dbl> <chr>             
#> 1 F             1 F = 1   0.5 Sufficient Cause 1
#> 2 Q             0 Q = 0   0.5 Sufficient Cause 1
#> 3 A             1 A = 1   1   Sufficient Cause 2
```
