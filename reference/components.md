# Identify components of a set of sufficient causes

`components()` identifies the unique components across all sufficient
causes. `necessary_causes()` identifies the components that appear in
all sufficient causes. `sufficient_causes()` identifies all the
components of a sufficient cause.

## Usage

``` r
components(causes)

necessary_causes(causes)

sufficient_causes(causes)
```

## Arguments

- causes:

  a data frame, the result of
  [`causify()`](https://malcolmbarrett.github.io/causalpie/reference/causes.md)

## Value

a character vector

## Examples

``` r
causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))

components(causes)
#> [1] "A"  "B"  "U1" "E"  "C"  "U2"
necessary_causes(causes)
#> [1] "A"
sufficient_causes(causes)
#> [1] "A = 1, B = 0, U1"        "A = 1, E = 1, C = 0, U2"
```
