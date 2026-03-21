# Minimalist themes for causal pies

Minimalist themes for causal pies

## Usage

``` r
theme_causal_pie(base_size = 12, base_family = "", ...)

theme_causal_pie_grid(base_size = 12, base_family = "", ...)
```

## Arguments

- base_size:

  base font size, given in pts.

- base_family:

  base font family

- ...:

  additional arguments passed to
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Examples

``` r
causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0)) |>
  causal_pie_necessary() +
  theme_causal_pie()
```
