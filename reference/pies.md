# Causal Pies

Causal pies are visual representations of sufficient causes.
`causal_pie()` plots all sufficient causes and highlights the unique
components. `causal_pie_necessary()` does the same but highlights
necessary causes (those components that appear in every sufficient
cause).

## Usage

``` r
causal_pie(causes, text_col = "black")

causal_pie_necessary(causes, text_col = "black")
```

## Arguments

- causes:

  a data frame, the result of
  [`causify()`](https://malcolmbarrett.github.io/causalpie/reference/causes.md)

- text_col:

  color of the text. Default is "black".

## Value

a ggplot

## Examples

``` r
causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))

causes |>
  causal_pie() +
  theme_causal_pie()


causes |>
  causal_pie_necessary() +
  theme_causal_pie_grid()

```
