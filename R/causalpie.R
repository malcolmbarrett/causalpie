#' Causal Pies
#'
#' Causal pies are visual representations of sufficient causes. `causal_pie()`
#' plots all sufficient causes and highlights the unique components.
#' `causal_pie_necessary()` does the same but highlights necessary causes (those
#' components that appear in every sufficient cause).
#'
#' @param causes a data frame, the result of [causify()]
#' @param text_col color of the text. Default is "black".
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
#'
#' causes %>%
#'   causal_pie() +
#'   theme_causal_pie()
#'
#' causes %>%
#'   causal_pie_necessary() +
#'   theme_causal_pie_grid()
#'
#' @name pies
causal_pie <- function(causes, text_col = "black") {
  p <- ggplot2::ggplot(data = causes, ggplot2::aes(x = 0, y = frac, fill = component)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = label), col = text_col,
              position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_x_continuous(expand = c(0 ,0)) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank())

  if (dplyr::n_distinct(causes$cause) > 1) p <- p + ggplot2::facet_wrap(~ cause)

  p
}

#' @export
#' @name pies
causal_pie_necessary <- function(causes, text_col = "black") {
  necessary_comp <- necessary_causes(causes)
  causes <- causes %>%
    dplyr::mutate(necessary = component %in% necessary_comp)
  p <- ggplot2::ggplot(data = causes, ggplot2::aes(x = 0, y = frac, fill = necessary)) +
    ggplot2::geom_bar(stat = "identity", col = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), col = text_col,
              position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_x_continuous(expand = c(0 ,0)) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank())

  if (dplyr::n_distinct(causes$cause) > 1) p <- p + ggplot2::facet_wrap(~ cause)

  p
}

#' Minimalist themes for causal pies
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional arguments passed to `theme()`
#'
#' @export
#'
#' @examples
#'
#' causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0)) %>%
#'   causal_pie_necessary() +
#'   theme_causal_pie()
#' @name themes
#'
#' @importFrom ggplot2 %+replace%
theme_causal_pie <- function(base_size = 12, base_family = "", ...) {
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   ..., complete = TRUE)
}

#' @export
#' @name themes
theme_causal_pie_grid <- function(base_size = 12, base_family = "", ...) {
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   ..., complete = TRUE)
}
