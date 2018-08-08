#' Identify components of a set of sufficient causes
#'
#' `components()` identifies the unique components across all sufficient causes.
#' `necessary_causes()` identifies the components that appear in all sufficient
#' causes. `sufficient_causes()` identifies all the components of a sufficient
#' cause.
#'
#' @param causes a data frame, the result of [causify()]
#'
#' @return a character vector
#' @export
#'
#' @examples
#'
#' causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
#'
#' components(causes)
#' necessary_causes(causes)
#' sufficient_causes(causes)
#'
#' @name components
components <- function(causes) {
  unique(causes$component)
}

#' @export
#' @name components
necessary_causes <- function(causes) {
  comps <- components(causes)
  necessary <- purrr::map_lgl(comps, function(.comp) {
    causes %>%
      dplyr::group_by(cause) %>%
      dplyr::select(component, cause) %>%
      tidyr::nest(-cause) %>%
      dplyr::mutate(contains_comp = purrr::map_lgl(data, ~ .comp %in% .x$component)) %>%
      dplyr::pull(contains_comp) %>%
      all()
 })

  comps[necessary]
}

#' @export
#' @name components
sufficient_causes <- function(causes) {
  causes %>%
      dplyr::group_by(cause) %>%
      dplyr::summarise(component_col = paste(label, collapse = ", ")) %>%
      dplyr::pull(component_col)
}
