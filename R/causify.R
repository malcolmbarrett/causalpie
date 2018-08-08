#' Create sufficient causes
#'
#' @param ... components of the cause
#' @param add_u logical. Should component "U" be added to the sufficient cause?
#'   Default is `TRUE`.
#'
#' @return `sc()`: a named vector; `causify()`: a `tibble` with sufficient causes.
#' @export
#'
#' @examples
#'
#' causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
#'
#' causify(sc(F = 1, Q = 0), sc(A = 1), add_u = FALSE)
#'
#' @name causes
sc <- function(...) {
  causes <- c(...)
  if (is.null(names(causes))) stop("Must be a named vector, e.g. c(x = 1)", call. = FALSE)
  causes
}

#' @export
#' @name causes
causify <- function(..., add_u = TRUE) {
  causes <- list(...)
  purrr::imap_dfr(causes, ~wrangle_causes(.x,
                                          cause_name = paste("Sufficient Cause", .y),
                                          u_name = paste0("U", .y),
                                          add_u = add_u))
}

wrangle_causes <- function(sufficient_cause, add_u = TRUE, u_name = "U",
                           u_value = "?",
                           u_frac = .5, cause_name = "Sufficient Cause") {
  n_components <- length(sufficient_cause)
  sufficient_cause <- tibble::enframe(sufficient_cause, name = "component")
  components <- paste(sufficient_cause$component, "=", sufficient_cause$value)

   if (add_u) {
     pie_fraction <- u_frac / n_components
     cause_df <- sufficient_cause %>%
         dplyr::add_row(component = u_name, value = u_value) %>%
         dplyr::bind_cols(
           tibble::data_frame(
             label = c(components, u_name),
             frac = c(rep(pie_fraction, n_components), u_frac),
             cause = cause_name
       )
     )
   } else {
     pie_fraction <- 1 / n_components
     cause_df <- sufficient_cause %>%
         dplyr::bind_cols(
           tibble::data_frame(
             label = components,
             frac = rep(pie_fraction, n_components),
             cause = cause_name
       )
     )
   }

  cause_df
}
