expect_doppelganger <- function(title, fig, ...) {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_on_ci()
  vdiffr::expect_doppelganger(title, fig, ...)
}
