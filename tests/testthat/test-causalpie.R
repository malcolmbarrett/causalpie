# ---- causal_pie() structural tests ----

test_that("causal_pie() returns a ggplot for a single cause", {
  causes <- causify(sc(A = 1, B = 0))
  p <- causal_pie(causes)
  expect_s3_class(p, "ggplot")
  # Single cause should not be faceted
  expect_false(inherits(p$facet, "FacetWrap"))
})

test_that("causal_pie() facets when multiple causes exist", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1))
  p <- causal_pie(causes)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("causal_pie() uses text_col argument", {
  causes <- causify(sc(A = 1, B = 0))
  p <- causal_pie(causes, text_col = "red")
  text_layer <- p$layers[[2]]
  expect_equal(text_layer$aes_params$colour, "red")
})

# ---- causal_pie_necessary() structural tests ----

test_that("causal_pie_necessary() returns a ggplot with necessary fill", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1))
  p <- causal_pie_necessary(causes)
  expect_s3_class(p, "ggplot")
  expect_equal(rlang::as_name(p$mapping$fill), "necessary")
})

test_that("causal_pie_necessary() does not facet with single cause", {
  causes <- causify(sc(A = 1, B = 0))
  p <- causal_pie_necessary(causes)
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p$facet, "FacetWrap"))
})

test_that("causal_pie_necessary() facets with multiple causes", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1))
  p <- causal_pie_necessary(causes)
  expect_true(inherits(p$facet, "FacetWrap"))
})

# ---- theme tests ----

test_that("theme_causal_pie() returns a complete theme without grid", {
  thm <- theme_causal_pie()
  expect_s3_class(thm, "theme")
  expect_s3_class(thm$panel.grid, "element_blank")
  expect_s3_class(thm$axis.text, "element_blank")
  expect_equal(thm$strip.text$face, "bold")
})

test_that("theme_causal_pie_grid() keeps panel grid", {
  thm <- theme_causal_pie_grid()
  expect_s3_class(thm, "theme")
  expect_false(inherits(thm$panel.grid, "element_blank"))
  expect_s3_class(thm$axis.text, "element_blank")
})

test_that("theme_causal_pie() respects base_size", {
  thm <- theme_causal_pie(base_size = 20)
  expect_equal(thm$text$size, 20)
})

test_that("theme_causal_pie() passes ... to theme()", {
  thm <- theme_causal_pie(
    plot.background = ggplot2::element_rect(fill = "red")
  )
  expect_equal(thm$plot.background$fill, "red")
})

# ---- vdiffr visual snapshot tests ----

test_that("causal_pie() single cause renders correctly", {
  causes <- causify(sc(A = 1, B = 0))
  expect_doppelganger(
    "causal-pie-single",
    causal_pie(causes) + theme_causal_pie()
  )
})

test_that("causal_pie() multiple causes renders correctly", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
  expect_doppelganger(
    "causal-pie-multi",
    causal_pie(causes) + theme_causal_pie()
  )
})

test_that("causal_pie() without U renders correctly", {
  causes <- causify(sc(A = 1, B = 0), add_u = FALSE)
  expect_doppelganger(
    "causal-pie-no-u",
    causal_pie(causes) + theme_causal_pie()
  )
})

test_that("causal_pie_necessary() renders correctly", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
  expect_doppelganger(
    "causal-pie-necessary",
    causal_pie_necessary(causes) + theme_causal_pie()
  )
})

test_that("causal_pie() with three causes renders correctly", {
  causes <- causify(
    sc(A = 1, B = 0),
    sc(A = 1, E = 1, C = 0),
    sc(B = 1, E = 1)
  )
  expect_doppelganger(
    "causal-pie-three-causes",
    causal_pie(causes) + theme_causal_pie()
  )
})

test_that("causal_pie() with many components renders correctly", {
  causes <- causify(sc(A = 1, B = 0, C = 1, D = 0, E = 1))
  expect_doppelganger(
    "causal-pie-many-components",
    causal_pie(causes) + theme_causal_pie()
  )
})

test_that("causal_pie() with text_col renders correctly", {
  causes <- causify(sc(A = 1, B = 0))
  expect_doppelganger(
    "causal-pie-text-col",
    causal_pie(causes, text_col = "white") + theme_causal_pie()
  )
})

test_that("causal_pie() with theme_causal_pie_grid renders correctly", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
  expect_doppelganger(
    "causal-pie-grid-theme",
    causal_pie(causes) + theme_causal_pie_grid()
  )
})

test_that("causal_pie_necessary() single cause renders correctly", {
  causes <- causify(sc(A = 1, B = 0))
  expect_doppelganger(
    "causal-pie-necessary-single",
    causal_pie_necessary(causes) + theme_causal_pie()
  )
})

test_that("causal_pie_necessary() without U renders correctly", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1), add_u = FALSE)
  expect_doppelganger(
    "causal-pie-necessary-no-u",
    causal_pie_necessary(causes) + theme_causal_pie()
  )
})

test_that("causal_pie_necessary() with text_col renders correctly", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
  expect_doppelganger(
    "causal-pie-necessary-text-col",
    causal_pie_necessary(causes, text_col = "white") + theme_causal_pie()
  )
})

test_that("causal_pie_necessary() with grid theme renders correctly", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
  expect_doppelganger(
    "causal-pie-necessary-grid-theme",
    causal_pie_necessary(causes) + theme_causal_pie_grid()
  )
})
