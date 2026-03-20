test_that("sc() returns a named vector", {
  result <- sc(A = 1, B = 0)
  expect_true(is.numeric(result))
  expect_named(result, c("A", "B"))
  expect_equal(result, c(A = 1, B = 0))
})

test_that("sc() errors without names", {
  expect_error(sc(1, 0), "Must be a named vector")
})

test_that("causify() works with a single cause", {
  result <- causify(sc(A = 1, B = 0))
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("component", "value", "label", "frac", "cause"))
  expect_true(is.numeric(result$value))
  expect_equal(nrow(result), 3) # A, B, U1
  expect_equal(result$component, c("A", "B", "U1"))
})

test_that("causify() works with multiple causes (GH #1)", {
  # This is the exact example from the bug report
  result <- causify(
    sc(A = 0, B = 1),
    sc(A = 0, E = 1),
    sc(B = 1, E = 1)
  )
  expect_s3_class(result, "tbl_df")
  expect_true(is.numeric(result$value))
  expect_equal(nrow(result), 9) # 3 causes x (2 components + 1 U each)
})

test_that("causify() U row has NA value, not character", {
  result <- causify(sc(A = 1))
  u_row <- result[result$component == "U1", ]
  expect_true(is.na(u_row$value))
  expect_true(is.numeric(u_row$value))
})

test_that("causify() works with add_u = FALSE", {
  result <- causify(sc(A = 1, B = 0), add_u = FALSE)
  expect_equal(nrow(result), 2)
  expect_false("U1" %in% result$component)
})
