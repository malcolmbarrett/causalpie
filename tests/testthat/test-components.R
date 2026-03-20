test_that("necessary_causes() identifies components in all sufficient causes", {
  causes <- causify(sc(A = 0, B = 1), sc(A = 0, E = 1), sc(B = 1, E = 1))
  result <- necessary_causes(causes)
  # A and B and E each appear in 2 of 3 causes; U always appears in all
  # Only U components (U1, U2, U3) are truly in every cause
  expect_true(all(grepl("^U", result)))
})

test_that("necessary_causes() produces no deprecation warnings", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1, C = 0))
  expect_no_warning(necessary_causes(causes))
})

test_that("components() returns unique components", {
  causes <- causify(sc(A = 1, B = 0), sc(A = 1, E = 1))
  result <- components(causes)
  expect_true("A" %in% result)
  expect_true("B" %in% result)
  expect_true("E" %in% result)
})

test_that("sufficient_causes() returns descriptions of each cause", {
  causes <- causify(sc(A = 1, B = 0), add_u = FALSE)
  result <- sufficient_causes(causes)
  expect_length(result, 1)
  expect_true(grepl("A", result[1]))
  expect_true(grepl("B", result[1]))
})
