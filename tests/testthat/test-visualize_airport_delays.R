test_that("visualize_airport_delays returns a ggplot object", {
  p <- visualize_airport_delays()
  expect_s3_class(p, "ggplot")
})
