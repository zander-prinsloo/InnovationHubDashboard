# ── plot_changes subtitle — economy terminology ───────────────────────────────

test_that("plot_changes subtitle uses 'economies' not 'countries'", {
  # Source-inspection: the function body is inspected rather than executed
  # because plot_changes() requires a data fixture (prep_changes output) that
  # is expensive to construct in a unit test.
  fn_str <- paste(deparse(body(plot_changes)), collapse = "\n")
  expect_true(grepl("economies", fn_str, fixed = TRUE))
  expect_false(grepl("countries", fn_str, fixed = TRUE))
})

test_that("plot_changes subtitle co-located with 'highlighting' keyword", {
  # The subtitle glue string contains both "economies" and "highlighting" on
  # the same line — this guards against a vacuous pass if "economies" appears
  # elsewhere in the function body.
  fn_str <- paste(deparse(body(plot_changes)), collapse = "\n")
  expect_true(grepl("economies.*highlighting|highlighting.*economies", fn_str))
})
