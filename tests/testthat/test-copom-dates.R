test_that("guesses are appended only after the last official date", {
  official <- as.Date(c("2025-11-05", "2025-12-10"))
  guesses <- as.Date(c("2026-01-28", "2025-12-10", "2025-06-01"))
  expect_equal(
    append_copom_guesses(official, guesses),
    as.Date(c("2025-11-05", "2025-12-10", "2026-01-28"))
  )
  expect_equal(append_copom_guesses(official, as.Date(character(0))), official)
})

test_that("guesses = TRUE keeps all official dates", {
  official <- get_copom_dates()
  with_guesses <- get_copom_dates(guesses = TRUE)
  expect_equal(with_guesses[seq_along(official)], official)
})
