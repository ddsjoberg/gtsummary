test_that(".update_vector()", {
  # y vector must be named
  expect_error(
    .update_vector(letters, LETTERS)
  )
  expect_error(
    .update_vector(
      c(a = 2, b = 3),
      c(a = 1, d = 5, 4)
    )
  )
})

test_that(".superscript_numbers ()", {
  # works with non character vector
  expect_error(
    .superscript_numbers(1:4),
    NA
  )
})
