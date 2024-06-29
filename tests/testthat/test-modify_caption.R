test_that("modify_caption(caption) works", {
  expect_equal(
    tbl_summary(trial, include = marker) |>
      modify_caption("**Adding a caption** N = {N}", text_interpret = "html") |>
      getElement("table_styling") |>
      getElement("caption"),
    "**Adding a caption** N = 200" |>
      structure(text_interpret = "html")
  )
})
