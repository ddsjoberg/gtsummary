man_create_image_tag <- function(file, dir = "man-images", width = 100) {

  repo_url <- "https://raw.githubusercontent.com/ddsjoberg/gtsummary/main"

  image_url <- file.path(repo_url, dir, file)

  paste0(
    "<img ",
    "src=\"", image_url, "\" ",
    "style=\"width:", width, "\\%;\">"
  )
}
