#! /usr/local/bin/Rscript
args <- system2(
  "git",
  c("diff", "upstream/master", "--name-only"),
  stdout = TRUE
)

if (!any(args == "NEWS.md")) {
  rlang::abort("Must have a news entry before pushing.")
}
