library(magrittr)
# A simple output
long_txt <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi
ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit
in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur
sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum"
short_txt <- gsub("(^[^.]+).*", "\\1", long_txt)

cbind(rep(short_txt, 2),
      rep(long_txt, 2)) %>%
  addHtmlTableStyle(col.rgroup = c("#FFF", "#EEF")) %>%
  interactiveTable(minimized.columns = ncol(.),
                   header = c("Short", "Long"),
                   rnames = c("First", "Second"))
