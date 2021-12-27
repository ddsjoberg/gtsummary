## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(forcats)
library(dplyr)
library(tidyr)
library(fs)

pretty_sec <- function(x) {
  x[!is.na(x)] <- prettyunits::pretty_sec(x[!is.na(x)])
  x
}

pretty_lgl <- function(x) {
  case_when(
    x == TRUE ~ "TRUE",
    x == FALSE ~ "FALSE",
    TRUE ~ ""
  )
}

read_benchmark <- function(file, desc) {
  vroom::vroom(file, col_types = c("ccccddddd")) %>%
    filter(op != "setup") %>%
    mutate(
      altrep = case_when(
        grepl("^vroom_no_altrep", reading_package) ~ FALSE,
        grepl("^vroom", reading_package) ~ TRUE,
        TRUE ~ NA
      ),
      reading_package = case_when(
        grepl("^vroom", reading_package) ~ "vroom",
        TRUE ~ reading_package
      ),
    label = fct_reorder(
      glue::glue("{reading_package}{altrep}\n{manip_package}",
        altrep = ifelse(is.na(altrep), "", glue::glue("(altrep = {altrep})"))
      ),
      case_when(type == "real" ~ time, TRUE ~ 0),
      sum),
    op = factor(op, desc)
  )
}

generate_subtitle <- function(data) {
  rows <- scales::comma(data$rows[[1]])
  cols <- scales::comma(data$cols[[1]])
  size <- fs_bytes(data$size[[1]])
  glue::glue("{rows} x {cols} - {size}B")
}

plot_benchmark <- function(data, title) {

  subtitle <- generate_subtitle(data)
  data <- data %>%
    filter(reading_package != "read.delim", type == "real")

  p1 <- data %>%
    ggplot() +
    geom_bar(aes(x = label, y = time, fill = op, group = label), stat = "identity") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    scale_y_continuous(labels = scales::number_format(suffix = "s")) +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, fill = NULL) +
    theme(legend.position = "bottom")

  p2 <- data %>%
    group_by(label) %>%
    summarise(max_memory = max(max_memory)) %>%
    ggplot() +
    geom_bar(aes(x = label, y = max_memory / (1024 * 1024)), stat = "identity") +
    scale_y_continuous(labels = scales::number_format(suffix = "Mb")) +
    coord_flip() +
    labs(title = "Maximum memory usage", x = NULL, y = NULL) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  library(patchwork)
  p1 + p2 + plot_layout(widths = c(2, 1))
}

make_table <- function(data) {
  data %>%
    filter(type == "real") %>%
    select(-label, -size, -type, -rows, -cols) %>%
    spread(op, time) %>%
    mutate(
      total = read + print + head + tail + sample + filter + aggregate,
      max_memory = as.character(bench::as_bench_bytes(max_memory))
    ) %>%
    arrange(desc(total)) %>%
    mutate_if(is.numeric, pretty_sec) %>%
    mutate_if(is.logical, pretty_lgl) %>%
    select(reading_package, manip_package, altrep, max_memory, everything()) %>%
    rename(
      "reading\npackage" = reading_package,
      "manipulating\npackage" = manip_package,
      memory = max_memory
    ) %>%
    knitr::kable(digits = 2, align = "r", format = "html")
}

desc <- c("setup", "read", "print", "head", "tail", "sample", "filter", "aggregate")

## ---- fig.height = 8, fig.width=10, warning = FALSE, echo = FALSE, message = FALSE----
taxi <- read_benchmark(path_package("vroom", "bench", "taxi.tsv"), desc)

plot_benchmark(taxi, "Time to analyze taxi trip data")

make_table(taxi)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
all_num <- read_benchmark(path_package("vroom", "bench", "all_numeric-long.tsv"), desc)

plot_benchmark(all_num, "Time to analyze long all numeric data")

make_table(all_num)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
all_num_wide <- read_benchmark(path_package("bench", "all_numeric-wide.tsv", package = "vroom"), desc)

plot_benchmark(all_num_wide, "Time to analyze wide all numeric data")

make_table(all_num_wide)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
all_chr <- read_benchmark(path_package("vroom", "bench", "all_character-long.tsv"), desc)

plot_benchmark(all_chr, "Time to analyze long all character data")

make_table(all_chr)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
all_chr_wide <- read_benchmark(path_package("vroom", "bench", "all_character-wide.tsv"), desc)

plot_benchmark(all_chr_wide, "Time to analyze wide all character data")

make_table(all_chr_wide)

## ---- echo = FALSE, message = FALSE, eval = TRUE------------------------------
mult <- read_benchmark(path_package("vroom", "bench", "taxi_multiple.tsv"), desc)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
plot_benchmark(mult, "Time to analyze multiple file data")

make_table(mult)

## ---- echo = FALSE, message = FALSE, eval = TRUE------------------------------
fwf <- read_benchmark(path_package("vroom", "bench", "fwf.tsv"), desc)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
plot_benchmark(fwf, "Time to analyze fixed width data")

make_table(fwf)

## ---- fig.height = 8, fig.width=10, warning = FALSE, message = FALSE, echo = FALSE----
taxi_writing <- read_benchmark(path_package("vroom", "bench", "taxi_writing.tsv"), c("setup", "writing")) %>%
  rename(
    package = reading_package,
    compression = manip_package
  ) %>% mutate(
    package = factor(package, c("base", "readr", "data.table", "vroom")),
    compression = factor(compression, rev(c("gzip", "multithreaded_gzip", "zstandard", "uncompressed")))
  ) %>% filter(type == "real")

subtitle <- generate_subtitle(taxi_writing)

taxi_writing %>%
  ggplot(aes(x = compression, y = time, fill = package)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE, padding = .05)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(labels = scales::number_format(suffix = "s")) +
  theme(legend.position = "bottom") +
  coord_flip() +
  labs(title = "Writing taxi trip data", subtitle = subtitle, x = NULL, y = NULL, fill = NULL)

taxi_writing %>%
  select(-size, -op, -rows, -cols, -type, -altrep, -label, -max_memory) %>%
  mutate_if(is.numeric, pretty_sec) %>%
  pivot_wider(names_from = package, values_from = time) %>%
  arrange(desc(compression)) %>%
  knitr::kable(digits = 2, align = "r", format = "html")

## ---- echo = FALSE, warning = FALSE, message = FALSE--------------------------
si <- vroom::vroom(path_package("vroom", "bench", "session_info.tsv"))
class(si) <- c("packages_info", "data.frame")
select(as.data.frame(si), package, version = ondiskversion, date, source) %>%
  knitr::kable()

