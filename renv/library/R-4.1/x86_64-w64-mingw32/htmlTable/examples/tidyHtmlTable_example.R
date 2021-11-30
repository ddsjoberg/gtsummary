library(tibble)
library(dplyr)
library(tidyr)

mtcars %>%
  rownames_to_column() %>%
  select(rowname, cyl, gear, hp, mpg, qsec) %>%
  pivot_longer(names_to = "per_metric",
               cols = c(hp, mpg, qsec)) %>%
  group_by(cyl, gear, per_metric) %>%
  summarise(
    Mean = round(mean(value), 1),
    SD = round(sd(value), 1),
    Min = round(min(value), 1),
    Max = round(max(value), 1)
  ) %>%
  pivot_longer(names_to = "summary_stat",
               cols = c(Mean, SD, Min, Max)) %>%
  ungroup() %>%
  mutate(
    gear = paste(gear, "Gears"),
    cyl = paste(cyl, "Cylinders")
  ) %>%
  addHtmlTableStyle(align = "r") %>%
  tidyHtmlTable(
    header = gear,
    cgroup = cyl,
    rnames = summary_stat,
    rgroup = per_metric,
    skip_removal_warning = TRUE)
