## ---- message=FALSE-----------------------------------------------------------
library(magrittr)
library(tidyr)
library(dplyr)
library(htmlTable)
library(tibble)

td <- mtcars %>%
  as_tibble(rownames = "rnames") %>% 
  pivot_longer(names_to = "per_metric", 
               cols = c(hp, mpg, qsec))

## -----------------------------------------------------------------------------
tidy_summary <- td %>%
  group_by(cyl, gear, per_metric) %>% 
  summarise(Mean = round(mean(value), 1),
            SD = round(sd(value), 1),
            Min = round(min(value), 1),
            Max = round(max(value), 1),
            .groups = 'drop') %>%
  pivot_longer(names_to = "summary_stat", 
               cols = c(Mean, SD, Min, Max)) %>% 
  ungroup() %>% 
  mutate(gear = paste(gear, "Gears"),
         cyl = paste(cyl, "Cylinders"))

## ---- warning=FALSE-----------------------------------------------------------
tidy_summary  %>% 
  arrange(per_metric, summary_stat) %>% 
  addHtmlTableStyle(align = "r") %>% 
  tidyHtmlTable(header = gear,
                cgroup = cyl,
                rnames = summary_stat,
                rgroup = per_metric)

## ---- warning=FALSE-----------------------------------------------------------
tidy_summary  %>% 
  arrange(cyl, gear) %>% 
  addHtmlTableStyle(align = "r") %>% 
  tidyHtmlTable(header = summary_stat,
                cgroup = per_metric,
                rnames = gear,
                rgroup = cyl)

