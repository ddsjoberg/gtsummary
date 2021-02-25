library(gtsummary, warn.conflicts = FALSE)
library(bench)
library(gert)
# gert::git_tag_list()
library(readr)
library(dplyr)
library(stringr)

# setup code
big_trial <- purrr::map_dfr(seq_len(5000), ~trial)

bm <- bench::mark(
  # simple summary
  simple = tbl_summary(trial),

  # simple calculation with comparisons+others
  complex = tbl_summary(trial, by = trt) %>% add_overall() %>% add_p() %>% add_q(quiet = TRUE) %>% add_n(),

  # big summary
  # big_data = big_trial %>% select(age, grade, trt) %>% tbl_summary(by = trt, missing = "no") %>% add_p(),

  check = FALSE,
  # min_iterations = 30
  min_iterations = 10
)

pr <- cb_fetch(remote = "upstream")

#Esto te lee todos los benchmark que hay en master
benchmarks_previos <- cb_read()
informacion <- gert::git_commit_info()
informacion$id #este es el commit actual (entiendo que del que esta subido, no del que estas subiendo)
taglists <- gert::git_tag_list()
taglists
hash136 <- taglists[str_detect(taglists$name, "1.3.6"), "commit"]

bm2 <- benchmarks_previos %>% filter(abbrev_commit_hash=="b19d0b96"|abbrev_commit_hash=="6367e968")

bm3 <- benchmarks_previos %>% filter(commit_hash==informacion$id)

bm3$benchmarks

bmTags <- benchmarks_previos %>% filter(str_detect(ref_names, "tag:"))

write_excel_csv(x = as.data.frame(bm3$benchmarks), file = "bench/benchmark.csv")

# bm1 <- benchmarks_previos[2,][4][[1]]

# as.data.frame(bm1)


# benchmarks_previos[[commit_hash=="376062d75141cda1f883f099721ef6b38c7fd9e0"]]

benchmarks_previos %>% filter(!is.na(benchmarks))

listo <- as.list(benchmarks_previos)

head(listo, 5)
bind_rows(benchmarks_previos[[4]])

lista <- benchmarks_previos[[4]]





# Hay que darle nombre a cada elemento de la lista,
# lo saco de un vector que contiene los hashs (commit_hash),
# luego bind rows une los dataframes y los identifica por el nombre
# de cada elemento de la lista.

# Name list elements
names(lista) <- commit_hash
# bind rows and add .id
library(dplyr)
df <- bind_rows(lista, .id = "hash")


library(ggplot2)

df %>% ggplot(aes(x = time, y=p50, color=hash))+
  geom_point()+facet_wrap(vars(name))
