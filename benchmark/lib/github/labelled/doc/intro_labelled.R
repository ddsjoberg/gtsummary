## -----------------------------------------------------------------------------
library(labelled)

var_label(iris$Sepal.Length) <- "Length of sepal"

## -----------------------------------------------------------------------------
var_label(iris) <- list(Petal.Length = "Length of petal", Petal.Width = "Width of Petal")

## -----------------------------------------------------------------------------
var_label(iris$Petal.Width)
var_label(iris)

## -----------------------------------------------------------------------------
var_label(iris$Sepal.Length) <- NULL

## ---- eval=FALSE--------------------------------------------------------------
#  View(iris)

## -----------------------------------------------------------------------------
look_for(iris)
look_for(iris, "pet")
look_for(iris, details = TRUE)

## -----------------------------------------------------------------------------
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 8, refused = 9))
v

## -----------------------------------------------------------------------------
val_labels(v)
val_label(v, 8)

## -----------------------------------------------------------------------------
val_labels(v) <- c(yes = 1, nno = 3, bug = 5)
v
val_label(v, 3) <- "no"
v

## -----------------------------------------------------------------------------
val_label(v, 2) <- "maybe"
val_label(v, 5) <- NULL
v

## -----------------------------------------------------------------------------
val_labels(v) <- NULL
v

## -----------------------------------------------------------------------------
val_label(v, 1) <- "yes"
v

## -----------------------------------------------------------------------------
f <- factor(1:3)
f
val_labels(f) <- c(yes = 1, no = 3)
f

## -----------------------------------------------------------------------------
df <- data.frame(v1 = 1:3, v2 = c(2, 3, 1), v3 = 3:1)

val_label(df, 1) <- "yes"
val_label(df[, c("v1", "v3")], 2) <- "maybe"
val_label(df[, c("v2", "v3")], 3) <- "no"
val_labels(df)

val_labels(df[, c("v1", "v3")]) <- c(YES = 1, MAYBE = 2, NO = 3)
val_labels(df)
val_labels(df) <- NULL
val_labels(df)
val_labels(df) <- list(v1 = c(yes = 1, no = 3), v2 = c(a = 1, b = 2, c = 3))
val_labels(df)

## -----------------------------------------------------------------------------
v <- c(1,2,2,2,3,9,1,3,2,NA)
val_label(v, 1) <- "yes"
val_label(v, 3) <- "no"
val_label(v, 9) <- "refused"
val_label(v, 2) <- "maybe"
val_label(v, 8) <- "don't know"
v

## -----------------------------------------------------------------------------
sort_val_labels(v)
sort_val_labels(v, decreasing = TRUE)

## -----------------------------------------------------------------------------
sort_val_labels(v, according_to = "l")

## -----------------------------------------------------------------------------
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 9))
v
na_values(v) <- 9
na_values(v)
v
is.na(v)
na_values(v) <- NULL
v
na_range(v) <- c(5, Inf)
na_range(v)
v

## -----------------------------------------------------------------------------
x <- c(1, 2, 2, 9)
na_values(x) <- 9
x

## -----------------------------------------------------------------------------
v <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
v
v2 <- user_na_to_na(v)
v2

## -----------------------------------------------------------------------------
v <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
v
v2 <- remove_user_na(v)
v2

## -----------------------------------------------------------------------------
v <- labelled_spss(1:10, c(Good = 1, Bad = 8), na_values = c(9, 10))
v
na_values(v) <- NULL
v

## -----------------------------------------------------------------------------
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, maybe = 2, no = 3))
v
nolabel_to_na(v)

## -----------------------------------------------------------------------------
size <- labelled(c(1.88, 1.62, 1.78, 99, 1.91), c("not measured" = 99))
size

## -----------------------------------------------------------------------------
val_labels_to_na(size)

## -----------------------------------------------------------------------------
v <- labelled(c(1,2,2,2,3,9,1,3,2,NA), c(yes = 1, no = 3, "don't know" = 8, refused = 9))
v
to_factor(v)

## -----------------------------------------------------------------------------
to_factor(v, levels = "v")
to_factor(v, levels = "p")

## -----------------------------------------------------------------------------
to_factor(v, ordered = TRUE)

## -----------------------------------------------------------------------------
to_factor(v, nolabel_to_na = TRUE)
to_factor(nolabel_to_na(v))

## -----------------------------------------------------------------------------
to_factor(v, sort_levels = "n")
to_factor(v, sort_levels = "v")
to_factor(v, sort_levels = "l")

## -----------------------------------------------------------------------------
f <- factor(1:3, labels = c("a", "b", "c"))
to_labelled(f)

## -----------------------------------------------------------------------------
v
to_labelled(to_factor(v))

## -----------------------------------------------------------------------------
v
to_character(v)

## -----------------------------------------------------------------------------
unclass(v)

## -----------------------------------------------------------------------------
remove_val_labels(v)

## -----------------------------------------------------------------------------
remove_val_labels(v)

## -----------------------------------------------------------------------------
x <- c(1, 2, 2, 9)
na_values(x) <- 9
val_labels(x) <- c(yes = 1, no = 2)
var_label(x) <- "A test variable"
x
remove_val_labels(x)
remove_user_na(x)
remove_user_na(x, user_na_to_na = TRUE)
remove_val_labels(remove_user_na(x))
unclass(x)

## -----------------------------------------------------------------------------
remove_labels(x, user_na_to_na = TRUE)
remove_labels(x, user_na_to_na = TRUE, keep_var_label = TRUE)

## -----------------------------------------------------------------------------
df <- data.frame(
  a = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2)),
  b = labelled(c(1, 1, 2, 3), labels = c(No = 1, Yes = 2, DK = 3)),
  c = labelled(c(1, 1, 2, 2), labels = c(No = 1, Yes = 2, DK = 3)),
  d = labelled(c("a", "a", "b", "c"), labels = c(No = "a", Yes = "b")),
  e = labelled_spss(
    c(1, 9, 1, 2), 
    labels = c(No = 1, Yes = 2),
    na_values = 9
    )
)
dplyr::glimpse(df)
dplyr::glimpse(unlabelled(df))
dplyr::glimpse(unlabelled(df, user_na_to_na = TRUE))
dplyr::glimpse(unlabelled(df, drop_unused_labels = TRUE))

## ---- eval=FALSE--------------------------------------------------------------
#    # from foreign
#    library(foreign)
#    df <- to_labelled(read.spss(
#      "file.sav",
#      to.data.frame = FALSE,
#      use.value.labels = FALSE,
#      use.missings = FALSE
#   ))
#   df <- to_labelled(read.dta(
#     "file.dta",
#     convert.factors = FALSE
#   ))
#  
#   # from memisc
#   library(memisc)
#   nes1948.por <- UnZip("anes/NES1948.ZIP", "NES1948.POR", package="memisc")
#   nes1948 <- spss.portable.file(nes1948.por)
#   df <- to_labelled(nes1948)
#   ds <- as.data.set(nes19480)
#   df <- to_labelled(ds)

## -----------------------------------------------------------------------------
library(dplyr)

df <- data_frame(s1 = c("M", "M", "F"), s2 = c(1, 1, 2)) %>% 
  set_variable_labels(s1 = "Sex", s2 = "Question") %>%
  set_value_labels(s1 = c(Male = "M", Female = "F"), s2 = c(Yes = 1, No = 2))
df$s2

## -----------------------------------------------------------------------------
df <- df %>%
  set_value_labels(s2 = c(Yes = 1, "Don't know" = 8, Unknown = 9))
df$s2

df <- df %>%
  add_value_labels(s2 = c(No = 2))
df$s2

## -----------------------------------------------------------------------------
df <- df %>%
  set_variable_labels(s1 = NULL)

# removing one value label
df <- df %>%
  remove_value_labels(s2 = 2)
df$s2

# removing several value labels
df <- df %>%
  remove_value_labels(s2 = 8:9)
df$s2

# removing all value labels
df <- df %>%
  set_value_labels(s2 = NULL)
df$s2

## -----------------------------------------------------------------------------
library(questionr)
data(fertility)
glimpse(women)
glimpse(women %>% unlabelled())

## -----------------------------------------------------------------------------
glimpse(to_factor(women))
glimpse(women %>% mutate_if(is.labelled, to_factor))
glimpse(women %>% mutate_at(vars(employed:religion), to_factor))

