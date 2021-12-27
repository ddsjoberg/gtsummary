## ---- echo=FALSE, include=FALSE-----------------------------------------------
set.seed(123)

## -----------------------------------------------------------------------------
scores <- data.frame(id = 1:5,
  prev_score = c(10, 8, 6, 8, 8),
  cur_score = c(8, 9, 7, 8, 9),
  change = c(-2, 1, 1, 0, 1))

## -----------------------------------------------------------------------------
scores

## -----------------------------------------------------------------------------
library(formattable)
formattable(scores)

## -----------------------------------------------------------------------------
plain_formatter <- formatter("span")
plain_formatter(c(1, 2, 3))

## -----------------------------------------------------------------------------
width_formatter <- formatter("span",
  style = x ~ style(width = suffix(x, "px")))
width_formatter(c(10, 11, 12))

## -----------------------------------------------------------------------------
sign_formatter <- formatter("span", 
  style = x ~ style(color = ifelse(x > 0, "green", 
    ifelse(x < 0, "red", "black"))))
sign_formatter(c(-1, 0, 1))

## -----------------------------------------------------------------------------
formattable(scores, list(change = sign_formatter))

## -----------------------------------------------------------------------------
above_avg_bold <- formatter("span", 
  style = x ~ style("font-weight" = ifelse(x > mean(x), "bold", NA)))
formattable(scores, list(
  prev_score = above_avg_bold,
  cur_score = above_avg_bold,
  change = sign_formatter))

## -----------------------------------------------------------------------------
formattable(scores, list(
  cur_score = formatter("span", 
    style = ~ style(color = ifelse(change >= 0, "green", "red")))))

## -----------------------------------------------------------------------------
formattable(scores, list(prev_score = FALSE))

## -----------------------------------------------------------------------------
products <- data.frame(id = 1:5, 
  price = c(10, 15, 12, 8, 9),
  rating = c(5, 4, 4, 3, 4),
  market_share = percent(c(0.1, 0.12, 0.05, 0.03, 0.14)),
  revenue = accounting(c(55000, 36400, 12000, -25000, 98100)),
  profit = accounting(c(25300, 11500, -8200, -46000, 65000)))
products

## -----------------------------------------------------------------------------
formattable(products)

## -----------------------------------------------------------------------------
formattable(products, list(profit = sign_formatter))

## -----------------------------------------------------------------------------
formattable(products, list(
  price = color_tile("transparent", "lightpink"),
  rating = color_bar("lightgreen"),
  market_share = color_bar("lightblue"),
  revenue = sign_formatter,
  profit = sign_formatter))

## -----------------------------------------------------------------------------
set.seed(123)
df <- data.frame(id = 1:10, 
  a = rnorm(10), b = rnorm(10), c = rnorm(10))
formattable(df, list(area(col = a:c) ~ color_tile("transparent", "pink")))

## -----------------------------------------------------------------------------
formattable(df[, -1], list(~ percent))

## -----------------------------------------------------------------------------
df <- cbind(data.frame(id = 1:10), 
  do.call(cbind, lapply(1:8, function(x) rnorm(10))))
formattable(df, lapply(1:nrow(df), function(row) {
  area(row, col = -1) ~ color_tile("lightpink", "lightblue")
}))

## ---- screenshot.force = FALSE------------------------------------------------
as.datatable(formattable(products))

## ---- screenshot.force = FALSE------------------------------------------------
as.datatable(formattable(products, list(
  price = color_tile("transparent", "lightpink"),
  revenue = sign_formatter,
  profit = sign_formatter)))

