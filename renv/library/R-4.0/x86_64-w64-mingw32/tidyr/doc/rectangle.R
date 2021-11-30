## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(tidyr)
library(dplyr)
library(repurrrsive)

## -----------------------------------------------------------------------------
users <- tibble(user = gh_users)

## -----------------------------------------------------------------------------
names(users$user[[1]])

## -----------------------------------------------------------------------------
users %>% unnest_wider(user)

## -----------------------------------------------------------------------------
users %>% hoist(user, 
  followers = "followers", 
  login = "login", 
  url = "html_url"
)

## -----------------------------------------------------------------------------
repos <- tibble(repo = gh_repos)
repos

## -----------------------------------------------------------------------------
repos <- repos %>% unnest_longer(repo)
repos

## -----------------------------------------------------------------------------
repos %>% hoist(repo, 
  login = c("owner", "login"), 
  name = "name",
  homepage = "homepage",
  watchers = "watchers_count"
)

## -----------------------------------------------------------------------------
repos %>% 
  hoist(repo, owner = "owner") %>% 
  unnest_wider(owner)

## -----------------------------------------------------------------------------
tibble(repo = gh_repos) %>% 
  unnest_auto(repo) %>% 
  unnest_auto(repo)

## -----------------------------------------------------------------------------
chars <- tibble(char = got_chars)
chars

chars2 <- chars %>% unnest_wider(char)
chars2

## -----------------------------------------------------------------------------
chars2 %>% select_if(is.list)

## -----------------------------------------------------------------------------
chars2 %>% 
  select(name, books, tvSeries) %>% 
  pivot_longer(c(books, tvSeries), names_to = "media", values_to = "value") %>% 
  unnest_longer(value)

## -----------------------------------------------------------------------------
chars2 %>% 
  select(name, title = titles) %>% 
  unnest_longer(title)

## -----------------------------------------------------------------------------
tibble(char = got_chars) %>% 
  unnest_auto(char) %>% 
  select(name, title = titles) %>% 
  unnest_auto(title)

## -----------------------------------------------------------------------------
has_key <- !identical(Sys.getenv("GOOGLE_MAPS_API_KEY"), "")
if (!has_key) {
  message("No Google Maps API key found; code chunks will not be run")
}

# https://developers.google.com/maps/documentation/geocoding
geocode <- function(address, api_key = Sys.getenv("GOOGLE_MAPS_API_KEY")) {
  url <- "https://maps.googleapis.com/maps/api/geocode/json"
  url <- paste0(url, "?address=", URLencode(address), "&key=", api_key)

  jsonlite::read_json(url)
}

## ---- eval = has_key----------------------------------------------------------
houston <- geocode("Houston TX")
str(houston)

## ---- eval = has_key, cache = TRUE--------------------------------------------
city <- c("Houston", "LA", "New York", "Chicago", "Springfield")
city_geo <- purrr::map(city, geocode)

## ---- eval = has_key----------------------------------------------------------
loc <- tibble(city = city, json = city_geo)
loc

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_wider(json)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_wider(json) %>% 
  unnest_longer(results)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_wider(json) %>% 
  unnest_longer(results) %>% 
  unnest_wider(results)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_wider(json) %>% 
  unnest_longer(results) %>% 
  unnest_wider(results) %>% 
  unnest_wider(geometry)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_wider(json) %>%
  unnest_longer(results) %>%
  unnest_wider(results) %>%
  unnest_wider(geometry) %>%
  unnest_wider(location)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_auto(json) %>%
  unnest_auto(results) %>%
  unnest_auto(results) %>%
  unnest_auto(geometry) %>%
  unnest_auto(location)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  unnest_wider(json) %>%
  hoist(results, first_result = 1) %>%
  unnest_wider(first_result) %>%
  unnest_wider(geometry) %>%
  unnest_wider(location)

## ---- eval = has_key----------------------------------------------------------
loc %>%
  hoist(json,
    lat = list("results", 1, "geometry", "location", "lat"),
    lng = list("results", 1, "geometry", "location", "lng")
  )

## -----------------------------------------------------------------------------
discs <- tibble(disc = discog) %>% 
  unnest_wider(disc) %>% 
  mutate(date_added = as.POSIXct(strptime(date_added, "%Y-%m-%dT%H:%M:%S"))) 
discs

## ---- error = TRUE------------------------------------------------------------
discs %>% unnest_wider(basic_information)

## -----------------------------------------------------------------------------
discs %>% unnest_wider(basic_information, names_repair = "unique")

## -----------------------------------------------------------------------------
discs %>% 
  select(!id) %>% 
  unnest_wider(basic_information)

## -----------------------------------------------------------------------------
discs %>% 
  hoist(basic_information,
    title = "title",
    year = "year",
    label = list("labels", 1, "name"),
    artist = list("artists", 1, "name")
  )

## -----------------------------------------------------------------------------
discs %>% 
  hoist(basic_information, artist = "artists") %>% 
  select(disc_id = id, artist) %>% 
  unnest_longer(artist) %>% 
  unnest_wider(artist)

discs %>% 
  hoist(basic_information, format = "formats") %>% 
  select(disc_id = id, format) %>% 
  unnest_longer(format) %>% 
  unnest_wider(format) %>% 
  unnest_longer(descriptions)

