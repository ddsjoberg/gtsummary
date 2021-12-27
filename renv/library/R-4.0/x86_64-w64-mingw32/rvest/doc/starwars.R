## ---- echo = FALSE, results = "asis"------------------------------------------
library(rvest)
crawl_html <- function(x) {
  x %>% 
    gsub("\r", "", .) %>% 
    gsub("\n\n", "</p><p>", .) %>% 
    gsub("\n", " ", .) %>% 
    paste0("<p>", ., "</p>")
}

film_desc <- function(x) {
  glue::glue_data(x, "
  <section>
    <h2 data-id='{episode_id}'>{title}</h2>
    <p>Released: {release_date}</p>
    <p>Director: <span class='director'>{director}</span></p>
    <div class='crawl'>{crawl_html(opening_crawl)}</div>
  </section>")
}

films <- repurrrsive::sw_films
films <- films[order(sapply(films, "[[", "episode_id"))]

descs <- vapply(films, film_desc, character(1))
writeLines(descs)

