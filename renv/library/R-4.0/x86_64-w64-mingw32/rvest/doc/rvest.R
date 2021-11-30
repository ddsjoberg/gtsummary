## ---- echo=FALSE--------------------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## -----------------------------------------------------------------------------
library(rvest)

## -----------------------------------------------------------------------------
html <- read_html("http://rvest.tidyverse.org/")
class(html)

## -----------------------------------------------------------------------------
html <- minimal_html("
  <p>This is a paragraph<p>
  <ul>
    <li>This is a bulleted list</li>
  </ul>
")
html

## -----------------------------------------------------------------------------
html <- minimal_html("
  <h1>This is a heading</h1>
  <p id='first'>This is a paragraph</p>
  <p class='important'>This is an important paragraph</p>
")

## -----------------------------------------------------------------------------
html %>% html_element("h1")
html %>% html_elements("p")
html %>% html_elements(".important")
html %>% html_elements("#first")

## -----------------------------------------------------------------------------
html <- minimal_html("
  <ol>
    <li>apple &amp; pear</li>
    <li>banana</li>
    <li>pineapple</li>
  </ol>
")
html %>% 
  html_elements("li") %>% 
  html_text2()

## -----------------------------------------------------------------------------
html %>% 
  html_elements("li") %>% 
  html_text()

## -----------------------------------------------------------------------------
html <- minimal_html("<body>
  <p>
  This is
  a
  paragraph.</p><p>This is another paragraph.
  
  It has two sentences.</p>
")


## -----------------------------------------------------------------------------
html %>% 
  html_element("body") %>% 
  html_text2() %>% 
  cat()

## -----------------------------------------------------------------------------
html %>% 
  html_element("body") %>% 
  html_text() %>% 
  cat()

## -----------------------------------------------------------------------------
html <- minimal_html("
  <p><a href='https://en.wikipedia.org/wiki/Cat'>cats</a></p>
  <img src='https://cataas.com/cat' width='100' height='200'>
")


## -----------------------------------------------------------------------------
html %>% 
  html_elements("a") %>% 
  html_attr("href")

html %>% 
  html_elements("img") %>% 
  html_attr("src")

## -----------------------------------------------------------------------------
html %>% 
  html_elements("img") %>% 
  html_attr("width")

html %>% 
  html_elements("img") %>% 
  html_attr("width") %>% 
  as.integer()

## -----------------------------------------------------------------------------
html <- minimal_html("
  <table>
    <tr>
      <th>x</th>
      <th>y</th>
    </tr>
    <tr>
      <td>1.5</td>
      <td>2.7</td>
    </tr>
    <tr>
      <td>4.9</td>
      <td>1.3</td>
    </tr>
    <tr>
      <td>7.2</td>
      <td>8.1</td>
    </tr>
  </table>
  ")

## -----------------------------------------------------------------------------
html %>% 
  html_node("table") %>% 
  html_table()

## -----------------------------------------------------------------------------
html <- minimal_html("
  <ul>
    <li><b>C-3PO</b> is a <i>droid</i> that weighs <span class='weight'>167 kg</span></li>
    <li><b>R2-D2</b> is a <i>droid</i> that weighs <span class='weight'>96 kg</span></li>
    <li><b>Yoda</b> weighs <span class='weight'>66 kg</span></li>
    <li><b>R4-P17</b> is a <i>droid</i></li>
  </ul>
  ")

## -----------------------------------------------------------------------------
html %>% html_elements("b") %>% html_text2()
html %>% html_elements("i") %>% html_text2()
html %>% html_elements(".weight") %>% html_text2()

## -----------------------------------------------------------------------------
characters <- html %>% html_elements("li")

characters %>% html_element("b") %>% html_text2()
characters %>% html_element("i") %>% html_text2()
characters %>% html_element(".weight") %>% html_text2()

## -----------------------------------------------------------------------------
data.frame(
  name = characters %>% html_element("b") %>% html_text2(),
  species = characters %>% html_element("i") %>% html_text2(),
  weight = characters %>% html_element(".weight") %>% html_text2()
)

