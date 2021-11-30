library(rggobi)
library(RGtk2)

pong <- function() {
x <- 100
lives <- 3
deaths <- 0

box <- as.data.frame(rbind(cbind(1:x, 0), cbind(1:x, x), cbind(x, 1:x), 
  cbind(0, 1:x), c(x/2, x/2), cbind(-20, seq(1,lives*4, by=4))))
box <- cbind(box, 0)

# Unfortunately, lives cannot be a variable since ggobi_data_set_variables
# can't handle categorical data

#calc_lives <- function() {
#  life_fact <- as.factor(c(rep("Lives", lives - deaths), rep("Deaths", deaths), 
#    rep(NA, 4*x - lives - deaths + 1)))
#  levels(life_fact) <- c("Lives", "Deaths")
#  life_fact
#}

#life <- calc_lives()
#box <- cbind(box, life)

colnames(box) <- c("X", "Y", "Z")

gg <- ggobi(box)
d <- gg[1]
glyph_color(d) <- c(rep(c(1,1,1,8), x), rep(3, lives+1))
shadowed(d) <- c(rep(FALSE, 3*x), rep(TRUE, x), FALSE, rep(FALSE, lives))

v <- c(.7, 1)
b <- nrow(d) - lives

blocked <- FALSE
update_cb <- function()
{
  if (blocked)
    return(TRUE)
  ball <- d[b,c(1,2)]
  if (ball[1] >= x)
    v[1] <<- -v[1]
  if (ball[2] <= 0 || ball[2] >= x)
    v[2] <<- -v[2]
  if (ball[1] <= 0) {
    box[b,c(1,2)] <- c(x/2,x/2)
    v <<- c(.7, 1)
    blocked <<- TRUE
    if (lives > deaths) {
      dialog <- gtkMessageDialog(NULL, 0, "info", "ok", "Sorry, you missed:",
        lives-deaths, "lives left")
      dialog$run()
    } else {
      dialog <- gtkMessageDialog(NULL, 0, "question", "yes-no", "Sorry, game over: play again?")
      if (dialog$run() == GtkResponseType["yes"])
        pong()
    }
    dialog$destroy()
    blocked <<- FALSE
    if (deaths == lives)
      return(FALSE);
    deaths <<- deaths + 1
    shadowed(d) <- c(rep(FALSE, 3*x), rep(TRUE, x), FALSE, 
      rep(FALSE, lives - deaths), rep(TRUE, deaths))
    #ggobi_data_set_variables(d, calc_lives(), "Lives")
  } else box[b,c(1,2)] <- ball + v
  d[,"X"] <- box[,1]
  d[,"Y"] <- box[,2]
  return(TRUE)
}

class(gg) <- "GObject"

gSignalConnect(gg, "identify-point", 
  function(gg, plot, id, data) { 
    if ((id+1) == b && v[1] < 0)
      v <<- v*c(-1,1)
  })

gTimeoutAdd(2, update_cb)
}
pong()
                                        
