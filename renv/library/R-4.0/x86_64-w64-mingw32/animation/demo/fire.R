## Original code by Linlin Yan
## See http://yihui.name/en/2009/06/simulation-of-burning-fire-in-r/ for the
##    introduction of this demo
## If your computer is too fast, just add Sys.sleep(0.01) somewhere in the loop
local({
  Fire <- function(row = 100, col = 100, time = 500, fade = 0.03, ...) {
    fire <- matrix(0, col, row);
    fire[,1] <- runif(col);
    
    for (t in 1:time) {
      dev.hold()
      image(fire, col = rev(heat.colors(row)),
            axes = FALSE, ...);
      dev.flush()
      
      fire <- (
        fire +
          cbind(fire[,1], fire[c(col,1:(col-1)), 1:(row - 1)]) +
          cbind(fire[,1], fire[                , 1:(row - 1)]) +
          cbind(fire[,1], fire[c(2:col,1)      , 1:(row - 1)])
      ) / 4;
      fire <- cbind(fire[,1], (fire + fade / 5 - runif(1, max = fade))[,-1]);
      fire[fire < 0] <- 0;
      
      r <- runif(1);
      if (r < .1) fire[,1] <- fire[,1][c(2:col, 1)];
      if (r > .9) fire[,1] <- fire[,1][c(col, 1:(col-1))];
    };
    NULL;
  }
  par(mar = rep(0, 4))
  Fire(50)
})
