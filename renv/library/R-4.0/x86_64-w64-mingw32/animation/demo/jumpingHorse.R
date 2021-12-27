## original code by Linlin Yan <linlin.yan@cos.name>
## URL: http://cos.name/cn/topic/102348/page/2#post-207851

jumpingHorse <- function(n, size = 8) {
  par(mar = rep(0, 4), bg = 'cornsilk')
  plot(c(0, size), c(0, size), type = 'n', xlab = '', ylab = '', 
       axes = FALSE)
  
  for (x in 1:8) for (y in 1:8) rect(x - 1, y - 1, x, y, col = ifelse((x + 
                                                                         y)%%2 == 0, 'black', NA))
  
  k <- expand.grid(-2:2, -2:2)
  steps <- as.matrix(k[abs(apply(k, 1, prod)) == 2, ])
  
  shape <- data.frame(x = c(0.486, 0.47, 0.402, 0.228, 0.128,
                            0.168, 0.224, 0.264, 0.46, 0.378, 0.318, 0.248, 0.742, 
                            0.664, 0.776, 0.854, 0.842, 0.722, 0.57),
                      y = c(0.952, 0.838, 0.79, 0.64, 0.594, 0.502, 0.462, 0.506,
                            0.538, 0.4, 0.174, 0.004, 0.004, 0.174, 0.174, 0.374,
                            0.596, 0.776, 0.85))
  pos <- sample(1:size, 2, replace = TRUE)
  
  polygon(t(t(shape - 1) + pos), col = 'gray')
  for (i in 1:n) {
    while (TRUE) {
      pos2 <- pos + steps[sample(1:nrow(steps), 1), ]
      if (all(pos2 %in% 1:size)) 
        break
    }
    rect(pos[1] - 1, pos[2] - 1, pos[1], pos[2],
         col = ifelse(sum(pos)%%2 == 0, 'black', 'cornsilk'), border = 'black')
    polygon(t(t(shape - 1) + pos2), col = 'gray')
    Sys.sleep(1)
    pos <- pos2
  }
}

jumpingHorse(100) 
