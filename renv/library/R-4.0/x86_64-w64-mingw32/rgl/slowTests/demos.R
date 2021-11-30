library(rgl)

# regression :  this failed for headless tests
par3d(userMatrix = diag(4))

if (!rgl.useNULL())
  for(demo in demo(package="rgl")$results[,"Item"]) 
    if (!(demo %in% c("rgl", "lsystem", "rglExamples", 
  	            	    "shinyDemo", "simpleShinyRgl",
  	            	    "shinyMouse")))
      demo(demo, package="rgl", character.only=TRUE)
