# SVG Spacewar is copyright 2005 by Nigel Tao: nigel.tao@myrealbox.com
# Licenced under the GNU GPL.
# Developed on cairo version 0.4.0.
#
# 2005-03-31: Version 0.1.

# ported to RGtk (cairo 0.5.1) by michael lawrence (7-21-05)
# not very playable but a good example of cairo + gtk

WIDTH <- 800
HEIGHT <- 600

# trig computations (and x, y, velocity, etc). are made in fixed po arithmetic
FIXED.POINT.SCALE.FACTOR <- 1024
FIXED.POINT.HALF.SCALE.FACTOR <- 32

# discretization of 360 degrees
NUMBER.OF.ROTATION.ANGLES <- 60
RADIANS.PER.ROTATION.ANGLE <- (2*pi / NUMBER.OF.ROTATION.ANGLES)

# equivalent to 25 fps
MILLIS.PER.FRAME <- 20

# a shot every 9/25 seconds <- 8 ticks between shots
TICKS.BETWEEN.FIRE <- 4

# fudge this for bigger or smaller ships
 GLOBAL.SHIP.SCALE.FACTOR <- 0.8

 SHIP.ACCELERATION.FACTOR <- 1
 SHIP.MAX.VELOCITY <- (10 * FIXED.POINT.SCALE.FACTOR)
 SHIP.RADIUS <- as.integer(38 * FIXED.POINT.SCALE.FACTOR * GLOBAL.SHIP.SCALE.FACTOR)

 SHIP.MAX.ENERGY <- 1000
 DAMAGE.PER.MISSILE <- 200
 ENERGY.PER.MISSILE <- 10

# bounce damage depends on how fast you're going
 DAMAGE.PER.SHIP.BOUNCE.DIVISOR <- 3

 NUMBER.OF.STARS <- 20

 MAX.NUMBER.OF.MISSILES <- 60

 MISSILE.RADIUS <- (4 * FIXED.POINT.SCALE.FACTOR)
 MISSILE.SPEED <- 8
 MISSILE.TICKS.TO.LIVE <- 60
 MISSILE.EXPLOSION.TICKS.TO.LIVE <- 6

#------------------------------------------------------------------------------

# global variables

players <- NULL

#------------------------------------------------------------------------------

missiles <- NULL
next.missile.index <- 1

init.missiles.array <- function()
{
	for (i in 1:MAX.NUMBER.OF.MISSILES) {
      missiles <<- c(missiles, list(list(p = list(radius = MISSILE.RADIUS), is.alive = FALSE)))
    }
}

#------------------------------------------------------------------------------

stars <- NULL

init.stars.array <- function()
{
    x.dist <- as.integer(runif(NUMBER.OF.STARS, 0, WIDTH - 1))
	y.dist <- as.integer(runif(NUMBER.OF.STARS, 0, HEIGHT - 1))
	rot.dist <- runif(NUMBER.OF.STARS)*2*pi
	scale.dist <- runif(NUMBER.OF.STARS) + 0.5
	
	for (i in 1:NUMBER.OF.STARS)
    {
	  stars <<- c(stars, list(list(x = x.dist[i], y = y.dist[i], 
	  	rotation = rot.dist[i], scale = scale.dist[i])))
    }
}

#------------------------------------------------------------------------------

debug.scale.factor <- 1.0
game.over.message <- NULL

#------------------------------------------------------------------------------

cos.table <- NULL
sin.table <- NULL

init.trigonometric.tables <- function()
{  
  q <- (NUMBER.OF.ROTATION.ANGLES / 4)
  angle.in.radians <- (q - 1:NUMBER.OF.ROTATION.ANGLES)*2*pi/NUMBER.OF.ROTATION.ANGLES
  
  # our angle system is "true north" - 0 is straight up, whereas
  # cos  sin take 0 as east (and in radians).
  cos.table <<- as.integer(cos(angle.in.radians)*FIXED.POINT.SCALE.FACTOR)
  
  # also, our graphics system is "y axis down", although in regular math,
  # the y axis is "up", so we have to multiply sin by -1.
  sin.table <<- as.integer(-sin(angle.in.radians)*FIXED.POINT.SCALE.FACTOR)
}

#------------------------------------------------------------------------------

on.expose.event <- function(widget, event)
{
  player1 <- players[[1]]
  player2 <- players[[2]]
  
  cr <- gdkCairoCreate(widget[["window"]])

  cr$save()

  scale.for.aspect.ratio(cr, widget[["allocation"]][["width"]],
			  widget[["allocation"]][["height"]])

  cr$scale(debug.scale.factor, debug.scale.factor)

  # draw background space color 
  cr$setSourceRgb(0.1, 0.0, 0.1)
  cr$paint()

  # draw any stars...
  for (i in 1:NUMBER.OF.STARS)
    {
      cr$save()
      cr$translate(stars[[i]]$x, stars[[i]]$y)
      cr$rotate(stars[[i]]$rotation)
      cr$scale(stars[[i]]$scale, stars[[i]]$scale)
      draw.star(cr)
      cr$restore()
    }

  # ... the energy bars...
  cr$save()
  cr$translate(30, 30)
  cr$rotate(0)
  draw.energy.bar(cr, player1)
  cr$restore()

  cr$save()
  cr$translate(WIDTH - 30, 30)
  cr$rotate(pi)
  draw.energy.bar(cr, player2)
  cr$restore()

  # the two ships...
  cr$save()
  cr$translate(player1$p$x / FIXED.POINT.SCALE.FACTOR,
		   player1$p$y / FIXED.POINT.SCALE.FACTOR)
  cr$rotate(player1$p$rotation * RADIANS.PER.ROTATION.ANGLE)
  draw.ship.body(cr, player1)
  cr$restore()

  cr$save()
  cr$translate(player2$p$x / FIXED.POINT.SCALE.FACTOR,
		   player2$p$y / FIXED.POINT.SCALE.FACTOR)
  cr$rotate(player2$p$rotation * RADIANS.PER.ROTATION.ANGLE)
  draw.ship.body(cr, player2)
  cr$restore()

  # ... and any missiles.
  for (i in 1:MAX.NUMBER.OF.MISSILES)
    {
      if (missiles[[i]]$is.alive)
	{
	  cr$save()
	  cr$translate(missiles[[i]]$p$x / FIXED.POINT.SCALE.FACTOR,
			   missiles[[i]]$p$y / FIXED.POINT.SCALE.FACTOR)
	  cr$rotate(missiles[[i]]$p$rotation * RADIANS.PER.ROTATION.ANGLE)
	  draw.missile(cr, missiles[[i]])
	  cr$restore()
	}
    }

  if (is.null(game.over.message))
    {
      if (player1$is.dead)
	{
	  if (player2$is.dead)
		  game.over.message <<- "DRAW"
	  else game.over.message <<- "RED wins"
	}
      else if (player2$is.dead)
	{
	  game.over.message <<- "BLUE wins"
	}
    }
	if (!is.null(game.over.message))
    {
      show.text.message(cr, 80, -30, game.over.message)
      show.text.message(cr, 30, +40, "Press [SPACE] to restart")
    }

  cr$restore()
  
  return(TRUE)
}

#------------------------------------------------------------------------------

scale.for.aspect.ratio <- function(cr, widget.width, widget.height)
{
  is.widget.wider <- (widget.width * HEIGHT) > (WIDTH * widget.height)

  if (is.widget.wider)
    {
      scale <- widget.height / HEIGHT
      playfield.width <- (WIDTH * widget.height) / HEIGHT
      playfield.height <- widget.height
      tx <- (widget.width - playfield.width) / 2
      ty <- 0
    }
  else
    {
      scale <- widget.width / WIDTH
      playfield.width <- widget.width
      playfield.height <- (HEIGHT * widget.width) / WIDTH
      tx <- 0
      ty <- (widget.height - playfield.height) / 2
    }

  cr$translate(tx, ty)
  cr$rectangle(0, 0, playfield.width, playfield.height)
  cr$clip()

  cr$scale(scale, scale)
}

#------------------------------------------------------------------------------

draw.energy.bar <- function(cr, p)
{
  alpha <- 0.6

  cr$save()

  cr$rectangle(0, -5, p$energy / 5, 10)

  pat <- cairoPatternCreateLinear(0, 0, SHIP.MAX.ENERGY / 5, 0)
  pat$addColorStopRgba(0, p$secondary.color$r, p$secondary.color$g,
  	p$secondary.color$b, alpha)
  pat$addColorStopRgba(1, p$primary.color$r, p$primary.color$g,
  	p$primary.color$b, alpha)

  cr$setSource(pat)
  cr$fillPreserve()
  
  cr$setSourceRgb(0, 0, 0)
  cr$stroke()
  cr$restore()
}

#------------------------------------------------------------------------------

draw.ship.body <- function(cr, p)
{
  if (p$is.hit)
    {
      cr$setSourceRgba(p$primary.color$r, p$primary.color$g,
			     p$primary.color$b, 0.5)
      cr$arc(0, 0, SHIP.RADIUS / FIXED.POINT.SCALE.FACTOR, 0, 2*pi)
      cr$stroke()
    }

  cr$save()
  cr$scale(GLOBAL.SHIP.SCALE.FACTOR, GLOBAL.SHIP.SCALE.FACTOR)

  if (!p$is.dead)
    {

      if (p$is.thrusting)
	{
	  draw.flare(cr, p$primary.color)
	}

      if (p$is.turning.left && !p$is.turning.right)
	{
	  draw.turning.flare(cr, p$primary.color, -1.0)
	}

      if (!p$is.turning.left && p$is.turning.right)
	{
	  draw.turning.flare(cr, p$primary.color, 1.0)
	}
    }

  cr$moveTo(0, -33)
  cr$curveTo(2, -33, 3, -34, 4, -35)
  cr$curveTo(8, -10, 6, 15, 15, 15)
  cr$lineTo(20, 15)
  cr$lineTo(20, 7)
  cr$curveTo(25, 10, 28, 22, 25, 28)
  cr$curveTo(20, 26, 8, 24, 0, 24)
  # half way po
  cr$curveTo(-8, 24, -20, 26, -25, 28)
  cr$curveTo(-28, 22, -25, 10, -20, 7)
  cr$lineTo(-20, 15)
  cr$lineTo(-15, 15)
  cr$curveTo(-6, 15, -8, -10, -4, -35)
  cr$curveTo(-3, -34, -2, -33, 0, -33)

  pat <- cairoPatternCreateLinear(-30.0, -30.0, 30.0, 30.0)
  pat$addColorStopRgba(0, p$primary.color$r, p$primary.color$g, p$primary.color$b, 1)
  pat$addColorStopRgba(1, p$secondary.color$r, p$secondary.color$g, p$secondary.color$b, 1)

  cr$setSource(pat)
  cr$fillPreserve()
  
  cr$setSourceRgb(0, 0, 0)
  cr$stroke()
  cr$restore()
}

#------------------------------------------------------------------------------

draw.flare <- function(cr, color)
{
  cr$save()
  cr$translate(0, 22)
  pat <- cairoPatternCreateRadial(0, 0, 2, 0, 5, 12)

  pat$addColorStopRgba(0.0, color$r, color$g, color$b, 1)
  pat$addColorStopRgba(0.3, 1, 1, 1, 1)
  pat$addColorStopRgba(1.0, color$r, color$g, color$b, 0)
  cr$setSource(pat)
  cr$arc(0, 0, 20, 0, 2*pi)
  cr$fill()
  cr$restore()
}

#------------------------------------------------------------------------------

draw.turning.flare <- function(cr, color, right.hand.side)
{
  cr$save()
  pat <- cairoPatternCreateRadial(0, 0, 1, 0, 0, 7)

  pat$addColorStopRgba(0.0, 1, 1, 1, 1)
  pat$addColorStopRgba(1.0, color$r, color$g, color$b, 0)
  cr$setSource(pat)

  cr$save()
  cr$translate(-23 * right.hand.side, 28)
  cr$arc(0, 0, 7, 0, 2*pi)
  cr$fill()
  cr$restore()

  cr$translate(19 * right.hand.side, 7)
  cr$arc(0, 0, 5, 0, 2*pi)
  cr$fill()

  
  cr$restore()
}

#------------------------------------------------------------------------------

draw.missile <- function(cr, m)
{
  cr$save()
  cr$scale(GLOBAL.SHIP.SCALE.FACTOR, GLOBAL.SHIP.SCALE.FACTOR)

  if (m$has.exploded)
    {
      draw.exploded.missile(cr, m)
    }
  else
    {
       alpha <- m$ticks.to.live / MISSILE.TICKS.TO.LIVE
      # non-linear scaling so things don't fade out too fast
      alpha <- 1.0 - (1.0 - alpha) * (1.0 - alpha)

      cr$save()
      cr$moveTo(0, -4)
      cr$curveTo(3, -4, 4, -2, 4, 0)
      cr$curveTo(4, 4, 2, 10, 0, 18)
      # half way po
      cr$curveTo(-2, 10, -4, 4, -4, 0)
      cr$curveTo(-4, -2, -3, -4, 0, -4)

      pat <- cairoPatternCreateLinear(0.0, -5.0, 0.0, 5.0)
      pat$addColorStopRgba(0,
					 m$primary.color$r,
					 m$primary.color$g,
					 m$primary.color$b, alpha)
      pat$addColorStopRgba(1, m$secondary.color$r,
					 m$secondary.color$g,
					 m$secondary.color$b, alpha)

      cr$setSource(pat)
      cr$fill()
      
      cr$restore()

      cr$save()
      cr$arc(0, 0, 3, 0, 2*pi)

      pat <- cairoPatternCreateLinear(0, 3, 0, -3)
      pat$addColorStopRgba(0,
					 m$primary.color$r,
					 m$primary.color$g,
					 m$primary.color$b, alpha)
      pat$addColorStopRgba(1, m$secondary.color$r,
					 m$secondary.color$g,
					 m$secondary.color$b, alpha)

      cr$setSource(pat)
      cr$fill()
      
      cr$restore()
    }

  cr$restore()
}

#------------------------------------------------------------------------------

 
draw.exploded.missile <- function(cr, m)
{
  cr$save()
  cr$scale(GLOBAL.SHIP.SCALE.FACTOR, GLOBAL.SHIP.SCALE.FACTOR)
  
  alpha <- m$ticks.to.live / MISSILE.EXPLOSION.TICKS.TO.LIVE
  alpha <- 1.0 - (1.0 - alpha) * (1.0 - alpha)

  cr$arc(0, 0, 30, 0, 2*pi)

  pat <- cairoPatternCreateRadial(0, 0, 0, 0, 0, 30)
  pat$addColorStopRgba(0,
				     m$primary.color$r, m$primary.color$g,
				     m$primary.color$b, alpha)
  pat$addColorStopRgba(0.5, m$secondary.color$r,
				     m$secondary.color$g,
				     m$secondary.color$b, alpha * 0.75)
  pat$addColorStopRgba(1, 0, 0, 0, 0)

  cr$setSource(pat)
  cr$fill()
  
  cr$restore()
}

#------------------------------------------------------------------------------

 
draw.star <- function(cr)
{
  a <- NUMBER.OF.ROTATION.ANGLES / 10
  r1 <- 5.0
  r2 <- 2.0

  cr$moveTo(r1 * cos.table[0 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r1 * sin.table[0 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r2 * cos.table[1 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r2 * sin.table[1 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r1 * cos.table[2 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r1 * sin.table[2 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r2 * cos.table[3 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r2 * sin.table[3 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r1 * cos.table[4 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r1 * sin.table[4 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r2 * cos.table[5 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r2 * sin.table[5 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r1 * cos.table[6 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r1 * sin.table[6 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r2 * cos.table[7 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r2 * sin.table[7 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r1 * cos.table[8 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r1 * sin.table[8 * a + 1] / FIXED.POINT.SCALE.FACTOR)
  cr$lineTo(r2 * cos.table[9 * a + 1] / FIXED.POINT.SCALE.FACTOR,
		 r2 * sin.table[9 * a + 1] / FIXED.POINT.SCALE.FACTOR)

  c <- 0.5
  cr$setSourceRgb(c, c, c)
  cr$fill()
}

#------------------------------------------------------------------------------

on.timeout <- function(data)
{
  player1 <- players[[1]]
  player2 <- players[[2]]
  
  player1$is.hit <- FALSE
  player2$is.hit <- FALSE

  player1 <- apply.physics.to.player(player1)
  player2 <- apply.physics.to.player(player2)

  if (check.for.collision(player1$p, player2$p))
    {
      enf <- enforce.minimum.distance(player1$p, player2$p)
      player1$p <- enf[[1]]
	  player2$p <- enf[[2]]
	  
       p1vx <- player1$p$vx
       p1vy <- player1$p$vy
       p2vx <- player2$p$vx
       p2vy <- player2$p$vy

       dvx <- (p1vx - p2vx) / FIXED.POINT.HALF.SCALE.FACTOR
       dvy <- (p1vy - p2vy) / FIXED.POINT.HALF.SCALE.FACTOR
       dv2 <- (dvx * dvx) + (dvy * dvy)
       damage <- sqrt(dv2) / DAMAGE.PER.SHIP.BOUNCE.DIVISOR

      player1$energy <- player1$energy - damage
      player2$energy <- player2$energy - damage
      player1$is.hit <- TRUE
      player2$is.hit <- TRUE

      player1$p$vx <- (p1vx * -2 / 8) + (p2vx * +5 / 8)
      player1$p$vy <- (p1vy * -2 / 8) + (p2vy * +5 / 8)
      player2$p$vx <- (p1vx * +5 / 8) + (p2vx * -2 / 8)
      player2$p$vy <- (p1vy * +5 / 8) + (p2vy * -2 / 8)
    }

  for (i in 1:MAX.NUMBER.OF.MISSILES)
    {
      if (missiles[[i]]$is.alive)
	{
	  missiles[[i]]$p <<- apply.physics(missiles[[i]]$p)

	  if (!missiles[[i]]$has.exploded)
	    {
	      if (check.for.collision(missiles[[i]]$p, player1$p))
		{
		  col <- on.collision(player1, missiles[[i]])
		  player1 <- col[[1]]
		  missiles[[i]] <<- col[[2]]
		}

	      if (check.for.collision(missiles[[i]]$p, player2$p))
		{
		  col <- on.collision(player2, missiles[[i]])
		  player2 <- col[[1]]
		  missiles[[i]] <<- col[[2]]
		}
	    }

	  missiles[[i]]$ticks.to.live <<- missiles[[i]]$ticks.to.live - 1
	  if (missiles[[i]]$ticks.to.live == 0)
	    {
	      missiles[[i]]$is.alive <<- FALSE
	    }
	}
    }

  if (player1$energy <= 0)
    {
      player1$energy <- 0
      player1$is.dead <- TRUE
    }
  else
    {
      player1$energy <- min(SHIP.MAX.ENERGY, player1$energy + 1)
    }

  if (player2$energy <= 0)
    {
      player2$energy <- 0
      player2$is.dead <- TRUE
    }
  else
    {
      player2$energy <- min(SHIP.MAX.ENERGY, player2$energy + 1)
    }

  players[[1]] <<- player1
  players[[2]] <<- player2
  
  data$queueDraw()
  return(TRUE)
}

#------------------------------------------------------------------------------

 
apply.physics.to.player <- function(player)
{
  p <- player$p

  if (!player$is.dead)
    {
      # check if player is turning left, ...
      if (player$is.turning.left)
	{
	  p$rotation <- p$rotation - 1
	  while (p$rotation < 0)
	    {
	      p$rotation <- p$rotation + NUMBER.OF.ROTATION.ANGLES
	    }
	}

      # ... or right.
      if (player$is.turning.right)
	{
	  p$rotation <- p$rotation + 1
	  while (p$rotation >= NUMBER.OF.ROTATION.ANGLES)
	    {
	      p$rotation <- p$rotation - NUMBER.OF.ROTATION.ANGLES
	    }
	}

      # check if accelerating
      if (player$is.thrusting)
	{
	  p$vx <- p$vx + SHIP.ACCELERATION.FACTOR * cos.table[p$rotation + 1]
	  p$vy <- p$vy + SHIP.ACCELERATION.FACTOR * sin.table[p$rotation + 1]
	}

      # apply velocity upper bound
       v2 <- (p$vx * p$vx) + (p$vy * p$vy)
       m2 <- SHIP.MAX.VELOCITY * SHIP.MAX.VELOCITY
      if (v2 > m2)
	{
	  p$vx <- as.integer((p$vx * m2) / v2)
	  p$vy <- as.integer((p$vy * m2) / v2)
	}

      # check if player is shooting
      if (player$ticks.until.can.fire == 0)
	{
	  if ((player$is.firing) && (player$energy > ENERGY.PER.MISSILE))
	    {
	      player$energy <- player$energy - ENERGY.PER.MISSILE

	       xx <- cos.table[p$rotation + 1]
	       yy <- sin.table[p$rotation + 1]

	      m <- missiles[next.missile.index]
		  old.missile.index <- next.missile.index
		  next.missile.index <<- next.missile.index + 1
		  
	      if (next.missile.index == MAX.NUMBER.OF.MISSILES)
		{
		  next.missile.index <<- 1
		}

	      m$p$x <- p$x + (SHIP.RADIUS + MISSILE.RADIUS) / FIXED.POINT.SCALE.FACTOR * xx
	      m$p$y <- p$y + (SHIP.RADIUS + MISSILE.RADIUS) / FIXED.POINT.SCALE.FACTOR * yy
	      m$p$vx <- p$vx + MISSILE.SPEED * xx
	      m$p$vy <- p$vy + MISSILE.SPEED * yy
	      m$p$rotation <- p$rotation
		  m$p$radius <- MISSILE.RADIUS
	      m$ticks.to.live <- MISSILE.TICKS.TO.LIVE
	      m$primary.color <- player$primary.color
	      m$secondary.color <- player$secondary.color
	      m$has.exploded <- FALSE
		  m$is.alive <- TRUE

		  missiles[[old.missile.index]] <<- m
		  
	      player$ticks.until.can.fire <- player$ticks.until.can.fire + TICKS.BETWEEN.FIRE
	    }
	}
      else
	{
	  player$ticks.until.can.fire <- player$ticks.until.can.fire - 1
	}
    }
  
  # apply velocity deltas to displacement  
  p <- apply.physics(p)
  
  player$p <- p
  player
}

#------------------------------------------------------------------------------

 
apply.physics <- function(p)
{
  p$x <- p$x + p$vx
  while (p$x > (WIDTH * FIXED.POINT.SCALE.FACTOR))
    {
      p$x <- p$x - (WIDTH * FIXED.POINT.SCALE.FACTOR)
    }
  while (p$x < 0)
    {
      p$x <- p$x + (WIDTH * FIXED.POINT.SCALE.FACTOR)
    }

  p$y <- p$y + p$vy
  while (p$y > (HEIGHT * FIXED.POINT.SCALE.FACTOR))
    {
      p$y <- p$y - (HEIGHT * FIXED.POINT.SCALE.FACTOR)
    }
  while (p$y < 0)
    {
      p$y <- p$y + (HEIGHT * FIXED.POINT.SCALE.FACTOR)
    }
	p
}

#------------------------------------------------------------------------------

check.for.collision <- function(p1, p2)
{
   dx <- (p1$x - p2$x) / FIXED.POINT.HALF.SCALE.FACTOR
   dy <- (p1$y - p2$y) / FIXED.POINT.HALF.SCALE.FACTOR
   r <- (p1$radius + p2$radius) / FIXED.POINT.HALF.SCALE.FACTOR
   d2 <- (dx * dx) + (dy * dy)
   col <- FALSE
   if (d2 < (r * r))
	   col <- TRUE
  return(col)
}

#------------------------------------------------------------------------------

 
enforce.minimum.distance <- function(p1, p2)
{
   dx <- p1$x - p2$x
   dy <- p1$y - p2$y
   d2 <- dx * dx + dy * dy
   d <- as.integer(sqrt(d2))

   r <- p1$radius + p2$radius

  # normalize dx and dy to length <- ((r - d) / 2) + fudge.factor
   desired.vector.length <- ((r - d) * 5) / 8

  dx <- dx * desired.vector.length
  dy <- dy * desired.vector.length
  dx <- dx / d
  dy <- dy / d

  p1$x <- p1$x + dx
  p1$y <- p1$y + dy
  p2$x <- p2$x - dx
  p2$y <- p2$y - dy
 
  list(p1, p2)
}

#------------------------------------------------------------------------------

 
on.collision <- function(p, m)
{
  p$energy <- p$energy - DAMAGE.PER.MISSILE
  p$is.hit <- TRUE
  m$has.exploded <- TRUE
  m$ticks.to.live <- MISSILE.EXPLOSION.TICKS.TO.LIVE
  m$p$vx <- 0
  m$p$vy <- 0
  list(p, m)
}

#------------------------------------------------------------------------------

 
show.text.message <- function(cr, font.size, dy, message)
{
  cr$save()
  
  cr$selectFontFace("Serif", "normal", "normal")

  cr$setFontSize(font.size)
  extents <- cr$textExtents(message)$extents
  x <- (WIDTH / 2) - (extents[["width"]] / 2 + extents[["xBearing"]])
  y <- (HEIGHT / 2) - (extents[["height"]] / 2 + extents[["yBearing"]])

  cr$setSourceRgba(1, 1, 1, 1)
  cr$moveTo(x, y + dy)
  cr$showText(message)
  cr$restore()
}

#------------------------------------------------------------------------------

 
reset <- function()
{
  player1 <- list()
  player1$p <- list()
  player1$p$x <- 200 * FIXED.POINT.SCALE.FACTOR
  player1$p$y <- 200 * FIXED.POINT.SCALE.FACTOR
  player1$p$vx <- 0
  player1$p$vy <- 0
  player1$p$rotation <- runif(1, 0, NUMBER.OF.ROTATION.ANGLES - 1)
  player1$p$radius <- SHIP.RADIUS
  player1$is.thrusting <- FALSE
  player1$is.turning.left <- FALSE
  player1$is.turning.right <- FALSE
  player1$is.firing <- FALSE
  player1$primary.color <- list()
  player1$primary.color$r <- 0.3
  player1$primary.color$g <- 0.5
  player1$primary.color$b <- 0.9
  player1$secondary.color <- list()
  player1$secondary.color$r <- 0.1
  player1$secondary.color$g <- 0.3
  player1$secondary.color$b <- 0.3
  player1$ticks.until.can.fire <- 0
  player1$energy <- SHIP.MAX.ENERGY
  player1$is.hit <- FALSE
  player1$is.dead <- FALSE

  player2 <- list()
  player2$p <- list()
  player2$p$x <- 600 * FIXED.POINT.SCALE.FACTOR
  player2$p$y <- 400 * FIXED.POINT.SCALE.FACTOR
  player2$p$vx <- 0
  player2$p$vy <- 0
  player2$p$rotation <- runif(1, 0, NUMBER.OF.ROTATION.ANGLES - 1)
  player2$p$radius <- SHIP.RADIUS
  player2$is.thrusting <- FALSE
  player2$is.turning.left <- FALSE
  player2$is.turning.right <- FALSE
  player2$is.firing <- FALSE
  player2$primary.color <- list()
  player2$primary.color$r <- 0.9
  player2$primary.color$g <- 0.2
  player2$primary.color$b <- 0.3
  player2$secondary.color <- list()
  player2$secondary.color$r <- 0.5
  player2$secondary.color$g <- 0.2
  player2$secondary.color$b <- 0.3
  player2$ticks.until.can.fire <- 0
  player2$energy <- SHIP.MAX.ENERGY
  player2$is.hit <- FALSE
  player2$is.dead <- FALSE

  init.stars.array()
  init.missiles.array()

  game.over.message <<- NULL
  
  players <<- list(player1, player2)
}

#------------------------------------------------------------------------------

on.key.press <- function(widget, event)
{
  return(on.key.event(widget, event, TRUE))
}

#------------------------------------------------------------------------------

on.key.release <- function(widget, event)
{
  return(on.key.event(widget, event, FALSE))
}

#------------------------------------------------------------------------------

on.key.event <- function(widget, event, key.is.on)
{
  player1 <- players[[1]]
  player2 <- players[[2]]
  
  kv <- event[["keyval"]]
  if (kv == GDK_Escape)
      quit.game()
  else if (kv == GDK_bracketleft && key.is.on)
	  debug.scale.factor <<- debug.scale.factor / 1.25
  else if (kv == GDK_bracketright && key.is.on)
	  debug.scale.factor <<- debug.scale.factor / 1.25
  else if (kv == GDK_space && !is.null(game.over.message))
	  reset()
  else if (kv == GDK_a)
      player1$is.turning.left <- key.is.on
  else if (kv == GDK_d)
      player1$is.turning.right <- key.is.on
  else if (kv == GDK_w)
      player1$is.thrusting <- key.is.on
  else if (kv == GDK_Control_L)
      player1$is.firing <- key.is.on
  else if (kv == GDK_KP_Left)
      player2$is.turning.left <- key.is.on
  else if (kv == GDK_KP_Right)
      player2$is.turning.right <- key.is.on
  else if (kv == GDK_KP_Up)
      player2$is.thrusting <- key.is.on
  else if (kv == GDK_KP_Insert)
      player2$is.firing <- key.is.on
  
  players[[1]] <<- player1
  players[[2]] <<- player2
  
  return(TRUE)
}

quit.game <- function(widget, event)
{
	gSourceRemove(timeout)
	window$destroy()
}
#------------------------------------------------------------------------------

# let's begin
# source("~/research/RGtk/inst/demo/svgspacewar.R")

  init.trigonometric.tables()
  reset()

  window <- gtkWindowNew("toplevel", show = F)
  gSignalConnect(window, "delete-event", quit.game)

  window$setDefaultSize(WIDTH, HEIGHT)

  gSignalConnect(window, "expose_event", on.expose.event)
  gSignalConnect(window, "key_press_event", on.key.press)
  gSignalConnect(window, "key_release_event", on.key.release)
  
  timeout <- gTimeoutAdd(MILLIS.PER.FRAME, on.timeout, window)

  window$showAll()

#------------------------------------------------------------------------------

