# motion event handler
{
  x <- motion_event$x
  y <- motion_event$y
  # handle (x,y) motion here
  motion_event$request_motions() # handles is_hint events
}
