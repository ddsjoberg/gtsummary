## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## -----------------------------------------------------------------------------
#  call(
#    a =       3,
#    b = 3213232
#  )
#  

## -----------------------------------------------------------------------------
#  call(
#    a = 3,
#    b = 3213232
#  )
#  

## -----------------------------------------------------------------------------
#  call(
#    # column 1  | column 2 |
#    abkj = f(2),         7,
#    more_ = "a", 2 # more
#  )

## -----------------------------------------------------------------------------
#  # all arguments of first column named -> must right align
#  # aligned if the (imaginary) comma on the last line is in line with the commas
#  # from the two top lines.
#  fell(
#    x  =    1,
#    y  =   23,
#    zz = NULL
#  )
#  
#  # this works also with more than one column
#  fell(
#    x  =    1, annoying = 3,
#    y  =   23, # nothing in column 2 for row 2
#    zz = NULL, finally = ""
#  )

## -----------------------------------------------------------------------------
#  # not all arguments of first column named, hence, only
#  # commas of all but the first column must agree.
#  gell(
#    p = 2,   g = gg(x),  n = 3 * 3, #
#    31,    fds =    -1, gz = f / 3,
#  )
#  

## -----------------------------------------------------------------------------
#  map(x, f,
#   arg1 =  121,
#   arg2 =    1
#  )

## -----------------------------------------------------------------------------
#  tibble::tribble(
#    ~key_here,  ~value_here,
#    "left",         "right", # comments are allowed
#    "long string",   "shrt" # columns can overlap ('~' above ',')
#  )
#  
#  purrr::map(x, fun, # arguments on same line as opening brace are not considered
#    arg2  =    2,
#    ar    = f(x)
#  )

## -----------------------------------------------------------------------------
#  # holds
#  call(
#    a =  3,
#    b = 32
#  )
#  
#  # doesn't hold
#  call(
#    a =  3,
#     b = 32
#  )

## -----------------------------------------------------------------------------
#  # holds
#  call(
#    a =  3, k  = 3,
#    b = 32,    222
#  )
#  
#  # doesn't hold
#  call(
#    a =  3 ,
#    b = 32
#  )

## -----------------------------------------------------------------------------
#  # holds
#  call(
#    a =  ff("pk"), k  = 3, x =  2,
#    b =     f(-g), 22 + 1, yy = 1,
#    c =         1,
#    f(x, y),
#    k
#  )
#  
#  # doesn't hold
#  call(
#    a =   3,
#    b = 32, c = 2
#  )

## -----------------------------------------------------------------------------
#  rge(
#    x  = 99, x =  2,
#    fs =  1,  y = 1,
#  )

