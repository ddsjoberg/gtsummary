fields <- c("base.width", "base.height", "min.width", "min.height", "width.inc",
            "height.inc")
hints[fields] <- char_width
toplevel$setGeometryHints(terminal, hints)
