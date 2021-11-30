glyphs <- scaled_font$textToGlyphs(x, y, utf8, utf8_len)

if (glyphs$retval == CairoStatus["success"]) {
  cr$showGlyphs(glyphs$glyphs, glyphs$num_glyphs)
}
