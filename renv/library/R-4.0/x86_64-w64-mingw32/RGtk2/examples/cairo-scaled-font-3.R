## R user obviously does not allocate things on the stack.
## This is the same as the first example.

glyphs <- scaled_font$textToGlyphs(x, y, utf8, utf8_len)

if (glyphs$retval == CairoStatus["success"])
  cr$showTextGlyphs(utf8, utf8_len,
                    glyphs$glyphs, glyphs$num_glyphs,
                    glyphs$clusters, glyphs$num_clusters,
                    glyphs$cluster_flags)
