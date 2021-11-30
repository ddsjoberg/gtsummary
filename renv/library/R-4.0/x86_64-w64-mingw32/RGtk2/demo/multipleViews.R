# source("/home/larman/research/RGtk2/RGtk2/demo/multipleViews.R")

recursive.attach.view <- function(depth, view, anchor)
{
  if (depth > 4)
    return()
  
  child.view <- gtkTextViewNewWithBuffer(view$getBuffer(), show = F)

  # Event box is to add a black border around each child view
  event.box <- gtkEventBoxNew(show = F)
  color <- gdkColorParse("black")$color
  event.box$modifyBg("normal", color)

  align <- gtkAlignmentNew(0.5, 0.5, 1.0, 1.0, show = F)
  align$setBorderWidth(1)
  
  event.box$add(align)
  align$add(child.view)
  
  view$addChildAtAnchor(event.box, anchor)

  recursive.attach.view(depth + 1, child.view, anchor)
}

popup <- NULL

easter.egg.callback <- function(button, data)
{
  if (!is.null(popup))
    {
      popup$present()
      return()
    }
  
  buffer <- gtkTextBufferNew(NULL)

  iter <- buffer$getStartIter()$iter

  buffer$insert(iter, "This buffer is shared by a set of nested text views.\n Nested view:\n")
  anchor <- buffer$createChildAnchor(iter)
  buffer$insert(iter, "\nDon't do this in real applications, please.\n")

  view <- gtkTextViewNewWithBuffer(buffer, show = F)
  
  recursive.attach.view(0, view, anchor)

  popup <- gtkWindowNew("toplevel", show = F)
  sw <- gtkScrolledWindowNew(NULL, NULL)
  sw$setPolicy("automatic", "automatic")

  popup$add(sw)
  sw$add(view)

  popup$setDefaultSize(300, 400)
  
  popup$showAll()
}

create.tags <- function(buffer)
{
   # Create a bunch of tags. Note that it's also possible to
   # create tags with gtkTextTagNew() then add them to the
   # tag table for the buffer, gtkTextBufferCreateTag() is
   # just a convenience function. Also note that you don't have
   # to give tags a name pass NULL for the name to create an
   # anonymous tag.
   #
   # In any real app, another useful optimization would be to create
   # a GtkTextTagTable in advance, and reuse the same tag table for
   # all the buffers with the same tag set, instead of creating
   # new copies of the same tags for every buffer.
   #
   # Tags are assigned default priorities in order of addition to the
   # tag table.	 That is, tags created later that affect the same text
   # property affected by an earlier tag will override the earlier
   # tag.  You can modify tag priorities with
   # gtkTextTagSetPriority().
   #

  buffer$createTag("heading",
			      weight = PangoWeight["bold"],
			      size = 15 * PANGO_SCALE)
  
  buffer$createTag("italic",
			      style = "italic")

  buffer$createTag("bold",
			      weight = PangoWeight["bold"])  
  
  buffer$createTag("big",
			      size = 20 * PANGO_SCALE)

  buffer$createTag("xx-small",
			      scale = PANGO_SCALE_XX_SMALL)

  buffer$createTag("x-large",
			      scale = PANGO_SCALE_X_LARGE)
  
  buffer$createTag("monospace",
			      family = "monospace")
  
  buffer$createTag("blue.foreground",
			      foreground = "blue")  

  buffer$createTag("red.background",
			      background = "red")

  buffer$createTag("big.gap.before.line",
			      pixels_above_lines = 30)

  buffer$createTag("big.gap.after.line",
			      pixels_below_lines = 30)

  buffer$createTag("double.spaced.line",
			      pixels_inside_wrap = 10)

  buffer$createTag("not.editable",
			      editable = FALSE)
  
  buffer$createTag("word.wrap",
			      wrap_mode = "word")

  buffer$createTag("char.wrap",
			      wrap_mode = "char")

  buffer$createTag("no.wrap",
			      wrap_mode = "none")
  
  buffer$createTag("center",
			      justification = "center")

  buffer$createTag("right.justify",
			      justification = "right")

  buffer$createTag("wide.margins",
			      left_margin = 50, right_margin = 50)
  
  buffer$createTag("strikethrough",
			      strikethrough = TRUE)
  
  buffer$createTag("underline",
			      underline = "single")

  buffer$createTag( "double.underline",
			      underline = "double")

  buffer$createTag("superscript", rise = 10 * PANGO_SCALE, size = 8 * PANGO_SCALE)
  
  buffer$createTag("subscript", rise = -10 * PANGO_SCALE, size = 8 * PANGO_SCALE)

  buffer$createTag("rtl.quote", wrap_mode = "word", direction = "rtl",
			      indent = 30, left_margin = 20, right_margin = 20)
}

insert.text <- function(buffer)
{
  
  pixbuf <- NULL
  filename <- imagefile("rgtk-logo.gif")
  if (file.exists(filename))
    {
      pixbuf <- gdkPixbufNewFromFile(filename)[[1]]
    }

  if (is.null(pixbuf))
    {
      warning("Failed to load image file rgtk-logo.gif\n")
    }

  pixbuf <- pixbuf$scaleSimple(32, 32, "bilinear")

   # get start of buffer each insertion will revalidate the
   # iterator to point to just after the inserted text.
   
  iter <- buffer$getIterAtOffset(0)$iter

  buffer$insert(iter, "The text widget can display text with all kinds of nifty attributes. It also supports multiple views of the same buffer this demo is showing the same buffer in two places.\n\n")
  buffer$insertWithTagsByName(iter, "Font styles. ", "heading")
  
  buffer$insert(iter, "For example, you can have ")
  buffer$insertWithTagsByName(iter, "italic", "italic")
  buffer$insert(iter, ", ")
  buffer$insertWithTagsByName(iter, "bold", "bold")
  buffer$insert(iter, ", or ")
  buffer$insertWithTagsByName(iter, "monospace (typewriter)", "monospace")
  buffer$insert(iter, ", or ")
  buffer$insertWithTagsByName(iter, "big", "big")
  buffer$insert(iter, " text. ")
  buffer$insert(iter, "It's best not to hardcode specific text sizes you can use relative sizes as with CSS, such as ")
  buffer$insertWithTagsByName(iter, "xx-small", "xx-small")
  buffer$insert(iter, " or ")
  buffer$insertWithTagsByName(iter, "x-large", "x-large")
  buffer$insert(iter, " to ensure that your program properly adapts if the user changes the default font size.\n\n")
  
  buffer$insertWithTagsByName(iter, "Colors. ", "heading")
  
  buffer$insert(iter, "Colors such as ")  
  buffer$insertWithTagsByName(iter, "a blue foreground", "blue.foreground")
  buffer$insert(iter, " or ")  
  buffer$insertWithTagsByName(iter, "a red background", "red.background")
  buffer$insert(iter, " or even ")  
  buffer$insertWithTagsByName(iter,
                              "a blue foreground on a red background",
                              "blue.foreground", "red.background")
  buffer$insert(iter, " (select that to read it) can be used.\n\n")  

  buffer$insertWithTagsByName(iter, "Underline, strikethrough, and rise. ", "heading")
  
  buffer$insertWithTagsByName(iter, "Strikethrough", "strikethrough")
  buffer$insert(iter, ", ")
  buffer$insertWithTagsByName(iter, "underline", "underline")
  buffer$insert(iter, ", ")
  buffer$insertWithTagsByName(iter, "double underline", "double.underline")
  buffer$insert(iter, ", ")
  buffer$insertWithTagsByName(iter, "superscript", "superscript")
  buffer$insert(iter, ", and ")
  buffer$insertWithTagsByName(iter, "subscript", "subscript")
  buffer$insert(iter, " are all supported.\n\n")

  buffer$insertWithTagsByName(iter, "Images. ", "heading")

  buffer$insert(iter, "The buffer can have images in it: ")
  buffer$insertPixbuf(iter, pixbuf)
  buffer$insertPixbuf(iter, pixbuf)
  buffer$insertPixbuf(iter, pixbuf)
  buffer$insert(iter, " for example.\n\n")

  buffer$insertWithTagsByName(iter, "Spacing. ", "heading")

  buffer$insert(iter, "You can adjust the amount of space before each line.\n")
  
  buffer$insertWithTagsByName(iter,
					    "This line has a whole lot of space before it.\n",
					    "big.gap.before.line", "wide.margins")
  buffer$insertWithTagsByName(iter,
					    "You can also adjust the amount of space after each line this line has a whole lot of space after it.\n",
					    "big.gap.after.line", "wide.margins")
  
  buffer$insertWithTagsByName(iter,
					    "You can also adjust the amount of space between wrapped lines this line has extra space between each wrapped line in the same paragraph. To show off wrapping, some filler text: the quick brown fox jumped over the lazy dog. Blah blah blah blah blah blah blah blah blah.\n",
					    "double.spaced.line", "wide.margins")

  buffer$insert(iter, "Also note that those lines have extra-wide margins.\n\n")

  buffer$insertWithTagsByName(iter, "Editability. ", "heading")
  
  buffer$insertWithTagsByName(iter,
					    "This line is 'locked down' and can't be edited by the user - just try it! You can't delete this line.\n\n",
					    "not.editable")

  buffer$insertWithTagsByName(iter, "Wrapping. ", "heading")

  buffer$insert(iter,
			  "This line (and most of the others in this buffer) is word-wrapped, using the proper Unicode algorithm. Word wrap should work in all scripts and languages that GTK+ supports. Let's make this a long paragraph to demonstrate: blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah\n\n")  
  
  buffer$insertWithTagsByName(iter,
					    "This line has character-based wrapping, and can wrap between any two character glyphs. Let's make this a long paragraph to demonstrate: blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah\n\n",
					    "char.wrap")
  
  buffer$insertWithTagsByName(iter,
					    "This line has all wrapping turned off, so it makes the horizontal scrollbar appear.\n\n\n",
					    "no.wrap")

  buffer$insertWithTagsByName(iter, "Justification. ", "heading")  
  
  buffer$insertWithTagsByName(iter, "\nThis line has center justification.\n", "center")

  buffer$insertWithTagsByName(iter, "This line has right justification.\n", "right.justify")

  buffer$insertWithTagsByName(iter,
					    "\nThis line has big wide margins. Text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text.\n",
					    "wide.margins")  

  buffer$insertWithTagsByName(iter, "Internationalization. ", "heading")
	  
  buffer$insert(iter,
			  "You can put all sorts of Unicode text in the buffer.\n\nGerman (Deutsch S\303\274d) Gr\303\274\303\237 Gott\nGreek (\316\225\316\273\316\273\316\267\316\275\316\271\316\272\316\254) \316\223\316\265\316\271\316\254 \317\203\316\261\317\202\nHebrew	\327\251\327\234\327\225\327\235\nJapanese (\346\227\245\346\234\254\350\252\236)\n\nThe widget properly handles bidirectional text, word wrapping, DOS/UNIX/Unicode paragraph separators, grapheme boundaries, and so on using the Pango internationalization framework.\n")  

  buffer$insert(iter, "Here's a word-wrapped quote in a right-to-left language:\n")
  buffer$insertWithTagsByName(iter, "\331\210\331\202\330\257 \330\250\330\257\330\243 \330\253\331\204\330\247\330\253 \331\205\331\206 \330\243\331\203\330\253\330\261 \330\247\331\204\331\205\330\244\330\263\330\263\330\247\330\252 \330\252\331\202\330\257\331\205\330\247 \331\201\331\212 \330\264\330\250\331\203\330\251 \330\247\331\203\330\263\331\212\331\210\331\206 \330\250\330\261\330\247\331\205\330\254\331\207\330\247 \331\203\331\205\331\206\330\270\331\205\330\247\330\252 \331\204\330\247 \330\252\330\263\330\271\331\211 \331\204\331\204\330\261\330\250\330\255\330\214 \330\253\331\205 \330\252\330\255\331\210\331\204\330\252 \331\201\331\212 \330\247\331\204\330\263\331\206\331\210\330\247\330\252 \330\247\331\204\330\256\331\205\330\263 \330\247\331\204\331\205\330\247\330\266\331\212\330\251 \330\245\331\204\331\211 \331\205\330\244\330\263\330\263\330\247\330\252 \331\205\330\247\331\204\331\212\330\251 \331\205\331\206\330\270\331\205\330\251\330\214 \331\210\330\250\330\247\330\252\330\252 \330\254\330\262\330\241\330\247 \331\205\331\206 \330\247\331\204\331\206\330\270\330\247\331\205 \330\247\331\204\331\205\330\247\331\204\331\212 \331\201\331\212 \330\250\331\204\330\257\330\247\331\206\331\207\330\247\330\214 \331\210\331\204\331\203\331\206\331\207\330\247 \330\252\330\252\330\256\330\265\330\265 \331\201\331\212 \330\256\330\257\331\205\330\251 \331\202\330\267\330\247\330\271 \330\247\331\204\331\205\330\264\330\261\331\210\330\271\330\247\330\252 \330\247\331\204\330\265\330\272\331\212\330\261\330\251. \331\210\330\243\330\255\330\257 \330\243\331\203\330\253\330\261 \331\207\330\260\331\207 \330\247\331\204\331\205\330\244\330\263\330\263\330\247\330\252 \331\206\330\254\330\247\330\255\330\247 \331\207\331\210 \302\273\330\250\330\247\331\206\331\203\331\210\330\263\331\210\331\204\302\253 \331\201\331\212 \330\250\331\210\331\204\331\212\331\201\331\212\330\247.\n\n",
						"rtl.quote")
      
  buffer$insert(iter, "You can put widgets in the buffer: Here's a button: ")
  anchor <- buffer$createChildAnchor(iter)
  buffer$insert(iter, " and a menu: ")
  anchor <- buffer$createChildAnchor(iter)
  buffer$insert(iter, " and a scale: ")
  anchor <- buffer$createChildAnchor(iter)
  buffer$insert(iter, " and an animation: ")
  anchor <- buffer$createChildAnchor(iter)
  buffer$insert(iter, " finally a text entry: ")
  anchor <- buffer$createChildAnchor(iter)
  buffer$insert(iter, ".\n")
  
  buffer$insert(iter, "\n\nThis demo doesn't demonstrate all the GtkTextBuffer features it leaves out, for example: invisible/hidden text (doesn't work in GTK 2, but planned), tab stops, application-drawn areas on the sides of the widget for displaying breakpoints and such...")

  # Apply word.wrap tag to whole buffer
  bounds <- buffer$getBounds()
  buffer$applyTagByName("word.wrap", bounds$start, bounds$end)
}

find.anchor <- function(iter)
{
  while (iter$forwardChar())
    {
      if (!is.null(iter$getChildAnchor()))
        return(TRUE)
    }
  return(FALSE)
}

attach.widgets <- function(text.view)
{
  buffer <- text.view$getBuffer()

  iter <- buffer$getStartIter()$iter

  i <- 0
  while (find.anchor(iter))
    {
      anchor <- iter$getChildAnchor()

      if (i == 0)
        {
          widget <- gtkButtonNewWithLabel("Click Me")

          gSignalConnect(widget, "clicked", easter.egg.callback)
        }
      else if (i == 1)
        {
          widget <- gtkComboBoxNewText()

          widget$appendText("Option 1")
          widget$appendText("Option 2")
          widget$appendText("Option 3")
        }
      else if (i == 2)
        {
          widget <- gtkHScaleNew(NULL)
          widget$setRange(0, 100)
          widget$setSizeRequest(70, -1)
        }
      else if (i == 3)
        {
			filename <- "/usr/share/gtk-2.0/demo/floppybuddy.gif"
			widget <- gtkImageNewFromFile(filename)
	    }
      else if (i == 4)
        {
          widget <- gtkEntryNew()
        }
      
      text.view$addChildAtAnchor(widget, anchor)

      widget$showAll()

      i = i + 1
    }
}

window <- NULL

window <- gtkWindowNew("toplevel", show = F)
window$setDefaultSize(450, 450)
      
window$setTitle("TextView")
window$setBorderWidth(0)

vpaned <- gtkVPanedNew()
vpaned$setBorderWidth(5)
window$add(vpaned)

# For convenience, we just use the autocreated buffer from
# the first text view you could also create the buffer
# by itself with gtkTextBufferNew(), then later create
# a view widget.

view1 <- gtkTextViewNew()
buffer <- view1$getBuffer()
view2 <- gtkTextViewNewWithBuffer(buffer)
      
sw <- gtkScrolledWindowNew(NULL, NULL)
sw$setPolicy("automatic", "automatic")
vpaned$add1(sw)

sw$add(view1)

sw <- gtkScrolledWindowNew(NULL, NULL)
sw$setPolicy("automatic", "automatic")
vpaned$add2(sw)

sw$add(view2)

create.tags(buffer)
insert.text(buffer)

attach.widgets(view1)
attach.widgets(view2)
      
window$showAll()
