
# rgl  0.106.8

## Minor changes

* Support for FreeType has been dropped in the experimental
  Windows Unicode (UCRT) build.
  
## Bug fixes 

* Some of the changes related to avoiding `testthat` errors
  in other files accidentally introduced a new error 
  in coloring meshes in `rgl`:  now fixed.
* readOBJ() was broken by the 0.106.x changes.
* merge.mesh3d() failed for meshes containing points or
  segments.
  
# rgl  0.106.6

## Major changes

*  Changes for `pkgdown` compatibility have been added.  See `vignette("pkgdown")`. 
*  Added `drape3d()`, to "drape" objects across a mesh.
   (Contributed by George Helffrich.) 
*  Added `shadow3d()` to project one mesh onto another. 
*  Added `facing3d()` to subset a mesh to parts facing 
   in a particular direction. 
*  Meshes may now include points and line segments as well
   as triangles and quads.  The arguments to `as.mesh3d.default()` 
   have changed accordingly, and a new function `mesh3d()`
   has been added. 
*  Reformatted the `inst/NEWS` file so it is visible here.
*  Added `asHomogeneous2()` and `asEuclidean2()` to work directly
   with `3 x n` and `4 x n` matrices. 
*  Added `rglExtrafonts()` to load additional fonts for
   use with FreeType rendering.  Requires the `extrafont` package.
   
## Minor changes

*  The help pages have been edited to continue to 
   de-emphasize `rgl.*` functions. 
*  Changes have been made for compatibility with the 
   experimental Windows-UTF8 build of R. 
*  Allowed infinite values for strip limits in `filledContour3d()`. 
*  Setting material property `point_antialias = TRUE` now
   gives round points in `rglwidget()` displays. 
*  The reuse argument in `rglwidget()` is no longer used. 
*  Sphere initialization in WebGL displays is now done
   entirely in Javascript. 
*  Set "window_group" in X11 so `rgl` windows are grouped,
   based on code by Ivan Krylov. 
*  `filledContour3d()` now accepts levels in decreasing order. 
*  `mergeVertices()` and `as.mesh3d.rglId()` have been improved.
*  `r3dDefaults$useFreeType` is now set to `FALSE` on 
Windows, so that system fonts will be used by default.
*  Text `family = "symbol"` has never really worked,
   and is no longer recommended.
*  Added `compare_proxy` method in case a recent `testthat`
   is being used.

## Bug fixes 

*  The width and height of an `rglwidget()` once again adapt
   to the viewer window in RStudio (issue #74). 

# rgl  0.105.22 

## Minor changes

*  Add `Biarch` to `DESCRIPTION` so both architectures are built on 
   Windows. 

## Bug fixes

*  Fixed error in new args to `snapshot3d()` (reported by Tony Hirst:
   https://github.com/dmurdoch/rgl/issues/21 .) 
*  Improved test for presence of WebGL (suggested by Git Demont, 
   https://github.com/dmurdoch/rgl/issues/31 ). 
*  On macOS an interaction between `rgl` and the `quartz()` device
   caused a segfault (see issue #27).  Added workaround.
   (reported by Rich Heiberger, 
   https://github.com/dmurdoch/rgl/issues/27). 
*  Fixed a bug affecting fat (`lwd > 1`) line segments in 
   `rglwidget()`. 
*  A bug in the `Makevars` files caused builds using a 
   parallel make to fail. 
*  A bug in conversion of displays to WebGL prevented planes
   from displaying properly. 
*  In `rglwidget()`, background images could cause other parts of
   the display to disappear. 

# rgl  0.105.13 

## Major changes

*  Inclusion in `knitr` documents will now be simplified
   in versions of `knitr` that incorporate its PR#1892. 
*  Added `webshot` argument to `snapshot3d()`, to use the
   `webshot2` package (currently only available from
   Github; see `?snapshot3d` for details) to produce snapshots
   even on headless systems. 
*  Moved development home from R-forge to Github. 
   
## Minor changes

*  Windows builds now download Freetype from `rwinlib`
   during the build.  (Contributed by Jeroen Ooms.) 
*  `shinySetPar3d()` now accepts a list, as returned in
   `input$par3d` by `shinyGetPar3d()`, as input.  (Suggestion
   of Yohann Demont.) 
*  The default color scheme for `filledContour3d()` changed in
   R versions previous to 3.6.0, as `hcl.colors()` didn't 
   exist in those versions.  (Reported by Daniel Baston.) 
*  Testing shows that with the above change, `rgl` will now
   work in R versions from 3.3.0 up. 
*  `snapshot3d()` now defaults to writing to a temporary file
   instead of failing if no filename is given. 
*  Both `snapshot3d()` and `rgl.snapshot()` now return the 
   output filename invisibly. 
*  `rglwidget()` no longer tries to include both a snapshot and
   a WebGL scene:  it can only do one or the other. 
*  Now builds the non-OpenGL DLL and puts it in `inst/useNULL`,
   so `options(rgl.useNULL=TRUE)` before loading `rgl` will cause
   it to not use X11 at all. 
*  Made the startup code more resilient in case X11 isn't
   working. 
*  Set up a `drat` repository to hold the unreleased `webshot2`
   package. 
   
## Bug fixes

*  Some bugs in `thigmophobe3d()`, `mergeVertices()` and 
   `as.mesh3d.default()` have been fixed. 

# rgl  0.104.16 

## Minor changes

*  Added `--disable-opengl` configure option to run entirely without
   OpenGL (to support Apple M1 machines without GLX,
   and others which don't have X11 or OpenGL devel
   files installed). 
*  Added explicit typecasts to suppress compile warnings. 
*  Restored some of the Windows configuration from pre-0.101.2
   to allow use on older R versions. 
*  Dropped use of `mathjaxr`, which caused issues on Debian. 
*  Experimental support for handling mouse selection in Shiny
   added, along with `"shinyMouse"` demo. 
*  The result of `open3d()` now has class `"rglOpen3d"`,
   and `knitr` will use this during auto-plotting. 
   
## Bug fixes

*  Fixed bug in `rglwidget()` that caused it to fail to display
   text if too many strings were in the same object.
   (Reported by Yohann Demont.) 
*  Fixed some small bugs, found by `lintr`. 
*  Fixed bugs in Shiny support, and moved Shiny demo code into
   single files in demo directory. 
*  Fixed bugs in `addNormals.mesh3d()` method, added `angleWeighted`
   argument, defaulting to `TRUE`. 
*  Fixed bugs in `rglwidget()` displays of transparent spheres. 

# rgl  0.103.5 

## Major changes

*  Added `clipObj3d()`, `contourLines3d()` and `filledContour3d()` functions. 
*  Modified `clipMesh3d()` function to make it more consistent
   with the above three functions.  The main incompatibility
   with the version in 0.100.26 is that only vertex
   coordinates are now passed to the clipping function. 

## Minor changes

*  Add `merge()` method for `"mesh3d"` objects, and use it in
   `filledContour3d()`. 
*  More deprecation of older `writeWebGL()` style controls. 
*  Add extra `knitr` hooks, so support for `rgl` should be
   very similar to support for standard graphics output. 
*  Major rewrite of the WebGL code so that transparency
   is handled better in `rglwidget()`.  It has also been
   split into multiple files which are joined with 
   "minification" on installation. 
*  Added utility function `makeDependency()` to support 
   Javascript library in source. 
*  WebGL code now supports fog in scenes.  The default
   `r3dDefaults` now sets `material$fog` to `TRUE`, and `bg$fog` to
   `"none"`.  (In `rgl`, fog must be set *both* in the background
   and in the object to display.)
   The formula used in WebGL is slightly different than in the 
   internal R display.)  
*  `getr3dDefaults()` now has two optional arguments to specify
   what to retrieve, e.g. `getr3dDefaults("material", "color")`
   to retrieve `r3dDefaults$material$color`, with NULL 
   if either part is missing. 
*  Added `fogScale` parameter to `bg3d()` and `rgl.bg()` to
   allow increased or decreased fog. 
*  Added `fastTransparency` parameter to `spheres3d()` and
   `rgl.spheres()`, to allow them to be drawn more quickly
   when transparency is used. 
*  `"mesh3d"` methods for `shade3d()`, `wire3d()`, and `dots3d()`
   have been rewritten to share code, allowing meshes
   to have different front and back material properties. 
*  New functions `cur3d()`, `set3d()`, `close3d()` and `ids3d()` have been 
   added.  Generally, users should use these rather than
   `rgl.cur()`, `rgl.set()`, `rgl.close()` and `rgl.ids()`. 
*  `snapshot3d()` now has optional width and height parameters for the saved snapshot. 
*  the cursor now reflects the mouse mode in `rglwidget()`
   displays. 
*  Texture coordinates in mesh objects now act the same
   as colors with respect to the `meshColor` variable. 
*  Touch events are now supported in WebGL. 
*  Added `"snapshot"` `knitr` option to use when autoprinting. 
*  Added defaults to `snapshot3d(width = NULL, height = NULL)`. 
*  Added `as.mesh3d.rglobject()` method. 
*  Added `clip_to_density` argument to `plot3d.lm()` method. 
*  The build files have been updated to work with Rtools40
   on Windows. 
*  `rglwidget()` now saves a copy of the original scene,
   so it can be reconstructed or modified later. 
   
## Bug fixes

*  Fixed some memory leaks found by `valgrind`, and problems seen
   on systems with no functional Asymptote or Pandoc. 
*  A bug in the initial color of a mesh object has been fixed. 
*  A bug in translating mouse coordinates (reported on 
   StackOverflow by Richard Morey) when an `rgl` widget is
   included in a `Gitbook` has been fixed.  
*  Modified `writeASY()` for compatibility with Asymptote
   2.65.  (Reported by Pavel Stříž.) 
*  `pop3d()` has been modified slightly so that it no
   longer opens a new window if none is already present 
*  added `setGraphicsDelay()` function to work around bug
   in macOS Catalina XQuartz. 
*  Made various improvements to reduce notes and warnings
   during install, including suppressing deprecated OpenGL
   warnings on macOS. 
*  Some declarations in WebGL made assumptions that were
   not valid on mobile devices. 
*  The `"depth_mask"` material property was being ignored
   in `rglwidget()`. 
*  `rgl.snapshot()` and `rgl.postscript()` could crash if a zero
   length filename was passed to them. 

# rgl  0.100.54 

## Minor changes

*  Changed `rgl.attrib(id, "normals")` so the normals will be returned
   whether or not the object is lit.  (Suggestion of Tyler 
   Morgan-Wall) 
*  The labels used in `rglwidget()` are now independent of `set.seed()`,
   using code borrowed from Shiny for our own private RNG. 
*  `getr3dDefaults()` now gets values from `rgl::r3dDefaults`
   if they are not present in the user's `r3dDefaults` list. 
*  `bgplot3d()` now uses the background colour from argument
   `bg.color` (defaulting to the background color from
   `getr3dDefaults()`) rather than always choosing white.
*  The maintainer email address has been changed to 
   murdoch.duncan@gmail.com. 
   
## Bug fixes

*  Fixed bug in `plot3d.rglscene()` that caused restored subscenes to 
   ignore the mouse. 
*  `next3d()` no longer messes up when a user changes active
   subscenes. 
*  If a sufficient version of Pandoc is not found, the 
   vignettes will still run, but won't execute any `rgl`
   code. 

# rgl  0.100.50 

## Minor changes

*  Added `?rgl.init` help topic to describe initialization
 issues. 
*  Added sanity check to setting of `par3d("windowRect")`. 

## Bug fixes

*  Rewrote the initialization code to deal with problems
 related to indirect GLX and `Xvfb`. 


# rgl  0.100.47 

## Minor changes

*  `demo(stereo)` now uses `plot.raster()` rather than `image()`. 
*  Added a section on textures to the main vignette. 
*  The configure script has been updated. 
*  The functions in the `tkrgl` package have been moved into `rgl`. 
*  Demo tests are suppressed when run with the `rgl` null device. 
*  The `anaglyph()` function in the `"stereo"` demo now prints
   information about failed pixel reads. 
*  Included textures have been compressed (and in some cases
   repaired). 
*  The tests of the demos have been moved to `inst/slowTests` so that
   running them is optional (and the CRAN checks will go faster).
   
## Bug fixes

*  Fixed a bug in `readOBJ()` that affected reading texture coordinates. 
*  `rgl.pixels()`, `rgl.snapshot()` and `snapshot3d()` now read from the
   back buffer, which should increase reliability. 
*  Fixed bug when setting `windowRect`:  `viewport` was not
   always updated. 
*  Fixed bug in handling mouse wheel events:  they were
   not directed to the correct subscene. 
*  Fixed bug in configure script for systems with `pkg-config` 
   but no freetype2. 
*  Fixed bug that caused `bg3d()` and `bgplot3d()` to wipe out
   fog setting. 
*  Fixed `writeASY()` to work with a more recent version of 
   Asymptote.  Use `ver244 = TRUE` for the older version. 
*  `plot3d(..., type = "s", add = TRUE)` chose a bad default
   radius for the spheres -- possibly even zero. 
*  `planes3d()` could fail to draw the plane if it intersected a
   vertex of the bounding box of the scene. 
*  In Shiny, controllers like `rglMouse()` did not automatically
   link to an `rglwidget()`. 

# rgl  0.100.30 

## Minor changes

*  Added `meshColor` as an argument to `tmesh3d()`, `qmesh3d()`
   and `dot3d()`; changed default to no longer give warning if
   `meshColor` is not specified. 
*  Added `all.equal()` method for `"mesh3d"` objects, so that
   changes caused by the above are ignored. 
*  Added `tri_to_keep` argument to `as.mesh3d.ashape3d()` for
   compatibility with conflicting method from `nat` package
   version 1.8.11. 
*  Removed deprecated C++ functions `std::bind2nd` and `std::ptr_fun`
   as requested by CRAN.  Other changes to remove compile
   warnings also made. 

# rgl  0.100.26 

## Major changes

*  added `clipMesh3d()` to allow smooth clipping of mesh objects 
*  Made `plot3d.lm()` method handle a larger variety of models, by
   allowing for curved surfaces. 
*  Added `as.mesh3d.default()` method to convert triangles or quads
   to a `"mesh3d"` object. 
*  Added `as.triangles3d()` generic with methods to convert `"mesh3d"`
   objects into matrices representing triangles. 
*  Added `as.triangles3d.rglId()` and `as.mesh3d.rglId()` methods to
   convert displayed objects to usable data. 
   
## Minor changes

*  `open3d()` now signals an error if unnamed parameters are used 
*  `toggleWidget()` now makes it easier to initialize the scene with
   some objects hidden.
   
## Bug fixes

*  Fixed the startup code so that systems that don't provide
   `uname` still work.  (Suggestion of Settra Khemri.) 

# rgl  0.100.24 

## Bug fixes

*  Fix `thigmophobe3d()` to try to keep up with changes in 
   `plotrix::thigmophobe()`. 
*  Stop `rgl.postscript()` from writing files to current directory 

# rgl  0.100.19 

## Bug fixes

*  Fix some bugs detected by `valgrind` 

# rgl  0.100.18 

## Major changes

*  Added `shinyGetPar3d()` and `shinySetPar3d()` functions for Shiny
   interaction. 
*  Added `thigmophobe3d()` function to place labels away from
   other points using `plotrix::thigmophobe()`. 
*  Added `arc3d()` function to draw spherical arcs. 
*  Added `"polygon_offset"` material property, to allow lines to be drawn
   on surfaces. 
*  Added `plot3d()`, `persp3d()` and `as.mesh3d()` methods for
   `"triSht"` and `"tri"` classes (produced by `interp` and `tripack`
   packages.) 
*  `plot3d()` methods for objects of class `"formula"` and `"lm"`
   and a `persp3d()` method for objects of class `"formula"`
   have been added.  (A bug in the implementation of
   `as.mesh3d.deldir()` was found and fixed during the 
   latter addition.) 
*  `as.mesh3d()`, `plot3d()` and `persp3d()` methods for `"ashape3d"`
   objects from the `alphashape3d` package have been added. 
*  The mouse mode (trackball, zoom, etc.) can now be applied
   separately to each individual subscene in a scene.
   (By default the mode is inherited from the root subscene.) 
*  Added `par3d("userProjection")`, to allow the user to supply
   a change to the projection after all other display calculations 
   have been done. 
*  Added `par3d("activeSubscene")`, to allow mouse callback
   functions to determine which subscene was clicked. 
   
## Minor changes

*  Added check for `"highp"` support to fragment shader in `rglwidget()`.
*  Updated `text3d()` and related functions:  dropped deprecated
   argument `justify`, added `pos` and `offset` like base graphics
   `text()`. 
*  Improved support of `"mesh3d"` objects:  added print methods,
   added `meshColor` argument to `wire3d()` and `shade3d()` to control how
   colors are interpreted, added `"rgl.meshColorWarning"`
   option to control warnings about these changes.
*  The `plot3d.mesh3d()` method now has the same default
   for `aspect` as the default method. 
*  `pch3d()` now allows separate `color` and `bg` specifications
   for each point.  In addition, the default for the `"lit"`
   material property is now `FALSE`, so by default filled
   symbols will match the requested colour regardless of 
   lighting. 
*  Minor fix ups to the vignettes. 
*  Now uses the `manipulateWidget::combineWidgets` function
   when putting multiple objects into a pipe. 
*  Now accepts fixed CSS units in width and height for `rglwidget()`. 
*  `playwidget()` is no longer an S3 generic function. 
*  The configure code to detect freetype has been updated
   to use `pkg-config` (code contributed by Dirk Eddelbuettel.) 
*  If a `playwidget()` has been initialized but it can't find
   the `rglwidget()` that it is controlling (likely due to a 
   typo somewhere), it now throws an alert message. 
   
## Bug fixes

*  Fixed texture bug introduced in fix in 0.99.16. 
*  The `persp3d.deldir()` method didn't display labels properly. 
*  When the X11 initialization failed, `rgl` messed up the S3
   methods system.  (Reported by Gregory Jefferis.) 
*  Probably due to a compiler change, `rgl.bbox()` was 
   returning 0/1 instead of the id of the axes. 
*  `pch3d()` was failing in `rglwidget()` for some shapes.
   (Reported by Luca Scrucca.) 
*  `par3d(mouseMode = "none")` was not implemented properly,
   so appeared to be a no-op. 
*  Selection functions did not work well with subscenes. 
*  Deleting an object that has been added as a 3D sprite
   caused `rgl` to crash. 
*  A number of memory bugs found by `rchk` have been fixed. 
*  Textures specified in global material list (e.g. by being
   used in `rgl.*` functions) were not handled properly.
   (Reported by Ty Tuff.) 

# rgl  0.99.9 

## Major changes

*  Added support for communication with other widgets
   using the `crosstalk` package. See `?rglShared` and
   `vignette("WebGL")` for details. 
*  Added the `rglMouse()` function to allow the mouse
   mode to be changed in a WebGL display.
   
## Minor changes

*  Christophe Geuzaine's GL2PS library (used by `rgl.postscript()`)
   updated to version 1.4.0. 
*  The Pandoc system requirement has been updated to
   1.14, as 1.13.1 is no longer sufficient. 
   
## Bug fixes

*  Fixed a bug causing the `rglwidget()` to fail to work
   in a `flexdashboard()` display. 
*  Fixed a bug in Shiny interaction 
*  Changed WebGL text rendering to avoid overloading
   browser. 
*  Sphere rendering within R sometimes showed strange
   artifacts. 

# rgl  0.98.22 

## Minor changes

*  Record context (`ioslides`, `shiny`, etc.) in scene 
   when `rglwidget()` is called. 
*  Allow more than 16 scenes in `html_document`, `ioslides_presentation` and
   `slidy_presentation`. 
*  `useSubscene3d()` now returns the id of the previously active subscene,
   to make temporary changes more convenient.
*  `renderRglwidget()` and `renderPlaywidget()` now have an optional argument
   `outputArgs` for use in dynamic R Markdown documents. 
*  `rglwidget()` now warns if an object has too many vertices.
*  added an approximation to "polar" mouse controls to WebGL display. 
*  the `"centers"` attribute now refers to the individual facets
   of spheres, rather than the whole sphere.  Use `"vertices"` for
   that. 
*  Tried to give a more helpful startup error message on macOS. 
*  Added documentation to `rglwidgetClass` in Javascript. 
*  `vertexSetter()` can now set plane parameters. 
*  Modified `platform.cpp` so it works with `__STRICT_ANSI__`
   defined. 
*  As many browsers have dropped support for setting line width 
   in WebGL scenes, this has been redone in `rglwidget()` 
   code using a vertex shader.  
   Line endings and joins are rounded, not squared as in OpenGL. 
*  The 65535 vertex limit has been removed (at least in browsers
   that support big indices). 
*  The requirement that colors being controlled by an `ageControl()` or `vertexControl()`
   be duplicated in the original has been removed. 

## Bug fixes
   
*  The rendering order is changed:  now all opaque objects are drawn first,
   then all transparent objects.  Previously this ordering was only done
   within subscenes, leading to rendering errors when transparent objects
   in one subscene were drawn before opaque objects in another. 
*  transparent spheres sometimes showed rendering
   artifacts because they were not drawn from back to front.  (Reported by Atte Tenkanen; original fix improved so nested
   spheres should now work.  WebGL display could still be
   improved.) 
*  `par3dinterp()` did not always choose the best direction for interpolation
   of the `userMatrix`. 
*  The `toggleWidget()` function didn't work properly in Shiny. 
*  Fixed addition of attribute to NULL.
*  Fixed bug where textures or normals caused `readOBJ()` to fail;
   added support for reading normals and texture coordinates. 
*  `axes3d("bbox")` didn't send parameters to `bbox3d()`. 
*  Fixed examples for `snapshot3d()` and `writeASY()` so that they don't
   change the working directory. 

# rgl  0.98.1 

## Minor changes

*  Cleaned up configure script. 
*  Cleaned up dynamic entry points. 
*  Added `add = FALSE` argument to `persp3d.deldir()`. 
*  `"shiny.tag"` objects are now supported as inputs to
   `playwidget()`, so that `rglwidget()` values can be 
   wrapped in `htmltools::div()` to set their style. 
*  Added `figWidth()` and `figHeight()` functions for sizing
   `rgl` plots in R Markdown documents. 

## Bug fixes

*  `layout3d()` handled multi-row cells incorrectly.  (Reported
   by Felix Carbonell.) 
*  Fixed a bug in `subsetControl()`, and added
   `toggleWidget()` 
*  Renamed the `texture` argument to `persp3d.function()`
   to `texcoords` for consistency with other functions, and
   to avoid a collision with the `"texture"` material property. 
*  Fixed bug in scene initialization that sometimes caused it
   to ignore initial control values. 

# rgl  0.97.0 

## Major changes

*  Added `plotmath3d()` function, and set `text3d()` to
   use it when necessary. 


## Minor changes

*  Added `fixedSize` argument to `rgl.sprites()` and related
   functions. 
*  ` material3d()` now silently ignores attempts to set
   read-only properties. 
*  Added `setUserShaders()` for user-specified shaders
   (currently for WebGL only). 
*  Added support for two-sided surfaces in WebGL. 
*  Added `demo("rglExamples")` to display all the examples
   in the `rgl` help in a collection of web pages.
   This showed up a number of small bugs, which have been
   fixed. 
*  `movie3d()` now optionally tries the R `magick` package first,
   then the external ImageMagick v7 command `magick`
   before trying `convert`.  (The external change
   suggested by Earl F. Glynn.) 
*  `par3d()` reports on the version of OpenGL that it sees
   (as component `"glVersion"`). 
   
## Bug fixes

*  Fixed bug in conversion of bounding box decorations
   used in `rglwidget()`. 
*  `addNormals()` gave an error if the mesh it was working
   with had degenerate triangles or quads.  (Reported
   by Rolf Turner and Graham Griffiths.) 
*  Auto-clipping sometimes changed result vectors into
   lists. 
*  The controllers did not recycle some values correctly. 
*  Fixed bug in initialization of `playwidget()`s. 
*  Fixed some bugs in `pch3d()` (reported by Gina Joue). 

# rgl  0.96.0 

## Major changes

*  Added `as.mesh3d()` and `plot3d.deldir()` and `persp3d.deldir()` methods to
   allow plotting of surfaces defined by irregular collections of points.
*  Added `rglToLattice()` and `rglToBase()` functions to compute Euler angles
   for the `lattice::wireframe()`, `lattice::cloud()`, and base graphics `persp()` functions.  
*  Added `arrow3d()` (based on the function of the same name in the
   `heplots` package). 
*  Added `pch3d()` to give an approximation
to plotting symbols using `pch=<number>` in
base graphics. 
*  Added support for control of multiple subscenes
   to `spin3d()`, `par3dinterp()`, `play3d()` and `movie3d()`. 
*  Added experimental function `writeASY()` for output in Asymptote format,
   which will allow inclusion in PDF files. 
*  Added `rgl.attrib.info()` to display information about
   object attributes. 
*  Merged `rglwidget` code back into `rgl`. 
*  Functions that modify the scene now return their
   value with class `"rglLowlevel"` or `"rglHighlevel"` (using the new `lowlevel()`
   or `highlevel()` functions) to indicate that a low- or high-level plotting function
   has been called.  If the new option 
   `"rgl.printRglwidget"` is `TRUE`, printing objects
   of either class will trigger automatic
   printing of the `rgl` scene using `rglwidget()`. 
   
## Minor changes

*  Gave better error when XQuartz is not found, tried for better test. 
*  Added more information on backgrounds to `scene3d()` to allow them to be used in `rglwidget()`. 
*  Now uses forward slashes in `rgl.postscript(fmt = "tex")` generated code.
   (Thanks to Uwe Ligges for the problem report.) 
*  `cylinder3d()` now defaults to a rotation minimizing local frame. 
*  Added this NEWS file.
*  Added better support for backgrounds. 
*  Added support for orthographic projections (`FOV = 0`). 
*  Added simple Shiny demo using tabs. 
*  Added version dependency for `jsonlite` so that the
   new faster matrix code will be used. 
*  The worker functions used by `subdivision3d()` have
   been exported for use on their own. 
*  The `rglwidget()` code now supports textures on spheres.  
   It now uses the same mesh as the one used inside R. 
   (The lack of support was pointed out by Justin McManus.) 

## Bug fixes
 
*  Background clearing was not handled properly.  (Thanks to Paul Morse
   for a bug report on this.) 
*  Fixed bug in rendering unlit 3D sprites. 
*  Web browsers only support a finite number of active
   WebGL sessions; `rglwidget()` code now works to make more
   careful use of this finite resource, so that large
   numbers of `rgl` scenes can be present on a single
   web page without exhausting it. 

# rgl  0.95.1441 

## Bug fixes

*  Changed `rgl.pixels()` to attempt to avoid segfault on
   OSX.  (Thanks to Greg Jefferis for testing and workaround.) 

# rgl  0.95.1435 

## Major changes

*  The Mac OS X native windowing system (`aglrgl.so`) has been
   dropped; it appears not to work in Yosemite and El Capitan. 
*  WebGL code has been moved to the `rglwidget` package (though
   the functions in `rgl` still work). 

## Minor changes

*  If `rgl.init()` fails, continue with the `NULL` device (with warnings). 
*  `scene3d()` now returns the normals and offsets of "planes"
   objects, as with "clipplanes" objects.  It still returns the triangles from embedding the
   planes in the most recent subscene. 
   
## Bug fixes

*  A memory leak when drawing semi-transparent objects has been
   fixed.  (Reported by Felix Kuehnl.) 
*  Bounding box objects sometimes had miscalculated vertices
   in `scene3d()`. 

# rgl  0.95.1367 

## Major changes

*  Added `show2d()` to allow a 2d plot on a quadrilateral
   in a scene. 
*  Added `matrixSetter()` function to allow multiple controls to
   modify one matrix. 
*  Added `vertexSetter()` function to allow easier access to
   vertex attributes.
   
## Minor changes

*  Made error and warning text more consistent. 
*  Dropped chunk option `"rgl.keepopen"`; replaced it
   with `"rgl.newwindow"`. 
*  Added `accumulate` argument to the subset WebGL controls. 
*  The `nticks` argument to `bbox3d()` was never used and has
   been removed.  Use `xlen`, `ylen` or `zlen`. 
*  Dependencies and imports have been updated. 
*  Used Jeroen Ooms' `js::jshints()` function to clean up the
   WebGL Javascript code. 
*  Allowed `values = NULL` in `propertySetter()` and `vertexSetter()`
   to allow code to directly set values. 
*  Shaders are now stored in Javascript strings, not separate
   script sections. 
*  Shape centers are now stored by `scene3d()`. 
*  Font family and numeric font number (style) are now returned
   by `rgl.attrib()` and are stored by `scene3d()`.

## Bug fixes

*  Fixed bug that sometimes prevented textures from displaying. 
*  `rgl.bbox()` (and hence `bbox3d()`, `decorate3d()`, `plot3d()`, etc.)
   did not return the correct id for the bounding box decoration. 
*  Modified configure script to work with OS X 10.11 (suggestion of
   Brian Ripley). 
*  Setting `xlen` etc. to zero in `bbox3d()` or `rgl.bbox()` now
   (correctly) suppresses tick marks. (Reported by
   Philipp Angerer.) 
*  Specifying `normals` or `texcoords` in both a
   `"mesh3d"` object and a call to `shade3d()` to display
   it caused an error; now the `shade3d()` specified
   value has priority if `override = TRUE` (the default). 
*  When used with clipping on the bounds, `persp3d()` and `plot3d()`
   did not work properly with a shared mouse.  (Reported by
   Marian Talbert.) 
*  Fixed a bug (reported by Dominick Samperi) that caused
   vignettes using WebGL code in `knitr` to fail to initialize
   properly.  This required adding the `setupKnitr()` function,
   which should be called at the start of each vignette.
   It is *not* called automatically. 
*  Fixed a bug (reported by Kurt Hornik) that caused `rgl` to
   fail to compile when `libfreetype` 2.6 was linked. 
*  Fixed a bug in `writePLY()` (reported by Kurt Hornik). 

# rgl  0.95.1247 

## Major changes

*  Added `subsetSlider()`, `subsetSetter()`, `clipplaneSlider()`,
   `propertySlider()`, `ageSetter()`, `propertySetter()`, `par3dinterpSetter()` and
   `toggleButton()` functions to output HTML/Javascript controls for WebGL. 
*  Added `hook_rgl()` and `hook_webgl()` functions, based on the `knitr`
   functions. 
*  Added clipping regions to `plot3d()` and `persp3d()`. 
*  Export the `GramSchmidt()` function (request of Remko Duursma) 
*  Added `readOBJ()`, with a very limited ability to read OBJ
   shapefiles. 
   
## Minor changes

*  If a template file is used in `writeWebGL()`, the string `%prefix%`
   will be replaced in it by the prefix argument. 
*  `writeWebGL()` now outputs a Javascript global variable named
   `"<prefix>rgl"` of class `"rglClass"` that allows access to many of the
   scene internals.  (Inspired by patch submitted by Jeff Allen.) 
*  User mouse callbacks can now be retrieved within R using
   `rgl.getMouseCallbacks()` and `rgl.getWheelCallback()`, and
   may be included in WebGL output. 
*  `writeWebGL()` now outputs information on object ids to allow
   them to be re-used in multiple figures on the same page.
   See the `reuse` parameter and attribute of the result. 
*  Started a vignette describing user interaction in WebGL. 
*  Set the class of the main `"canvas"` element in
   `writeWebGL()` output to `"rglWebGL"`. 
*  `rgl.snapshot()` now evaluates the `top` argument after `filename`
   and `fmt`, so windows created when those are evaluated don't
   overlay the `rgl` window.  (Suggestion of Keith Jewell.) 
*  `writeWebGL()` now includes an argument `commonParts`, to allow
   omission of common code in multi-figure displays. 
*  If `template` is `NULL` in `writeWebGL()`, no template file is used. 
*  The `persp.function()` method is now smarter about setting default
   axis labels. 
*  The package now contains a vignette giving an overview of
   the functions. 
*  `triangulate()` now supports polygons expressed with 3
   coordinates (though they are still assumed to be planar). 
*  `par3d()` now includes `"listeners"`, a list of subscenes that
   respond to mouse actions in the current subscene. 
*  The Windows configuration file has been modified to work in
   R-devel (to become R 3.2.0).
   
## Bug fixes

*  Fixed bug in `abclines3d()` that caused it to skip lines that passed
   through the corners of the bounding box.  (Reported by Sven Laur.) 
*  The `NULL` device did not handle changes to `par3d("windowRect")`
   properly. 
*  Subscenes with `ignoreExtent = TRUE` were not plotted. 
*  The bounding box calculations now take clipping planes into account. 
*  `writeWebGL()` did not display the `bboxdeco` properly when working
   in a subscene. 

# rgl  0.95.1158 

## Minor changes

*  `rgl.snapshot()` now works with the `NULL` device (but produces a
   black snapshot).  This allows testing with `RGL_USE_NULL`. 

# rgl  0.95.1157 

## Major changes

*  Allowed background of window to show bitmap;  added `bgplot3d()`
   and `legend3d()` functions. 
   
## Bug fixes

*  Reverted misguided changes to `par3d("modelMatrix")` from 0.94.
   This affects `rgl.projection()` as well. 
*  Fixed bug (introduced in 0.94) causing loss of rectangle showing
   selection area.  (Reported by John Fox and others.) 
*  The `NULL` device now does not make any spurious OpenGL calls. 

# rgl  0.94.1143 

## Major changes

*  Added function methods for `persp3d()` and `plot3d()`, to allow
   surfaces to be plotted just by specifying the function. 
## Bug fixes

*  Fixed a bug introduced in 0.94 that made user callbacks crash R.
   (Reported by Dave Hadka.) 
*  Fixed a bug exposed in 0.94 (but really introduced in 0.93.952)
   that caused `writeWebGL()` to fail when a `NULL` device was active. 
*  Fixed a bug introduced in 0.94 with writing 3D sprite objects. 
*  Fixed a bug computing the bounding box of an embedded subscene. 

# rgl  0.94 

## Major changes

*  Added "subscenes", i.e. scenes of objects nested within the
   main window.  This affects a lot of other functions as well,
   which now act either on a single subscene or on the
   overall scene. 
*  Added configurable mouse wheel actions via `par3d()` or
   `rgl.setWheelCallback()`.
   
## Minor changes

*  Allowed the coordinates of the viewport to be set. 
*  Changed the behaviour of `pop3d()` and `rgl.pop()`:  the type is
   now ignored if `id` is non-zero. 
*  `par3d("modelMatrix")` no longer includes the observer translation 
*  The `par3d()`, `par3dinterp()`, and `spin3d()` functions now
   have arguments dev and subscene to specify where they apply. 
*  Included a copy of the source to `CanvasMatrix.js` (used by
   `writeWebGL()`) at the request of the Debian administrators. 
*  Some of the animations have been sped up at the request of CRAN. 

## Bug fixes

*  The `NULL` device was not removed from the device list when
   it was closed.  (Reported by Henrik Bengtsson.) 

# rgl  0.93.1098 

## Minor changes

*  `rgl.material()` (for textures), `rgl.postscript()` and `rgl.snapshot()`
   now call `normalizePath()` on filenames, so tilde expansion should
   be supported. 
*  internals are updated to be consistent with macOS 10.9 requirements 
*  Improved the approximation to the surface normal for degenerate
   grids in `surface3d()` and `persp3d()`.  (Problem found by Graham Griffiths
   using polar coordinates; all `r=0` points were at the same location.) 
*  The new surface normals are now saved in memory, so `rgl.attrib()`
   will return them even if they were calculated by `rgl`. *  `scene3d()` now records light settings. 

## Bug fixes

*  `par3d()` could generate an error if an unnamed list was passed in. 
*  ` material3d()` lost settings for textures 
*  fixed a bug in triangulation code:  it failed on `locator()` input. 
*  The Aqua support now works again, XQuartz is only needed for command
   line use in Mac OSX. 
*  Bounding box calculations for surfaces with user normals were
   incorrect. 
*  An array-overrun bug in `rgl.attrib()` showed up in `writeWebGL()`.
   (Reported by Brian Ripley.) 

# rgl  0.93.991 

## Major changes

*  Added `clipplanes3d()` function to implement clip planes.  (Still
   only partially implemented.) 
   
## Minor changes

*  Some cleanup of the declarations (submitted by Karl Millar). 

# rgl  0.93.986 

## Bug fixes

*  The FTGL functions were mistakenly added to the `rgl` namespace
   on some OSX compiles. 
*  Changes have been made to satisfy the stringent requirements of
   the Solaris compiler. 

# rgl  0.93.984 

## Minor changes

*  most `rgl` C++ functions and classes are now in namespace "rgl".
   Others have prefix rgl_, with the exception of gl2ps functions,
   which all have that prefix, and FTGL functions, which generally
   have an FT prefix. 
*  entry points to the `rgl` DLL are now registered within the DLL,
   and on systems that support it, all entry points other than the
   registration function are hidden. 
   
## Bug fixes

*  `writeWebGL()` and the other write methods did not handle material
   information properly after 0.93.975. 

# rgl  0.93.975 

## Minor changes

*  the `scene3d()` function now records complete information about the
   bounding box and the background. 
*  `rgl` declares most of its C++ objects in the global namespace.
   Recently this has caused clashes with the `igraph` package, which
   does the same, and which also has a Shape class.  As a temporary
   workaround the `rgl` class has been renamed to `"rglShape"`.  A full
   `rgl` namespace will eventually be added, with only the API functions
   left in the global namespace. 

## Bug fixes

*  `rgl.texts()` without a window failed because it queried the window
   before opening it. 

# rgl  0.93.963 

## Minor changes

*  font selection assumed `rgl` was on the search path; now it may be
   imported but not attached.  Similarly, `r3dDefaults` need not be on
   the search path. 

# rgl  0.93.960 

## Minor changes

*  `writeWebGL()` now forces the position attribute to location 0, a
   recommended optimization strategy.  The color attribute is
   forced to location 1.
*  gl2ps has been updated to version 1.3.8 and support for point and line
   sizes has been added (bug 4792) 
*  internal functions `.check3d()` and `rgl.select()` have been exported,
   as they were used by the car package. 
*  `rgl` now prints a warning when a requested font is unavailable and the
   default font is substituted.
   
## Bug fixes

*  we now check for invalid characters when drawing text using bitmapped
   fonts (bug 4787) 
*  `writePLY()` had errors writing points and lines. 
 
# rgl  0.93.952 

## Major changes

*  added `triangulate()`, `polygon3d()`, `extrude3d()` and `turn3d()`
    for display of shapes based on two-dimensional polygons or curves. 
*  added support for "headless" operation:  see help for new
   function `rgl.useNULL()`. 
   
## Minor changes

*  added name of device to result returned from `rgl.cur()`; added function
   `rgl.dev.list()` to list all open devices. 
*  examples and demos now check `rgl.useNULL()`, and don't run invisible
   animations. 
   
## Bug fixes

*  fixed formatting of vertex reference numbers in `writeOBJ()` (issue
   4732, reported by Alejandro Baranek). 

# rgl  0.93.944 

## Major changes

*  added `identify3d()` function 

## Minor changes

*  write the `rgl` version into the WebGL file 
*  cleaned up use of `CHECKGLERROR`, so that setting `USE_GLGETERROR` to 1 in
   `R.h` will enable detailed checking 

## Bug fixes

*  fixed bbox bug in `writeOBJ()` (reported by Matthias Zeeman), `writePLY()`
   and `writeSTL()`. 
*  `aspect3d()` (called by `plot3d()`) caused the scene to be redrawn,
   even if `par3d("skipRedraw")` was `TRUE`. 
*  `addNormals.mesh3d()` failed on objects when the matrices
   of triangles or quadrilaterals had zero columns. 
*  `rotate3d.mesh3d()` did not transform normals properly 
*  the `writeWebGL()` function produced fragment shaders that would not
   work in some browsers (e.g. Firefox and Chrome with the ANGLE
   WebGL engine). 

# rgl  0.93.935 

## Bug fixes

*  in certain circumstances since 0.93.930, text would fail to appear.
   (Reported by Karline Soetaert.) 

# rgl  0.93.932 

## Bug fixes

*  calling `rgl.material()` before any rendering caused a crash on OSX.
   (Reported by Dan Tenenbaum.) 

# rgl  0.93.930 

## Minor changes

*  Now handles local (not just directional) lighting.  Based on
   code contributed by Alexander Senger.) 
*  `writeWebGL()` handles lighting properly.  Based on code contributed
   by Alexander Senger. 
   
## Bug fixes

*  `writeWebGL()` did not handle `snapshot=FALSE` properly.  (Reported
   by Yihui Xie.) 

# rgl  0.93.928 

## Minor changes

*  Updated the configure file using autoconf 2.69 
*  Forced OSX installs to put `/usr/X11/bin` at the head of the path
   when looking for freetype-config 

# rgl  0.92.879 

## Major changes

*  Added `writeWebGL()` function, to allow scenes to be viewed in a web
   browser. 
   
## Minor changes

*  Removed `rgl.save.texture()`:  textures are not saveable! 
*  Added "centers" to the attributes that can be queried, for depth sorted
   transparent rendering.
   
# rgl 0.92.880

## Minor changes

*  Rearranged declarations for compatibility with gcc 4.7.

# rgl 0.92.881 

## Bug fixes

*  Fixed degenerate (e.g. straight line) cases in `cylinder3d()`.

# rgl 0.92.883

## Major changes

*  Added 3d "sprites" -- shapes that maintain their initial orientation.

# rgl 0.92.887 

## Minor changes

*  Added "caps" to the end of `cylinder3d()` objects.

# rgl 0.92.891 

## Minor changes

*  Added support for 3d sprites to `writeWebGL()`.

# rgl 0.92.892 

## Minor changes

*  Added declaration needed by Solaris.

# rgl 0.92.893 

## Bug fixes

*  `rgl.light()` and `light3d()` did not return the light ID value.

# rgl 0.92.894 

## Bug fixes

*  remove debugging code from `configure.win` that was causing problems
   on the CRAN WinBuilder system
   
# rgl 0.93

## Major changes

*  Added `readSTL()` and `writeSTL()` functions 
*  Added `writePLY()` and `writeOBJ()` functions 
*  Added `scene3d()` function 
*  Added `selectpoints3d()` function to select points from the scene. 

## Minor changes

*  Added `expand` argument to `decorate3d()` and `axes3d()` 
*  Added `base` argument to `spin3d()` result 
*  Added section argument to `cylinder3d()` 
*  Added `res_name="rgl"` and `res_class="R_x11"` to the `WM_CLASS` property of X11
   windows.  (Contributed by Philip Johnson.) 
*  Added code to work with R 3.0.0 `setHook()` changes 
*  The `rgl` window now handles `ESC` key presses.  During selection
   and `play3d()` they abort the process; otherwise they are ignored. 
*  Copied the `R_pretty0()` function from R sources to avoid warning. 

## Bug fixes

*  `writeWebGL()` did not render semi-transparent surfaces properly.
   (Reported by Robert Esswein.) 

# rgl  0.92.861 

## Minor changes

*  Added `rgl.save.texture()` to get texture from an object. 

## Bug fixes

*  Fixed segfault on startup on Windows in MDI mode. 

# rgl  0.92.858 

## Major changes

*  Added `Sweave()` support through the `rgl.Sweave()` driver and the `Sweave.snapshot()`
   function.
*  Added `rgl.abclines()`, `rgl.planes()`, `abclines3d()` and `planes3d()` to draw
   lines and planes intersecting with the bounding box.
*  Functions `rgl.attrib.count()` and `rgl.attrib()` (and internal function
   `rgl.getmaterial()`) added to allow objects in the scene to be examined. 
   
## Minor changes

*  Added declarations for Solaris compatibility (from Brian Ripley) 
*  Fixed `configure.win` for bi-arch compatibility.  Windows installers can
   set `HAVE_PNG` to a non-empty value, and `rgl` will look for
   the libpng files in the default `LOCAL_SOFT` location when installing. 
*  Added `"depth_mask"` and `"depth_test"` material properties, to allow control
   over how objects are obscured by each other. 
*  Added iterative computation of the bounding box to handle objects like
   spheres, which need to maintain their apparent shape as the scaling
   changes. 
*  Improved the bounding box decoration in two ways:  it can now draw the
   front faces (to surround the whole graph), and can label edges with pretty
   labels.  `plot3d()` was modified to use this instead of manually setting axis
   locations and using `box3d()` to draw a box, allowing resizable labelled
   axes. 
*  Removed some unnecessary declarations from `rglmath.h` that were causing
   problems in an old version of gcc on Solaris. 
*  `rgl.postscript()` now adjusts the size of text following the `cex` setting.
   The `font` and `family` settings are still ignored. 
*  Transparency in material textures was not always rendered properly. 
*  In OSX, the Carbon system has been replaced by a Cocoa system.
   (Code contributed by Adam Strzelecki).  For compatibility with the
   Windows build system, the new files have been put into `src/osx`. 
*  Hardware antialiasing is now used if the OpenGL driver supports
   it.  Set `options(rgl.antialias=0)` to disable it. 
*  Updated gl2ps to version 1.3.6 
   
## Bug fixes

*  Bug fix for `divide.mesh3d()` in handling normals. 
*  `rgl.ids()` did not return all object ids as documented. 

# rgl  0.92 

## Minor changes

*  Added detection of 64 bit MacPorts compiler to configure script.
   (Bug #861) 
*  Allowed texture coordinates to be specified in mesh objects. 
*  Updated gl2ps to version 1.3.5 
*  Should now install using `--merge-multiarch` on Windows 

# rgl  0.91 

## Minor changes

*  Added `R_ARCH*` macros to `configure.win` for Win64 compatibility 

## Bug fixes

*  Fixed bug in `rgl.texts()`:  zero-length texts argument caused crash.
   (Reported by Michael Friendly.) 
*  Fixed bad declaration in `rglmath.h` 

# rgl  0.90 

## Minor changes

*  Added `startTime` argument to `play3d()` and `movie3d()`. 
*  Fixed `configure.ac` as suggested by Jens Elkner. 
*  Updated declarations for libpng 1.4.0 compatibility.

## Bug fixes

*  An off-by-one error caused the `"alpha"` component of the material
   properties to be messed up. (Bug #809) 

# rgl  0.89 

## Bug fixes

*  Fixed rounding errors and `Xvfb` errors in `rgl.pixels()` examples
   and demo. 

# rgl  0.88 

## Minor changes

*  Add `keepVars` argument to `cylinder3d()`, for debugging or special
   effects. 
*  Add `BugReports` field to `DESCRIPTION`. 

# rgl  0.87 

## Minor changes

*  Allowed `FOV` to be set to 0, for an orthogonal projection. 
*  Changed `seq(along=...)` to `seq_along(...)`. 

## Bug fixes

*  Fixed crash when zero-length color vector was used. 
*  Fixed crash in X11 after closing a window 
*  Fixed typo in `cylinder3d()`. 
*  Cleaned up bad links in Rd files. 

# rgl  0.85 

## Major changes

*  Added `addNormals()` generic, to add normals for smooth surface rendering. 
*  Added `cylinder3d()` function, to make cylindrical or "tube" plots. 

## Minor changes

*  Added some namespace declarations to the C++, and renamed `math.h`, for
   compatibility with Sun compilers (contributed by Brian Ripley). 
*  Fixed visibility of some `shade3d()`, `wire3d()` and `points3d()` methods. 

## Bug fixes

*  Fixed `material3d("color")` bug introduced in 0.82. 

# rgl  0.84 

## Major changes

*  Added triangle meshes, shape lists, the Platonic solids and a cuboctahedron. 
*  Added classes `"mesh3d"` and `"shapelist3d"`; `"qmesh3d"` is only kept for
   back compatibility.
   
## Bug fixes

*  Bug fix to stop crashes when material is set before the first window is
   opened. 

# rgl  0.83-3 

## Bug fixes

*  Quick fix for R 2.9.x compatibility, and to remove accidental change
   introduced in v0.83 which caused errors on plotting without `open3d()`. 

# rgl  0.83-1 

## Minor changes

*  Don't try to build Carbon driver in 64 bit Mac OS (contributed by
   Brian Ripley).
*  Did not assume OpenGL 1.2 was available in material properties. 
*  Added numerous error checks. 

## Bug fixes

*  Fixed `rgl.pixels()` example for weird displays. 
*  Fixed `demo(stereo)` to add sync in X11:  X windows seemed to grab
   images before they were redrawn. 
*  Rearranged headers for Win64 compatibility (contributed by Alex
   Chen). 

# rgl  0.82 

## Major changes

*  added `rgl.pixels()` to read the generated image, and `demo("stereo")`
   to illustrate its use.
   
## Minor changes

*  rewrote internal rendering of transparent and anti-aliased shapes,
   so they will be rendered better when there are several in the same
   scene 
*  added material properties `"point_antialias"`, which causes points to be
   drawn as smooth circles, and `"line_antialias"`, which causes
   lines to be antialiased. 
*  added material parameter `"lwd"` for line width; `"size"` now applies only
   to points. 
*  increased default point size to 3 pixels across.
*  `movie3d()` gains a "type" argument to set the output type, and the
   `convert` argument is more flexible. 
*  `rgl.snapshot()` gives more informative error messages when libpng is
   not available. 
*  `axis3d()` now uses `format()` rather than `as.character()` to give
   nicer looking labels. 
*  use R `warning()` to report messages, rather than popups or `REprintf`. 
    
## Bug fixes

*  fixed a bug in the bounding box decoration which caused axis labels
   to be plotted in the wrong place. 
*  fixed a bug in the Windows driver which caused the standard system
   font to disappear when justified. 
*  fixed bug in `open3d()`:  "..." was being ignored. 
*  fixed bug in `qmesh3d()`:  `homogeneous=FALSE` coordinates were not
   handled properly. 
*  the clipping volume calculation was incorrect when scaling was used. 
*  corrected the `?rgl` example to display this file. 

# rgl  0.81 

## Minor changes

*  converted Freetype font error into warning 

## Bug fixes

*  `rglFonts()` was being set at install time, but it should be set at load
   time. 
*  fixed configuration problems in OS X 
*  fixed executable marker on a number of files 

# rgl  0.80 

## Minor changes

*  worked around bug(?) in Mac OSX FTGL rendering 
*  updated FTGL to 2.1.3rc5 

# rgl  0.79 

## Minor changes

*  added `mouseCallbacks()` demo, to show R implementations of standard
   mouse handlers, multiple connected windows, stereo view, etc. 
*  added "silent" argument to `rgl.set()`, to allow temporary changes
   to focus without changing window labels. 
*  added natural spline extrapolation to `par3dinterp()`. 

## Bug fixes

*  `rgl.pop()` could cause corruption when multiple windows were open. 

# rgl  0.76 

## Minor changes

*  rename ChangeLog file to NEWS, as per discussion on R-devel 
*  add `"windowRect"` to `par3d()` parameters to allow window size to be
   controlled from R.
   
## Bug fixes

*  put our own `assert()` macro in place to avoid crashing R. 

   
# rgl 0.77 

## Bug fixes

*  `par3d("windowRect")` returned garbage if there was no window open. 
*  `persp3d()` and `plot3d()` sometimes miscalculated ranges involving NAs. 
*  `select3d()` and `rgl.select()` produced a very inefficient test function.

# rgl 0.78 

## Minor changes

*  `rgl.texts()` and `text3d()` can now handle font and size specifications
   using the FreeType library on any platform, or GDI on Windows. 
*  `adj` is supported both horizontally and vertically in drawing text. 

## Bug fixes

*  fix miscalculation of `mouseMatrix` that caused disappearing views. 
*  `rgl.pop()` was very slow when given a long list of ids. 
*  a workaround for OSX 10.5 OpenGL problems has been incorporated
   (thanks to mkv22@cam.ac.uk). 

# rgl  0.75 

## Major changes

*  add `play3d()`, `movie3d()`, `par3dinterp()`, and `spin3d()` functions, with flag demo 

## Bug fixes

*  rounding error could cause `par3d("userMatrix")` to generate NaNs and fail 
*  workaround for `Xvfb` on macOS problems 

# rgl  0.74 

## Major changes

*  add `rgl.setMouseCallbacks()` to allow user actions 

## Minor changes

*  clean up `#include`s 
*  clean up some calls for SunStudio 12 compiler 

# rgl  0.73 

## Minor changes

*  partial changes to avoid crash on macOS with `Xvfb` 
*  change to `rgl_init()` for R 2.6.0 compatibility 

# rgl  0.72 

## Minor changes

*  declaration changes for compatibility with R 2.6.0 (from Brian Ripley) 

# rgl  0.71 

## Major changes

*  allowed normals and texture coordinates to be specified in triangles, quads and surfaces 

## Minor changes

*  changes to configure script from Laszlo Kajan and Brian Ripley:  should
   now be much more portable 
*  removed deprecated OSX font setting calls 
*  texture properties are now returned by `material3d()` 
*  normals may be specified in `qmesh` objects, but (at present) `subdivision3d()` removes them 
*  ` material3d()` now preserves the values of unspecified parameters (as documented,
   but not previously functioning) 
*  `clear3d()` can now reset material properties to the defaults, and
   `open3d()` does this. 
*  minor fix for gcc 4.3 compatibility 
*  minor fix for R 2.5+ compatibility 
*  allowed more general surfaces to be plotted by `rgl.surface()`, `surface3d()` and
   `persp3d()`, by specifying matrices for x and y coordinates 
*  added world map texture, used in `example(persp3d)`. 

# rgl  0.70 

## Minor changes

*  OSX now builds two libraries, one for AGL, one for X11 
*  resolve entry points at load time, not call time 
*  updated gl2ps to version 1.3.2 
*  tweaked positioning of labels in bounding box decoration 
*  moved this file (ChangeLog) to inst directory, so it will be installed,
   and added code to display it to the `rgl` help topic. 

## Bug fixes

*  fixed bug in `rgl.postscript()` in Linux, added text support to it 
*  `snapshot3d()` wasn't being exported, and snapshots were from the back buffer 
*  fixed bug that could cause crash on shutdown 

# rgl  0.69 

## Minor changes

*  allow selection to use any button 
*  allow NA in primitives, surfaces, texts, and sprites 
*  report error in OSX if the wrong configure options were used. 

## Bug fixes

*  `persp3d()` partially ignored `add=TRUE` 
*  `plot3d.qmesh3d()` did not return result 
*  display was not being updated properly in OSX 

# rgl  0.68 

## Major changes

*  added `grid3d()`, added `nticks` argument to `bbox3d()`, `axis3d()` and `axes3d()`. 

## Minor changes

*  fixed sphere drawing so spheres are spheres regardless of `par3d("scale")` 
*  added `type="s"` to `plot3d()` to draw spheres 
*  fixed handling of "..." in axis related functions 
*  added full MDI support 
*  removed use of `List` and `ListIterator` internally 
*  fixed handling of axes and boxes when a coordinate had zero extent 
*  changed `rgl.viewpoint()` default to be compatible with `r3dDefaults` 
*  added id return values to primitives and higher level functions,
   and to `rgl.pop()`; added `rgl.ids()` to report on them. 
*  updated gl2ps to version 1.3.1, adding support for svg and pgf output formats. 


# rgl  0.67-2 

*  minor correction 

# rgl  0.67 

*  added support for png files with palettes, and grayscale pngs with 1, 2 or 4 bits
   per pixel 
*  added `"ignoreExtent"` option to `par3d()`:  objects plotted when this is true
   are ignored when calculating the bounding box 
*  added `axis3d()`, `axes3d()`, `box3d()`, `mtext3d()`, `title3d()` functions from `djmrgl` for
   annotating plots. 
*  added `plot3d()` high level plot function 
*  added ` material3d()`, which can both set and query material properties; changed
   most `*3d` functions so they leave material invariant across calls. 
*  changed `open3d()` to set background and material defaults 
*  added `aspect3d()` to control the aspect ratio of the bounding box. 
*  added `xAxis`, `yAxis` and `zAxis` mouse modes, set `zAxis` as `r3d` default. 
*  added `persp3d()` function 
*  changed error messages to go through `REprintf` in X11 and OSX 
*  fixed segfault if `rgl_init()` failed 
*  converted type of `viewport` argument in `user2window()` and `window2user()` calls 
*  if the `rgl_init()` call fails, the package will still load with a warning (but
   most function calls will result in errors). 
*  added `par3d("scale")` to handle `aspect3d()` operations internally. 
*  added `ellipse3d()` generic and methods for drawing confidence ellipsoids 
*  added `decorate3d()` to draw all the decorations, `plot3d.qmesh3d()` method. 
*  changed zoom to use ratio scale over larger range 
*  fixed bug causing jump after resize in Mac OSX (and maybe other platforms) 
*  `rgl.primitive()` now does sanity checks on inputs 

# rgl  0.66 

*  added `"all"` and `"viewpoint"` to `rgl.clear()` and `clear3d()` 
*  added static libpng build support and user customizable prefix (unix) 
*  used `xyz.coords()` in all functions taking x, y, z coordinates, allowing
   matrix or dataframe arguments (among others) 
*  added `par3d(skipRedraw=TRUE)` to allow drawing to be done without
   being rendered 
*  fixed display list memory leak when drawing shapes (e.g. spheres) 
*  Changes for compatibility with strict enforcement of file naming rules in R 2.3.0. 

# rgl  0.65 

*  simplified build system: uses 'R' build system 
*  added generic visualization/rendering interface (R3D) 
*  text justification from 0 to 1 
*  added primitive type: `linestrip` 
*  fixed `rgl.bringtotop()`, added stay option (win32) 
*  added 4x4 matrix functions from `djmrgl` 
*  added `rgl.user2window()` and `rgl.window2user()` functions 
*  added user-selectable mouse handlers 
*  added selection mouse handler 
*  added trackball mouse handler 
*  added z-distance sorted rendering of alpha-blended faces 
*  added gl2ps patch ( contributed by Albrecht Gebhard ) 
*  added port: native Mac OS X Carbon 
*  bugfix: `rgl.close()`, `rgl.quit()` crashed on X11 occasionally. 
*  generalized `rgl.surface()` to allow surface over any coordinate plane. 
*  added `r3dDefaults` variable to allow user to set defaults 
*  added environment texture-mapping 

# rgl  0.64-13 

*  DESCRIPTION fix: moved R 1.41 -> R 1.4.1 dependency 

# rgl  0.64-12 

*  CRAN bugfix: permissions of cleanup fixed. 

# rgl  0.64-11 

*  removed several redundant semicolons, required by gcc 3.4 ansi-pedantic mode. 
*  win32: uses R's `zlib` and `libpng` sources 
*  win32: added virtual destructor in `Win32GUIFactory` (removes warning) 

# rgl  0.64-10 

*  updated `.C()` calls using `PACKAGE="rgl"` 
*  updated `Maintainer.mk` using correct `zlib` version 
*  improved dynamic unload using `library.dynam.unload()` 
*  conditional macOS x Darwin code in `.First.lib()` 

# rgl  0.64-9 

*  macOS X 'Panther' G5 fix for OpenGL library loading in .first.lib 
*  removed `lpng` and `zlib` from source tree 
*  support for automatic downloading of `zlib` and `lpng` on win32 
*  added demo directory with several examples using `demo(rgl)` 

# rgl  0.64-8 

*  build bugfix : removed `rgl/src/Makefile` 
*  updated configure to check and setup `LDFLAGS` for `OpenGLU` library 

# rgl  0.64-7 

*  added mouse capturing 
*  `rgl.sprites()` 'radius' bug fixed 
*  memory leak bugfix: texture objects are now `AutoDestroy` and used through `Ref`'s 
*  resource management improvement: pixmaps get `free`'d when they become unused
   e.g. texture objects are created. 
*  no limitations on pixmap sizes 
*  mipmap support 
*  support for different texture minification and magnification filters 

# rgl  0.64-6 

*  updated build system: added `setversion.sh` 
*  with MinGW version 3.0.1 pixmap loading does work 
*  `project.mk`, `win32.mk` and `x11.mk` in `src/build` changed
   now a single variable MODS will extend. 
*  MinGW build system changed.
   `rgl.dll` now contains an R compliant Resource information
   generated by R `perl` script 
*  bug fix: R 1.8.0/win32 does not detach packages when quit
   it is safe now to call `rgl_quit()` and `lib_quit()` multiple times
   `win32lib.cpp`: added `dllmain` that calls `rgl_quit()` on process exit 
*  added core support for `devcpp` IDE 

# rgl  0.64-5 

*  macOS X/X11 port 

# rgl  0.64-4 

*  manual update 
*  acquired valid CRAN package status,
   `R CMD check` runs through with 2 WARNINGS
   (according to `latex`, and `codoc`) 
*  uploaded to cvs 

# rgl  0.64-3 

*  configure.ac: X11 library path broken, fixed 
*  `x11gui`: `glXChooseVisual()` part fixed 
*  code cleanup: `rglview.h` 
*  added: `man/maintainer.Rd` maintainer information 

# rgl  0.64-2 

*  `rgl.quads()`: `enum` id was broken, fixed ("quads" -> "quadrangles") 

# rgl  0.64 

*  autoconf build system 
*  moved textures to `inst/` directory 
*  x11 port 
*  `win32/vc`: fixed fpu control word precision to remain on 64 bit
   links with `fp10.obj` 
*  changed texture mapping t coordinate for Surface node 

# rgl  0.63 

*  API: added `rgl_init()`, `rgl_quit()`: explicit client initialization 
*  added `setup.bat`: build setup for windows 
*  win32 setup: MinGW compiler 
*  win32 setup: visual c++ compiler through `gui` environment 

# rgl  0.62 

*  modified sphere set 
*  support R color strings 
*  use `system.file( <subpath>, package="rgl" )` in examples to retrieve texture files 
*  rewrote R code :
   * clear `enum` types
   * vertex vector datatype (internal representation matrix) 

# rgl  0.61 

*  added: `rgl.sprites()` 
*  added: fps counter 
*  added: `autoUpdate`, modified win32 main loop, on hide, `autoUpdate` disabled, on show enabled 
*  modified material: added alpha vector 

# rgl  0.60 

*  (mini-thesis release) 

