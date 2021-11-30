void
S_pango_font_class_init(PangoFontClass * c, SEXP e)
{
  static void (*fun)(PangoFontClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(PangoFontClass *, SEXP))R_GetCCallable("RGtk2", "S_pango_font_class_init"));
  return(fun(c, e));
} 

void
S_pango_font_face_class_init(PangoFontFaceClass * c, SEXP e)
{
  static void (*fun)(PangoFontFaceClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(PangoFontFaceClass *, SEXP))R_GetCCallable("RGtk2", "S_pango_font_face_class_init"));
  return(fun(c, e));
} 

void
S_pango_font_family_class_init(PangoFontFamilyClass * c, SEXP e)
{
  static void (*fun)(PangoFontFamilyClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(PangoFontFamilyClass *, SEXP))R_GetCCallable("RGtk2", "S_pango_font_family_class_init"));
  return(fun(c, e));
} 

void
S_pango_font_map_class_init(PangoFontMapClass * c, SEXP e)
{
  static void (*fun)(PangoFontMapClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(PangoFontMapClass *, SEXP))R_GetCCallable("RGtk2", "S_pango_font_map_class_init"));
  return(fun(c, e));
} 

void
S_pango_fontset_class_init(PangoFontsetClass * c, SEXP e)
{
  static void (*fun)(PangoFontsetClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(PangoFontsetClass *, SEXP))R_GetCCallable("RGtk2", "S_pango_fontset_class_init"));
  return(fun(c, e));
} 

void
S_pango_renderer_class_init(PangoRendererClass * c, SEXP e)
{
  static void (*fun)(PangoRendererClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(PangoRendererClass *, SEXP))R_GetCCallable("RGtk2", "S_pango_renderer_class_init"));
  return(fun(c, e));
} 

