gboolean
S_PangoFontsetForeachFunc(PangoFontset* fontset, PangoFont* font, gpointer data)
{
  static gboolean (*fun)(PangoFontset*, PangoFont*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(PangoFontset*, PangoFont*, gpointer))R_GetCCallable("RGtk2", "S_PangoFontsetForeachFunc"));
  return(fun(fontset, font, data));
} 

gboolean
S_PangoAttrFilterFunc(PangoAttribute* attribute, gpointer data)
{
  static gboolean (*fun)(PangoAttribute*, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(PangoAttribute*, gpointer))R_GetCCallable("RGtk2", "S_PangoAttrFilterFunc"));
  return(fun(attribute, data));
} 

#if PANGO_CHECK_VERSION(1, 18, 0)
gboolean
S_PangoCairoShapeRendererFunc(cairo_t* cr, PangoAttrShape* attr, gboolean do_path, gpointer data)
{
  static gboolean (*fun)(cairo_t*, PangoAttrShape*, gboolean, gpointer) = NULL;
  if(!fun) fun = ((gboolean (*)(cairo_t*, PangoAttrShape*, gboolean, gpointer))R_GetCCallable("RGtk2", "S_PangoCairoShapeRendererFunc"));
  return(fun(cr, attr, do_path, data));
}
#endif 

