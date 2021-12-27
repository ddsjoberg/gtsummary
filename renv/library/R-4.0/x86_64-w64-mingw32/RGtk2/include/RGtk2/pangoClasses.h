#ifndef S_PANGO_CLASSES_H
#define S_PANGO_CLASSES_H
#include <RGtk2/gobject.h>
#include <RGtk2/pango.h>

void
S_pango_font_class_init(PangoFontClass * c, SEXP e); 
void
S_pango_font_face_class_init(PangoFontFaceClass * c, SEXP e); 
void
S_pango_font_family_class_init(PangoFontFamilyClass * c, SEXP e); 
void
S_pango_font_map_class_init(PangoFontMapClass * c, SEXP e); 
void
S_pango_fontset_class_init(PangoFontsetClass * c, SEXP e); 
void
S_pango_renderer_class_init(PangoRendererClass * c, SEXP e); 
#endif
