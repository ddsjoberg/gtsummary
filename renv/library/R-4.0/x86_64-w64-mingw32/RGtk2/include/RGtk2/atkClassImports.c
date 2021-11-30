void
S_atk_hyperlink_class_init(AtkHyperlinkClass * c, SEXP e)
{
  static void (*fun)(AtkHyperlinkClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkHyperlinkClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_hyperlink_class_init"));
  return(fun(c, e));
} 

void
S_atk_object_class_init(AtkObjectClass * c, SEXP e)
{
  static void (*fun)(AtkObjectClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkObjectClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_object_class_init"));
  return(fun(c, e));
} 

void
S_atk_gobject_accessible_class_init(AtkGObjectAccessibleClass * c, SEXP e)
{
  static void (*fun)(AtkGObjectAccessibleClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkGObjectAccessibleClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_gobject_accessible_class_init"));
  return(fun(c, e));
} 

void
S_atk_no_op_object_class_init(AtkNoOpObjectClass * c, SEXP e)
{
  static void (*fun)(AtkNoOpObjectClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkNoOpObjectClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_no_op_object_class_init"));
  return(fun(c, e));
} 

void
S_atk_object_factory_class_init(AtkObjectFactoryClass * c, SEXP e)
{
  static void (*fun)(AtkObjectFactoryClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkObjectFactoryClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_object_factory_class_init"));
  return(fun(c, e));
} 

void
S_atk_no_op_object_factory_class_init(AtkNoOpObjectFactoryClass * c, SEXP e)
{
  static void (*fun)(AtkNoOpObjectFactoryClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkNoOpObjectFactoryClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_no_op_object_factory_class_init"));
  return(fun(c, e));
} 

void
S_atk_registry_class_init(AtkRegistryClass * c, SEXP e)
{
  static void (*fun)(AtkRegistryClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkRegistryClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_registry_class_init"));
  return(fun(c, e));
} 

void
S_atk_relation_class_init(AtkRelationClass * c, SEXP e)
{
  static void (*fun)(AtkRelationClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkRelationClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_relation_class_init"));
  return(fun(c, e));
} 

void
S_atk_relation_set_class_init(AtkRelationSetClass * c, SEXP e)
{
  static void (*fun)(AtkRelationSetClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkRelationSetClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_relation_set_class_init"));
  return(fun(c, e));
} 

void
S_atk_state_set_class_init(AtkStateSetClass * c, SEXP e)
{
  static void (*fun)(AtkStateSetClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkStateSetClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_state_set_class_init"));
  return(fun(c, e));
} 

void
S_atk_util_class_init(AtkUtilClass * c, SEXP e)
{
  static void (*fun)(AtkUtilClass *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkUtilClass *, SEXP))R_GetCCallable("RGtk2", "S_atk_util_class_init"));
  return(fun(c, e));
} 

void
S_atk_table_class_init(AtkTableIface * c, SEXP e)
{
  static void (*fun)(AtkTableIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkTableIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_table_class_init"));
  return(fun(c, e));
} 

void
S_atk_streamable_content_class_init(AtkStreamableContentIface * c, SEXP e)
{
  static void (*fun)(AtkStreamableContentIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkStreamableContentIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_streamable_content_class_init"));
  return(fun(c, e));
} 

void
S_atk_selection_class_init(AtkSelectionIface * c, SEXP e)
{
  static void (*fun)(AtkSelectionIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkSelectionIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_selection_class_init"));
  return(fun(c, e));
} 

void
S_atk_implementor_class_init(AtkImplementorIface * c, SEXP e)
{
  static void (*fun)(AtkImplementorIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkImplementorIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_implementor_class_init"));
  return(fun(c, e));
} 

void
S_atk_image_class_init(AtkImageIface * c, SEXP e)
{
  static void (*fun)(AtkImageIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkImageIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_image_class_init"));
  return(fun(c, e));
} 

void
S_atk_hypertext_class_init(AtkHypertextIface * c, SEXP e)
{
  static void (*fun)(AtkHypertextIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkHypertextIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_hypertext_class_init"));
  return(fun(c, e));
} 

void
S_atk_editable_text_class_init(AtkEditableTextIface * c, SEXP e)
{
  static void (*fun)(AtkEditableTextIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkEditableTextIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_editable_text_class_init"));
  return(fun(c, e));
} 

void
S_atk_component_class_init(AtkComponentIface * c, SEXP e)
{
  static void (*fun)(AtkComponentIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkComponentIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_component_class_init"));
  return(fun(c, e));
} 

void
S_atk_action_class_init(AtkActionIface * c, SEXP e)
{
  static void (*fun)(AtkActionIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkActionIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_action_class_init"));
  return(fun(c, e));
} 

void
S_atk_value_class_init(AtkValueIface * c, SEXP e)
{
  static void (*fun)(AtkValueIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkValueIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_value_class_init"));
  return(fun(c, e));
} 

void
S_atk_text_class_init(AtkTextIface * c, SEXP e)
{
  static void (*fun)(AtkTextIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkTextIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_text_class_init"));
  return(fun(c, e));
} 

void
S_atk_document_class_init(AtkDocumentIface * c, SEXP e)
{
  static void (*fun)(AtkDocumentIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkDocumentIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_document_class_init"));
  return(fun(c, e));
} 

#if ATK_CHECK_VERSION(1, 12, 1)
void
S_atk_hyperlink_impl_class_init(AtkHyperlinkImplIface * c, SEXP e)
{
  static void (*fun)(AtkHyperlinkImplIface *, SEXP) = NULL;
  if(!fun) fun = ((void (*)(AtkHyperlinkImplIface *, SEXP))R_GetCCallable("RGtk2", "S_atk_hyperlink_impl_class_init"));
  return(fun(c, e));
}
#endif 

