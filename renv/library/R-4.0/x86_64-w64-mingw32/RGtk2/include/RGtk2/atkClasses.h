#ifndef S_ATK_CLASSES_H
#define S_ATK_CLASSES_H
#include <RGtk2/gobject.h>
#include <RGtk2/atk.h>

void
S_atk_hyperlink_class_init(AtkHyperlinkClass * c, SEXP e); 
void
S_atk_object_class_init(AtkObjectClass * c, SEXP e); 
void
S_atk_gobject_accessible_class_init(AtkGObjectAccessibleClass * c, SEXP e); 
void
S_atk_no_op_object_class_init(AtkNoOpObjectClass * c, SEXP e); 
void
S_atk_object_factory_class_init(AtkObjectFactoryClass * c, SEXP e); 
void
S_atk_no_op_object_factory_class_init(AtkNoOpObjectFactoryClass * c, SEXP e); 
void
S_atk_registry_class_init(AtkRegistryClass * c, SEXP e); 
void
S_atk_relation_class_init(AtkRelationClass * c, SEXP e); 
void
S_atk_relation_set_class_init(AtkRelationSetClass * c, SEXP e); 
void
S_atk_state_set_class_init(AtkStateSetClass * c, SEXP e); 
void
S_atk_util_class_init(AtkUtilClass * c, SEXP e); 
void
S_atk_table_class_init(AtkTableIface * c, SEXP e); 
void
S_atk_streamable_content_class_init(AtkStreamableContentIface * c, SEXP e); 
void
S_atk_selection_class_init(AtkSelectionIface * c, SEXP e); 
void
S_atk_implementor_class_init(AtkImplementorIface * c, SEXP e); 
void
S_atk_image_class_init(AtkImageIface * c, SEXP e); 
void
S_atk_hypertext_class_init(AtkHypertextIface * c, SEXP e); 
void
S_atk_editable_text_class_init(AtkEditableTextIface * c, SEXP e); 
void
S_atk_component_class_init(AtkComponentIface * c, SEXP e); 
void
S_atk_action_class_init(AtkActionIface * c, SEXP e); 
void
S_atk_value_class_init(AtkValueIface * c, SEXP e); 
void
S_atk_text_class_init(AtkTextIface * c, SEXP e); 
void
S_atk_document_class_init(AtkDocumentIface * c, SEXP e); 
#if ATK_CHECK_VERSION(1, 12, 1)
void
S_atk_hyperlink_impl_class_init(AtkHyperlinkImplIface * c, SEXP e);
#endif 
#endif
