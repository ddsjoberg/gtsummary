gClass("FooBar", "GtkButton",
       .prop_overrides=c("related-action", "use-action-appearance"),
       GObject=list(
         dispose=function(object) {
           object$doSetRelatedAction(NULL)
         },
         set_property=function(object, id, value, pspec) {
           if (pspec$name == "related-action") {
             assignProp(object, pspec, value)
             object$doSetRelatedAction(value)
           } else if (pspec$name == "use-action-appearance") {
             if (value != getProp(pspec)) {
               assignProp(object, pspec, value)
               object$syncActionProperties(object$"related-action")
             }
           } else {
             warning("invalid property: ", pspec$name)
           }
         }
       ),
       GtkActivatable=list(
         sync_action_properties=function(activatable, action) {
           if (is.null(action)) {
             return()
           }
           activatable$visible <- action$visible
           activatable$sensitive <- action$sensitive
           ## ...
           if (activatable$use_action_appearance) {
             if (!is.null(action$stock_id)) {
               activatable$label <- action$stock_id
             } else {
               activatable$label <- action$label
             }
             activatable$use_stock <- !is.null(action$stock_id)
           }
           ## ...
         },
         update=function(activatable, action, property_name) {
           if (property_name == "visible") {
             activatable$visible <- action$visible
           } else if (property_name == "sensitive") {
             activatable$sensitive <- action$sensitive
           }
           ## ...
           if (activatable$use_action_appearance) {
             if (property_name == "stock-id") {
               activatable$label <- action$stock_id
               activatable$use_stock <- !is.null(action$stock_id)
             } else if (property_name == "label") {
               activatable$label <- action$label
             }
           }
           ## ...
         }
       ))
