# We want to ignore irrelevant modifiers like ScrollLock
all_accels_mask <- GdkModifierType["control-mask"] | 
GdkModifierType["shift-mask"] | GdkModifierType["mod1-mask"]
state <- gdkKeymapTranslateKeyboardState(keymap, event[["hardware_keycode"]],
                                         event[["state"]], event[["group"]])
unconsumed <- all_accels_mask & event[["state"]] & !as.flag(state$consumed) 
if (state$keyval == .gdkPlus && unconsumed == GdkModifierType["control-mask"])
  print("Control was pressed")
