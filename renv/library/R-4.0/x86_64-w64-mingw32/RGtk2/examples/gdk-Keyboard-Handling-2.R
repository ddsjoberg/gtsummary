# XXX Don't do this XXX
unconsumed <- all_accel_mask & event[["state"]] & !as.flag(state$consumed)
if (state$keyval == accel_keyval &&
    unconsumed == accel_mods & !as.flag(state$consumed))
  print("Accellerator was pressed")
