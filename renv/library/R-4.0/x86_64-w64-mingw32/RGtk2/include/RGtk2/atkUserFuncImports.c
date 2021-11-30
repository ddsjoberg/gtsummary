gint
S_AtkKeySnoopFunc(AtkKeyEventStruct* event, gpointer func_data)
{
  static gint (*fun)(AtkKeyEventStruct*, gpointer) = NULL;
  if(!fun) fun = ((gint (*)(AtkKeyEventStruct*, gpointer))R_GetCCallable("RGtk2", "S_AtkKeySnoopFunc"));
  return(fun(event, func_data));
} 

