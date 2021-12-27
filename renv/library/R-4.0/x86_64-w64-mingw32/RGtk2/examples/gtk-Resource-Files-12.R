path <- widget$path()$path
class_path <- widget$classPath()$path
gtkRcGetStyleByPaths(widget$getSettings(), path, class_path, class(widget)[1])
