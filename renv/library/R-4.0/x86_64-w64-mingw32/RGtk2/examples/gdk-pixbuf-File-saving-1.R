formats <- gdkPixbufGetFormats()
writeable_formats <- formats[sapply(formats, gdkPixbufFormatIsWritable)]
