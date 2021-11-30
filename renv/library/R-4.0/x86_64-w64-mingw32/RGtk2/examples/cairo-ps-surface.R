surface <- cairoPsSurfaceCreate(filename, width, height)
# ...
surface$dscComment("%Title: My excellent document")
surface$dscComment("%Copyright: Copyright (C) 2006 Cairo Lover")
# ...
surface$dscBeginSetup()
surface$dscComment("%IncludeFeature: *MediaColor White")
# ...
surface$dscBeginPageSetup()
surface$dscComment("%IncludeFeature: *PageSize A3")
surface$dscComment("%IncludeFeature: *InputSlot LargeCapacity")
surface$dscComment("%IncludeFeature: *MediaType Glossy")
surface$dscComment("%IncludeFeature: *MediaColor Blue")
# ... draw to first page here ..
cr$showPage()
# ...
surface$dscComment(surface, "%IncludeFeature: *PageSize A5")
# ...
