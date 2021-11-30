## ---- warning=F, message=F----------------------------------------------------
library(kableExtra)
dt <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Text_1 = c("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ","In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "),
  Text_2 = c("Duis posuere placerat magna, ac aliquam lorem viverra non. Ut ultrices tempus eros, quis sodales libero commodo non. In non neque ut lacus vestibulum dictum a quis ipsum. ", "Aenean ut justo interdum, laoreet enim nec, viverra eros. Donec vel pharetra nunc. Suspendisse vel ipsum ac lectus semper aliquam ac a orci. Suspendisse libero mauris, egestas semper auctor sit amet, tempor et orci. ", "Phasellus quis neque aliquet, finibus nunc eget, lacinia neque. Sed auctor lectus vel ex scelerisque commodo. ")
)

## -----------------------------------------------------------------------------
kable(dt, "latex", booktabs = T, 
      col.names = c("Item", "Short Title", "Very Very Very Very Very Very Long Title")) %>%
  column_spec(2:3, width = "5cm")

## -----------------------------------------------------------------------------
linebreak("a\nb")

## -----------------------------------------------------------------------------
dt2 <- data.frame(
  Item = c("Hello\nWorld", "This\nis a cat"), 
  Value = c(10, 100)
) 
dt2$Item <- linebreak(dt2$Item)

dt2 %>%
  kable("latex", booktabs = T, escape = F,
        caption = "Main Title\\\\Subtitle",
        col.names = linebreak(c("Item\n(Name)", "Value\n(Number)"), align = "c"))

## -----------------------------------------------------------------------------
dt2 %>%
  kable("latex", booktabs = T, escape = F,
        col.names = linebreak(c("Item\n(Name)", "Value\n(Number)"), align = "c")) %>%
  add_header_above(c("Combined\nTitle" = 2)) %>%
  pack_rows("Group\n1", 2, 2)

