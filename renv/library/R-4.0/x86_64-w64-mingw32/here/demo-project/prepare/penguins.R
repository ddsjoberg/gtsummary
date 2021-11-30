here::i_am("prepare/penguins.R", uuid = "115147ca-fedd-42ca-8650-2cb3dad0aebc")
library(here)

readr::write_csv(palmerpenguins::penguins, here("data/penguins.csv"))
