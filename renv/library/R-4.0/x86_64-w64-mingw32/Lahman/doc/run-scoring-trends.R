## ----Setup, message=FALSE-----------------------------------------------------

# package load 
library(Lahman)
library(ggplot2)
library(dplyr)


## ---- message=FALSE-----------------------------------------------------------


data(Teams)


MLB_RPG <- Teams %>%
  filter(yearID > 1900, lgID != "FL") %>%
  group_by(yearID) %>%
  summarise(R=sum(R), RA=sum(RA), G=sum(G)) %>%
  mutate(leagueRPG=R/G, leagueRAPG=RA/G)


## ---- message=FALSE-----------------------------------------------------------

MLBRPGplot <- ggplot(MLB_RPG, aes(x=yearID, y=leagueRPG)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  scale_x_continuous(breaks = seq(1900, 2015, by = 20)) +
  scale_y_continuous(limits = c(3, 6), breaks = seq(3, 6, by = 1))

MLBRPGplot


## ----message=FALSE------------------------------------------------------------
  
MLBRPGplot +
  ggtitle("MLB run scoring, 1901-2016") +
  theme(plot.title = element_text(hjust=0, size=16)) +
  xlab("year") +
  ylab("team runs per game")


## ---- message=FALSE-----------------------------------------------------------

MLBRPGplot +
  labs(title = "MLB run scoring, 1901-2016",
       subtitle = "Run scoring in 2016 was the highest in seven years",
       caption = "Source: the Lahman baseball database", 
       x = "year", y = "team runs per game") 



