## ----nomessages, echo = FALSE-------------------------------------------------
# set some default options for chunks
knitr::opts_chunk$set(
  warning = FALSE,   # avoid warnings and messages in the output
  message = FALSE,
  collapse = TRUE,   # collapse all output into a single block
  tidy = FALSE,      # don't tidy our code-- assume we do it ourselves
  fig.height = 5,
  fig.width = 7
)
options(digits=4)    # number of digits to display in output; can override with chunk option R.options=list(digits=)

set.seed(1234)       # reproducibility

## ----load-data----------------------------------------------------------------
library("dplyr")
data(Batting, package="Lahman")
str(Batting) #take a look at the data

## ----select-mutate------------------------------------------------------------
batting <- Batting %>% 
  # select the variables that we want left after we filter the data
  select(yearID, H, X2B, X3B, HR) %>%
  # select the years from 1871+
  filter(yearID >= 1871) %>% 
  group_by(yearID) %>%
#  summarise_each(funs(sum(., na.rm=TRUE))) %>% 
  summarise_all(funs(sum(., na.rm=TRUE))) %>% 
  # we summarize by year, and then na.rm takes care of 0's in the data
  mutate(X1 = H-(X2B+X3B+HR)) %>% #create a column for singles
  # we eventually want these as a percentage of hits, so we can do the math now 
  mutate(Single = X1/H*100) %>% 
  mutate(Double = X2B/H*100) %>% 
  mutate(Triple = X3B/H*100) %>% 
  mutate(HomeRun = HR/H*100)

## ----select2------------------------------------------------------------------
bat <- batting %>% 
  select(yearID, Single, Double, Triple, HomeRun)
#this makes a nice looking data frame before we move on

## -----------------------------------------------------------------------------
library(reshape2)
bat_long <- melt(bat, id.vars = c("yearID"))
head(bat_long)

## ----plot1--------------------------------------------------------------------
library(ggplot2)
hitsperyear <- ggplot(bat_long, aes(x=yearID, y= value, col=variable)) +
       geom_line() + 
  xlab("Major League Baseball Season") + 
  ylab("Percentage") + 
  ggtitle("Hits by Type in Major League Baseball") + 
  scale_x_continuous(breaks = c(1870, 1885, 1900, 1915, 1930, 1945, 
                                1960, 1975, 1990, 2005, 2020 )) + 
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  guides(colour=guide_legend(reverse=TRUE, 
                             aes(ggtitle= "Type of Hit")))
hitsperyear

## ----plot2--------------------------------------------------------------------
hitsperyear + geom_smooth(method="lm")


