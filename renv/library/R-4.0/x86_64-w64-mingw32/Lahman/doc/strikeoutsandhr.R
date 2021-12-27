## ----nomessages, echo = FALSE-------------------------------------------------
# set some default options for chunks
knitr::opts_chunk$set(
  warning = FALSE,   # avoid warnings and messages in the output
  message = FALSE,
  collapse = TRUE,   # collapse all output into a single block
  tidy = FALSE,      # don't tidy our code-- assume we do it ourselves
  fig.height = 5,
  fig.width = 5
)
options(digits=4)    # number of digits to display in output; can override with chunk option R.options=list(digits=)
par(mar=c(3,3,1,1)+.1)

set.seed(1234)       # reproducibility

## ----load-packages------------------------------------------------------------
library(Lahman) 
library(ggplot2) 
library(dplyr)
library(car)

## ----Batting-names------------------------------------------------------------
data("Batting", package="Lahman") # load the data
str(Batting) # take a look at the structure of the complete data set, as it is

## ----Batting-filter-----------------------------------------------------------
Batting <- Batting %>%
  select(yearID, AB, SO, HR) %>% # select the variables that we need
  group_by(yearID) %>% # group by year, so that each row is one year
  summarise_each(funs(sum)) # we want the sum of AB, HR, and SO in the other rows

FullBatting<- Batting %>% # create a new variable that has SO rate and HR rate
  filter(yearID >= 1950) %>% # select the years from 1900+
  mutate(SO_rate = (SO/AB)*100, HR_rate = (HR/AB)*100) #add SO rate and HR rate as percentages to our data frame  
  
some(FullBatting) # look at a set of random observations

## -----------------------------------------------------------------------------
dim(FullBatting) # show the dimensions of the data frame

## -----------------------------------------------------------------------------
sum(FullBatting$SO) # find the sum of strikeout column

## -----------------------------------------------------------------------------
mean(FullBatting$SO_rate) # find the mean of the strikeout rate column

## -----------------------------------------------------------------------------
sum(FullBatting$HR) # find the sum of home run column

## -----------------------------------------------------------------------------
mean(FullBatting$HR_rate) # find the mean of the home run rate column

## -----------------------------------------------------------------------------
corr <- cor.test(FullBatting$SO_rate, FullBatting$HR_rate)
corr # find the correlation between strikeout rate and home run rate

## -----------------------------------------------------------------------------
Model_Totals <- lm(SO_rate~HR_rate, data=FullBatting)
summary(Model_Totals) # look at the model totals

## -----------------------------------------------------------------------------
plot <- ggplot(FullBatting, aes(x= SO_rate, y= HR_rate))+
geom_point()+ 
  xlab("Strikeout Rate") +
  ylab("Home Run Rate") +
  ggtitle("Relationship Between Strikeouts and Home Runs")
plot + stat_smooth(method= "lm") ##stat_smooth fits the model and then we plot the linear regression model

