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

## ----load-packages, echo=FALSE------------------------------------------------
library(Lahman)   # Load additional packages here 
library(ggplot2)
library(dplyr)

## -----------------------------------------------------------------------------
data("Salaries", package="Lahman")
str(Salaries)
sample_n(Salaries, 10)

## ----Teams-names--------------------------------------------------------------
data("Teams", package="Lahman")
dim(Teams)
names(Teams)

## ----Teams-filter-------------------------------------------------------------
Teams <- Teams %>%
  select(yearID, lgID, teamID, name, divID, Rank, WSWin, attendance) %>%
  filter(yearID >= 1985) %>%
  mutate(teamID = droplevels(teamID))

sample_n(Teams, 10)

## ----SeriesPost-names---------------------------------------------------------
data("SeriesPost", package="Lahman")
names(SeriesPost)

## -----------------------------------------------------------------------------
table(SeriesPost$round)

## ----filter-WS----------------------------------------------------------------
WS <- SeriesPost %>%
  filter(yearID >= 1985 & round == "WS") %>%
  select(-ties, -round) %>%
  mutate(lgIDloser = droplevels(lgIDloser),
         lgIDwinner = droplevels(lgIDwinner))
dim(WS)
sample_n(WS, 6)

## -----------------------------------------------------------------------------
table(Salaries$yearID)

## -----------------------------------------------------------------------------
range(Salaries$salary)

## -----------------------------------------------------------------------------
Salaries %>% 
  group_by(yearID) %>% 
  summarise(min=min(salary),
            max=max(salary))

## -----------------------------------------------------------------------------
which(Salaries$salary==0)

## ----which-zero---------------------------------------------------------------
Salaries[which(Salaries$salary==0),]

## ----filter-salaries----------------------------------------------------------
Salaries <- Salaries %>%
    filter(salary !=0)

## ----summarise-salaries-------------------------------------------------------
payroll <- Salaries %>%
                group_by(teamID, yearID) %>%
                summarise(payroll = sum(salary)/1000000)
head(payroll)

## ----merge-teams--------------------------------------------------------------
payroll <- merge(payroll, Teams[,c("yearID", "teamID","name", "WSWin")], 
                 by=c("yearID", "teamID")) 
sample_n(payroll, 10)

## ----eval=FALSE---------------------------------------------------------------
#  left_join(payroll, Teams[,c("yearID", "teamID","name", "WSWin")],
#            by=c("yearID", "teamID")) %>%
#    sample_n(10)

## ----make-WSWin-factor--------------------------------------------------------
payroll <- payroll %>%
  mutate(WSWin = factor(WSWin))

## -----------------------------------------------------------------------------
table(payroll$WSWin, useNA="ifany")

## ----payroll-boxplot, fig.width=7---------------------------------------------
boxplot(payroll ~ yearID, data=payroll, ylab="Payroll ($ millions)")

## ---- payroll-Boxplot, fig.width=7--------------------------------------------
out <- car::Boxplot(payroll ~ yearID, data=payroll,
             id=list(n=1, 
                     labels=as.character(payroll$teamID)), 
             ylab="Payroll ($ millions)")

## -----------------------------------------------------------------------------
table(out)

## -----------------------------------------------------------------------------
inflation = c(1,    1.02, 1.06, 1.10, 1.15, 1.21, 
              1.27, 1.30, 1.34, 1.38, 1.42, 1.46, 1.49, 1.51, 1.55, 1.60,
              1.65, 1.67, 1.71, 1.76, 1.82, 1.87, 1.93, 2.00, 1.99, 2.03,
              2.09, 2.13, 2.16, 2.20, 2.20 )

inflation.df <- data.frame(year=1985:2015, inflation)

# plot inflation rate
ggplot(inflation.df, aes(y=inflation, x=year)) +
  geom_point() +
  geom_line() +
  geom_smooth(method="lm")
  

## -----------------------------------------------------------------------------
infl.lm <- lm(inflation ~ year, data=inflation.df)
(coefs <- coef(infl.lm))

## -----------------------------------------------------------------------------
payroll <- payroll %>%
  mutate(payrollStd = payroll / (coefs[1] + coefs[2] * yearID))

## ---- payroll-Boxplot2, fig.width=7-------------------------------------------
car::Boxplot(payrollStd ~ yearID, data=payroll,
             id = list(labels=as.character(payroll$teamID)), 
             ylab="Payroll (1985-adjusted $ millions)")

## ---- payroll-winners-plot, fig.width=7---------------------------------------
Cols <- ifelse(payroll$WSWin=='Y', "red", gray(.7, alpha=0.5))
with(payroll, {
  plot(payrollStd ~ jitter(yearID, 0.5), 
       ylab = "Payroll (inflation-adjusted $ millions)",
       ylim = c(5,125), log = "y",
       xlab = "Year",
       pch = 19, cex = 0.8, col = Cols)
})
with(payroll[payroll$WSWin == 'Y',],
     text(y = payrollStd, x = yearID, labels = teamID, pos = 3, cex = 0.8) )

