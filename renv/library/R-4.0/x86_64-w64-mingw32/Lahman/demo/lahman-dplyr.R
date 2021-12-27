# some examples using dplyr with Lahman

library(Lahman)
library(dplyr)

#' ## Basic manipulations

# select some variables
batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H, HR) 
# sort by player, year, team
batting <- arrange(batting, playerID, yearID, teamID)
# keep only recent years
batting <- filter(batting, yearID > 1985)

# add salary to Batting data; need to match by player, year and team
# NB:  dplyr coerces yearID to character because it is a factor in Salaries
(batting <- batting %>% left_join(Salaries))

# the same in base R using merge():
batting2 <- merge(batting, 
                 Salaries[,c("playerID", "yearID", "teamID", "salary")], 
                 by=c("playerID", "yearID", "teamID"), all.x=TRUE)


# Add name, age and bat hand information from People
master <- select(tbl_df(People), playerID, birthYear, birthMonth, 
                                 nameLast, nameFirst, bats)
batting <- batting %>%
	left_join(master) %>%
	mutate(age = yearID - birthYear - ifelse(birthMonth < 10, 0, 1)) %>%
	select(-(birthYear:birthMonth))

# same with base R	                                 
People[, c('playerID', 'birthYear', 'birthMonth',
                          'nameLast', 'nameFirst', 'bats')]
batting2 <- merge(batting, masterInfo, all.x = TRUE)
batting2$age <- with(batting, yearID - birthYear -
                             ifelse(birthMonth < 10, 0, 1))

#' ## Queries about players

# form a players data.frame, that is grouped by playerID
players <- group_by(batting, playerID)

# For each player, find the two years with most hits
filter(players, min_rank(desc(H)) <= 2 & H > 0)

# Within each player, rank each year by the number of games played
mutate(players, G_rank = min_rank(G))

# For each player, find every year that was better than the previous year
filter(players, G > lag(G))
# For each player, compute avg change in games played per year
mutate(players, G_change = (G - lag(G)) / (yearID - lag(yearID)))

# For each player, find all where they played more games than average
filter(players, G > mean(G))
# For each, player compute a z score based on number of games played
mutate(players, G_z = (G - mean(G)) / sd(G))
