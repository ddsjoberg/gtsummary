############################################
## batting-summstats   Calculate summary statistics for the Batting data frame

## Add some useful statistics:

require('plyr')
require('reshape2')
#require('data.table')
#require('ggplot2')
#require('lattice')

# mutate() is a faster version of transform()
# from the plyr package that allows one to use
# newly created variables in subsequent variable
# definitions within the same call.
#
# The function summstats() adds the following
# statistics for each line of a player's career
# as a hitter:
#   * batting average (BA)
#   * plate appearances (PA)
#   * total bases (TB)
#   * slugging percentage (SlugPct)
#   * on-base percentage (OBP)
#   * on-base percentage + slugging (OPS)
#   * batting average on balls in play (BABIP)
#
summstats <- function(d) {
    require('plyr')
    NAmassage <- function(x) {
    # Takes a column vector and replaces NAs by zeros
        x[is.na(x)] <- 0
        x
      }
    vars <- c('AB', 'R', 'H', 'X2B', 'X3B',
              'HR', 'RBI', 'SB', 'CS', 'BB', 'SO', 'IBB', 'HBP',
              'SH', 'SF', 'GIDP')
    d2 <- apply(d[, vars], 2, NAmassage)
    d2 <- if(is.vector(d2)) {as.data.frame(as.list(d2)) } else {
                as.data.frame(d2) }
    d2 <- mutate(d2,
      BA = ifelse(AB > 0, round(H/AB, 3), NA),
      PA = AB + BB + HBP + SH + SF,
      TB = H + X2B + 2 * X3B + 3 * HR,
      SlugPct = ifelse(AB > 0, round(TB/AB, 3), NA),
      OBP = ifelse(PA > 0,
           round((H + BB + HBP)/(PA - SH - SF), 3), NA),
      OPS = round(OBP + SlugPct, 3),
      BABIP = ifelse(AB > 0, round(H/(AB - SO), 3), NA)
      )
    data.frame(d, d2[, 18:22])
}

# This function creates a one-line summary of a player's
# cumulative career statistics, including those generated
# in the previous function. The career percentages are
# actually a weighted average of the seasonal percentages.

careerTotals <- function(d) {
    require('plyr')
    sumstats <-
      as.data.frame(as.list(colSums(as.matrix(d[, 6:24, drop = FALSE]),
                                               na.rm = TRUE)))
    cstats <- with(d, data.frame(
            beginYear = min(yearID),
            endYear = max(yearID),
            nyears = sum(stint == 1L),
            nteams = length(unique(teamID))  ))
    extrastats <- mutate(sumstats,
           BA = ifelse(AB > 0, round(H/AB, 3), NA),                   # batting average
           PA = AB + BB + HBP + SH + SF,                              # plate appearances
           TB = H + X2B + 2 * X3B + 3 * HR,                           # total bases
           SlugPct = ifelse(AB > 0, round(TB/AB, 3), NA),             # slugging percentage
           OBP = ifelse(PA > 0,                                       # on-base percentage
                       round((H + BB + HBP)/(PA - SH - SF), 3), NA),
           OPS = round(OBP + SlugPct, 3),
           BABIP = ifelse(AB > 0, round(H/(AB - SO), 3), NA)  )
    cbind(cstats, extrastats)
}

# Each of these takes about a minute or two...
# Create a *list* for the season profiles of each player
playerBattingProfiles <- dlply(Batting, .(playerID), summstats)

# Summarize into career summaries
# the output object is a *data frame*
careerBattingProfiles <- ldply(playerBattingProfiles, careerTotals)

# Individual player season records
# The difference in calls has to do with the types of objects
# from which one is extracting information
playerBattingProfiles[['iorgga01']]   # Garth Iorg
subset(careerBattingProfiles, playerID == 'brettge01') # George Brett

#########################################################################
