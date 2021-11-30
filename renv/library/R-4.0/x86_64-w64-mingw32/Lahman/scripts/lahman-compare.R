# Compare data sets from Lahman package to new ones created from .csv files


datalist <- scan(system.file("data", "datalist", package="Lahman"), what="character")

#datalist <- scan(what="character")
#AllstarFull
#Appearances
#AwardsManagers
#AwardsPlayers
#AwardsShareManagers
#AwardsSharePlayers
#Batting
#BattingPost
#Fielding
#FieldingOF
#FieldingPost
#HallOfFame
#Managers
#ManagersHalf
#Master
#Pitching
#PitchingPost
#Salaries
#Schools
#SchoolsPlayers
#SeriesPost
#Teams
#TeamsFranchises
#TeamsHalf

# read in lahman2012 data files and change names from XXX to XXX2012
# so they can be compared with the existing ones.

dir <- "C:/R/data/lahman2012/RData/"
for (i in 1:length(datalist)) {
	load(paste0(dir, datalist[i], ".RData")) 
	assign(paste0(datalist[i], "2012"), get(datalist[i]))
	}

rm(list=datalist)

# match two data frames based on ID variables
XnotinY <- function(X,Y){
	IDvars <- unlist(lapply(X, function(x) grep('.*ID$', colnames(X), value=TRUE)))
	X <- X[,IDvars]
	Y <- Y[,IDvars]
   X.vec <- apply(X, 1, paste, collapse = "")
   Y.vec <- apply(Y[,IDvars], 1, paste, collapse = "")
   X.without.Y.rows <- X[!X.vec %in% Y.vec,]
   return(nrow(X.without.Y.rows))
 }

# compare two versions of a data frame	
compare <- function(df1, df2) {
	name1 <- df1
	name2 <- df2
	df1 <- get(df1)
	df2 <- get(df2)
	vars1 <- colnames(df1)
	vars2 <- colnames(df2)
	cat(paste("\nComparing ", name1, "<->", name2, "\n"))
	if (setequal(vars1, vars2)) {
		cat(paste("   ", length(vars1), "variable names are in both:\n     ",
		          paste(vars1, collapse=', '),"\n"))
		ord <- all(vars1==vars2)
		cat(paste("    variable names in", ifelse(ord, "same", "different"), "order\n"))
		}
	else {
		cat(paste("   variable names in", name1, "but not in", name2, ":", paste(setdiff(vars1, vars2), collapse=", "), "\n"))
		cat(paste("   variable names in", name2, "but not in", name1, ":", paste(setdiff(vars2, vars1), collapse=", "), "\n"))
		}
	obs1 <- nrow(df1); obs2 <- nrow(df2)
	cat(paste("   ", obs1, "cases in", name1, "\n   ", obs2, "cases in", name2,
	          " (diff=", obs2-obs1, ")\n"))
	if (setequal(vars1, vars2)) {
		cat("    --", paste(XnotinY(df2, df1), "cases in", name2, "but not in", name1, "\n"))
		cat("    --", paste(XnotinY(df1, df2), "cases in", name1, "but not in", name2, "\n"))
	}	
}

# load existing versions
library(Lahman)
data(list=datalist)

#
# compare old to new ones
for (i in 1:length(datalist)) {
	compare(datalist[i], paste0(datalist[i], "2012"))
}

