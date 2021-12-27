# Script to read the Lahman data base .csv files & create Rdata files

# directory where the .csv files will be created
indir <- "D:/Dev/R/Lahman/source-data"

# directory where the .RData files will be created
outdir <- "D:/Dev/R/Lahman/data"

setwd(indir)

# local data location
dataFile <- "../source-data/baseballdatabank-2021.2.zip"

# no need to download if we already have the file
if (!file.exists(dataFile)) {
  zipfile <- "https://github.com/chadwickbureau/baseballdatabank/archive/refs/tags/v2021.2.zip"
  download.file(zipfile, dataFile)
}

unzip(dataFile, exdir=indir)

# Read the Lahman MLB .csv files and create .RData and .Rd files
#Batting <- read.csv(file="Batting.csv", header=TRUE, stringsAsFactors=FALSE, na.strings="")
#Master <- read.csv(file="Master.csv", header=TRUE, stringsAsFactors=FALSE)

# set indir to the directories the csv are extracted to
indir <- paste0(indir, "/baseballdatabank-2021.2/core")
setwd(indir)

(files <- list.files(path=getwd(), pattern="*.csv$"))

for (i in 1:length(files)) {
	inp <- read.csv(file=files[i], header=TRUE, stringsAsFactors=FALSE, na.strings="")
	cat("Read:", files[i], "\trows: ", nrow(inp), " cols: ", ncol(inp), "\n")

	# make a few variables factors
	if("lgID" %in% names(inp)) inp$lgID <- factor(inp$lgID)
	if("teamID" %in% names(inp)) inp$teamID <- factor(inp$teamID)
	if("inducted" %in% names(inp)) inp$inducted <- factor(inp$inducted)
	if("category" %in% names(inp)) inp$category <- factor(inp$category)
	if("plyrMgr" %in% names(inp)) inp$plyrMgr <- factor(inp$plyrMgr)
	if("franchID" %in% names(inp)) inp$franchID <- factor(inp$franchID)
	if("lgIDwinner" %in% names(inp)) inp$lgIDwinner <- factor(inp$lgIDwinner)
	if("lgIDloser" %in% names(inp)) inp$lgIDloser <- factor(inp$lgIDloser)
	if("teamIDwinner" %in% names(inp)) inp$teamIDwinner <- factor(inp$teamIDwinner)
	if("teamIDloser" %in% names(inp)) inp$teamIDloser <- factor(inp$teamIDloser)

	cname <- name <- sub(".csv", "", files[i])
	assign( name, inp)
  
	# these will be saved below after being compressed
	#save(inp, file=paste(cname, ".RData", sep=""))
  #promptData(inp, name=cname)
}

# fix column names or perform any needed data cleanup here
#colnames(HallOfFame)[2] <- 'yearID'


# Date variables

# $ debut       : chr  "4/13/1954 0:00:00" "4/10/1962 0:00:00" "7/26/1977 0:00:00" "9/10/2001 0:00:00" ...
# $ finalGame   : chr  "10/3/1976 0:00:00" "9/26/1971 0:00:00" "10/3/1990 0:00:00" "4/13/2006 0:00:00" ...
 
##################
# Convert debut/finalGame from character to Date class
# The format in the as.Date() function is the 'informat', 
# in SASspeak; the output format is a calendar date in 
# yyyy-mm-dd format.
People <- within(People, {
#    debut = as.Date(debut, 
#                    format = '%m/%d/%Y %H:%M:%s',
#                    origin = '1970-01-01')
#    finalGame = as.Date(finalGame, 
#                        format = '%m/%d/%Y %H:%M:%s',
#                        origin = '1970-01-01')
   birthDate = as.Date(paste(birthYear, birthMonth, birthDay, sep = '-'),
                       format = '%Y-%m-%d')
   deathDate = as.Date(paste(deathYear, deathMonth, deathDay, sep = '-'),
                       format = '%Y-%m-%d')
   bats = factor(bats)
   throws = factor(throws)
})

#* checking data for non-ASCII characters ... WARNING
#  Warning: found non-ASCII string(s)
#  'named Guillermo VelC!zquez' in object 'Master'
#  'Martmn Magdaleno Dihigo (Llanos)' in object 'Master'

tools:::showNonASCII(paste0(indir, 'People.csv'))

# then, fix manually, because I don't know an R way ...

setwd(outdir)

# compress mightily on save
#options(save.defaults=list(compress="xz", compression_level=9))

save(AllstarFull,         file="AllstarFull.RData", version = 2)        
save(Appearances,         file="Appearances.RData", version = 2)        
save(AwardsManagers,      file="AwardsManagers.RData", version = 2)     
save(AwardsPlayers,       file="AwardsPlayers.RData", version = 2)      
save(AwardsShareManagers, file="AwardsShareManagers.RData", version = 2)
save(AwardsSharePlayers,  file="AwardsSharePlayers.RData", version = 2) 
save(Batting,             file="Batting.RData", version = 2)            
save(BattingPost,         file="BattingPost.RData", version = 2)
save(CollegePlaying,      file="CollegePlaying.RData", version = 2)
save(Fielding,            file="Fielding.RData", version = 2)           
save(FieldingOF,          file="FieldingOF.RData", version = 2)
save(FieldingOFsplit,     file="FieldingOFsplit.RData", version = 2)
save(FieldingPost,        file="FieldingPost.RData", version = 2)       
save(HallOfFame,          file="HallOfFame.RData", version = 2)         
save(HomeGames,           file="HomeGames.RData", version = 2)             
save(Managers,            file="Managers.RData", version = 2)           
save(ManagersHalf,        file="ManagersHalf.RData", version = 2)
save(Parks,               file="Parks.RData", version = 2)
save(People,              file="People.RData", version = 2)
save(Pitching,            file="Pitching.RData", version = 2)           
save(PitchingPost,        file="PitchingPost.RData", version = 2)       
save(Salaries,            file="Salaries.RData", version = 2)           
save(Schools,             file="Schools.RData", version = 2)
save(SeriesPost,          file="SeriesPost.RData", version = 2)         
save(Teams,               file="Teams.RData", version = 2)              
save(TeamsFranchises,     file="TeamsFranchises.RData", version = 2)    
save(TeamsHalf,           file="TeamsHalf.RData", version = 2)

# Master table was changed to People in the 2017 data
# We will maintain Master as part of the package for now as it's likely to be a breaking change
Master <- People
save(Master,              file="Master.RData", version = 2)

# version currently still defaults to 2 here, but setting for backwards compatibility
tools::resaveRdaFiles(outdir, compress="xz", compression_level=9, version = 2)

# only ran this once, since all .Rd files were extensively edited
# TODO: come up with a better way to automatically update Rd files with count/year/etc updates
if (FALSE) {
  promptData(Allstar,             filename="Allstar.Rd")            
  promptData(AllstarFull,         filename="AllstarFull.Rd")        
  promptData(Appearances,         filename="Appearances.Rd")        
  promptData(AwardsManagers,      filename="AwardsManagers.Rd")     
  promptData(AwardsPlayers,       filename="AwardsPlayers.Rd")      
  promptData(AwardsShareManagers, filename="AwardsShareManagers.Rd")
  promptData(AwardsSharePlayers,  filename="AwardsSharePlayers.Rd") 
  promptData(Batting,             filename="Batting.Rd")            
  promptData(BattingPost,         filename="BattingPost.Rd")
  promptData(CollegePlaying,      filename="CollegePlaying.Rd") 
  promptData(Fielding,            filename="Fielding.Rd")           
  promptData(FieldingOF,          filename="FieldingOF.Rd")         
  promptData(FieldingPost,        filename="FieldingPost.Rd")       
  promptData(HallOfFame,          filename="HallOfFame.Rd")         
  promptData(HOFold,              filename="HOFold.Rd")             
  promptData(Managers,            filename="Managers.Rd")           
  promptData(ManagersHalf,        filename="ManagersHalf.Rd")       
  promptData(Master,              filename="Master.Rd")             
  promptData(Pitching,            filename="Pitching.Rd")           
  promptData(PitchingPost,        filename="PitchingPost.Rd")       
  promptData(Salaries,            filename="Salaries.Rd")           
  promptData(Schools,             filename="Schools.Rd")
  promptData(SeriesPost,          filename="SeriesPost.Rd")         
  promptData(Teams,               filename="Teams.Rd")              
  promptData(TeamsFranchises,     filename="TeamsFranchises.Rd")    
  promptData(TeamsHalf,           filename="TeamsHalf.Rd")          
  promptData(Xref_Stats,          filename="Xref_Stats.Rd")         
}
