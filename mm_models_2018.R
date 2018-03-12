#######################################
## Sports Analytics Club at NC State ##
##### March Madness Model Building ####
########### March 12, 2018 ############
#######################################

## read in KenPom data, custom functions
dat <- read.csv("kenpom2018_merged.csv", stringsAsFactors = F)
source("mm_functions_2018.R")
head(dat)

## remove unnecessary columns, scale useful variable
clean_dat <- clean_dataset(dat)

## pick variables to use
colnames(clean_dat)
vars <- c("AdjO", "AdjD","FTRate","ARate","StlRate")

## assign weight to each variable
weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)
weights <- c(0.40, 0.40, 0.1, 0.05, 0.05)

## multiply weights by variables
ability <- t(weights) %*% t(clean_dat[,vars])

## subset to this year's NCAA teams
tourney_abilities <- ability[dat$tournamentTeam]
names(tourney_abilities) <- dat$Team[dat$tournamentTeam]

## calculate all head-to-head matchups
brad_terry_exp <- outer(tourney_abilities, tourney_abilities, 
                    FUN = function(a,b){exp(a)/(exp(a)+exp(b))})
View(brad_terry_exp)

## Probability that first team beats second team
brad_terry_exp["North Carolina St.","Seton Hall"]
brad_terry_exp["Virginia", "UMBC"]

## remove first four teams to get bracket of 64
first_four_teams <- c(12,18,30,68)
#first_four_teams <- c(11,17,29,67)

## get Kaggle-ready submission file
submission <- makeSubmission(seeds18, tourney_abilities)

## single bracket predictions
pred_bracket <- simBracket(submission, seeds18, first_four_teams)
pred_bracket

## 1000 simulated brackets
all_tourneys <- runSimul(submission, seeds18, nsim = 1000, first_four_teams)
View(all_tourneys)
