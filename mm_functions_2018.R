library(dplyr)
## get seeds, tourney structure
seeds <- read.csv('KaggleData/NCAATourneySeeds.csv', stringsAsFactors = F)
seeds <- seeds %>% 
  mutate(SeedNum = gsub("[A-Z+a-z]", "", Seed)) %>% select(Season, TeamID, SeedNum)

seeds18 <- seeds %>% filter(Season == 2018) %>% 
  mutate(seed = as.numeric(SeedNum)) %>% select(seed, TeamID)
seeds18$teamName <- sapply(1:68, function(x) dat$Team[which(dat$TeamID == seeds18$TeamID[x])])

clean_dataset <- function(dat){
  dat$X <- NULL
  dat$Season <- NULL
  dat$Rank <- NULL
  dat$AdjD <- - dat$AdjD
  colnames(dat)
  head(dat)
  
  ## scale numeric columns
  dat2 <- dat
  for(j in 1:ncol(dat2)){
    if(colnames(dat2)[j] %in% c("Team","tournamentTeam", "Conf", "TeamID") ){
      next
    } else{
      dat2[,j] <- scale(dat2[, j])
    }
  }
  
  return(dat2)
}
  
  
makeSubmission <- function(tourney, ability, curyear = 2018){
  ability <- ability[tourney$teamName]
  id <- c()
  pred <- c()
  for (i in 1:68)
  {
    for (j in i:68)
    {
      if (i != j)
      {
        id <- c(id,paste(curyear, tourney$TeamID[i], tourney$TeamID[j], sep="_"))
        pred <- c(pred, exp(ability[i]) / (exp(ability[i]) + exp(ability[j])))
      }
    }
  }
  
  submissionMatrix <- data.frame(id = id, pred = pred)
  return(submissionMatrix)
}

simBracket <- function(submission, tourney, remove_vec)
{
  #seeds18 <- seeds18[-c(12,18,30,68),]
 if(nrow(tourney) == 68){
   tourney <- tourney[-remove_vec,]  
 }
 
  
  ## puts 64 teams in order they would compete in the bracket
  victors <- tourney[c(c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 0 * 16,
                       c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 1 * 16,
                       c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 2 * 16,
                       c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 3 * 16),]
  
  ## record winners for each round of tournament
  out <- vector("list")
  for (j in 1:6) ## number of rounds
  {
    seed <- c() # fill with seeds of winners
    winner <- c() # fill with teamID of winners
    for (i in seq(1,nrow(victors)-1, by=2) )
    {
      gameVec <- victors[i:(i+1),"TeamID"]
      ind <- which(substring(submission$id,6,14) %in% paste(gameVec,collapse="_"))
      if (length(ind) == 0) { # if team is listed second
        gameVec <- gameVec[2:1]
        ind <- which(substring(submission$id,6,14) %in% paste(gameVec,collapse="_"))
      }
      winprob <- submission$pred[ind]
      winner <- c(winner, ifelse(winprob > 0.5, gameVec[1], gameVec[2])) # runif(1) <= winprob
      if(length(winprob) == 0){print(i)} # in case there are issues
    }
    seed <- victors$seed[which(victors$TeamID %in% winner)]
    names <- victors$teamName[which(victors$TeamID %in% winner)]
    victors <- data.frame(seed=seed,TeamID=winner, teamName=names)
    out[[j]] <- victors
  }
  
  names(out) <- c("RoundOf32","Sweet16","Elite8","Final4","NatlChamp","Champion")
  return(out)
}

runSimul <- function(submit, tourn, nsim = 1000, remove_vec) {
  set.seed(1994)
  if(nrow(tourn) == 68){
    tourn <- tourn[-remove_vec,]  
  }
  tourney_sims <- vector("list")
  #nsim <- 1000
  for(k in 1:nsim){
    out <- vector("list")
    victors <- tourn[c(c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 0 * 16,
                       c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 1 * 16,
                       c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 2 * 16,
                       c(1,16,8,9,5,12,4,13,3,14,6,11,7,10,2,15) + 3 * 16),]
    
    for (j in 1:6) ## number of rounds
    {
      seed <- c() # fill with seeds of winners
      winner <- c() # fill with TeamID of winners
      for (i in seq(1,2^(7-j)-1, by=2) )
      {
        gameVec <- victors[i:(i+1),"TeamID"]
        ind <- which(substring(submit$id,6,14) %in% paste(gameVec,collapse="_"))
        if (length(ind) == 0) { # if team is listed second
          gameVec <- gameVec[2:1]
          ind <- which(substring(submit$id,6,14) %in% paste(gameVec,collapse="_"))
        }
        winprob <- submit$pred[ind]
        winner <- c(winner, ifelse(runif(1) <= winprob, gameVec[1], gameVec[2]))
        if(length(winprob) == 0){print(i)} # in case there are issues
      }
      seed <- victors$seed[which(victors$TeamID %in% winner)]
      names <- victors$teamName[which(victors$TeamID %in% winner)]
      victors <- data.frame(seed=seed,TeamID=winner, teamName=names)
      out[[j]] <- victors
    }
    tourney_sims[[k]] <- out
  }
  
  num_rounds <- matrix(0, ncol=64, nrow=nsim)
  colnames(num_rounds) <- tourn$teamName
  for(m in 1:nsim) {
    winnerID <- unlist(sapply(tourney_sims[[m]], "[", 2))
    num_rounds[m,] <- sapply(tourn$TeamID, function(x) sum(winnerID == x))
  }
  
  team_success <- apply(num_rounds, 2, function(y) sapply(0:6, function(x) sum(y == x)))
  #team_success <- rbind(tourney$seed,team_success)
  #out <- data.frame(paste0("(",tourney$seed,") ",tourney$teamName),t(team_success))
  out <- data.frame(paste0("(",tourn$seed,") ",tourn$teamName),
                    t(apply(t(team_success), 1, function(x) rev(cumsum(c(rev(x))))))[,-1])
  colnames(out) <- c("Team","RoundOf32","Sweet16","Elite8","Final4","NatlChamp","Champion")
  
  out <- dplyr::arrange(out, desc(Champion))
  return(out)
}
