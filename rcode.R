# Replication file for: "Replicating the Axelrod Tournaments"
# RPubs-link: https://rpubs.com/mstefan-rpubs/axelrod
# (c) Martin Stefan, August 2020

rm(list = ls())

ALLC <- function(p,h) return("C")

ALLD <- function(p,h) return("D")

RANDOM <- function(p,h) return(sample(c("C","D"),1))

ALTERNATE <- function(p,h) {
  if(nrow(h) %% 2 == 0) return("D")
  return("C")
}

DAVIS <- function(p,h) {
  if(nrow(h) < 10) return("C")
  if(h[nrow(h),-p] == "D") return("D")
  return("C")
}

FELD <- function(p,h,nturns=200) {
  if(nrow(h) == 0) return("C")
  if(h[nrow(h),-p] == "C") {
    prob <- seq(from=1,to=.5,length.out=nturns)[nrow(h)+1]
    return(sample(c("C","D"), 1, prob=c(prob,1-prob)))
  }
  return("D")
}

FRIEDMAN <- function(p,h) {
  if(nrow(h) == 0) return("C")
  if(any(h[,-p] == "D")) return("D")
  return("C")
}

GROFMAN <- function(p,h) {
  if(nrow(h) == 0) return("C")
  if(h[nrow(h),p] != h[nrow(h),-p]) {
    return(sample(c("C","D"), 1, prob=c(2/7,5/7)))
  }
  return("C")
}

TFT <- function(p,h) {
  if(nrow(h) == 0) return("C")
  if(h[nrow(h),-p] == "C") return("C")
  return("D")
}

JOSS <- function(p,h) {
  if(nrow(h) == 0) return("C")
  if(h[nrow(h),-p] == "C") {
    return(sample(c("C","D"), 1, prob=c(.9,.1)))
  }
  return("D")
}

TF2T <- function(p,h) {
  if(nrow(h) < 2) return("C")
  if(all(h[(nrow(h)-1):nrow(h),-p] == "D")) return("D")
  return("C")
}

NYDEGGER <- function(p,h) {
  if(nrow(h) == 0) return("C") # 1st move
  if(nrow(h) == 1) { # 2nd move
    ifelse(h[1,-p] == "C", return("C"), return("D")) 
  }
  if(nrow(h) == 2) { # 3rd move
    if(identical(h, matrix(c("D","C","C","D"),2))) return("D")
    ifelse(h[2,-p] == "C", return("C"), return("D")) 
  }
  mat <- h[(nrow(h)-2):nrow(h),] # any other move
  points <- rep(NA,3)
  for(i in 1:3) {
    if(identical(mat[i,] , c("C","D"))) points[i] <- 1
    if(identical(mat[i,] , c("D","C"))) points[i] <- 2
    if(identical(mat[i,] , c("D","D"))) points[i] <- 3
  }
  points <- sum(points*c(16,4,1))
  if(points %in% c(1,6,7,17,22,23,26,29,30,31,33,38,39,45,49,54,55,58,61)) {
    return("D")
  }
  return("C")
}

SHUBIK <- function(p, h) {
  
  # first round
  if(nrow(h) == 0) return("C")
  
  # start of retaliation
  if(h[nrow(h),p] == "C" &  h[nrow(h),-p] == "D") return("D")
  
  # ongoing retaliation
  if(nrow(h) >= 2) {
    k = 0
    for(i in 2:nrow(h)) if(h[i-1,p] == "D" & h[i,p] == "C") k <- k+1
    if(h[nrow(h),p] == "D" & any(h[(nrow(h)-k):nrow(h),p] == "C")) return("D")
  }
  
  # else
  return("C")
}

TULLOCK <- function(p,h){
  
  # fist eleven rounds
  if(nrow(h) < 11) return("C")
  
  # subsequent rounds
  shareD <- sum(h[((nrow(h)-9):nrow(h)),-p] == "D")/10
  return(sample(c("C","D"),T,prob=c(1-shareD,shareD)))
  
}


# payoff function
payoff <- function(h, r=3, t=5, s=0, p=1) {
  if(h[1] == "C" & h[2] == "C") result = c(r,r)
  if(h[1] == "C" & h[2] == "D") result = c(s,t)
  if(h[1] == "D" & h[2] == "C") result = c(t,s)
  if(h[1] == "D" & h[2] == "D") result = c(p,p)
  names(result) <- colnames(h)
  return(result)
}

# game function
game <- function(STRATEGY1, STRATEGY2, snames=c("P1","P2"), nturns=200) {
  
  # empty history matrix
  history <- matrix(NA,nturns,2)
  colnames(history) <- snames

  # run game
  for(t in 1:nturns) {
    decision1 <- STRATEGY1(p=1, h=history[0:(t-1),,drop=F])
    decision2 <- STRATEGY2(p=2, h=history[0:(t-1),,drop=F])
    history[t,] <- c(decision1,decision2)
  }
  
  # compute payoffs and total scores
  payoffs <- t(apply(history, MAR=1, FUN=payoff))
  colnames(payoffs) <- snames
  totals <- colSums(payoffs)
  
  # return
  return(list("History"=history, "Totals"=totals))
  
}


# tournament function
tournament <- function(strategies, names=NULL, nturns=200) {
  
  # empty results matrix
  nstrat <- length(strategies)
  results <- matrix(NA,nstrat,nstrat)
  if(is.null(names)) names <- paste("Strategy", 1:nstrat)
  colnames(results) <- names
  rownames(results) <- names
  
  # fill matrix
  for(i in 1:nstrat){
    for(j in 1:nstrat){
      if(i > j) next # skip if already filled in via [j,i]
      result <- game(strategies[[i]],strategies[[j]],nturns=nturns)
      results[i,j] <- result$Totals[1]
      results[j,i] <- result$Totals[2]
    }
  }
  
  # average across all strategies
  avg <- apply(results,MAR=1,FUN=mean)
  avgpturn <- avg/nturns

  
  # order by average score
  order <- order(avg, decreasing = T)
  results <- results[order,] # order rows
  results <- results[,order] # order columns
  
  # add averages to results matrix
  results <- cbind(round(avg[order],1),round(avgpturn[order],2),results)
  colnames(results)[1:2] <- c("Avgerage","Avg./Turn")
  
  # return
  return(results)
  
}

# run small tournament
tournament(strategies=c(TFT,FRIEDMAN,TF2T), names=c("TFT","FRIEDMAN","TF2T"))

# run large tournament
strategies <- c(RANDOM,ALLC,ALLD,ALTERNATE,FRIEDMAN,
                TFT,TF2T,DAVIS,JOSS,GROFMAN,
                SHUBIK,FELD,NYDEGGER,TULLOCK)
names <- c("RANDOM","ALLC","ALLD","ALTERNATE","FRIEDMAN",
           "TFT","TF2T","DAVIS","JOSS","GROFMAN",
           "SHUBIK","FELD","NYDEGGER","TULLOCK")
tournament(strategies=strategies,names=names,nturns=200)
