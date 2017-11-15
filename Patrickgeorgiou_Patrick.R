# by Patrick Schornboeck, 1326634

# a binary CostMatrix Builder can take an input Matrix, consisting just of binary outcomes
# at different steps and build a Cost Matrix, indicating all possible costs.

# step1 <- c(10,20)
# step2 <- c(40,30)
# step3 <- c(50,70)
# # step4 <- c(100,180)
# 
# inputMat <- rbind(step1, step2, step3) #step4) 
# The whole proess works for more than 3 steps as well. 
# But keep in mind, it is an exponentional problem!

# the names of the two choices can be specified
# choiceNames <- c("L", "R")

# the choice accuracy, meaning the probability of actually going,
# where one decides to go can be inputted like that.
# choiceAccur <- c(0.75, 0.75)


# The binary CostMatrix Builder
buildCostMat <- function(inputMat) {
  numbsteps <- nrow(inputMat)
  numbposs <- 2^numbsteps
  CostMat <- matrix(NA, numbposs, numbsteps)
  
  # build it up properly row by row
  for (i in 1:numbsteps) {
    split <- numbposs / (2^i)
    addVect_i <- rep(inputMat[i,], each = split)
    while (length(addVect_i) < numbposs) {
      addVect_i <- c(addVect_i, addVect_i)
      }
    CostMat[,i] <- addVect_i
  }
  CostMat
}

# For running the experiment, it was found useful to get to the dimensions of the input matrix
# this is used as input for other functions.
getInputMatDims <- function(CostMat) {
  Matrows <- ncol(CostMat)
  Matcols <- 2
  InputMatDims <- matrix(NA, Matrows, Matcols)
  InputMatDims
}

# The choice matrix is built automatically, taking into account the number of steps.
buildChoiceMat <- function(inputMat = matrix(NA, 3, 2), choiceNames = c("L", "R")) {
  numbsteps <- nrow(inputMat)
  numbposs <- 2^numbsteps
  ChoiceMat <- matrix(NA, numbposs, numbsteps)
  
  for (i in 1:numbsteps) {
    split <- numbposs / (2^i)
    addVect_i <- rep(choiceNames, each = split)
    while (length(addVect_i) < numbposs) {
      addVect_i <- c(addVect_i, addVect_i)
    }
    ChoiceMat[,i] <- addVect_i
  }
  ChoiceMat
}

# build probability matrix according to the chosen strategy
# takes in choices (first indicates up, second indicates down, names don't really matter)
buildPropMat <- function(
  inputMat = matrix(NA, 3, 2), choiceNames = c("L", "R"), choiceAccur = c(0.7, 0.7), strat = c("L", "L", "L")) {
  
  numbsteps <- nrow(inputMat)
  numbposs <- 2^numbsteps
  PropMat <- matrix(NA, numbposs, numbsteps)
  
  # build PropMat it carefully row by row --> first it gives independent probabilities
  for (i in 1:numbsteps) {
    split <- numbposs / (2^i)
    
    # read in choice for step i 
    choice <- strat[i]
    
    # calculate probs for up or down
    # chosen: UP
    if (choice == choiceNames[1]) {
      up <- choiceAccur[1]
      down <- 1-up
    }
    # chosen: DOWN
    else {
      down <- choiceAccur[2]
      up <- 1 - down
    }
    # build accurVec
    accurVec <- c(up, down)
    
    # attach it the same way as before
    addVect_i <- rep(accurVec, each = split)
    while (length(addVect_i) < numbposs) {
      addVect_i <- c(addVect_i, addVect_i)
    }
    PropMat[,i] <- addVect_i
    # this gives us just a probility matrix with independent probabilities.
  }
  
  # Calculate dependent probabilities in PropMat
  for (irow in 1:nrow(PropMat)) {
    for (icol in 2:ncol(PropMat)) {
      PropMat[irow, icol] <- PropMat[irow, icol-1] * PropMat[irow, icol]
    }
  }
  
  PropMat
}

# for a given probability matrix, this function will 
# calculate the expected costs and return the according number.
CalcExpecCosts <- function(PropMat, CostMat) {
  PathCosts <- apply(CostMat, 1, sum)
  PathProp <- PropMat[, ncol(PropMat)]
  ExpecCosts <- sum(PathCosts * PathProp)
  ExpecCosts
}

# Given a Cost matrix, this function returns the result matrix, 
# showing the expected costs for every possible strategy
# It also prints the best strategy possible.
RunExperiment <- function(CostMat, choiceAccur = c(0.7, 0.7), choiceNames = c("L", "R")) {
  # build fake (empty) input matrix to use in build Mat functions (just dimensions are of importance for function use)
  inputMatDims <- getInputMatDims(CostMat = CostMat)
  # build Choice Matrix
  ChoiceMat <- buildChoiceMat(inputMat = inputMatDims, choiceNames = choiceNames)
  
  # Go through each choice ( = row of ChoiceMat)
  costvect <- c(rep(NA, each = nrow(CostMat)))
  ResultMat <- cbind(ChoiceMat, costvect)
  
  for (i in 1:nrow(ChoiceMat)) {
    strat_i <- ChoiceMat[i, ]
    PropMat <- buildPropMat(
      inputMat = inputMatDims, choiceNames = choiceNames, choiceAccur = choiceAccur, strat = strat_i)
    stratcosts <- CalcExpecCosts(PropMat = PropMat, CostMat = CostMat)
    ResultMat[i, ncol(ResultMat)] <- stratcosts
  }
  MinCosts <- min(ResultMat[,ncol(ResultMat)])
  BestStrat <- ResultMat[ResultMat[, ncol(ResultMat)] == MinCosts, ]
  print("Best Strategy: ")
  print(BestStrat)
  ResultMat
}


