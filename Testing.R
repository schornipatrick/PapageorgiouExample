# Source lecture scripts

source("BinominalTreeGenerator.R")
source("CostMatrixGenerator.R")
source("MainTimeBased.R")
source("TimeBasedControl.R")

# build Cost Matrix
CostMat1 <- CostMatrixGenerator()

# Run Experiment
TimeBasedControl(CostMat1)

# Source my solution 

source("Patrickgeorgiou_Patrick.R")

# name the choices once
choiceNames <- c("L", "R")

# specify the accuracy once, make it easily changeable
choiceAccur <- c(0.75, 0.75)

# Run the experiment and save the resulting matrix in own object
ResultMat1 <- RunExperiment(CostMat = CostMat1, choiceAccur = choiceAccur, choiceNames = choiceNames)
## --> Delivers same result but is way easier to adapt.


# For another example, make it easier to build a binary cost matrix by only specifying steps
step1 <- c(10,20)
step2 <- c(40,30)
step3 <- c(50,70)
step4 <- c(100,180)
inputMat <- rbind(step1, step2, step3, step4) 

# Everything else staying the same .. 
CostMat2 <- buildCostMat(inputMat = inputMat)
  
ResultMat2 <- RunExperiment(CostMat = CostMat2, choiceAccur = choiceAccur, choiceNames = choiceNames)