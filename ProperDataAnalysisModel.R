
require(lattice)
require(vegan)
require(mgcv)
source("/Users/frederik/Documents/work/functions/Functions.R")
#get locations of simulated data and corresponding substances and concentrations
#...after specifying what simulations to use for analyses. 
sims <- "VI"
ResultsPath <- paste("/Users/frederik/Documents/work/BD_EF/simulations",sims,"/output/",sep="")#Say where the results are
#specify destination for plots and other output
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"
#color specs: input for all possible color codes are generated here,
#for later input into rgb
cols <- rgb(runif(0,1,100), runif(0,1,100), runif(0,1,100), 1)
Iterations <- 50 #nr of iterations per level
n <- 20#initial nr of species
Concs <- seq(0, 200, 20) #seq(0, 200, 20)#tested concentrations
Alphas <- c(0.2,0.6)#c(0.2, 0.4, 0.6, 0.8)#c(0.5) #tested alphas #
Unifs <- c("False")#c("True","False") #initial SAD: uniform or skewed
DeltasAlphas <- c(0.1, 0.3)#, 0.2)#0.2, 0.4)#, 0.4) #c(0, 0.1, 0.2)c(0.49)#
Corrs <- c(-1,0,1)

#where do the counts start in the files with counts? 
#take into account that fake time (needed for bdef function), treatment, 
#and three EF scenarios need to be added
CountColsStart <- 6
#starting and ending dates considered for analysis 
#(exclude before and after exposure period)
StartDates <- -1e10
EndDates <- 1e10
#names given to indicate time in the data files
TimeNames <- "Time"
#names given to indicate treatment in the data files
TreatmentNames <- "Treatment"
#what will this analysis use as endpoints?
#..."Richness" and "EF" should be listed as 1 and 2 in this vector
endpoints <- c("Richness", "EF_0", "EF_1", 
               "EF__1", "Sim")

Combinations <- expand.grid(Alphas, Unifs,
                            Corrs, DeltasAlphas)
indLow <- which((Combinations$Var1==0.2)&(Combinations$Var4==0.1))
indHigh <- which((Combinations$Var1==0.6)&(Combinations$Var4==0.3))

Combinations <- Combinations[c(indLow, indHigh),]

#allocate object to store effects on ef 
#...occurring with no effect on richness
EFEffectsAtInvarRichness <- NULL
#allocate object to store effects on similarity with control
#...occurring with no effect on richness
EFEffectsAtInvarRichnessComp <- NULL
#allocate object to store dose responses for "Richness" and "EF"
DoseResps <- NULL
#allocate object to store dose response data for "Richness" and "EF"
DoseRespDatas <- NULL

for (i in c(1:nrow(Combinations)))
{
  Alpha <- Combinations[i,1]
  Unif <- Combinations[i,2]
  Corr <- Combinations[i,3]
  DeltaAlphas <- Combinations[i,4]

  #allocate object for later bdef application
  Data <- NULL
  
  for (Conc in Concs)
  {
    #open files with simulated abundances and
    #...transpose to make ready for BDEF function
    source("GetAbundances.R")  
    Abundances <- t(Abundances)
    #calculate EF based on abundances
    source("CalculateEF.R")
    Data <- rbind(Data, cbind(1, match(Conc, Concs), 
                              EF_0, EF_1, EF__1, Abundances))
  }
  colnames(Data)[c(1:2)] <- c("Time", "Treatment")
  Data <- as.data.frame(Data)
  Result <- BDEF(data=Data, 
                 CountCols=CountColsStart,          
                 TimeName=TimeNames,                
                 TreatmentName=TreatmentNames,
                 Affected = StartDates-1e-10, #have to substract
                 NoAffected = EndDates+1e-10, #or add small nr cause 
                 endpoints = "Richness",          #< and > in BDEF function
                 x=0)
  ConcsNew <- Concs
  if (min(Concs)==0) #Concentrations will be log-transformed later 
  {                       #so we need to replace zero by a low nr.
    ConcsNew[1] <- Concs[2]/2
  }
  #here's the log transform
  Result$Conc <- log10(ConcsNew[as.numeric(Result$Treatment)]) 
  source("DRM.r")
}

colnames(DoseResps) <- c("Study", "Scaled Log Concentration", 
                         "Effect on mean richness", 
                         "Effect on mean richness -", 
                         "Effect on mean richness +",
                         "Effect on mean EF", 
                         "Effect on mean EF -", 
                         "Effect on mean EF +")

warning("Only no correlation implemented in analysis!")




