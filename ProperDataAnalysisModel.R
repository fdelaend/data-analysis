
require(lattice)
require(vegan)
require(mgcv)
source("/Users/frederik/Documents/work/functions/Functions.R")
#get locations of simulated data and corresponding substances and concentrations
#...after specifying what simulations to use for analyses. 
sims <- "VII"
ResultsPath <- paste("/Users/frederik/Documents/work/BD_EF/simulations",sims,"/output/",sep="")#Say where the results are
#specify destination for plots and other output
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"
#color specs: 
cols <- c("burlywood4", "cadetblue", "chartreuse", 
          "chartreuse4", "chocolate1", "cyan",
          "darkblue", "darkgoldenrod1", "darkgray",
          "firebrick1", "gray0", "hotpink")
Iterations <- 50 #nr of iterations per level
n <- 20#initial nr of species
Concs <- c(0.0, 10.0, 20.0, 40.0, 80.0, 160.0, 320.0)
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
#what will this analysis use as endpoints?
endpoints <- c("Richness", "EF_0", "Sim")
#these will be plotted in dose-response mode
selectedEndpoints <- c("Richness", "EF_0")
#and effects on these will be plotted for cases where richness is not affected
selectedEndpointsNotRichness <- c("EF_0", "Sim")
#"EF_0", "EF_1", "EF__1"

Combinations <- expand.grid(Alphas, Unifs,
                            Corrs, DeltasAlphas)
indLow <- which((Combinations$Var1==0.2)&(Combinations$Var4==0.1))
indHigh <- which((Combinations$Var1==0.6)&(Combinations$Var4==0.3))

Combinations <- Combinations[c(indLow, indHigh),]

#allocate object to store effects on endpoint 
#...occurring with no effect on richness
EffectsAtInvarRichness <- NULL
#allocate object to store dose responses for endpoints
DoseResps <- NULL
#allocate object to store dose response data for endpoints
DoseRespDatas <- NULL
#NO, you don't want to plot the simulated data
dataToo <- FALSE

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
    Data <- rbind(Data, cbind(1, match(Conc, Concs), c(1:Iterations),
                              EF_0, EF_1, EF__1, Abundances))
  }
  colnames(Data)[c(1:3)] <- c("Time", "Treatment", "systemTag")
  Data <- as.data.frame(Data)
  Result <- BDEF(data=Data, 
                 CountCols=CountColsStart,          
                 TimeName=TimeNames,                
                 TreatmentName=TreatmentNames,
                 Affected = StartDates-1e-10, #have to substract
                 NoAffected = EndDates+1e-10, #or add small nr cause 
                 endpoints = "Richness",          #< and > in BDEF function
                 systemTag= "systemTag",
                 x=0)
  ConcsNew <- Concs
  if (min(Concs)==0) #Concentrations will be log-transformed later 
  {                       #so we need to replace zero by a low nr.
    ConcsNew[1] <- Concs[2]/2
  }
  #here's the log transform
  Result$Conc <- log10(ConcsNew[as.numeric(Result$Treatment)]) 
  #and do immediately the transform to 0-1
  Result$Conc <- (Result$Conc-min(Result$Conc))/(max(Result$Conc)-min(Result$Conc))
  #now add simulated effects for endpoints
  #first get control
  ControlTemp <- Result[which(Result$Treatment==1),endpoints]
  #replace NAs for Similarity with 1's. 
  ControlTemp$Sim <- 1
  Control <- ControlTemp
  #copy control and stack vertically
  for (Conc in Concs[c(2:length(Concs))]) {Control <- rbind(Control,ControlTemp)}
  #calculate effect sizes
  Result <- cbind(Result, (Result[,endpoints]-Control)/Control)
  DoseResps <- rbind(DoseResps, cbind(Study=i, Result))
}

colnames(DoseResps)[which(colnames(DoseResps)=="Conc")] <- c("Scaled Log Concentration")
colnames(DoseResps)[c((ncol(DoseResps)-2):ncol(DoseResps))] <- paste("mean", endpoints)

#get indices where effect on richness not different from zero
#...as decided based on the standard errors encompassing 0
#...and track
Ind <- which(DoseResps[,"mean Richness"]==0)
if (length(Ind)>0) 
{
  EffectsAtInvarRichness <- DoseResps[Ind,]
}

source("Plots.R")



