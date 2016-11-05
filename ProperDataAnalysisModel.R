
require(lattice)
require(vegan)
require(mgcv)
source("/Users/frederik/Documents/work/functions/Functions.R")
#get locations of simulated data and corresponding substances and concentrations
#...after specifying what simulations to use for analyses. 
sims <- "V2"
ResultsPath <- paste("/Users/frederik/Documents/work/BD_EF/simulations",sims,"/output/",sep="")#Say where the results are
#specify destination for plots and other output
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"
#color specs: 
cols <- c("cyan", "cyan3", "cyan4", 
          "darkgoldenrod1", "darkgoldenrod3", "darkgoldenrod4")
Iterations <- 50 #nr of iterations per level
n <- 20#initial nr of species
Concs <- c(20.0, 40.0, 80.0, 160.0, 320.0, 640.0) #removed 0 cause 20 can serve as control
Alphas <- c(0.2,0.8)#c(0.2, 0.4, 0.6, 0.8)#c(0.5) #tested alphas #
Unifs <- c("False")#c("True","False") #initial SAD: uniform or skewed
DeltasAlphas <- c(0)#, 0.2)#0.2, 0.4)#, 0.4) #c(0, 0.1, 0.2)c(0.49)#
Corrs <- c(-1,0,1)

#where do the counts start in the files with counts? 
#take into account that fake time (needed for bdef function), treatment, 
#and three EF scenarios need to be added
CountColsStart <- 7
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
endpoints <- c("Richness", "EF__1", "EF_0", "EF_1", "Sim")
#these will be plotted in dose-response mode
selectedEndpoints <- c("Richness", "EF__1", "EF_0", "EF_1")
#and effects on these will be plotted for cases where richness is not affected
selectedEndpointsNotRichness <- c("Sim", "EF__1", "EF_0", "EF_1")
#"EF_0", "EF_1", "EF__1"

Combinations <- expand.grid(Alphas, Unifs,
                            Corrs, DeltasAlphas)
indLow <- which((Combinations$Var1==0.2)&(Combinations$Var4==0))
indHigh <- which((Combinations$Var1==0.8)&(Combinations$Var4==0))

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
  #now add simulated effects for endpoints
  #first get control
  ControlTemp <- Result[which(Result$Treatment==1),endpoints]
  #replace NAs for Similarity with 1's. 
  ControlTemp$Sim <- 1
  Control <- ControlTemp
  #copy control and stack vertically
  for (Conc in Concs[c(2:length(Concs))]) {Control <- rbind(Control,ControlTemp)}
  #calculate effect sizes
  effectSizes <- (Result[,endpoints]-Control)/Control
  colnames(effectSizes) <- paste("mean", endpoints, sep="")
  Result_i <- cbind(Study=i, Result[,c(1:3)], effectSizes)
  DoseResps <- rbind(DoseResps, Result_i)
}
#Add concentrations to data
ConcsNew <- Concs
if (min(Concs)==0) #Concentrations will be log-transformed later 
{                       #so we need to replace zero by a low nr.
  ConcsNew[1] <- Concs[2]/2
}
#here's the log transform
DoseResps$'Scaled Log Concentration' <- log10(ConcsNew[as.numeric(DoseResps$Treatment)]) 
DoseResps$'Scaled Log Concentration' <- (DoseResps$'Scaled Log Concentration' - min(DoseResps$'Scaled Log Concentration'))/(max(DoseResps$'Scaled Log Concentration')-min(DoseResps$'Scaled Log Concentration'))
#get indices where effect on richness not different from zero
#...and track. 
Ind <- which((DoseResps[,"meanRichness"]==0)&(DoseResps$Treatment!=1))
if (length(Ind)>0) 
{
  EffectsAtInvarRichness <- DoseResps[Ind,]
}

quartz("",8,4,type="pdf",
       file=paste(ResultsFolder,"TestSims.pdf",sep=""))
par(mar=c(3,4,2,0.5), las=1, tck=-0.02,
    mgp=c(2,0.5,0), mfrow=c(2,4))
XLAB <- "Case"
YLABs <- c("Richness", "EF,-1", "EF,0", "EF,+1", "Similarity")
names(YLABs) <- endpoints
source("Plots.R")
legend(1,1.2, expression(alpha), col="transparent", lwd=1, seg.len=0.5,
       bty="n")
legend(4,1.2, expression(alpha), col="transparent", lwd=1, seg.len=0.5,
       bty="n")
legend(1,1, Combinations[1,1], col=cols[1], lwd=2, bty="n", seg.len=0.5)
legend(1,0.8, Combinations[2,1], col=cols[2], lwd=2, bty="n", seg.len=0.5)
legend(1,0.6, Combinations[3,1], col=cols[3], lwd=2, bty="n", seg.len=0.5)

legend(4,1, Combinations[4,1], col=cols[4], lwd=2, bty="n", seg.len=0.5)
legend(4,0.8, Combinations[5,1], col=cols[5], lwd=2, bty="n", seg.len=0.5)
legend(4,0.6, Combinations[6,1], col=cols[6], lwd=2, bty="n", seg.len=0.5)


legend(2,1.2, expression(rho), col="transparent", lwd=1, seg.len=0.5,
       bty="n")
legend(5,1.2, expression(rho), col="transparent", lwd=1, seg.len=0.5,
       bty="n")
legend(2,1, Combinations[1,3], col="transparent", lwd=2, bty="n", seg.len=0.5)
legend(2,0.8, Combinations[2,3], col="transparent", lwd=2, bty="n", seg.len=0.5)
legend(2,0.6, Combinations[3,3], col="transparent", lwd=2, bty="n", seg.len=0.5)

legend(5,1, Combinations[4,3], col="transparent", lwd=2, bty="n", seg.len=0.5)
legend(5,0.8, Combinations[5,3], col="transparent", lwd=2, bty="n", seg.len=0.5)
legend(5,0.6, Combinations[6,3], col="transparent", lwd=2, bty="n", seg.len=0.5)

dev.off()
#expression(paste(alpha,"=",
#                 Combinations[,1],";",
#                 rho,"=",Combinations[,3])),
#pch="", lwd=2, col=cols[1:nrow(Combinations)])



