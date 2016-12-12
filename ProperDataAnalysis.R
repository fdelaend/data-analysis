#This is now the new branch
require(lattice)
require(vegan)
require(mgcv)
source("/Users/frederik/Documents/work/functions/Functions.R")
#get locations of phytodata and corresponding substances and concentrations
source("/Users/frederik/Documents/work/BD_EF/data-analysis/PhytData.R")
#specify destination for plots and other output
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"
#color specs: 
cols <- c("burlywood4", "cadetblue", "chartreuse", 
          "chartreuse4", "chocolate1", "cyan",
          "darkblue", "darkgoldenrod1", "darkgray",
          "firebrick1", "gray0", "hotpink", "black")
#where do the counts start in the files with counts? 
CountColsStart <- c(7, 6, 7, NA, 5, 
                    6, NA, 5, rep(NA,3), NA, 8)
#starting and ending dates considered for analysis 
#(exclude before and after exposure period)
StartDates <- c(1, 1, 21, -1e10, 1, 1, -1e10, 1, rep(-1e10, 3), -1e10,
                1)
EndDates <- c(21, 24, 21, 1e10, 4, 11, 1e10, 4, rep(1e10, 3), 1e10,
              10)
#names given to indicate time in the data files
TimeNames <- c("Days.p.a.", "Days.p.a.", "Days.p.a.", 
               "Time", "Week", "Week", "Time", "Week", 
               "Time", "Time", "Time", "Time", "week") 
#names given to indicate treatment in the data files
TreatmentNames <- rep("Treatment", length(PhytData))
#what will this analysis use as endpoints?
endpoints <- c("Richness", "EF", "Sim")
#these will be plotted in dose-response mode
selectedEndpoints <- c("Richness", "EF")
#and effects on these will be plotted for cases where richness is not affected
selectedEndpointsNotRichness <- c("EF")#, "Sim")
  
#allocate object to store effects on endpoint 
#...occurring with no effect on richness
EffectsAtInvarRichness <- NULL
#allocate object to store dose responses for endpoints
DoseResps <- NULL
#allocate object to store dose response data for endpoints
DoseRespDatas <- NULL
#YES, you want to plot the data too 
dataToo <- TRUE

for (i in c(1:length(PhytData)))
{
  #Reading of data and EF calc
  Data   <- read.delim(PhytData[i])
  #Calculation of richness (similarity automatically done if counts present)
  Result <- BDEF(data=Data,  
                 CountCols=CountColsStart[i],            
                 TimeName=TimeNames[i],                 
                 TreatmentName=TreatmentNames[i],
                 Affected = StartDates[i]-1e-10, #have to substract
                 NoAffected = EndDates[i]+1e-10, #or add small nr cause 
                 endpoints = "Richness",          #< and > in BDEF function
                 x=0)
  if (min(Concs[[i]])==0) #Concentrations will be log-transformed later 
  {                       #so we need to replace zero by a low nr.
    Concs[[i]][1] <- Concs[[i]][2]/2
  }
  Result$Conc <- log10(Concs[[i]][as.numeric(Result$Treatment)]) #here's the log transform
  source("DRM.r")
}

#add systemTag so clear it's all 1 system within 1 Study
DoseResps <- cbind(DoseResps, systemTag=DoseResps[,"Study"]^0)
#get indices where effect on richness not different from zero
#...as decided based on the standard errors encompassing 0
#...and track
Ind <- which((DoseResps[,"upRichness"]>0)*(DoseResps[,"lowRichness"]<0)==1)
if (length(Ind)>0) 
{
  EffectsAtInvarRichness <- DoseResps[Ind,]
}

quartz("",7.5,2.5,type="pdf",
       file=paste(ResultsFolder,"TestData.pdf",sep=""))
par(mar=c(3,4,2,0.5), las=1, mfrow=c(1,3), 
    tck=-0.02, mgp=c(2,0.5,0))
XLAB <- "Study"
YLABs <- c("Richness", "EF", "Similarity")
names(YLABs) <- endpoints
source("Plots.R")
legend("topleft", cex=0.7,
       paste("Data set", c(1:length(PhytData))), ncol=3,
       pch="", lwd=2, col=cols[1:length(PhytData)])
dev.off()


