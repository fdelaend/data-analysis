
require(lattice)
require(vegan)
require(mgcv)
source("/Users/frederik/Documents/work/functions/Functions.R")
#get locations of phytodata and corresponding substances and concentrations
source("/Users/frederik/Documents/work/BD_EF/data-analysis/PhytData.R")
#specify destination for plots and other output
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"
#color specs: input for all possible color codes are generated here,
#for later input into rgb
cols <- c("burlywood4", "cadetblue", "chartreuse", 
          "chartreuse4", "chocolate1", "cyan",
          "darkblue", "darkgoldenrod1", "darkgray",
          "firebrick1", "gray0", "hotpink")
#where do the counts start in the files with counts? 
CountColsStart <- c(7, 6, 7, NA, 5, 
                    6, NA, 5, rep(NA,3), NA)
#starting and ending dates considered for analysis 
#(exclude before and after exposure period)
StartDates <- c(1, 1, 21, -1e10, 1, 1, -1e10, 1, rep(-1e10, 3), -1e10)
EndDates <- c(21, 24, 21, 1e10, 4, 11, 1e10, 4, rep(1e10, 3), 1e10)
#names given to indicate time in the data files
TimeNames <- c("Days.p.a.", "Days.p.a.", "Days.p.a.", 
               "Time", "Week", "Week", "Time", "Week", 
               "Time", "Time", "Time", "Time") 
#names given to indicate treatment in the data files
TreatmentNames <- rep("Treatment", length(PhytData))
#what will this analysis use as endpoints?
#..."Richness" and "EF" should be listed as 1 and 2 in this vector
endpoints <- c("Richness", "EF", "Sim")

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
#allocate object to store BEF trajectories
BEF <- NULL

for (i in c(1:length(PhytData)))
{
  #Reading of data and EF calc
  Data   <- read.delim(PhytData[i])
  #Calculation of effects 
  Result <- BDEF(data=Data, #will throw a warning cause similarity not yet done 
                 CountCols=CountColsStart[i],           #correctly. No prob 
                 TimeName=TimeNames[i],                 #cause not used for now.
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

colnames(DoseResps) <- c("Study", "Scaled Log Concentration", 
                         "Effect on mean richness", 
                         "Effect on mean richness -", 
                         "Effect on mean richness +",
                         "Effect on mean EF", 
                         "Effect on mean EF -", 
                         "Effect on mean EF +")

colnames(DoseRespDatas) <- c("Study", "Scaled Log Concentration",
                             "Effect on mean richness",
                             "Effect on mean EF")

colnames(BEF)       <- c("Study", "Richness", "EF")




