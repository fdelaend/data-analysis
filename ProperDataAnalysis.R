
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
cols <- expand.grid(c(0, 0.8), c(0.5, 0.8), c(0.5, 0.8))
#where do the counts start in the files with counts? 
CountColsStart <- c(7, 6, 7, NA, 5, 
                    6, NA, 5)
#starting and ending dates considered for analysis 
#(exclude before and after exposure period)
StartDates <- c(1, 1, 21, -1e10, 1, 1, -1e10, 1)
EndDates <- c(21, 21, 21, 1e10, 28, 80, 1e10, 4)
#names given to indicate time in the data files
TimeNames <- c("Days.p.a.", "Days.p.a.", "Days.p.a.", 
               "Time", "Week", "Week", "Time", "Week") 
#names given to indicate treatment in the data files
TreatmentNames <- rep("Treatment", length(PhytData))
#what will this analysis use as endpoints?
endpoints <- c("Richness", "EF")

quartz("",6,3,type="pdf",
       file=paste(ResultsFolder,"Test.pdf",sep=""))
par(mar=c(5,5,2,0.5), las=1, mfrow=c(1,2))
plot(0,0,  main="A",
     xlim=c(-1,1), ylim=c(-1,1), 
     pch=19, xlab="Effect on richness", 
     ylab="Effect on EF")
#allocate object to put in effects on ef 
#occurring with no effect on richness
EFEffectsAtInvarRichness <- NULL
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
                 endpoints = endpoints,          #< and > in BDEF function
                 x=0)[[1]]
  #Put everything together
  Result <- cbind(Result$Richness, 
                  Result$EF[,c("EF","EFEffect", 
                               "EFEffectTzero")])
  if (min(Concs[[i]])==0) #Concentrations will be log-transformed later 
  {                       #so we need to replace zero by a low nr.
    Concs[[i]][1] <- Concs[[i]][2]/2
  }
  Result$Conc <- log10(Concs[[i]][as.numeric(Result$Treatment)]) #here's the log transform
  source("DRM.r")
}
abline(h=0)
abline(v=0)
legend("topright", 
       as.character(c(1:length(PhytData))),
       pch=NA, lty="solid", 
       cex=0.65, ncol=2,
       col=rgb(cols[[1]][c(1:length(PhytData))], 
               cols[[2]][c(1:length(PhytData))], 
               cols[[3]][c(1:length(PhytData))], 1))

plot(EFEffectsAtInvarRichness[,1],
     EFEffectsAtInvarRichness[,2], main="B",
     col=rgb(cols[[1]][EFEffectsAtInvarRichness[,1]], 
             cols[[2]][EFEffectsAtInvarRichness[,1]], 
             cols[[3]][EFEffectsAtInvarRichness[,1]], 1),
     xlab="Study", ylab="Effect on EF \n at invariant richness",
     pch=15)
abline(h=0)

dev.off()

