#test
require(lattice)
require(vegan)
require(mgcv)
source("/Users/frederik/Documents/work/functions/Functions.R")
BioVs     <- read.delim("/Users/frederik/Documents/litdata/bvd_species_ag_030614.txt") 
#get locations of phytodata and corresponding substances and concentrations
source("/Users/frederik/Documents/work/BD_EF/data analysis/PhytData.R")
#specify destination for plots and other output
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data analysis"
AllData <- PhytoData
#do you want to calculate ef from counts or use EF available in the data file (the 'EF' entry)?
Calc <- rep(0,105) #c(1, 1, rep(0,6), 1, 0, 0, 1, 0, 1)
#color specs
cols <- expand.grid(c(0, 0.5, 1), c(0, 0.5, 1), c(0, 0.5, 1))
#where do the counts start? 
CountColsStart <- c(7, 7, NA, NA, NA, NA, NA, 
                    NA, 5, NA, NA, 6, NA, 5)
#dates selected for analysis
PickDates <- c(21, 21, 56.3, 4, 999, 999, 999, 
               999, 3, 999, 999, 8, 999, 3)
#names given to indicate time in the data files
TimeNames <- c("Days.p.a.", "Days.p.a.", "Time", "Time", "Time", 
               "Time", "Time", "Time", "Week", "Time", "Time", "Week", 
               "Time", "Week")
#names given to indicate treatment in the data files
TreatmentNames <- c("Treatment","Treatment","Treatment", #Should always be 'Treatment'
                    "Treatment", "Treatment","Treatment",
                    "Treatment","Treatment","Treatment",
                    "Treatment","Treatment", "Treatment", 
                    "Treatment","Treatment")
#what will this analysis use as endpoints?
endpoints <- c("Richness", "EF")

quartz("",3,3,type="pdf",
       file=paste(ResultsFolder,"Test.pdf",sep=""))
par(mar=c(5,5,1,1), las=1)
plot(0,0, 
     xlim=c(-1,1), ylim=c(-1,1), 
     pch=19, xlab="Effect on richness", 
     ylab="Effect on EF")

SelectedStudies <- c(8, 13, 1, 2, 9, 12, 14)

for (i in SelectedStudies)
{
  #Reading of data and EF calc
  Data   <- read.delim(AllData[i])
  source("EF calc.R")
  #Calculation of effects 
  Result <- BDEF(data=Data, 
                 CountCols=CountColsStart[i], 
                 TimeName=TimeNames[i], 
                 TreatmentName=TreatmentNames[i],
                 Affected = 0,
                 NoAffected = 1e10,
                 endpoints = endpoints,
                 x=0)[[1]]
  #Put everything together
  Result <- cbind(Result$Richness, 
                  Result$EF[,c("EF","EFEffect", 
                               "EFEffectTzero")])
  if (min(Concs[[i]])==0) 
  {
    Concs[[i]][1] <- Concs[[i]][2]/2
  }
  Result$Conc <- log10(Concs[[i]][as.numeric(Result$Treatment)])
  #Do some plotting of raw data for both endpoints
  #source("PlotRawData.R")
  #Do subsetting of data: only use 'PickDates' in the analysis
  if (PickDates[i]!=999)
  {
    Ind <- which(Result[,TimeNames[i]]==PickDates[i])
    Result <- Result[Ind,]
  }
  source("DRM.r")
#  quartz("",6,3,type="pdf",file=paste(i,"Test.pdf"))
#  print(get("Richness"), position=c(0, 0, 0.5, 0.5), more=TRUE)
#  print(get("EF"), position=c(0.5, 0, 1, 0.5), more=TRUE)
#  print(get("RichnessDR"), position=c(0, 0.5, 0.5, 1), more=TRUE)
#  print(get("EFDR"), position=c(0.5, 0.5, 1, 1))
#  dev.off()
}
abline(h=0)
abline(v=0)
legend("bottomright", 
       as.character(SelectedStudies),
       pch=NA, lty="solid", 
       cex=0.5, ncol=2,
       col=rgb(cols[[1]][SelectedStudies], 
               cols[[2]][SelectedStudies], 
               cols[[3]][SelectedStudies], 1))
dev.off()



