
#Model code used for statistical analysis 
#...of empirical data in Spaak et al. 
require(lattice)
require(vegan)
require(mgcv)
print("You will need to download Functions.R in the functions repository,")
print("put it in the working dir. for this to work, and ")
print("remove the path below. Sorry.")
source("/Users/frederik/Documents/work/functions/Functions.R")

#Studies where we have community composition
compositionsAv               <- c(1:3, 5, 6, 8, 13)
#Preferred order of appearance of studies in the MS and legend of figure with empirical data
orders                       <- c(compositionsAv, 4, 7, 9:12)
#get locations of phytodata and corresponding substances and concentrations
source("/Users/frederik/Documents/work/BD_EF/data-analysis/PhytData.R")
#specify destination for plots and other output. You'll need to change this.
ResultsFolder                <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"
#color specs: 
cols                         <- c("burlywood4", "cadetblue", "chartreuse", 
                                  "chartreuse4", "chocolate1", "cyan",
                                  "darkblue", "darkgoldenrod1", "darkgray",
                                  "firebrick1", "gray0", "hotpink", "darkviolet")[orders]
#Specify where the counts start in the files with counts. 
#Put NA where the file does not contain counts
CountColsStart               <- c(7, 6, 7, NA, 5, 
                                  6, NA, 5, rep(NA,3), NA, 8)[orders]
#Define starting and ending dates considered for analysis 
#(exclude data points before and after exposure period)
#-1e10 and 1e10 are taken to include all data.
StartDates                   <- c(1, 1, 21, -1e10, 1, 1, -1e10, 1, 
                                  rep(-1e10, 3), -1e10, 1)[orders]
EndDates                     <- c(21, 24, 21, 1e10, 4, 11, 1e10, 4, 
                                  rep(1e10, 3), 1e10, 10)[orders]
#Specify names given to indicate time in the data files
TimeNames                    <- c("Days.p.a.", "Days.p.a.", "Days.p.a.", 
                                  "Time", "Week", "Week", "Time", "Week", 
                                  "Time", "Time", "Time", "Time", "week")[orders] 
#Specify names given to indicate treatment in the data files
TreatmentNames               <- rep("Treatment", length(PhytData))
#Specify endpoints to use  in this analysis
endpoints                    <- c("Richness", "EF", "Sim")
#Specify endpoints to be plotted in dose-response mode
selectedEndpoints            <- c("Richness", "EF", "Sim")
#Specify endpoints for which effects need to be plotted 
#...when richness is not affected
selectedEndpointsNotRichness <- c("EF", "Sim")
  
#Allocate object to store effects on endpoint 
#...occurring with no effect on richness
EffectsAtInvarRichness       <- NULL
#Allocate object to store dose response estimates for endpoints
DoseResps                    <- NULL
#Allocate object to store dose response data for endpoints
DoseRespDatas                <- NULL
#YES, you want to plot the data too 
dataToo                      <- TRUE
#Keep track of some summary stats
SummStats                    <- NULL

for (i in c(1:length(PhytData)))
{
  #Reading of data and EF calc
  Data                       <- read.delim(PhytData[i])
  #Calculation of richness (similarity automatically done if counts present)
  Result                     <- BDEF(data=Data,  
                                     CountCols=CountColsStart[i],            
                                     TimeName=TimeNames[i],    
                                     TreatmentName=TreatmentNames[i],
                                     Affected = StartDates[i]-1e-10, #have to substract
                                     NoAffected = EndDates[i]+1e-10, #or add small nr cause 
                                     endpoints = "Richness",         #< and > in BDEF function
                                     Binary=TRUE, x=0)               #Binary setting: such that 'only' changes in density are ignored
  if (min(Concs[[i]])==0) #Concentrations will be log-transformed later 
  {                       #so we need to replace zero by a low nr.
    Concs[[i]][1]            <- Concs[[i]][2]/2 #change factor 2 to 5 or 10 to get figures in SI
  }
  Result$Conc                <- log10(Concs[[i]][as.numeric(Result$Treatment)]) #here's the log transform
  SummStats                  <- rbind(SummStats, 
                                      cbind(mean(Result$Richness[which(Result$Treatment==1)]),
                                            length(unique(Result[,TimeNames[i]]))))

  source("DRM.r") #Dose response modelling
}

#add systemTag so clear it's all 1 system within 1 Study
DoseResps                    <- cbind(DoseResps, systemTag=DoseResps[,"Study"]^0)

#get indices where effect on richness not different from zero
#...as decided based on the standard errors encompassing 0
#...and track
Ind                          <- which((DoseResps[,"upRichness"]>0)*(DoseResps[,"lowRichness"]<0)==1)
if (length(Ind)>0) 
{
  EffectsAtInvarRichness     <- DoseResps[Ind,]
}

#Do some plotting. Non-Macs need different function than quartz
quartz("",7.5,5,type="pdf",
       file=paste(ResultsFolder,"FigEmpirical.pdf",sep=""))
par(mar=c(3,4,2,0.5), las=1, mfrow=c(2,3), 
    tck=-0.02, mgp=c(2,0.5,0))
XLAB                         <- "Data set"
YLABs                        <- c("Richness", "EF", "Similarity")
names(YLABs)                 <- endpoints
source("Plots.R")
par(col.axis="transparent", xaxt="n", yaxt="n",
    col.lab="transparent")
plot(0,0, col="transparent", axes = 0)
legend("topleft", cex=1, 
       paste("Data set", c(1:length(PhytData))), ncol=2,
       pch="", lwd=2, col=cols[1:length(PhytData)])
dev.off()


