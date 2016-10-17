
require('lattice')
require('mgcv')
require('vegan')
require("latticeExtra")
#redo 6, 7

#FILL OUT THESE
sims <- "VI"#

#what BD index to plot on the y axis of a BEF plot?
BD <- "Richness" #or "Richness" or "Evenness"
ResultsPath <- paste("/Users/frederik/Documents/work/BD_EF/simulations",sims,"/output/",sep="")#Say where the results are
Iterations <- 50 #nr of iterations per level
n <- 20#20 #20initial nr of species
Concs <- seq(0, 200, 20) #seq(0, 200, 20)#tested concentrations
Alphas <- c(0.2,0.6)#c(0.2, 0.4, 0.6, 0.8)#c(0.5) #tested alphas #
Unifs <- c("False")#c("True","False") #initial SAD: uniform or skewed
DeltasAlphas <- c(0.1, 0.3)#, 0.2)#0.2, 0.4)#, 0.4) #c(0, 0.1, 0.2)c(0.49)#
Corrs <- c(-1,0,1)
Slope <- 3 #slope of dose-response curves for toxicity

Combinations <- expand.grid(Concs, Alphas, Unifs,
                            Corrs, DeltasAlphas)
indLow <- which((Combinations$Var2==0.2)&(Combinations$Var5==0.1))
indHigh <- which((Combinations$Var2==0.6)&(Combinations$Var5==0.3))

Combinations <- Combinations[c(indLow, indHigh),]

AllData <- NULL

for (Comb in c(1:nrow(Combinations)))
{
  Conc <- Combinations[Comb,1]
  Alpha <- Combinations[Comb,2]
  Unif <- Combinations[Comb,3]
  Corr <- Combinations[Comb,4]
  DeltaAlphas <- Combinations[Comb,5]
  
  source("AbundancesAndRichness.R")
  
  #EF and the bloody correlations with f
  #first open ecxs:
  ECs <- read.table(paste(ResultsPath,"EC50ga",
                          Alpha,
                          "d",Unif,
                          "c",Corr,
                          "delta",DeltaAlphas,sep=""))
  ECorder <- apply(ECs,2, rank)

  #No corr:
  f <- ECorder/ECorder #just one in case no correlations. Biovolume-specific fs sum to 20
  EF_i <- Abundances*f
  EFcontrol_i <- Abundancescontrol*f
  EF_0 <- colSums(EF_i)
  EFcontrol_0 <- colSums(EFcontrol_i)
  #corr>0: higher EC means higher f. Biovolume-specific fs still sum to 20. 
  f <- ECorder/210*20 #
  EF_i <- Abundances*f
  EFcontrol_i <- Abundancescontrol*f
  EF_1 <- colSums(EF_i)
  EFcontrol_1 <- colSums(EFcontrol_i)
  #corr<0: higher EC means lower f. Biovolume-specific fs still sum to 20. 
  f <- (21-ECorder)/210*20 #
  EF_i <- Abundances*f
  EFcontrol_i <- Abundancescontrol*f
  EF__1 <- colSums(EF_i)
  EFcontrol__1 <- colSums(EFcontrol_i)
  
  AllData <- rbind(AllData,
                   cbind(c(1:Iterations), Richness, Richnesscontrol, Conc, 
                         Alpha, Unif, Corr, DeltaAlphas, 
                         EF_0, EFcontrol_0,
                         EF_1, EFcontrol_1, EF__1, EFcontrol__1))
  
}

colnames(AllData) <- c("Iteration","Richness", "Richnesscontrol", 
                       "Dose", "InterspecificCompetition",
                       "Uniform", "Correlation", "DeltaAlpha",
                       "EF_0", "EFcontrol_0", "EF_1", "EFcontrol_1",
                       "EF__1", "EFcontrol__1")

AllDataDF <- as.data.frame(AllData)
AllDataDF$InterspecificCompetition <- as.factor(AllDataDF$InterspecificCompetition)
AllDataDF$DeltaAlpha <- as.factor(AllDataDF$DeltaAlpha)
AllDataDF$Correlation <- as.factor(AllDataDF$Correlation)
AllDataDF$Iteration <- as.factor(AllDataDF$Iteration)

#Just check how many survived
Rs <- AllDataDF$Richnesscontrol[which(AllDataDF$InterspecificCompetition==0.2)]
survival1 <- histogram(Rs, breaks=10, xlab="Final species richness", xlab.top="A")
Rs <- AllDataDF$Richnesscontrol[which(AllDataDF$InterspecificCompetition==0.6)]
survival2 <- histogram(Rs, breaks=10, xlab="Final species richness", xlab.top="B")

#Now loop over subsets 
Corrs <- c(0,-1,1)
Combinations <- expand.grid(DeltasAlphas,Alphas,Corrs)
indLow <- which((Combinations$Var2==0.2)&(Combinations$Var1==0.1))
indHigh <- which((Combinations$Var2==0.6)&(Combinations$Var1==0.3))
Combinations <- Combinations[c(indLow,indHigh),]

quartz("",6,9,type="pdf",file=paste(sims, "LikeData.pdf"))
positions=list(c(0, 2/3, 1/2, 1), c(1/2, 2/3, 1, 1),
               c(0, 1/3, 1/2, 2/3), c(1/2, 1/3, 1, 2/3), 
               c(0, 0, 1/2, 1/3), c(1/2, 0, 1, 1/3))
ordering <- c(1,4,2,5,3,6)

for (i in c(1:length(ordering)))
{
  ind <- which((AllDataDF$InterspecificCompetition==Combinations[ordering[i],2])&(AllDataDF$DeltaAlpha==Combinations[ordering[i],1])&(AllDataDF$Correlation==Combinations[ordering[i],3]))
  AllDataDF_i <- AllDataDF[ind,]
  #First make richness dose-response
  Form0 <- paste0("s(Dose, by=as.numeric(Iteration == ",c(1:Iterations),
                  ", k=3))",sep="", collapse = "+")
  Form  <- paste("Richness~Iteration+", Form0, sep="")
  Model <- gam(as.formula(Form), data=AllDataDF_i)
  PredRichness<- predict(Model, type="response")
  #convert to effect on richness
  PredRichnessControl <- rep(PredRichness[c(1:Iterations)], length(Concs))
  PredRichness<- PredRichness - PredRichnessControl
  PredRichness<- PredRichness/PredRichnessControl

  q <- 0
  for (lastbit in c("__1", "_1","_0"))
  {
    q <- q+1
    #make gam dose-response for all Iterations at once (but per Iteration!)
    Form0 <- paste0("s(Dose, by=as.numeric(Iteration == ",c(1:Iterations),
                  ", k=3))",sep="", collapse = "+")
    Form  <- paste("EF",lastbit, "~Iteration+", Form0, sep="")
    Model <- gam(as.formula(Form), data=AllDataDF_i)
    PredEF<- predict(Model, type="response")
    #convert to effect on EF
    PredEFControl <- rep(PredEF[c(1:Iterations)], length(Concs))
    PredEF<- PredEF - PredEFControl
    PredEF<- PredEF/PredEFControl
    
    form <- "PredEF~PredRichness"
    test <-  xyplot(as.formula(form), type="l",
                    xlab.top=LETTERS[i],
                    groups=Iteration,
                    panel = function(...) {
                      panel.xyplot(...)
                      panel.abline(h=0)
                      panel.abline(v=0)
                    }, ylab="Effect on EF", xlab="Effect on richness",
                    par.settings = simpleTheme(col=rainbow(3)[q]),
                    data=cbind(AllDataDF_i,
                               PredRichness=PredRichness,
                               PredEF=PredEF), 
                    ylim=c(-1,1), xlim=c(-1,1))
    print(test, position=positions[[i]], more=TRUE)
  }
}
dev.off()

quartz("",6,9,type="pdf",file=paste(sims, "LikeData_Res.pdf"))

#Now plot the residuals of the linear models
for (i in c(1:length(ordering)))
{
  q <- 0
  for (lastbit in c("__1", "_1","_0"))
    {
    q <- q+1
    print(get(paste(i,q,sep="_")), position=positions[[i]], more=TRUE)
    }
}  
dev.off()

quartz("",6,9,type="pdf",file=paste(sims, "LikeData_Slopes.pdf"))

#Now plot the slopes of the linear models
for (i in c(1:length(ordering)))
{
  q <- 0
  for (lastbit in c("__1", "_1","_0"))
  {
    q <- q+1
    print(get(paste(i,q,"Slope",sep="_")), position=positions[[i]], more=TRUE)
    print(get(paste(i,q,"Slope_2",sep="_")), position=positions[[i]], more=TRUE)
  }
}  
dev.off()

quartz("",6,9,type="pdf",file=paste(sims, "LikeData_Debts.pdf"))

#Now plot the debts/credits of the linear models
for (i in c(1:length(ordering)))
{
  q <- 0
  for (lastbit in c("__1", "_1","_0"))
  {
    q <- q+1
    print(get(paste(i,q,"Intercept",sep="_")), position=positions[[i]], more=TRUE)
    print(get(paste(i,q,"Intercept_2",sep="_")), position=positions[[i]], more=TRUE)
    
  }
}  
dev.off()

quartz("",6,3,type="pdf",file=paste(sims, "Survival.pdf"))
print(survival1, position=c(0,0,0.5,1), more=TRUE)
print(survival2, position=c(0.5,0,1,1))
dev.off()



