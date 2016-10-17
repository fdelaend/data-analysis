Combination <- paste(Conc,".0",
                     "a",Alpha,
                     "d",Unif,
                     "c",Corr,
                     "delta",DeltaAlphas,
                     sep="")

Abundances <- read.table(paste(ResultsPath,"X",Combination,".out",sep=""))
if (min(Abundances)<0)
{
  stop("<0")
}
#Now read abundances from the 100 last timesteps
AbundancesTest <- read.table(paste(ResultsPath,"XTest",Combination,".out",sep=""))
#Calculate the difference between both
Difference <- (AbundancesTest-Abundances)/Abundances
#Set abundance to zero of any sp that decreased more than 10% during the last 100 timesteps
Abundances <- Abundances - Abundances*(Difference < -0.1)
#Do second check on very low abundances that were probably dropping but with a precision that exceeds the software's precision
Abundances <- Abundances - Abundances*(Abundances < 1e-100)
#Now replace NAs (that resulted from dividing by zero)
Abundances[which(is.na(Abundances),arr.ind=TRUE)] <- 0

