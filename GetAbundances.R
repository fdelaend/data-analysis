Combination <- paste(Conc,".0",
                     "a",Alpha,
                     "d",Unif,
                     "c",Corr,
                     "delta",DeltaAlphas,
                     sep="")

Abundances <- read.table(paste(ResultsPath,"X",Combination,".out",sep=""))
#check for negative nrs and stop if so
if (min(Abundances)<0)
{
  stop("<0")
}
#remove densities smaller than 0.000001
Abundances <- Abundances - (Abundances<0.000001)*Abundances
