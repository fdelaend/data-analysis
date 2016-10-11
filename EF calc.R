#todo
#read from biov file
#if sp NA in biov file --> throw out and report cell density

if (Calc[i]==1)
{
#Get bioVs
BioVs <- read.delim(PhytoVs[i])
BioVs[,1] <- as.character(BioVs[,1])
#Check if all species have NO bioVs and throw them out of data and bioV, 
#keeping track of their relative cell densities
Ind <- which(is.na(BioVs[,2]))
if (length(Ind)>0) 
  {
  Missing <- BioVs[Ind,"Sp"]
  Densities <- Data[,which(colnames(Data)%in%Missing)]
  RelDensities <- diag(1/rowSums(Data[,CountColsStart[i]:ncol(Data)])) %*% data.matrix(Densities)
  #kick out
  Data <- Data[,-which(colnames(Data)%in%Missing)]
  BioVs <- BioVs[-which(BioVs[,1]%in%Missing),]
  }

#Now do the conversion of counts to bioV
Data$EF <- rowSums(data.matrix(Data[,c(CountColsStart[i]:ncol(Data))])  %*% diag(BioVs[,2]))
}
