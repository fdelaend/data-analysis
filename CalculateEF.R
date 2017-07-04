#Calculate EF as Abundance*fi 
#and allow correlations between fi and sensitivity
#first open ECs (these are the sensitivies):
ECs <- read.table(paste(ResultsPath,"EC50ga",
                        Alpha,
                        "d",Unif,
                        "c",Corr,
                        "delta",DeltaAlphas,sep=""))
ECorder <- apply(ECs,2, rank) #gets the sensitivity rank per species per iteration

#Case 1: No correlation:
#...Biovolume-specific fs sum to 20
#...and need to be transposed to be 
#...able to match Abundances
f <- t(ECorder/ECorder)
EF_i <- Abundances*f
EF_0 <- rowSums(EF_i)
#Case 2: Positive correlation: 
#...Higher EC means higher f. 
#...Biovolume-specific fs still sum to 20. 
f <- t(ECorder/210*20) 
EF_i <- Abundances*f
EF_1 <- rowSums(EF_i)
#Case 3: Negative correlation:
#...Higher EC means lower f. 
#...Biovolume-specific fs still sum to 20. 
f <- t((21-ECorder)/210*20) #
EF_i <- Abundances*f
EF__1 <- rowSums(EF_i)
