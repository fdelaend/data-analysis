#try DRMs for both endpoints

Uppers <- NULL
Lowers <- NULL
Medians<- NULL
for (endpoint in endpoints)
{
  # first test if endpoint available
  if (endpoint%in% colnames(Result))
  {
    #Add really small nr to endpoint to avoid zeros (very rarerly the case)
    Result[,endpoint] <- Result[,endpoint] +1e-6
    #Now calculate average endpoint over considered period per treatment
    #If there are replicates also the mean of the replicated is taken
    form <- paste(endpoint,"~ Conc")
    ResultEndpoint <- aggregate(as.formula(form), data=Result, mean)
    #Now do the dose-response
    Model <- gam(as.formula(paste(endpoint," ~ s(Conc,k=3)")), 
                 family=Gamma(), data=ResultEndpoint)
    #predict with dose response along conc gradient
    NewConcs <- seq(min(Result$Conc), max(Result$Conc), length.out = 100)
    Preds <- predict(Model, type="response", se=T, 
                     newdata=data.frame(Conc=NewConcs))
    Upper <- Preds$fit+Preds$se.fit 
    Upper <- (Upper-Preds$fit[1])/Preds$fit[1]
    Lower <- Preds$fit-Preds$se.fit
    Lower <- (Lower-Preds$fit[1])/Preds$fit[1]
    Median <- (Preds$fit-Preds$fit[1])/Preds$fit[1]
    Uppers<- cbind(Uppers, Upper) 
    Lowers<- cbind(Lowers, Lower) 
    Medians<- cbind(Medians, Median)
  } 
}

#plot emerging B-EF trajectory
lines(Medians[,1], Medians[,2], lwd=1.5,
      col=rgb(cols[[1]][i], cols[[2]][i], cols[[3]][i], 1))

#get indices where effect on richness not different from zero
#as decided based on the standard errors encompassing 0
Ind <- which((Uppers[,1]>0)*(Lowers[,1]<0)==1)
if (length(Ind)>0) 
  {
  EFEffectsAtInvarRichness <- rbind(EFEffectsAtInvarRichness,
                                    cbind(i,Medians[Ind,2]))
  }
