#try DRMs for both endpoints

Uppers <- NULL
Lowers <- NULL
Medians<- NULL
for (endpoint in endpoints)
{
  #Add really small nr to endpoint to avoid zeros (very rarerly the case)
  Result[,endpoint] <- Result[,endpoint] +1e-6
  #Now calculate average EF and richness over considered period per treatment
  #If there are replicates also the mean of the replicated is taken
  form <- paste(endpoint,"~ Conc")
  ResultEndpoint <- aggregate(as.formula(form), data=Result, mean)
  #Now do the dose-response
  Model <- gam(as.formula(paste(endpoint," ~ s(Conc,k=3)")), 
               family=Gamma(), data=ResultEndpoint)
  NewConcs <- seq(min(Result$Conc), max(Result$Conc), length.out = 10)
  Preds <- predict(Model, type="response", se=T, 
                   newdata=data.frame(Conc=NewConcs))
  #plot(Result$Conc, Result[,endpoint], 
  #     main=i, ylab=endpoint)
  lines(NewConcs, Preds$fit, lty="solid")
  Upper <- Preds$fit+Preds$se.fit #When error bars overlap --> no sign diff
  #lines(NewConcs, Upper, lty="dashed")
  Upper <- (Upper-Preds$fit[1])/Preds$fit[1]
  Lower <- Preds$fit-Preds$se.fit
  #lines(NewConcs, Lower, lty="dashed")
  Lower <- (Lower-Preds$fit[1])/Preds$fit[1]
  Median <- (Preds$fit-Preds$fit[1])/Preds$fit[1]
  Uppers<- cbind(Uppers, Upper) 
  Lowers<- cbind(Lowers, Lower) 
  Medians<- cbind(Medians, Median)
}

#polygon(c(Medians[,1], rev(Medians[,1])), 
#        c(Lowers[,2], rev(Uppers[,2])), 
#        col=rgb(cols[[1]][i], cols[[2]][i], cols[[3]][i], 0.5), border=NA)
#polygon(c(Lowers[,1], rev(Uppers[,1])), 
#        c(Medians[,2], rev(Medians[,2])), 
#        col=rgb(cols[[1]][i], cols[[2]][i], cols[[3]][i], 0.5), border=NA)
lines(Medians[,1], Medians[,2], 
      col=rgb(cols[[1]][i], cols[[2]][i], cols[[3]][i], 1))

