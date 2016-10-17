#try DRMs for both endpoints

Uppers  <- NULL
Lowers  <- NULL
Means <- NULL
DoseResp<- NULL
DoseRespData <- NULL

for (endpoint in endpoints)
{
  # first test if endpoint available
  if (endpoint%in% colnames(Result))
  {
    #Add really small nr to endpoint to avoid zeros (very rarerly the case)
    Result[,endpoint] <- Result[,endpoint] +1e-6
    #Now calculate average endpoint over considered period per treatment
    #If there are replicates also the mean of the replicates is taken
    form <- paste(endpoint,"~ Conc")
    ResultEndpoint <- aggregate(as.formula(form), data=Result, mean)

    #Now do the dose-response
    Model <- gam(as.formula(paste(endpoint," ~ s(Conc,k=3)")), 
                 family=Gamma(), data=ResultEndpoint)
    #predict with dose response along conc gradient
    NewConcs <- seq(min(Result$Conc), max(Result$Conc), length.out = 1000)
    Preds <- predict(Model, type="response", se=T, 
                     newdata=data.frame(Conc=NewConcs))
    #calculate mean prediction, 
    #...and upper and lower limits as mean predicition +- 1se
    Mean <- Preds$fit
    Upper <- Preds$fit+Preds$se.fit 
    Lower <- Preds$fit-Preds$se.fit
    #convert to effect sizes (stress intensity as in Steudel et al)
    Mean <- (Mean-Preds$fit[1])/Preds$fit[1]
    Upper <- (Upper-Preds$fit[1])/Preds$fit[1]
    Lower <- (Lower-Preds$fit[1])/Preds$fit[1]
    #Now track for later plotting
    DoseResp <- cbind(DoseResp, 
                      Mean, Lower, Upper)
    #now track differently for analysis on effects at invariant richness
    Uppers<- cbind(Uppers, Upper) 
    Lowers<- cbind(Lowers, Lower) 
    Means<- cbind(Means, Mean)
    
    #track original data for all studies of effects on richness and EF
    #...important: divide data by PREDICTED control, not by OBSERVED control!
    #...reason: the predicted control is a better estimator of the real control
    #...because it accounts for the whole dose-response shape.
    if (endpoint %in%c("Richness", "EF"))
    {
      DoseRespData <- cbind(DoseRespData, 
                            (ResultEndpoint[,2]-Preds$fit[1])/Preds$fit[1])
    }
    
  } 
}
#check if Means only 2 cols. If so, add 3rd one with NA Dissim values
if (ncol(Means)==2) {Means <- cbind(Means, NA)}

DoseResp <- cbind((NewConcs-NewConcs[1])/(max(NewConcs)-min(NewConcs)), 
                  DoseResp)
DoseRespData <- cbind((ResultEndpoint$Conc-ResultEndpoint$Conc[1])/(max(ResultEndpoint$Conc)-min(ResultEndpoint$Conc)), 
                      DoseRespData)

#track dose responses: 1-7 because concentration+med,low,up for B and EF
DoseResps <- rbind(DoseResps, 
                   cbind(i, 
                         DoseResp[,c(1:7)]))

#track dose response data
DoseRespDatas <- rbind(DoseRespDatas,
                       cbind(i, DoseRespData))

#get indices where effect on richness not different from zero
#...as decided based on the standard errors encompassing 0
#...and track
Ind <- which((Uppers[,1]>0)*(Lowers[,1]<0)==1)
if (length(Ind)>0) 
  {
  EFEffectsAtInvarRichness <- rbind(EFEffectsAtInvarRichness,
                                    cbind(i,Means[Ind,2],
                                          Lowers[1,2], Uppers[1,2]))
  EFEffectsAtInvarRichnessComp <- rbind(EFEffectsAtInvarRichnessComp,
                                        cbind(i,Means[Ind,3])) 
  }
