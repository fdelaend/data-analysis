#try DRMs for all endpoints
#length of concentration vector to simulate effect for with gams
LengthNewConcs <- 1000 
#allocate empty objects for dose-response models and data
#*3 because you want to store mean, low, and upper limits
DoseResp<- mat.or.vec(LengthNewConcs, length(endpoints)*3)*NA 
DoseRespData <- mat.or.vec(length(unique(Result$Conc)),
                           length(endpoints))*NA 
colnames(DoseResp) <- c(1:ncol(DoseResp))
colnames(DoseRespData) <- c(1:ncol(DoseRespData))

for (endpoint in endpoints)
{
  colnames(DoseResp)[c(1:3)+3*(match(endpoint, endpoints)-1)] <- paste(c("mean", "low", "up"), 
                                                                       endpoint, sep="")
  colnames(DoseRespData)[match(endpoint, endpoints)] <- endpoint

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
    Model <- gam(as.formula(paste(endpoint," ~ s(Conc,k=4)")), 
                 family=Gamma(), data=ResultEndpoint)
    #predict with dose response along conc gradient
    NewConcs <- seq(min(Result$Conc), max(Result$Conc), 
                    length.out = LengthNewConcs)
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
    DoseResp[,paste(c("mean", "low", "up"), 
                    endpoint, sep="")] <- cbind(Mean, Lower, Upper)
    
    #track original data for all studies of effects on richness and EF
    #...important: divide data by PREDICTED control, not by OBSERVED control!
    #...reason: the predicted control is a better estimator of the real control
    #...because it accounts for the whole dose-response shape.
    DoseRespData[,endpoint] <- (ResultEndpoint[,2]-Preds$fit[1])/Preds$fit[1]
  } 
}

DoseResp <- cbind("Scaled Log Concentration"=(NewConcs-NewConcs[1])/(max(NewConcs)-min(NewConcs)), 
                  DoseResp)
DoseRespData <- cbind("Scaled Log Concentration"=(ResultEndpoint$Conc-ResultEndpoint$Conc[1])/(max(ResultEndpoint$Conc)-min(ResultEndpoint$Conc)), 
                      DoseRespData)

#track dose responses: 1-7 because concentration+med,low,up for B and EF
DoseResps <- rbind(DoseResps, 
                   cbind("Study"=i, 
                         DoseResp))

#track dose response data
DoseRespDatas <- rbind(DoseRespDatas,
                       cbind("Study"=i, 
                             DoseRespData))

