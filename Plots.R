#get nr of studies from i
nrOfStudies <- i

#First dose-responses for SelectedEndpoints only
for (endpoint in selectedEndpoints)
{
  plot(-10,0,  main=LETTERS[match(endpoint, 
                                  selectedEndpoints)],
       xlim=c(0,1), ylim=c(-1,1.2), 
       pch=19, xlab="Log concentration (scaled 0-1)", 
       ylab=paste("Effect on", YLABs[endpoint]))
  for (i in c(1:nrOfStudies))
  {
    #The data
    xy <- DoseRespDatas[which(DoseRespDatas[,"Study"]==i),
                        c("Scaled Log Concentration",
                          endpoint)]
    if (dataToo) {points(xy[,1], xy[,2], col=cols[i])}
    #The model predictions. Aggregate makes sure summary stat(s) are done
    #in case there are systemTags
    xyOriginal <- DoseResps[which(DoseResps[,"Study"]==i),
                    c("Scaled Log Concentration",
                      paste("mean", endpoint, sep=""))]
    xy <- aggregate(xyOriginal[,paste("mean", endpoint, sep="")], 
                        by=list(xyOriginal[,"Scaled Log Concentration"]),
                        FUN=median)
    lines(xy[,1], xy[,2], lwd=1.5, col=cols[i])
    #xy <- aggregate(xyOriginal[,paste("mean", endpoint, sep="")], 
    #                by=list(xyOriginal[,"Scaled Log Concentration"]),
    #                FUN=min)
    #lines(xy[,1], xy[,2], lwd=1.5, col=cols[i])
    #xy <- aggregate(xyOriginal[,paste("mean", endpoint, sep="")], 
    #                by=list(xyOriginal[,"Scaled Log Concentration"]),
    #                FUN=max)
    #lines(xy[,1], xy[,2], lwd=1.5, col=cols[i])
    #  
  }
}

for (endpoint in selectedEndpointsNotRichness)
{
  form <- paste("mean",endpoint, " ~ as.factor(Study)", sep="")
  subset <- which(is.na(EffectsAtInvarRichness[,paste("mean",endpoint,sep="")])==0)
  EffectsAtInvarRichnessSubset <- EffectsAtInvarRichness[subset,]
  boxplot(as.formula(form), data=EffectsAtInvarRichnessSubset,
       main=LETTERS[3+match(endpoint,
                          selectedEndpointsNotRichness)],
       ylim=c(-1.1,1.1), border="grey",
       col=cols,#[EffectsAtInvarRichness[,"Study"]],
       xlab=XLAB, ylab=paste("Effect on", YLABs[endpoint],
       " \n at invariant richness"),
       xaxt="n",
       pch=15)
  axis(1, at=c(1:nrOfStudies),
       labels=c(1:nrOfStudies),
       las=2, cex.axis=1)
  abline(h=0)
  
}


