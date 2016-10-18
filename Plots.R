#get nr of studies from i
nrOfStudies <- i

quartz("",6,6,type="pdf",
       file=paste(ResultsFolder,"Test.pdf",sep=""))
par(mar=c(5,5,2,0.5), las=1, mfrow=c(2,2))

#First dose-responses for SelectedEndpoints only
for (endpoint in selectedEndpoints)
{
  plot(-10,0,  main=LETTERS[match(endpoint, 
                                  selectedEndpoints)],
       xlim=c(0,1), ylim=c(-1,1), 
       pch=19, xlab="Log concentration (scaled 0-1)", 
       ylab=paste("Effect on", endpoint))
  for (i in c(1:nrOfStudies))
  {
    #The data
    xy <- DoseRespDatas[which(DoseRespDatas[,"Study"]==i),
                        c("Scaled Log Concentration",
                          endpoint)]
    if (dataToo) {points(xy[,1], xy[,2], col=cols[i])}
    #The model predictions: mean, -, and +
    xy <- DoseResps[which(DoseResps[,"Study"]==i),
                    c("Scaled Log Concentration",
                      paste("mean", endpoint))]
    lines(xy[,1], xy[,2], lwd=1.5,
          col=cols[i])
  }
  
}

for (endpoint in selectedEndpointsNotRichness)
{
  plot(EffectsAtInvarRichness[,"Study"],
       EffectsAtInvarRichness[,paste("mean",endpoint)], 
       main="TEMP",
       ylim=c(-1.1,1.1),
       col=cols[EffectsAtInvarRichness[,"Study"]],
       xlab="Study", ylab=paste("Effect on", endpoint,
       " \n at invariant richness"),
       xaxt="n",
       pch=15)
  axis(1, at=c(1:nrOfStudies),
       labels=c(1:nrOfStudies),
       las=2, cex.axis=1)
  abline(h=0)
  
}

dev.off()

