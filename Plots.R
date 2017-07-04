#get nr of studies from i
nrOfStudies <- i

#First dose-responses for SelectedEndpoints only
for (endpoint in selectedEndpoints)
{
  plot(-10,0,  main=LETTERS[5+match(endpoint, 
                                  selectedEndpoints)],
       xlim=c(0,1), ylim=c(-100,120), 
       pch=19, xlab="Log concentration (scaled 0-1)", 
       ylab=paste("% Effect on", YLABs[endpoint]))
  for (i in c(1:nrOfStudies))
  {
    #The data
    xy <- DoseRespDatas[which(DoseRespDatas[,"Study"]==i),
                        c("Scaled Log Concentration",
                          endpoint)]
    if (dataToo) {points(xy[,1], xy[,2]*100, col=cols[i])}
    #The model predictions. Aggregate makes sure summary stat(s) are done
    #in case there are systemTags
    xyOriginal <- DoseResps[which(DoseResps[,"Study"]==i),
                    c("Scaled Log Concentration",
                      paste("mean", endpoint, sep=""))]
    xy <- aggregate(xyOriginal[,paste("mean", endpoint, sep="")], 
                        by=list(xyOriginal[,"Scaled Log Concentration"]),
                        FUN=median)
    lines(xy[,1], xy[,2]*100, lwd=1.5, col=cols[i])
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

<<<<<<< HEAD
#Now emerging BEF
#plot(-10,0,  main="C",
#     xlim=c(-1,1), ylim=c(-1,1), 
#     xlab="Effect on richness", 
#     ylab="Effect on EF")
#for (i in c(1:length(PhytData)))
#{
#  xy <- BEF[which(BEF[,"Study"]==i),
#            c("Richness", "EF")]
#  lines(xy[,1], xy[,2], lwd=1.5,
#        col=cols[i])
#}
#abline(h=0)
#abline(v=0)

plot(EFEffectsAtInvarRichness[,1],
     EFEffectsAtInvarRichness[,2], main="C",
     ylim=c(-1.1,1.1),
     col=cols[EFEffectsAtInvarRichness[,1]],
     xlab="Study", ylab="Effect on EF \n at invariant richness",
     xaxt="n",
     pch=15)
axis(1, at=c(1:length(PhytData)),
     labels=c(1:length(PhytData)),
     las=2, cex.axis=1)
abline(h=0)

plot(EFEffectsAtInvarRichnessComp[,1],
     EFEffectsAtInvarRichnessComp[,2], main="D",
     col=cols[EFEffectsAtInvarRichnessComp[,1]],
     xlab="Study", 
     ylab="Effect on similarity with control \n at invariant richness",
     xaxt="n",
     pch=15)
axis(1, at=c(1:length(PhytData)),
     labels=c(1:length(PhytData)),
     las=2, cex.axis=1)
abline(h=0)

dev.off()
=======
for (endpoint in selectedEndpointsNotRichness)
{
  form <- paste("100*mean",endpoint, " ~ as.factor(Study)", sep="")
  subset <- which(is.na(EffectsAtInvarRichness[,paste("mean",endpoint,sep="")])==0)
  EffectsAtInvarRichnessSubset <- EffectsAtInvarRichness[subset,]
  boxplot(as.formula(form), data=EffectsAtInvarRichnessSubset,
       main=LETTERS[5+length(selectedEndpoints)+match(endpoint,
                          selectedEndpointsNotRichness)],
       ylim=c(-110,110), border="grey",
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
>>>>>>> simulations

