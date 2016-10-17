
quartz("",6,6,type="pdf",
       file=paste(ResultsFolder,"Test.pdf",sep=""))
par(mar=c(5,5,2,0.5), las=1, mfrow=c(2,2))

#First dose-responses for richness and EF
for (endpoint in c("richness", "EF"))
{
  plot(-10,0,  main=LETTERS[match(endpoint, 
                                  c("richness", "EF"))],
       xlim=c(0,1), ylim=c(-1,1), 
       pch=19, xlab="Log concentration (scaled 0-1)", 
       ylab=paste("Effect on", endpoint))
  for (i in c(1:length(PhytData)))
  {
    #The data
    xy <- DoseRespDatas[which(DoseRespDatas[,"Study"]==i),
                        c("Scaled Log Concentration",
                          paste("Effect on mean", endpoint))]
    points(xy[,1], xy[,2], 
           col=cols[i])
    #The model predictions: mean, -, and +
    xy <- DoseResps[which(DoseResps[,"Study"]==i),
                    c("Scaled Log Concentration",
                      paste("Effect on mean", endpoint))]
    lines(xy[,1], xy[,2], lwd=1.5,
          col=cols[i])
    xy <- DoseResps[which(DoseResps[,"Study"]==i),
                    c("Scaled Log Concentration",
                      paste("Effect on mean", endpoint,"-"))]
    #lines(xy[,1], xy[,2], lwd=1.5, lty="dotted",
    #      col=rgb(cols[[1]][i], cols[[2]][i], cols[[3]][i], 1))
    xy <- DoseResps[which(DoseResps[,"Study"]==i),
                    c("Scaled Log Concentration",
                      paste("Effect on mean", endpoint,"+"))]
    #lines(xy[,1], xy[,2], lwd=1.5, lty="dotted",
    #      col=rgb(cols[[1]][i], cols[[2]][i], cols[[3]][i], 1))
    
  }
  
}

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
points(EFEffectsAtInvarRichness[,1],
       EFEffectsAtInvarRichness[,3])
points(EFEffectsAtInvarRichness[,1],
       EFEffectsAtInvarRichness[,4])

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

