
#Dummy presence/absence data for 4sp
SpA      <- c(1,1,1,1,1,1,0)
SpB      <- c(1,1,1,1,0,0,0)
SpC      <- c(1,1,0,0,0,0,0)
SpD      <- c(0,0,1,1,1,1,1)
Sp       <- as.data.frame(cbind(SpA, SpB, 
                                SpC, SpD))
Richness <- rowSums(Sp)
Similar  <- 1-data.matrix(vegdist(decostand(Sp,
                              method="pa")))
Similar  <- Similar[,1] #only similarity with control suffices
Levels   <- seq(0,1,length.out=nrow(Sp))

quartz("",4,3,type="pdf",
       file=paste(ResultsFolder,"Fig1.pdf",sep=""))
par(mar = c(5,5,5,5), las=1, xpd=T)
plot(Levels, Richness, xaxt="n",
     yaxp= c(1, 3, 2),
     type="l", xlab="Environmental change level", 
     ylab="Richness (----)")
axis(side=1, at=seq(0,1,length.out=7), 
     labels=c("0", paste(c(1:5),"/6",sep=""),
              "1"))
par(new = T)
plot(Levels, Similar, axes=F, xlab=NA, ylab=NA,
     type="l", lty="dashed")
axis(side = 4)
mtext(side = 4, line=3, las=0,
      "Similarity (- - -)")
legend("topleft", legend=t(Sp), cex=0.8,
       ncol=nrow(Sp), inset=c(-0.1,-0.85), bty="n")
legend("topleft", legend=paste("Sp", LETTERS[1:ncol(Sp)]), 
       cex=0.8, inset=c(-0.35, -0.85), bty="n")
#Now the levels of change for which EF is calculated
#Case1: invariant comp
LevelsInv <- Levels[which(Similar==1)]
rect(LevelsInv[1], 0, 
     LevelsInv[2], 0.05, 
     border=NA, col="green")
#Case2: variant comp
LevelsInv <- Levels[which(Richness==max(Richness))]
rect(LevelsInv[1], 0.1, 
     max(LevelsInv), 0.15, 
     border=NA, col="blue")
dev.off()


