
ResultsFolder <- "/Users/frederik/Documents/Results/BD_EF/data-analysis/"

alpha <- seq(-0.9,-0.1,by=0.01)
mu2<-1
f2<-1
e2 <- 0.5
b <- c(-2,-0.4,0.4,2) #(dis)proportionality between mur and er
c <- c(-1.99,-0.39,0.39,1.99) #(dis)proportionality between fr and er
bcs <- expand.grid(b,c)
ers <- seq(1,0.5,-0.1) #relative sensitivities; arbitrary choice
ylims <- c(-1,1)

deltaEF <- function(alpha, mu2, f2, e2,
                    er, b, c)
{
  deltaEF <- f2*mu2*e2/(alpha^2-1)*(alpha*(er^(b+1)+er^c)+er^(b+c+1)+1)
  return(deltaEF)
}

plotDeltaEF <- function(alpha, mu2, f2, e2,
                        ers, b, c)
{
  for (i in c(1:length(ers)))
  {
    er  <- ers[i]
    e1  <- e2*er
    if (e1>1) {stop("e1>1!")}
    #check for coexistence in stressed condition
    mur <- er^b
    erLimit1 <- 1/e2+(1/e2-1)*1/(alpha*mur)
    erLimit2 <- 1/e2+(1/e2-1)*alpha/mur
    coexStress <- (er>erLimit1)*(er<erLimit2)
    #check for coexistence in control condition
    #...and eliminate cases where not possible
    coexControl <- (mur < -1/alpha)*(mur > -alpha)
    deltaEFCase <-deltaEF(alpha, mu2, f2, e2,
                          er=er, b=b, c=c)
    deltaEFCase[which(coexControl+coexStress==0)] <- NA
    colour <- i
    colour <- colour/length(ers)
    lines(alpha,deltaEFCase, col=rgb(red=colour, 
                                     green=1-colour, blue=0))
  }
}

quartz("",5,5,type="pdf",
       file=paste(ResultsFolder,"Analytic.pdf",sep=""))
par(mar=c(3,4,2,0.5), las=1, mfrow=c(4,4), 
    tck=-0.02, mgp=c(2,0.5,0))

for (i in 1:nrow(bcs))
{
  plot(alpha,rep(NA,length(alpha)), 
       ylim=ylims, main=paste("b=",bcs[i,1], 
                              "c=",bcs[i,2]),
       ylab="Delta EF",
       xlim=c(max(alpha),min(alpha)))
  abline(h=0)
  plotDeltaEF(alpha, mu2, f2, e2, 
              ers=ers, b=bcs[i,1], c=bcs[i,2])
  colour <- c(1:length(ers))/length(ers)
  if (i==nrow(bcs))
  {
    legend("topleft", legend=ers, pch="", lty="solid", cex=0.5,
           ncol=2,
           col=rgb(red=colour, green=1-colour, blue=0))
  }
}

dev.off()
