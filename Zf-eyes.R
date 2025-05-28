library(caTools)


#sim <- matrix ( 0, nrow= 411 , ncol = 22)
slide <- 5
ZF <- function(x, y, slide)
{ #[slide:length(x)]
  Z =c(1:length(x));
  temp = runmean(x,  slide) +runmean(y,  slide);
  z = (temp)^2/ ( runmean(x,  slide)^2 + runmean(y,  slide)^2);
  return (z);
}



lambda <- cbind (1:411)

for (i in 1: 411) {lambda[i] = 701 - i }

dataBTB <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\Bromothymolazul\\BTB.csv")



for (i in 1:10)
{
  par(new=FALSE)
  for (j in 2:11)
  {
    sim=ZF (dataBTB[,i+1],dataBTB[,j],slide)
   plot(lambda[,1],sim, type = "l", xlab = "step", ylab = "Zf", main=expression(paste("Zfs vs" ,lambda)), col=j,las = 1)
   legend("bottomright",legend=c(i,j), lty = 0, lwd = 2)
   par(new=FALSE)
    
   }

}

