library(sqldf)
library(TTR)
library(caTools)

setwd("C:\\Users\\gerardo\\Google Drive\\Reporte SNIFFER\\crotalina-yo")

# open the database
#db <- dbConnect(SQLite(), dbname="electrodes_1B-12B/1B-6B_B_1.db")
# read the data
#data <- dbReadTable(db, "data") 

db <- dbConnect(SQLite(), dbname="crotab.db")
# read the data
data <- dbReadTable(db, "data")

datalength = length (data$time)


l = length(data$time)

KalmanFilter <- function(x)
{
  kalman = c(1:length(x)); 
  kalman[1] = x[1];
  for(i in 2:length(x))
    kalman[i] = 0.67*x[i] + 0.33*x[i-1];    
  
  # returns the kalman value
  return (kalman);
}

A0filter = runmean(data$A0,10)
A1filter = runmean(data$A1,10)
A2filter = runmean(data$A2,10)
A3filter = runmean(data$A3,10)
A4filter = runmean(data$A4,10)
A5filter = runmean(data$A5,10)



plot(data[,1],A0filter, type = "l", xlab = "step", ylab = "E(mV)", ylim = c(100,400),  main="Potential vs time", col=1,las = 1)
par(new=TRUE)
plot(data[,1],A1filter, type = "l", xlab = "", ylab = "",  main="",ylim = c(100,400), col=2,las = 1, axes=FALSE)
par(new=TRUE)
plot(data[,1],A2filter, type = "l", xlab = "", ylab = "",main="",ylim = c(100,400), col=3,las = 1, axes=FALSE)
par(new=FALSE)
legend("bottomright", legend = c("Sensor 1", "Sensor 2", "Sensor 3"), col = c(1, 2, 3), lty = 1, lwd = 3)


plot(data[,1],A3filter, type = "l", xlab = "step", ylab = "E(mV)", ylim = c(100,400), main="Potential vs time", col=1,las = 1)
par(new=TRUE)
plot(data[,1],A4filter, type = "l", xlab = "", ylab = "", main="", ylim = c(100,400), col=2,las = 1, axes=FALSE)
par(new=TRUE)
plot(data[,1],A5filter, type = "l", xlab = "", ylab = "", main="", ylim = c(100,400),  col=3,las = 1, axes=FALSE)
par(new=FALSE)
legend("bottomright", legend = c("Sensor 4", "Sensor 5", "Sensor 6"), col = c(1, 2, 3), lty = 1, lwd = 3)



# Similaridad entre dos sensores (señales)

ZF <- function(x, y, slide)
{ #[slide:length(x)]
  Z =c(1:length(x));
  temp = runMean(x) +runMean(y);
  z = (temp)^2/ ( runMean(x)^2 + runMean(y)^2 - temp);
  return (z);
}

slide= 5

Zf1 = ZF (data$A1, data$A2, slide )
Zf2 = ZF (data$A1, data$A3, slide )
Zf3 = ZF (data$A2, data$A3, slide )
#Zf4 = ZF (data$A0, data$A1, slide )
Zf= cbind (data$time[1:l], Zf1[1:l], Zf2[1:l], Zf3[1:l])

#View (Zf)

plot(Zf[,1],Zf[,2], type = "l", xlab = "step", ylab = "Zf", main="Zfs vs time", col="blue",las = 1)
par(new=TRUE)
plot(Zf[,1],Zf[,3], type = "l", xlab = "", ylab = "", col="black", lwd = 3, axes = FALSE)
par(new=TRUE)
plot(Zf[,1],Zf[,4], type = "l", xlab = "", ylab = "", col="red", lwd = 3, axes = FALSE)

legend("bottomright", legend = c("Zf 1/2", "Zf 1/3", "Zf 2/3"), col = c("blue", "black", "red"), lty = 1, lwd = 3)



EmaF <- function(x, alpha)
{
  ema = c(1:length(x)); 
  ema[1] = 0;
  for(i in 2:length(x))
    ema[i] = (1-alpha)*ema[i-1]+ alpha * (x[i]-x[i-1]);    
  return (ema);
}


nsensors=6
start_at =3
transient <- matrix ( 0, nrow= datalength , ncol = nsensors)
#transientMean <- matrix ( 0, nrow= datalength , ncol = nsensors)
#transientSD <- matrix ( 0, nrow= datalength , ncol = nsensors)

for (i in 0:nsensors-1)
{ 
  # transient [1:l,i] = EmaF (data[,i+start_at], 0.1) # para valores a vuelta de 600
  transient [,i+1] <- EmaF (data[,start_at+i], 0.1) # para valores a vuelta de 60
#  transientMean[1:l,i] = runMean(transient[,i])
#  transientSD[1:l,i] = runSD(transient[,i])
   if ( i < 1)
   {
  #   plot(data[,i+1],transient[,i+1], type = "l", xlab = "step", ylab = "EMA",  main="EMA vs time", col=1+i,las = 1)
   }
   else
   {
     plot(data[,i+1],transient[,i+1], type = "l", xlab = "", ylab = "", main="", col=i+1,las = 1, axes=FALSE)
   }
  #par(new=TRUE)
  
 }
  
par(new=FALSE)

  
#A0mean = runMean(data$A0, 40)




