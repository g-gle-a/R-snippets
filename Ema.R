
library(sqldf)
library(TTR)
library(cluster)
# Scatterplot Matrices from the glus Package 
library(gclus)
library(MASS)

#Datos iniciales, introducidos por el operador, despu√©s no hacer nada m√°s!
#Dados iniciais, introduzidos pelo operador, n√£o √© precisso fazer mais nada a seguir!
#Initial data, given by the operator, then nothing more is needed!

# open the database
#setwd("/home/gerardo/Documents/Artigos/Sw_Sensor")
#db <- dbConnect(SQLite(), dbname="1Ce-6Ce_BC_0.db")

#setwd("/home/gerardo/Documents/1-SAGE")
#db <- dbConnect(SQLite(), dbname="B0-B3PA_maio_12.db")
#db <- dbConnect(SQLite(), dbname="C0-C3 13 de maio.db")


setwd("C:\\Users\\gerardo\\Google Drive\\Reporte SNIFFER\\crotalina-yo")
#db <- dbConnect(SQLite(), dbname="C:\\Users\\gerardo\\Google Drive\\QACS\\A\\joao_A_H2O.db")
db <- dbConnect(SQLite(), dbname="crotab.db")


# joao_A_H2O_2.db es bromuro de tetraoctilamonio
# joao_A_H2O.db es cloruro de benzalconio
# joao_A_H2O_3.db brometo de didodecilamÛnio.
# joao_A_H2O_4.db brometo de tetrahexadecilamÛnio.
# joao_A2_H2O.db  cloreto de benzalcÛnio.
# joao_A2_H2O_2.db cloreto de benzalcÛnio.
# joao_A3_H2O_2.db  cloreto de benzalcÛnio.

nsensors = 6
start_at = 3

##############################################################################

# read the data
data <- dbReadTable(db, "data") 


#data <- read.csv ("electrodes_1B-12B/1B-6B_B_1.csv")

datalength = length (data$time)



# Zmi Semejanzas entre todos los sensores
#) 	Kennedy, H. L. A New Statistical Measure of Signal Similarity. 
# In Information, Decision and Control, 2007. IDC '07; 2007; pp. 112-117.

sum <- c(1 : datalength)
sum2 <- c(1 :datalength)

#start_at = start_at -1
sum = rowSums (data[3:nsensors +start_at-1])/nsensors 
#sum2 = rowSums (cbind (data[2]^2,data[3]^2, data[4]^2, data[5]^2, data[6]^2, data[7]^2 ))/6 
sum2 = rowSums (data[3: nsensors + start_at-1]^2)/nsensors 

ZMi = runMean (cbind(sum^2), 20)/(runMean (cbind(sum2),20)- runMean (cbind(sum^2),20))  

# Zmi terrmina aqui
##########################################################################

plot(data$time,ZMi, type = "l", xlab = "step", ylab = "Similarity", col="red", main = "Similarity of all electrodes" , lwd = 3, axes = TRUE)


#start_at = start_at -1
sum = rowSums (data[3:6])/nsensors-3 
#sum2 = rowSums (cbind (data[2]^2,data[3]^2, data[4]^2, data[5]^2, data[6]^2, data[7]^2 ))/6 
sum2 = rowSums (data[3:6]^2)/nsensors-3 

ZMi = runMean (cbind(sum^2), 20)/(runMean (cbind(sum2),20)- runMean (cbind(sum^2),20))  

# Zmi terrmina aqui
##########################################################################

plot(data$time,ZMi, type = "l", xlab = "step", ylab = "Similarity", col="red", main = "Similarity of NIP electrodes" , lwd = 3, axes = TRUE)



#start_at = start_at -1
sum = rowSums (data[6:nsensors +start_at-1])/nsensors-3 
#sum2 = rowSums (cbind (data[2]^2,data[3]^2, data[4]^2, data[5]^2, data[6]^2, data[7]^2 ))/6 
sum2 = rowSums (data[6: nsensors + start_at-1]^2)/nsensors-3 

ZMi = runMean (cbind(sum^2), 20)/(runMean (cbind(sum2),20)- runMean (cbind(sum^2),20))  

# Zmi terrmina aqui
##########################################################################

plot(data$time,ZMi, type = "l", xlab = "step", ylab = "Similarity", col="red", main = "Similarity of MIP electrodes" , lwd = 3, axes = TRUE)



rMeans <- matrix ( nrow=datalength, ncol =nsensors)
rSD <- matrix ( nrow=datalength, ncol =nsensors)


for (i in 1:nsensors)
{

rMeans[, i] = runMean (data[,i+start_at-1])
rSD[, i] = runSD (rMeans[,i])
par (new = TRUE)

plot(data$time, rMeans[1:datalength, i],  type = "l", xlab = "", ylab = "", col= i, lwd = 3, axes = FALSE) 
}  


#for (i in 1:nsensors)
#{
  #rSD[1:datalength, i] = runSD (rMeans[,i])
#}

par (new = FALSE)

 

for (i in 1:nsensors)
{  
 if (i==1){
  plot(data$time, rMeans[1:datalength, i],  type = "l", xlab=  "time" , ylab = "Electrode Potential", main = "Electrode response to B. cereus additions ",col= i, lwd = 3, axes = TRUE) 
 }
else {plot(data$time, rMeans[, i],  type = "l", xlab=  "" , ylab = "", main = "",col= i, lwd = 3, axes = FALSE) 
}
  par (new = TRUE)
} 


par (new = FALSE)

# Ema : exponential move averaging as:
#
# 	Muezzinoglu, M. K.; Vergara, A.; Huerta, R.; Rulkov, N.; Rabinovich, M. I.;
#Selverston, A.; Abarbanel, H. D. I. Acceleration of Chemo-Sensory Information 
# Processing Using Transient Features. Sens. Actuators B Chem. 2009, 137, 507-512.


EmaF <- function(x, alpha)
{
  ema = c(1:length(x)); 
  ema[1] = 0;
  for(i in 2:length(x))
    ema[i] = (1-alpha)*ema[i-1]+ alpha * (x[i]-x[i-1]);    
  return (ema);
}


k  <-  matrix ( 0, nrow= 40, ncol = nsensors )
transient <- matrix ( 0, nrow= datalength , ncol = nsensors)
transientMean <- matrix ( 0, nrow= datalength , ncol = nsensors)
transientSD <- matrix ( 0, nrow= datalength , ncol = nsensors)
xregresion <-  matrix ( 0, nrow= 40, ncol = nsensors )

for (i in 1:nsensors)
{ 
 # transient [1:datalength,i] = EmaF (data[,i+start_at], 0.1) # para valores a vuelta de 600
  transient [1:datalength,i] = EmaF (data[,i+start_at-1], 0.05) # para valores a vuelta de 60
  transientMean[1:datalength,i] = runMean(transient[,i])
  transientSD[1:datalength,i] = runSD(transient[,i])
  z=1  
#  plot(data$time,transient[,i],  type = "l", xlab = "step", ylab = "Response", col=i, main = "Transient features of electrodes" , lwd = 3, axes = (i == 1)*TRUE)
  par(new = TRUE)
  xregresion[k[z,i], i] = rMeans[25, i]
# Hacer aqui el algoritmo: leo transient, escojo el valor , pongo la media en la variable regresion "" con la primera columna "conc"
  #for (j in 25: length (data$time) )
  #{
   #if (abs(transient[j,i]) > abs(transientMean[j,i])+2.45*transientSD[j,i]) ###### para valores en los >600 
   # if (abs(transient[j,i]) > abs(transientMean[j,i])+2.85*transientSD[j,i]) ###### INCOMPLETO no funciona para los 60-100
 #   if (abs(transient[j,i]) > abs(transientMean[j-10,i])*1050)
   # {
   #    xregresion[k[z,i]+1, i] = data[j, i+start_at] # hubo un cambio de concentraci√≥n, la media es tomada del 25th valor anterior....
       # marcador de concentraciones
  #    z = z+1
  #    k[z,i] = k[z-1,i]+1
   #   j= j+10
      
   # }
 # }
}

par (new=FALSE)
plot(data$time,transient[,4],  type = "l", xlab = "step", ylab = expression("EMA"[0.1]), col="black", main = "Exponential Moving  Average of a sensor" , lwd = 3, axes = TRUE)
par(new=TRUE)
plot(data$time,data$conc, type = "l", xlab = "step", ylab = "", col="red", main = "" , lwd = 3, axes = FALSE)


#[400:600]

 plot(data$time,transient[,4], ylim = c(-0.2,0.2), type = "l", xlab = "step", ylab = expression("EMA"[0.1]), col="black", main = "Measurement, EMA and Running Standard Deviation" , lwd = 3, axes = TRUE)
par(new = TRUE)
plot(data$time,transientMean[,4],  type = "l", xlab = "step", ylab = expression("EMA"[0.1]), col="green", lwd = 3, axes = TRUE)
par(new = TRUE)
plot(data$time, rMeans[,4],  type = "l", xlab = "", ylab = "", col= "red", lwd = 3, axes = FALSE) 
#par(new = TRUE)
#plot(data$time , rSD[,2],  type = "l", xlab = "", ylab = "", col= "cyan", lwd = 3, axes = FALSE) 
legend("topright", legend = c("Measurement", "Running SD", "EMA"), col = c("black", "green", "red"), bty ='n', lty = 1, lwd = 2)


par(new=FALSE)
#transiente detecta el cambio m√°s r√°pido que SD.... check that


#k
#xregresion

#transient

#sink ("Fitting.out")
  



for (i in 1:nsensors)
{
  #sink (fout, type="output")
glm.K <- glm(k[,i] ~ xregresion[,i] )


summary(glm.K)

glm.K$anova

#write(glm.K,file=(")
#step.K <- stepAIC (glm.K,  k[,i] ~ xregresion[,i] , direction = "both", trace=100)
#summary (step.K)
#step.K$anova

print (model.matrix (glm.K))
#sink()
}
#sink()

  
par (new = FALSE)

starts = start_at 
ends = start_at + nsensors -1 
pairs(data[,starts:ends], main="Sensor Scatterplot Matrix")

dta <-data.frame ( data[40:datalength, starts:ends], check.names= TRUE)

dta.r <- abs(cor(dta)) # get correlations
write.matrix(dta.r,file="normalized-c_mean.cm")
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Sensors Ordered and Coloured by Correlation" )
library(relimp, pos=26)

