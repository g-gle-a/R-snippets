
library(corr2D)
library(e1071)

dataBTB <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\Bromothymolazul\\BTB.csv")
dataY <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\pyrro_conc.txt")
datacurcS <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\CurcuminS\\curcumS.csv")
dataninhd <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\Ninhydrin\\ninhydr.csv")
dataRhod560 <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\Rhod560\\Rhod560.csv")
dataRhod6G <- read.csv("~\\Projectos\\TEKEVER\\SNIFFER-EU\\Resultado\\crotalina\\Crotalina\\Rhodamina 6G\\Rhod6G.csv")



lambda <- cbind (1:411)

for (i in 1: 411) {lambda[i] = 701 - i }



datlambda <-data.frame(lambda)

for (i in 18:23)
{ 
  if (i>2) {
    par(new=TRUE)
  }
  
  plot(lambda, dataBTB[,i], col=i, type="l",ylab = " Absorbance",  xlab= expression (lambda), yaxt='n',  lwd=1, main = "Indicator btb")
  if (i>1) {
    box()
  }
}
par(new=F)


for (i in 1:8)
{ 
  if (i>1) {
    par(new=TRUE)
  }
  
  plot(datacurcS[,1], datacurcS[,i+1], col=i, type="l",ylab = " Absorbance",  xlab= expression (lambda), yaxt='n',  lwd=1, main = "Indicator curcS")
  if (i>1) {
    box()
  }
}
par(new=F)

for (i in 1:6)
{ 
  if (i>1) {
    par(new=TRUE)
  }
  
  plot(dataninhd[,1], dataninhd[,i+1], col=i, type="l",ylab = " Absorbance" , xlab= expression (lambda), yaxt='n',  lwd=1, main = "Indicator ninhyd")
  if (i>1) {
    box()
  }
}
par(new=F)

for (i in 1:11)
{ 
  if (i>1) {
    par(new=TRUE)
  }
  
  plot(dataRhod560[,1], dataRhod560[,i+1], col=i, type="l",ylab = " Absorbance",   xlab= expression (lambda), yaxt='n',  lwd=1, main = "Indicator rhod560")
  if (i>1) {
    box()
  }
}
par(new=F)

for (i in 1:9)
{ 
  if (i>1) {
    par(new=TRUE)
  }
  
  plot(dataRhod6G[,1], dataRhod6G[,i+1], col=i, type="l",ylab = " Absorbance", xlab= expression (lambda), yaxt='n',  lwd=1, main = "Indicator rhod6G")
  if (i>1) {
    box()
  }
}
par(new=F)

### Modified crotalin data with Et-Phe-amine !!!!!

## original code: 
## datFram1 <-  t(data.matrix (dataBTB[, 2:11]))
## but I have added column 19 (some concentration for MePhe-amine)
# c(1, 2:11, 19)
datFram1 <-  t(data.matrix (dataBTB[,c(1,18:23, 19)]))
datFram2<-  t(data.matrix (datacurcS[,3:9]))
datFram3<-  t(data.matrix (dataninhd[,3:7]))
datFram4<-  t(data.matrix (dataRhod560[,3:12]))
datFram5<-  t(data.matrix (dataRhod6G[,3:10]))




colnames(datFram1) <-  lambda
colnames(datFram2) <-  datacurcS[,1]
colnames(datFram3) <-  dataninhd[,1]
colnames(datFram4) <-  dataRhod560[,1]
colnames(datFram5) <-  dataRhod6G[,1]

par(new=T)

 par (plt = c(1,1,51,51))

btb2d <- corr2d (datFram1)#, Time =dataY[2:10, 1])
#graphics.off()
par("mar")
par(mar=c(1,1,1,1))
#plot_corr2din3d(Re(twod$FT))
#plot_corr2d(twod, Contour=F)
plot_corr2d(btb2d, what = Re(btb2d$FT) , xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm))  )
plot_corr2d(btb2d, what = Im(btb2d$FT) , xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm)) )
#

curc2d <- corr2d (datFram2 )#, Time =dataY[11:16, 2])
plot_corr2d(curc2d,what = Re(curc2d$FT) , xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ nm) )
plot_corr2d(curc2d, what = Im(curc2d$FT),  xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm)) )

ninh2d <- corr2d ( datFram3)#, Time =dataY[17:22, 3])
plot_corr2d(ninh2d, what = Re(ninh2d$FT), xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm))  )
plot_corr2d(ninh2d, what = Im(ninh2d$FT),  xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm))  )

rhod5602d <- corr2d (datFram4)#, Time =dataY[11:16, 2])
plot_corr2d(rhod5602d,what = Re(rhod5602d$FT), xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm)) )
plot_corr2d(rhod5602d, what = Im(rhod5602d$FT),  xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm))  )

rhod6g <- corr2d (datFram5)#, Time =dataY[11:16, 2])
plot_corr2d(rhod6g,what = Re(rhod6g$FT),  xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm)) )
plot_corr2d(rhod6g, what = Im(rhod6g$FT),  xlab = expression (lambda ~ (nm)), ylab = expression (lambda ~ (nm)) )

## TRYING DEEP LEARNING FROM HERE!!!


dat <- cbind(dataBTB[,1:11], datacurcS[,2:9], dataninhd[,2:7], dataRhod560[,2:12], dataRhod6G[,2:9])

data <-  t(dat)

model <- svm(data, dataY)
print(model)
summary(model)
# test with train data
pred <- predict(model, data)
# (same as:)
pred <- fitted(model)



rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778


tuneResult <- tune.svm (  data, dataY)

print(tuneResult)
# best performance: MSE = 8.371412, RMSE = 2.89 epsilon 1e-04 cost 4
# Draw the tuning graph


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, data) 




par(new=T)

library(pls)
pls.options(plsralg = "simpls")
print("####################   PLSR used here  ##################################")

Y <-as.matrix(dataY)

gas1 <- plsr( Y ~ .,  data = as.data.frame(  data), validation = "LOO")
summary(gas1)

plot(RMSEP(gas1), legendpos = "topright")

plot(gas1, ncomp = 7, asp = 1, line = TRUE)

plot(gas1, plottype = "scores", comps = 1:7)

plot(gas1, "loadings", comps = 1:7,  cex=0.5, pch=1,  xlab = "nm") #labels = "numbers",  legendpos = "bottom",

abline(h = 0)

predict(gas1, ncomp = 5, newdata = as.data.frame (data[5:12,]))
print("__________________________________________________________________________________")
print(predict)


gas2 <- pcr( Y ~ .,  data = as.data.frame(  data), validation = "LOO")
summary(gas2)

plot(RMSEP(gas2), legendpos = "topright")

plot(gas2, ncomp = 7, asp = 1, line = TRUE)

plot(gas2, plottype = "scores", comps = 1:7)

plot(gas2, "loadings", comps = 1:7,  cex=0.5, pch=1,  xlab = "nm")

#RMSEP(gas1, newdata = Validate)
print("__________________________________________________________________________________")

#plot(gas1, plottype = "coef",  xlab = "nm") #, legendpos = "bottomleft", labels = "numbers"

plot(gas2, plottype = "correlation", main="Correlation")

#predplot(gas1,  newdata = as.data.frame( XValidate), asp = 1, line = TRUE)
predict(gas2, ncomp = 5, newdata = as.data.frame (data[5:12,]))
print("__________________________________________________________________________________")
print(predict)

print("###################################   GLMNET from here ########################################")

library(glmnet)

glmnet.model <- glmnet(data , Y)# = dataX,  scale= FALSE)#, cost = 100, gamma = 1)

plot(glmnet.model, xvar = c("norm", "lambda", "dev"), main="Glmnet model")

## glmnet.cvmodel <- cv.glmnet(data, Y, family = "multinomial" ) = dataX,  scale= FALSE)#, cost = 100, gamma = 1)

print ("_____________________________ GLMNET FINISHED_____________________________")


library(kernlab)
# fit model
fit <- ksvm(Y, data)
     # summarize the fit
  summary(fit)
     # make predictions
 predictions <- predict(fit, data)
     # summarize accuracy
 mse <- mean((dataY - predictions)^2)
print(mse)


print("############################################## KERNLAB ################################")

library(kernlab)

test <- gausspr(Y ~.,data=data,var=5)
print (test)
test

predict(test, data[4:10,])


test1 <- ksvm(Y ~.,data=data,var=5)
print (test1)
test1

predict(test1, data[4:10,])
predict(test1, data[20:30,])

foo <- rvm(data, Y)
foo
# print relevance vectors
alpha(foo)
RVindex(foo)

ytest <- predict(foo, data[4:10,])
ytest
print("############################################## END KERNLAB ################################")


print("############################################## Begin NN  ################################")


# Create Vector of Column Max and Min Values
maxs <- apply(data[1:44,], 2, max)
mins <- apply(data[1:44,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(data[1:44,],center = mins, scale = maxs - mins))

datann = cbind(dataY,scaled.data)

library(caTools)
set.seed(101)

# Create Split (any column is fine)
split = sample.split(datann$conc, SplitRatio = 0.70)

# Split based off of split Boolean Vector
train = subset(datann, split == TRUE)
test = subset(datann, split == FALSE)


feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('conc ~  ',f)

# Convert to formula
f <- as.formula(f)

f

#train2<-train[complete.cases(train),]

#install.packages('neuralnet')
library(neuralnet)
nn <- neuralnet(f,train,rep=5,hidden=c(100,100,50,10),linear.output=FALSE, algorithm = "backprop")

# Compute Predictions off Test Set
predicted.nn.values <- compute(nn,test[,2:412])

# Check out net.result
print(head(predicted.nn.values$net.result))

predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

table(test$conc,predicted.nn.values$net.result)



