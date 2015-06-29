rm(list=ls())
library(caret)
library(doMC)
library(tree)
library(pROC)
registerDoMC(8)
generateData = function(npos, nneg){
  z1 = rnorm(npos); z2 = rnorm(npos)
  pos = cbind( abs(z2*sin(z1)), z1*z2^2 )
  
  z1 = rnorm(nneg); z2 = rnorm(nneg)
  neg = cbind( cos(z1^2), abs(z2)*sqrt(z1^2+1))
  
  simData = data.frame(
    rbind(pos, neg), 
    factor(rep(0:1, times=c(npos, nneg)), labels = c("Positive", "Negative"))
  )
  names(simData) = c('X1', 'X2', 'Diagnosis')
  return(simData)
}

set.seed(1024)
simData = generateData(500, 500)

logistCV = train(Diagnosis~ X1 + X2 , data=simData, method = "glm")
logistCVSlope = -coef(logistCV$finalModel)[2]/ coef(logistCV$finalModel)[3]
logisticCVInt = - coef(logistCV$finalModel)[1] / coef(logistCV$finalModel)[3]
write.csv(file='foo.csv',summary(logistCV$finalModel)$coefficients)

#Display data and show logistic regression decision rule.
idx = sample(1:1000, 100, replace=FALSE) #Randomly sample points to plot
par(mfrow=c(1,2))
with(simData[idx,], plot(X1, X2, col=c("red","blue")[unclass(Diagnosis)], 
                   pch = c('o', '+')[unclass(Diagnosis)], 
                   main='Logistic Regression Classifier \n Classify as Positive Above Line'))
legend('bottomleft', legend=levels(simData$Diagnosis), col=c('blue', 'red'), pch=c('+', 'o'))
abline(a = logisticCVInt, b = logistCVSlope, col='black', lty=1, lwd=3)

#Fit decision tree and plot tree rules.
with(simData[idx,], plot(X1, X2, col=c("red","blue")[unclass(Diagnosis)], 
                         pch = c('o', '+')[unclass(Diagnosis)], 
                         main='Decision Tree Classifier'))
legend('bottomleft', legend=levels(simData$Diagnosis), col=c('blue', 'red'), pch=c('+', 'o'))
simTree = prune.tree(tree(Diagnosis ~ X1 + X2, data=simData), best=3)
partition.tree(simTree, ordvars = c('X1', 'X2'), label='yval', cex=1, add = TRUE)

#Compare Logistic, CART, RandomForest
ctrl = trainControl(method = "repeatedcv", number = 10, repeats = 10, savePred=TRUE)
RF = train(Diagnosis~ X1 + X2, data=simData, importance = TRUE, trControl=ctrl, method = "rf")
treeCV = train(Diagnosis~ X1 + X2, data=simData, trControl=ctrl, method = "rpart")
SVM = train(Diagnosis~ X1 + X2, data=simData, importance = TRUE, trControl=ctrl, method = "svmRadial")


write.csv(file="LogitConfusion.csv", confusionMatrix(logistCV)$table)
write.csv(file="TreeConfusion.csv", confusionMatrix(treeCV)$table)
write.csv(file="RFConfusion.csv", confusionMatrix(RF)$table)

#Logistic Regression ROC
newSimData = generateData(500,250)
LogistPredicts = predict(logistCV$finalModel, newSimData, type='response')
TreePredicts = predict(treeCV$finalModel, newSimData, type='prob')
RFpredicts = predict(RF, newSimData, type = "prob")

pdf(file='ROC_LogitRF.pdf')
plot(roc(newSimData$Diagnosis ~ LogistPredicts), col='blue', xlim=c(1,0), cex.main=.9,
     main='ROC for RandomForest (Black, AUC=.986), Logistic Regression (Blue, AUC=.858),\n and CART (Red, AUC = .842)') #Logistic ROC
plot(roc(newSimData$Diagnosis ~ RFpredicts$Positive), add=TRUE) #RandomForest ROC
plot(roc(newSimData$Diagnosis ~ TreePredicts[,'Positive']), col='red', add=TRUE) #Tree ROC
dev.off()