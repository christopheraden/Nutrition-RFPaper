library(randomForest)
library(gdata)
library(ggplot2)
library(gridExtra)
load("~/Dropbox/School/RandomForest Paper/Data/HK_Percentile.RData")
keep(list="HK_Percentile", sure=TRUE)
HK_Percentile = HK_Percentile[,c(3:45, 179, 182)]

HK_Percentile_Complete = na.omit(HK_Percentile)
# write.csv(HK_Percentile_Complete, file="HK_Percentile_Complete.csv")

designMat = model.matrix( ~ . -1 -T13_Percentile, data=HK_Percentile_Complete)
simpleRF = tuneRF(x = designMat, y=HK_Percentile_Complete$T13_Percentile, doBest=TRUE)
RFPred = as.numeric(predict(object=simpleRF, newdata=HK_Percentile_Complete))

BmiPreds = as.numeric(predict(object=simpleRF, newdata=HK_Percentile_Complete, predict.all=TRUE)$individual)
BmiPreds = as.numeric(predict(object=simpleRF, newdata=HK_Percentile_Complete[1,], predict.all=TRUE)$individual)

plot(RFPred~ HK_Percentile_Complete$T13_Percentile)
abline(a =0, b=1)

var = 44
boxplot(BmiPreds ~ as.factor(rep(HK_Percentile_Complete[, var], each=500)), 
          xlab=names(HK_Percentile_Complete)[var],
          ylab="Predicted T13 BMI Percentile")
varImpPlot(simpleRF, n.var = 7)

qplot(y = RFPred, x = HK_Percentile_Complete$T3HKQ18) + stat_smooth(method='loess')
plot(RFPred~HK_Percentile_Complete$T3HKQ18)
by(data = RFPred, INDICES = as.factor(HK_Percentile_Complete$T3HKQ18), median)
by(data = RFPred, INDICES = as.factor(HK_Percentile_Complete$T3HKQ18), mean)

plots = list()
for(i in 1:(ncol(HK_Percentile_Complete)-1) ){
  if(i == 167){
    plots[[i-2]] = ggplot(data.frame(RFPred, x = HK_Percentile_Complete[,i]), aes(x, RFPred)) + 
      geom_point() + xlab(names(HK_Percentile_Complete)[i]) + ylab("Pred T13 BMI %ile") + ylim(0, 100)
  } else{
    plots[[i-2]] = ggplot(data.frame(RFPred, x = HK_Percentile_Complete[,i]), aes(x, RFPred)) + 
      geom_point() + geom_smooth(se=F) + xlab(names(HK_Percentile_Complete)[i]) + 
      ylab("Pred T13 BMI %ile") + ylim(0, 100)
  }
}
save(plots, file="plots.RData")
ggsave("2x2.pdf", do.call(marrangeGrob, c(plots, list(nrow=2, ncol=2))))
plot(simpleRF)

varImpPlot(simpleRF, n.var=10)


