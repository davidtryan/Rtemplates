#http://www2.cs.uregina.ca/~dbd/cs831/notes/lift_chart/lift_chart.html

#http://0agr.ru/wiki/index.php/Cumulative_Gain_Chart

#http://0agr.ru/wiki/index.php/ROC_Analysis
#http://www.saedsayad.com/model_evaluation_c.htm

#Gain/Lift - http://www.simafore.com/blog/bid/57175/how-to-evaluate-classification-models-for-business-analytics-part-1
#ROC - http://www.simafore.com/blog/bid/57470/How-to-evaluate-classification-models-for-business-analytics-Part-2

#Lift is a measure of the effectiveness of a predictive model calculated as the ratio between the 
#results obtained with and without the predictive model.
# - Cumulative gains and lift charts are visual aids for measuring model performance
# - Both charts consist of a lift curve and a baseline
# - The greater the area between the lift curve and the baseline, the better the model

#Lift measures sensitivity (recall; tp/tp+fn) vs. support (% pop; tp+fp/n)

######################
#### IDEAL EXAMPLE ###
######################

#install.packages("ROCR")
require('ROCR')

cost <- seq(10000, 100000, 10000)
totCont <- seq(10000, 100000, 10000)
posResps <- c(6000, 10000, 13000, 15800, 17000, 18000, 18800, 19400, 19800, 20000)

perPosResp <- posResps/20000
baseline <- totCont/100000

#Cumulative gains chart with baseline and ideal lines
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="% Positive Responses", 
     xlab="% Customers Contacted", main="Cumulative Gains Chart")
#     xlim=c(0,1), ylim=c(0.6,1.6))
#lines(x=c(1, 1, 1), y=c(1, 1, 1), col="darkgreen", lwd=2)
lines(x=c(0,baseline), y=c(0,perPosResp), col="orange", lwd=2)
legend("bottomright",c("Lift Curve","Baseline"),lty=c(1,1),lwd=c(2.5,2.5),col=c("orange",'red'))

#Lift Chart
lft <- perPosResp/baseline

#Cumulative gains chart with baseline and ideal lines
plot(x=c(0.1, 1), y=c(1, 1), type="l", col="red", lwd=2,
     ylab="Lift", 
     xlab="% Customers Contacted",
     main = "Lift Chart",
     xlim=c(0.1,1), ylim=c(0,3.5))

#lines(x=c(1, 1, 1), y=c(1, 1, 1), col="darkgreen", lwd=2)
lines(x=baseline, y=lft, col="orange", lwd=2)
legend("bottomright",c("Lift Curve","Baseline"),lty=c(1,1),lwd=c(2.5,2.5),col=c("orange",'red'))




########################################################################################
########################################################################################
############################ NON IDEAL EXAMPLE #########################################
########################################################################################
########################################################################################

#install.packages("ROCR")
require('ROCR')

names <- c('Alan','Bob','Jessica','Elizabeth','Hilary','Fred','Alex','Margot','Sean',
         'Chris','Philip','Catherine','Amy','Erin','Trent','Preston','John','Nancy',
         'Kim','Laura')

height <- c(70, 72, 65, 62, 67, 69, 65, 63, 71, 73, 75, 70, 69, 68, 72, 68, 64, 64, 72, 62)
age <- c(39, 21, 25, 30, 19, 48, 12, 51, 65, 42, 20, 23, 13, 35, 55, 25, 76, 24, 31, 29)
actResponse <- c(0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1)

dta <- data.frame(names, height,age,actResponse)

pred = prediction((100-age), actResponse)
gain = performance(pred, "tpr", "rpp")

#Initial cumulative gains chart
plot(gain, col="orange", lwd=2)

#Cumulative gains chart with baseline and ideal lines
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="% Positive Responses \n True Positive Rate", 
     xlab="% Customers Contacted \n Rate of Positive Predictions",
     main= "Cumulative Gains Chart Problem 2")
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)
legend("bottomright",c("Lift Curve","Baseline"),lty=c(1,1),lwd=c(2.5,2.5),col=c("orange",'red'))

gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)


############
dta$Px <- unlist(slot(pred, "predictions"))

dta <- dta[order(dta$Px, decreasing=T),]

totResp <- data.frame(seq(2,20,2))
colnames(totResp) <- c("totContact")

count =1
i = 1
while (i < 20) {
  currSum <- sum(dta$actResponse[i:(i+1)])
  if (count > 1) {
    sumList <- c(sumList,sumList[count-1] + currSum)
  } else {
    sumList <- c(currSum)
  }
  i = i+2
  count = count +1
}
totResp$totResponse <- sumList
totResp$respRate <- sumList/10
totResp$perCont <- totResp$totContact/20

#Cumulative gains chart with baseline and ideal lines
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="% Positive Responses", 
     xlab="% Customers Contacted",
     main="Cumulative Gains Chart Problem 2")
legend("bottomright",c("Lift Curve","Baseline"),lty=c(1,1),lwd=c(2.5,2.5),col=c("orange",'red'))
#lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))

lines(x=totResp$perCont, y=totResp$respRate, col="orange", lwd=2)

totResp$Lift <- totResp$respRate/totResp$perCont

#Cumulative gains chart with baseline and ideal lines
plot(x=c(0.1, 1), y=c(1, 1), type="l", col="red", lwd=2,
     ylab="% Positive Responses", 
     xlab="% Customers Contacted",
     xlim=c(0.1,1), ylim=c(0.6,1.6),
     main="Lift Chart Problem 2")
#lines(x=c(1, 1, 1), y=c(1, 1, 1), col="darkgreen", lwd=2)
legend("bottomright",c("Lift Curve","Baseline"),lty=c(1,1),lwd=c(2.5,2.5),col=c("orange",'red'))

lines(x=totResp$perCont, y=totResp$Lift, col="orange", lwd=2)








#####################################################################################################
#####################################################################################################
#####################################################################################################

## ROC CURVES: http://en.wikipedia.org/wiki/Receiver_operating_characteristic

#In statistics, a receiver operating characteristic (ROC), or ROC curve, is a graphical plot that illustrates the performance of a binary classifier system as its discrimination threshold is varied. The curve is created by plotting the true positive rate against the false positive rate at various threshold settings. (The true-positive rate is also known as sensitivity in biomedicine, or recall in machine learning. The false-positive rate is also known as the fall-out and can be calculated as 1 - specificity). The ROC curve is thus the sensitivity as a function of fall-out. In general, if the probability distributions for both detection and false alarm are known, the ROC curve can be generated by plotting the cumulative distribution function (area under the probability distribution from -\infty to +\infty) of the detection probability in the y-axis versus the cumulative distribution function of the false-alarm probability in x-axis.

## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)
## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)
## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)




# plotting a ROC curve:
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance( pred, "tpr", "fpr" )
plot( perf )
# To entertain your children, make your plots nicer
# using ROCR's flexible parameter passing mechanisms
# (much cheaper than a finger painting set)
par(bg="lightblue", mai=c(1.2,1.5,1,1))
plot(perf, main="ROCR fingerpainting toolkit", colorize=TRUE,
     xlab="Mary's axis", ylab="", box.lty=7, box.lwd=5,
     box.col="gold", lwd=17, colorkey.relwidth=0.5, xaxis.cex.axis=2,
     xaxis.col='blue', xaxis.col.axis="blue", yaxis.col='green', yaxis.cex.axis=2,
     yaxis.at=c(0,0.5,0.8,0.85,0.9,1), yaxis.las=1, xaxis.lwd=2, yaxis.lwd=3,
     yaxis.col.axis="orange", cex.lab=2, cex.main=2)





data(ROCR.hiv)
attach(ROCR.hiv)
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
perf.svm <- performance(pred.svm, 'tpr', 'fpr')
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
perf.nn <- performance(pred.nn, 'tpr', 'fpr')
plot(perf.svm, lty=3, col="red",main="SVMs and NNs for prediction of
HIV-1 coreceptor usage")
plot(perf.nn, lty=3, col="blue",add=TRUE)
plot(perf.svm, avg="vertical", lwd=3, col="red",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
plot(perf.nn, avg="vertical", lwd=3, col="blue",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
legend(0.6,0.6,c('SVM','NN'),col=c('red','blue'),lwd=3)




# plot ROC curves for several cross-validation runs (dotted
# in grey), overlaid by the vertical average curve and boxplots
# showing the vertical spread around the average.
data(ROCR.xval)
pred <- prediction(ROCR.xval$predictions, ROCR.xval$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="grey82",lty=3)
plot(perf,lwd=3,avg="vertical",spread.estimate="boxplot",add=TRUE)


