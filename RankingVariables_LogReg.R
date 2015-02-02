###################################################################
############ Ranking Predictors in Logistic Regression ############
###################################################################

## Generating Data
bdL20 <- sample((15:17),20,replace=T)
bdL20 <- c(bdL20,sample((17:19),20,replace=T))
bdL20 <- c(bdL20,sample((18:20),61,replace=T))
bd2024 <- sample((21:24),105,replace=T)
bd24 <- sample((25:28),20,replace=T)
bd24 <- c(bd24,sample((29:31),10,replace=T))
bd24 <- c(bd24,sample((31:36),6,replace=T))

bdT <- c(bdL20,bd2024,bd24)
bdT <- data.frame(bdT, rep(1,length(bdT)))
names(bdT) <- c("Age","BirthDefect")
bdT$smoke <- sample(c(0,1),size=length(bdT$Age),replace=TRUE,prob=c(0.35,0.65))
bdT$parity <- sample(c(0,1),size=length(bdT$Age),replace=TRUE,prob=c(0.55,0.45))

nbdL20 <- sample((15:17),260,replace=T)
nbdL20 <- c(nbdL20,sample((17:19),260,replace=T))
nbdL20 <- c(nbdL20,sample((18:20),865,replace=T))
nbd2024 <- sample((21:24),3755,replace=T)
nbd24 <- sample((25:39),5500,replace=T)
nbd24 <- c(nbd24,sample((40:41),650,replace=T))
nbd24 <- c(nbd24,sample((42:44),300,replace=T))
nbd24 <- c(nbd24,sample((45:46),50,replace=T))
nbd24 <- c(nbd24,sample((47:53),11,replace=T))

nbdT <- c(nbdL20,nbd2024,nbd24)
nbdT <- data.frame(nbdT, rep(0,length(nbdT)))
names(nbdT) <- c("Age","BirthDefect")
nbdT$smoke <- sample(c(0,1),size=length(nbdT$Age),replace=TRUE,prob=c(0.51,0.49))
nbdT$parity <- sample(c(0,1),size=length(nbdT$Age),replace=TRUE,prob=c(0.485,0.515))

#Cumulative Data on birth defects as a result of age, smoking habits and parity
bT <- rbind(bdT,nbdT)

#Plot the resulting data table
plot(bT)


## Create a logistic model to predict presence/abscense of birth defect based on maternal age
bdlog <- glm(BirthDefect~Age,binomial, data=bT)
summary(bdlog)

#Plot a representation of the model
plot(bT$Age,fitted(bdlog),xlab="Maternal Age",ylab="P(Birth Defect)",pch=15)

#Example: Predict the probability of birth defect as a result of Age=20
pAge(20)

#Example: How much does the predicted probability differ between women of ages 20 and 25
ageDiff(20,25)



exp(cbind(OR=coef(bdlog),CI=confint(bdlog)))



library(arm)
jitter.binary <- function(a,jitt=0.05) {ifelse (a==0,runif(length(a),0,jitt),runif(length(a),1-jitt,1))}
bd.jitter <- jitter.binary(bT$BirthDefect)
plot(bT$Age,bd.jitter)
curve(invlogit(coef(bdlog)[1]+coef(bdlog)[2]*x),add=TRUE)



bT$Agecat3 <- ifelse(bT$Age>25,c(1),c(0))
bT$Agecat2 <- ifelse(bT$Age>=20 & bT$Age<25,c(1),c(0))
bT$Agecat1 <- ifelse(bT$Age<20,c(1),c(0))




bdlog2 <- glm(BirthDefect~Agecat1+Agecat2,binomial,data=bT)
summary(bdlog2)


bdlog3 <- glm(BirthDefect~Agecat1+Agecat2+smoke+parity,binomial,data=bT)
summary(bdlog3)



OR <- exp(cbind(OR=coef(bdlog3),CI=confint(bdlog3)))
OR

anova(bdlog3,test="Chisq")


bdlog3.red <- glm(BirthDefect~Agecat1+Agecat2+smoke,binomial,data=bT)
1-pchisq(deviance(bdlog3.red)-deviance(bdlog3),df.residual(bdlog3.red)-df.residual(bdlog3))



x <- predict(bdlog3)
y <- resid(bdlog3)
binnedplot(x,y)


varRank.wald <- function(model) {
  out <- summary(model)
  out <- out$coefficients[-1,]
  waldstat <- out[order(out[,4]),4]
  waldstat
}
varRank.wald(bdlog3)














#Predict risk of birth defect at age 20
pAge <- function(inqAge) {
  lgt <- predict(bdlog, newdata=data.frame(Age=inqAge))[[1]]
  pAge <- exp(lgt)/(1+exp(lgt))
  sprintf("When predicting the birth defects as a function of maternal age, the birth defect probability for a woman of age %d is %f",inqAge,pAge)
}


#ODDS RATIO - measures how the fitted probability changes between different values of the explanatory variable
ageDiff <- function(xmin,xmax) {
  hOdds <- exp(coef(bdlog)[[2]]*(xmax-xmin))
  lOdds <- exp(coef(bdlog)[[2]]*(xmin-xmax))
  hOddsP <- ((1-hOdds)*100)
  sprintf("The odds of a %d year old woman having a birth defect is %f (%f%% less) that of a %d year old woman \r\n
          The odds of a %d year old woman having a birth defect is about %f times that of a %d year old woman",xmax,hOdds,hOddsP,xmin,xmin,lOdds,xmax)
#   sprintf("The odds of a %d year old woman having a birth defect is about %f times that of a %d year old woman",xmin,lOdds,xmax)
}

sprintf("I am trying\n to figure this out")
