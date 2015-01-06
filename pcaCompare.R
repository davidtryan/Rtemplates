
## References
# http://statweb.stanford.edu/~jtaylo/courses/stats202/olympic.html
# http://www.statistik.tuwien.ac.at/public/filz/students/seminar/ws1011/hoffmann_ausarbeitung.pdf
# http://factominer.free.fr/classical-methods/principal-components-analysis.html
# http://www.inside-r.org/packages/cran/FactoMineR/docs/plot.PCA

###################################################################
## Installing and initializing
install.packages('ade4')
library(ade4)
library(FactoMineR)

## Loading data
data(olympic)

###################################################################
## Compare initial plots
par(mfrow=c(1,1))
par(mar=c(5,4,4,2))
#princomp
pca.olympic_nc <- princomp(olympic$tab)     #no correlation selected (nc)
pca.olympic <- princomp(olympic$tab, cor=T)
#Biplot gives a graphical summary of both 1) cases (athletes) in terms of scores and 2) the variables in terms of loadings
biplot(pca.olympic, main="princomp Results")
text(pca.olympic$scores[,1], pca.olympic$scores[,2], labels=rep("X", nrow(olympic$tab)), cex= 0.7)
# biplot(pca.olympic, pch=5, main="princomp Results", xlabs=rep("X", nrow(olympic$tab))
abline(h=0, lty=2)
abline(v=0, lty=2)

par(mfrow=c(1,2))
#FactoMineR
# PCA.olympic_nc <- PCA(olympic$tab)
PCA.olympic <- PCA(olympic$tab, scale.unit=T, ncp=5, graph=T)
###################################################################


###################################################################
## compare loadings
#princomp pca.olympic$loadings
PCA.olympic.loadings <- sweep(PCA.olympic$var$coord, 2, sqrt(PCA.olympic$eig[1:5,1]), FUN="/")

#FactoMineR PCA.olympic.loadings
ld_compare <- cbind(-PCA.olympic.loadings[,1], pca.olympic$loadings[,1], round((PCA.olympic.loadings[,1] + pca.olympic$loadings[,1]), digits=10))
colnames(ld_compare) <- c('FactoMineR PCA (inv)', 'princomp PCA', 'diff')

ld_compare
###################################################################


###################################################################
## Compare eigenvalues
PCA.olympic$eig
#First two dimensions resume 50% of the total inertia (i.e. total variance)

pca.eig <- pca.olympic$sdev^2
vars <- pca.olympic$sdev^2 
vars <- vars/sum(vars) 
pca.eig <- cbind(pca.eig, vars*100, cumsum(vars)*100)
colnames(pca.eig) <- colnames(PCA.olympic$eig)
pca.eig

eigDiff <- round(PCA.olympic$eig - pca.eig, digits = 10)
eigDiff
###################################################################


###################################################################
## Compare plots
par(mfrow=c(2,2))
#princomp
plot(pca.olympic)
plot(pca.olympic, type='l')

#FactoMineR
PCA.bp <- barplot(PCA.olympic$eig[,1], main="PCA.olympic")
plot(x=PCA.bp, y=PCA.olympic$eig[,1], main="PCA.olympic")
lines(x=PCA.bp, y=PCA.olympic$eig[,1], type='l')

par(mfrow=c(1,1))
plot(PCA.olympic)
points(-pca.olympic$scores[,1], pca.olympic$scores[,2], col='red', pch=0)
legend("topright", c('FactoMineR', 'princomp'), pch=c(19,0), col=c("black","red"), cex=0.75, bty='n')
###################################################################


###################################################################
## Compare Plots (extended - examples)
# biplot(pca.olympic)
# data.frame(olympic$tab[, '1500'], pca.olympic$scores[,1])
#princomp         
par(mfrow=c(1,3))
plot(olympic$tab[, '1500'], pca.olympic$scores[,1], pch=23, bg='red', cex=2, xlab='1500', ylab='pca score')
plot(olympic$tab[, 'jave'], pca.olympic$scores[,2], pch=23, bg='red', cex=2, xlab='javelin', ylab='pca score')
#Overall score
plot(pca.olympic$scores[,1], olympic$score, xlab='Comp. 1', pch=23, bg='red', cex=2, ylab='pca score')
#Since the first variable (PC) seems related to speed, we should not be surprised that an increase in this variable (PC) decreases the overall decathlon score

#FactoMineR
par(mfrow=c(1,3))
plot(olympic$tab[, '1500'], -PCA.olympic$ind$coord[,1], pch=23, bg='red', cex=2, xlab='1500', ylab='PCA score')
plot(olympic$tab[, 'jave'], PCA.olympic$ind$coord[,2], pch=23, bg='red', cex=2, xlab='javelin', ylab='PCA score')
#Overall score
plot(-PCA.olympic$ind$coord[,1], olympic$score, xlab='Comp. 1', pch=23, bg='red', cex=2, ylab='PCA score')
#Since the first variable (PC) seems related to speed, we should not be surprised that an increase in this variable (PC) decreases the overall decathlon score
###################################################################


###################################################################
#The first axis opposes athletes who are "good everywhere" like Karpov during the Olympic Games between those who are 
#"bad everywhere" like Bourguignon during the Decastar. This dimension is particularly linked to the variables of speed 
#and long jump which constitute a homogeneous group.

#Variables linked to first principal component
#princomp
impList <- sort(pca.olympic$loadings[,1])
maxInd <- which(impList==max(abs(impList)))
maxIndList <- c(maxInd)
swp <- 0
while (swp==0 && length(maxIndList)<4) {
  catImpList <- impList[-c(maxIndList)]
  maxInd_t <- which(abs(impList)==max(abs(catImpList)))
  if (maxInd_t!=(maxInd-1)) {
    swp=1
  }
  maxIndList <- c(maxIndList, maxInd_t)
}
impNums <- impList[maxIndList]
names(impNums)
#FactoMineR
PCA_contrb2 <- data.frame(PCA.olympic$var$contrib)
PCA_contrb2 <- PCA_contrb2[order(-PCA_contrb2$Dim.1),]
rownames(PCA_contrb2)[1:length(impNums)]
#These tables give the correlation coefficient and the p-value of the 
#variables which are significantly correlated to the principal dimensions. 
#Both active and supplementary variables whose p-value is smaller than 0.05 appear.dimdesc(PCA.olympic, axes=c(1,2)) 
charCat <- dimdesc(PCA.olympic, axes=c(1,2))$Dim.1$quanti
names(sort(-abs(charCat[,1]))[1:length(impNums)])


#Plot variables linked to first principal component to see how chanages in varaibles contribute to score
#princomp
par(mfrow=c(2,2))
olympic_p <- olympic$tab

var1 <- names(impNums)[1]
plot(olympic_p[,var1], -pca.olympic$scores[,1], xlab=var1, ylab='score')
abline(lm(-pca.olympic$scores[,1]~olympic_p[,var1]))
var2 <- names(impNums)[2]
plot(olympic_p[,var2], -pca.olympic$scores[,1], xlab=var2, ylab='score')
abline(lm(-pca.olympic$scores[,1]~olympic_p[,var2]))
var3 <- names(impNums)[3]
plot(olympic_p[,var3], -pca.olympic$scores[,1], xlab=var3, ylab='score')
abline(lm(-pca.olympic$scores[,1]~olympic_p[,var3]))
#Overall score
plot(-PCA.olympic$ind$coord[,1], olympic$score, xlab='Comp. 1', ylab='PCA score')
# plot(olympic_p[,c('400')], -pca.olympic$scores[,1], xlab='400', ylab='score')
# abline(lm(-pca.olympic$scores[,1]~olympic_p[,c('400')]))

#FactoMineR
par(mfrow=c(2,2))
olympic_p <- olympic$tab

var1 <- rownames(PCA_contrb2)[1:length(impNums)][1]
plot(olympic_p[,var1], PCA.olympic$ind$coord[,1], xlab=var1, ylab='score')
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,var1]))
var2 <- rownames(PCA_contrb2)[1:length(impNums)][2]
plot(olympic_p[,var2], PCA.olympic$ind$coord[,1], xlab=var2, ylab='score')
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,var2]))
var3 <- rownames(PCA_contrb2)[1:length(impNums)][3]
plot(olympic_p[,var3], PCA.olympic$ind$coord[,1], xlab=var3, ylab='score')
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,var3]))
#Overall score
plot(-PCA.olympic$ind$coord[,1], olympic$score, xlab='Comp. 1', ylab='PCA score')
# plot(olympic_p[,c('400')], PCA.olympic$ind$coord[,1], xlab='400', ylab='score')
# abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,c('400')]))


###########VARIABLES BASED ON STRENGTH###################
#The second axis opposes athletes who are strong (variables "Discus" and "Shot.put") between those who are not. 
#The variables "Discus", "Shot.put" and "High.jump" are not much correlated to the variables "X100m", "X400m", "X110m.hurdle" 
#and "Long.jump". This means that strength is not much correlated to speed.

#Variables linked to second principal component
#princomp
impList <- sort(pca.olympic$loadings[,2])
maxInd <- which(impList==max(abs(impList)))
maxIndList <- c(maxInd)
swp <- 0
while (swp==0 && length(maxIndList)<4) {
  catImpList <- impList[-c(maxIndList)]
  maxInd_t <- which(abs(impList)==max(abs(catImpList)))
  if (maxInd_t!=(maxInd-1)) {
    swp=1
  }
  maxInd <- maxInd_t
  maxIndList <- c(maxIndList, maxInd_t)
}
impNums <- impList[maxIndList]
names(impNums)
#FactoMineR
PCA_contrb2 <- data.frame(PCA.olympic$var$contrib)
PCA_contrb2 <- PCA_contrb2[order(-PCA_contrb2$Dim.2),]
rownames(PCA_contrb2)[1:length(impNums)]
#These tables give the correlation coefficient and the p-value of the 
#variables which are significantly correlated to the principal dimensions. 
#Both active and supplementary variables whose p-value is smaller than 0.05 appear.dimdesc(PCA.olympic, axes=c(1,2)) 
charCat <- dimdesc(PCA.olympic, axes=c(1,2))$Dim.2$quanti
names(sort(-abs(charCat[,1]))[1:length(impNums)])

#Overall score
par(mfrow=c(1,1))
plot(-PCA.olympic$ind$coord[,2], olympic$score, xlab='Comp. 2', ylab='PCA score')

#princomp
par(mfrow=c(2,2))
olympic_p <- olympic$tab

var1 <- names(impNums)[1]    #Discus 
plot(olympic_p[,var1], pca.olympic$scores[,2], xlab=var1, ylab='score')
abline(lm(pca.olympic$scores[,2]~olympic_p[,var1]))
var2 <- names(impNums)[2]     #Shotput
plot(olympic_p[,var2], pca.olympic$scores[,2], xlab=var2, ylab='score')
abline(lm(pca.olympic$scores[,2]~olympic_p[,var2]))
var3 <- names(impNums)[3]
plot(olympic_p[,var3], pca.olympic$scores[,2], xlab=var3, ylab='score')
abline(lm(pca.olympic$scores[,2]~olympic_p[,var3]))
var4 <- names(impNums)[4]
plot(olympic_p[,var4], pca.olympic$scores[,2], xlab=var4, ylab='score')
abline(lm(pca.olympic$scores[,2]~olympic_p[,var4]))

#FactoMineR
par(mfrow=c(2,2))
olympic_p <- olympic$tab

var1 <- rownames(PCA_contrb2)[1:length(impNums)][1]     #Discus
plot(olympic_p[,var1], PCA.olympic$ind$coord[,2], xlab=var1, ylab='score')
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,var1]))
var2 <- rownames(PCA_contrb2)[1:length(impNums)][2]       #Shot put
plot(olympic_p[,var2], PCA.olympic$ind$coord[,2], xlab=var2, ylab='score')
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,var2]))
var3 <- rownames(PCA_contrb2)[1:length(impNums)][3]  
plot(olympic_p[,var3], PCA.olympic$ind$coord[,2], xlab=var3, ylab='score')
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,var3]))
var4 <- rownames(PCA_contrb2)[1:length(impNums)][4]
plot(olympic_p[,var4], PCA.olympic$ind$coord[,2], xlab=var4, ylab='score')
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,var4]))

#At the end of this first approach, we can divide the factorial plan into four parts: fast and strong athletes (like Sebrle), 
#slow athletes (like Casarsa), fast but weak athletes (like Warners) and slow and weak (relatively speaking!) athletes (like Lorenzo).
###################################################################


###################################################################
############## Supplementary variables #############

#FactoMineR ONLY!!!!
olympic_comb <- cbind(olympic$tab, olympic$score, rank(-olympic$score))
colnames(olympic_comb) <- c(colnames(olympic$tab), 'score', 'rank')

#Adding supplementary continuous variables (Rank and Score)
par(mfrow=c(1,2))
PCA.olympic_supp <- PCA(olympic_comb[,1:12], scale.unit=T, ncp=5,
               quanti.sup=c(11,12), graph=T)
#Winners are those who score the most and therefore have the lowest rank
PCA_contrb2 <- data.frame(PCA.olympic_supp$var$contrib)
PCA_contrb2 <- PCA_contrb2[order(-PCA_contrb2$Dim.1),]
#Variables most linked to number of points
pointVars <- rownames(PCA_contrb2)[1:4]
pointVars
insigVars <- rownames(PCA_contrb2[order(PCA_contrb2$Dim.1),])[1:2]
insigVars

#Qualitative supplementary variables
#....

#Color individuals according to the categories' center of gravity
# plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13)
#FactoMineR
data(decathlon)
par(mfrow = c(1,2))
res.PCA <- PCA(decathlon, scale.unit=T, ncp=5, quanti.sup=c(11:12), quali.sup=13, graph=T)
par(mfrow = c(1,1))
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T)

#princomp
res.pca <- princomp(decathlon[,1:10], cor=T)
plot(-res.pca$scores[,1], res.pca$scores[,2],  pch=19, cex=0.7,  col=ifelse(decathlon[,13]==catNames[1],'black','red'))
catNames <- names(table(decathlon[,13]))
text(-res.pca$scores[,1], res.pca$scores[,2], rownames(res.pca$scores), cex=0.8, pos=4, col=ifelse(decathlon[,13]==catNames[1],'black','red'))
abline(h=0, lty=2)
abline(v=0, lty=2)
legend("topleft", c('Decastar', 'OlympicG'), text.col=c("black","red"), cex=0.75)

###################################################################


###################################################################
#The package FactoMineR provides the function estim_ncp to calculate the best number
#of dimensions. In this function generalized cross validation approximation (GCV) and
#the smoothing method (Smooth) are implemented.

##ONLY WITH FACTOMINER
estim_ncp(olympic_p, ncp.min=0, ncp.max=NULL, scale=TRUE, method="Smooth")
estim_ncp(olympic_p, ncp.min=0, ncp.max=NULL, scale=TRUE, method="GCV")
###################################################################


###################################################################

#Selecting individuals with specific features to plot
#CAN ONLY BE DONE WITH FACTOMINER
#Plot individuals who have a cos2>0.7
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T)
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7')
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7', unselect=0)
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7', unselect=1)
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7', unselect='grey70')

plot.PCA(res.PCA, habillage=13, cex=0.8, shadow=T, select='cos2 5')
plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='contrib 5')     #Those with the largest contributions are the most extreme individuals
          
###################################################################


###################################################################

          #Graphing variables
          plot(res.PCA, choix='var', shadow=T)
          plot(res.PCA, choix='var', shadow=T, select="contrib 5")
          plot(res.PCA, choix='var', shadow=T, select="cos2 5")
          
          #Ranking variable contribution
          for (i in 1:dim(res.pca$var$contrib)[1]) {
            dimsum[i]<- sum(res.pca$var$contrib[i,1:2])
          }
          sort(dimsum, decreasing=T)
          
          for (i in 1:dim(res.pca$var$cos2)[1]) {
            dimsum[i]<- sum(res.pca$var$cos2[i,1:2])
          }
          sort(dimsum, decreasing=T)
          
          #When looking at the points representing "Decastar" and "Olympic Games", we notice that this last one has higher coordinates on the 
          #first axis than the first one. This shows an evolution in the performances of the athletes. All the athletes who participated to the two 
          #competitions have then slightly better results for the Olympic Games. 
          #However, there is no difference between the points "Decastar" and "Olympic Games" for the second axis. This means that the athletes have 
          #improved their performance but did not change profile (except for Zsivoczky who went from slow and strong during the Decastar to fast and 
          #weak during the Olympic Games).
          
          #We can see that the points which represent the same individual are in the same direction. For example, Sebrle got good results in both 
          #competition but the point which represents his performance during the O.G. is more extreme. Sebrle got more points during the O.G. than during the Decastar.
          
          
          #These tables give the correlation coefficient and the p-value of the variables which are significantly correlated to the principal dimensions. 
          #Both active and supplementary variables whose p-value is smaller than 0.05 appear.
          dimdesc(res.pca, axes=c(1,2))
          #The tables of the description of the two principal axes show that the variables "Points" and "Long.jump" are the most correlated to the first dimension 
          #and "Discus" is the most correlated to the second one. This confirms our first interpretation that atheletes who participate in O.G. are better than those
          #who participate in Decastar
          
          
          #To see whether the categories of the supplementary variable are significantly different from each other, we can draw confidence ellipses around them.
          concat <- cbind.data.frame(decathlon[,13], res.pca$ind$coord)
          ellipse.coord <- coord.ellipse(concat, bary=T)
          plot.PCA(res.pca, habillage=13, ellipse=ellipse.coord, cex=0.8, axes=c(1,3))
          
          
          
          
          
          
          
          
          
          #http://pbil.univ-lyon1.fr/ade4/ade4-html/olympic.html
          par(mfrow=c(2,2))
          pc.bp <- barplot(res.pca$eig[,1])
          lines(x=pc.bp, y=res.pca$eig[,1])
          points(x=pc.bp, y=res.pca$eig[,1])
          
          plot(res.pca, choix='var', shadow=T)
          
          #library(ade4)
          # plot(decathlon$Points, res.pca$dist)
          plot(decathlon$Points, res.pca$ind$coord[,1])
          abline(lm(res.pca$ind$coord[,1]~decathlon$Points))
          
          s.label(res.pca$ind$coord, clab=0.5)
          s.arrow(2*res.pca$var$coord, add.p=T)
          
          par(mfrow=c(1,1))
          plot(res.pca$eig[,1], decathlon$Points, xlab="Dim1", pch=23, bg='red', cex=2)
