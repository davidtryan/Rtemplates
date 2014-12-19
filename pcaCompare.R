

install.packages('ade4')
library(ade4)
library(FactoMineR)

data(olympic)

par(mfrow=c(1,1))
#princomp
pca.olympic_nc <- princomp(olympic$tab)
pca.olympic <- princomp(olympic$tab, cor=T)
biplot(pca.olympic, pch=5)
abline(h=0, lty=2)
abline(v=0, lty=2)

par(mfrow=c(1,2))
#FactoMineR
PCA.olympic_nc <- PCA(olympic$tab)
PCA.olympic <- PCA(olympic$tab, scale.unit=T, ncp=5, graph=T)

#compare loadings
# pca.olympic$loadings
PCA.olympic.loadings <- sweep(PCA.olympic$var$coord, 2, sqrt(PCA.olympic$eig[1:5,1]), FUN="/")
# PCA.olympic.loadings
ld_compare <- cbind(-PCA.olympic.loadings[,1], pca.olympic$loadings[,1], (PCA.olympic.loadings[,1] + pca.olympic$loadings[,1]))
colnames(ld_compare) <- c('FactoMineR PCA (inv)', 'princomp PCA', 'diff')
ld_compare

par(mfrow=c(1,1))
plot(PCA.olympic)
points(-pca.olympic$scores[,1], pca.olympic$scores[,2], col='red', pch=0)


#Compare plots
par(mfrow=c(2,2))
plot(pca.olympic)
plot(pca.olympic, type='l')

PCA.bp <- barplot(PCA.olympic$eig[,1])
plot(x=PCA.bp, y=PCA.olympic$eig[,1])
lines(x=PCA.bp, y=PCA.olympic$eig[,1], type='l')


#Compare eigenvalues
PCA.olympic$eig
#First two dimensions resume 50% of the total inertia (i.e. total variance)

pca.eig <- pca.olympic$sdev^2
vars <- pca.olympic$sdev^2 
vars <- vars/sum(vars) 
pca.eig <- cbind(pca.eig, vars*100, cumsum(vars)*100)
colnames(pca.eig) <- colnames(PCA.olympic$eig)
pca.eig






























          biplot(pca.olympic)
          data.frame(olympic$tab[, '1500'], pca.olympic$scores[,1])
          
          par(mfrow=c(1,2))
          plot(olympic$tab[, '1500'], pca.olympic$scores[,1], pch=23, bg='red', cex=2)
          plot(olympic$tab[, 'jave'], pca.olympic$scores[,2], pch=23, bg='red', cex=2)
          
          #Overall score
          plot(pca.olympic$scores[,1], olympic$score, xlab='Comp. 1', pch=23, bg='red', cex=2)











  







          PCA.olympic$var
          #Long.jump and x100m are negatively correlated (faster x100m correlates with longer shot put)
          
          ###########VARIABLES BASED ON SPEED###################
          res.pca_contrb <- data.frame(res.pca$var$contrib)
          res.pca_contrb[order(-res.pca_contrb$Dim.1),]
#The first axis opposes athletes who are "good everywhere" like Karpov during the Olympic Games between those who are 
#"bad everywhere" like Bourguignon during the Decastar. This dimension is particularly linked to the variables of speed 
#and long jump which constitute a homogeneous group.
par(mfrow=c(2,2))
olympic_p <- olympic$tab
plot(olympic_p[,c('100')], PCA.olympic$ind$coord[,1])
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,c('100')]))

plot(olympic_p[,c('110')], PCA.olympic$ind$coord[,1])
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,c('110')]))

plot(olympic_p[,c('long')], PCA.olympic$ind$coord[,1])
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,c('long')]))

plot(olympic_p[,c('400')], PCA.olympic$ind$coord[,1])
abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,c('400')]))

#################################################################

par(mfrow=c(2,2))
plot(olympic_p[,c('100')], -pca.olympic$scores[,1])
abline(lm(-pca.olympic$scores[,1]~olympic_p[,c('100')]))

plot(olympic_p[,c('110')], -pca.olympic$scores[,1])
abline(lm(-pca.olympic$scores[,1]~olympic_p[,c('110')]))

plot(olympic_p[,c('long')], -pca.olympic$scores[,1])
abline(lm(-pca.olympic$scores[,1]~olympic_p[,c('long')]))

plot(olympic_p[,c('400')], -pca.olympic$scores[,1])
abline(lm(-pca.olympic$scores[,1]~olympic_p[,c('400')]))

#########################################################
###########VARIABLES BASED ON STRENGTH###################
res.pca_contrb2 <- data.frame(res.pca$var$contrib)
res.pca_contrb2[order(-res.pca_contrb2$Dim.2),]
#The first axis opposes athletes who are "good everywhere" like Karpov during the Olympic Games between those who are 
#"bad everywhere" like Bourguignon during the Decastar. This dimension is particularly linked to the variables of speed 
#and long jump which constitute a homogeneous group.
par(mfrow=c(2,2))
#Discus
plot(olympic_p[,c('disq')], PCA.olympic$ind$coord[,2])
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,c('disq')]))
#Shot put
plot(olympic_p[,c('poid')], PCA.olympic$ind$coord[,2])
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,c('poid')]))

plot(olympic_p[,c('400')], PCA.olympic$ind$coord[,2])
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,c('400')]))

plot(olympic_p[,c('1500')], PCA.olympic$ind$coord[,2])
abline(lm(PCA.olympic$ind$coord[,2]~olympic_p[,c('1500')]))

#################################################################

par(mfrow=c(2,2))
#Discus
plot(olympic_p[,c('disq')], pca.olympic$scores[,2])
abline(lm(pca.olympic$scores[,2]~olympic_p[,c('disq')]))
#Shotput
plot(olympic_p[,c('poid')], pca.olympic$scores[,2])
abline(lm(pca.olympic$scores[,2]~olympic_p[,c('poid')]))

plot(olympic_p[,c('400')], pca.olympic$scores[,2])
abline(lm(pca.olympic$scores[,2]~olympic_p[,c('400')]))

plot(olympic_p[,c('1500')], pca.olympic$scores[,2])
abline(lm(pca.olympic$scores[,2]~olympic_p[,c('1500')]))







#The second axis opposes athletes who are strong (variables "Discus" and "Shot.put") between those who are not. 
#The variables "Discus", "Shot.put" and "High.jump" are not much correlated to the variables "X100m", "X400m", "X110m.hurdle" 
#and "Long.jump". This means that strength is not much correlated to speed.

#At the end of this first approach, we can divide the factorial plan into four parts: fast and strong athletes (like Sebrle), 
#slow athletes (like Casarsa), fast but weak athletes (like Warners) and slow and weak (relatively speaking!) athletes (like Lorenzo).

####################################################
############## Supplementary variables #############
####################################################
olympic_comb <- cbind(olympic$tab, olympic$score)
colnames(olympic_comb) <- c(colnames(olympic$tab), 'score')

PCA.olympic <- PCA(olympic_comb[,1:11], scale.unit=T, ncp=5,
               quanti.sup=c(11), graph=T)
                    


          res.pca_contrb <- data.frame(res.pca$var$contrib)
          res.pca_contrb <- res.pca_contrb[order(-res.pca_contrb$Dim.1),]
          #The variables the most linked to the number of points are the variables which refer to the speed ("X100m", "X110m.hurdle", "X400m") 
          #and the long jump. On the contrary, "Pole-vault" and "X1500m" do not have a big influence on the number of points. Athletes who are 
          #strong for these two events are not favoured.
          
          
          #Add supllementary categorical variable (Completition)
          res.pca <- PCA(decathlon, scale.unit=T, ncp=5, quanti.sup=c(11:12), quali.sup=13, graph=T)
          #The categories' centres of gravity of this new variable appear on the graph of the individuals. They are located at the barycentre 
          #of the individuals who took them and they represent an average individual. 
          
          summary(res.pca)
          # summary(res.pca, nbelements=Inf, file='pcaresults.txt')
          
          #Color individuals according to the categories' center of gravity
          # plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13)
          plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T)
          
          #Plot individuals who have a cos2>0.7
          plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7')
          plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7', unselect=0)
          plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7', unselect=1)
          plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='cos2 0.7', unselect='grey70')
          
          plot.PCA(res.pca, habillage=13, cex=0.8, shadow=T, select='cos2 5')
          plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T, select='contrib 5')     #Those with the largest contributions are the most extreme individuals
          
          #Graphing variables
          plot(res.pca, choix='var', shadow=T)
          plot(res.pca, choix='var', shadow=T, select="contrib 5")
          plot(res.pca, choix='var', shadow=T, select="cos2 5")
          
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
