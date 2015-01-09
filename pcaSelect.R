## References
# http://statweb.stanford.edu/~jtaylo/courses/stats202/olympic.html
# http://www.statistik.tuwien.ac.at/public/filz/students/seminar/ws1011/hoffmann_ausarbeitung.pdf
# http://factominer.free.fr/classical-methods/principal-components-analysis.html
# http://www.inside-r.org/packages/cran/FactoMineR/docs/plot.PCA
# http://pbil.univ-lyon1.fr/ade4/ade4-html/olympic.html

## Installing and initializing
#princomp (ade4) package
if('ade4' %in% rownames(installed.packages()) == FALSE) {
  install.packages('ade4')
  library(ade4)
}
#FactoMineR package
if('FactoMineR' %in% rownames(installed.packages()) == FALSE) {
  install.packages('FactoMineR')
  library(FactoMineR)
}

## Loading data
data(olympic)
### Name: olympic
### Title: Olympic Decathlon
### Aliases: olympic
### Keywords: datasets
data(decathlon)
### Name: decathlon
### Title: Decathlon Data (Olympic and non-olympic)
### Aliases: decathlon
### Keywords: datasets
  
pcaSelect <- function(method, file='output.pdf', varData, resultData, example) {
  
  pdf(file)
  
  if (example=='YES') {
    data <- olympic$tab
    data_scores <- olympic$score
    data_d <- decathlon
  } else {
    data <- varData
    data_scores <- resultData
    data_d <- decathlon
  }  
  
  if (method=='princomp') {
    ###################################################################
    ###################################################################
    ##########################   princomp    ##########################
    ###################################################################
    ###################################################################
    
    ###################################################################
    ## Compare initial plots
    par(mfrow=c(1,1))
    par(mar=c(5,4,4,2))
  
    pca_nc <- princomp(data)     #no correlation selected (nc)
    pca <- princomp(data, cor=T)
    #Biplot gives a graphical summary of both 1) cases (athletes) in terms of scores and 2) the variables in terms of loadings
    biplot(pca, main="PCA Factor Map (princomp)")
    text(pca$scores[,1], pca$scores[,2], labels=rep("X", nrow(data)), cex= 0.7)
    # biplot(pca.olympic, pch=5, main="princomp Results", xlabs=rep("X", nrow(data))
    abline(h=0, lty=2)
    abline(v=0, lty=2)
    ###################################################################
    
    ###################################################################
    ## compare loadings
    pca$loadings
    ###################################################################
    
    ###################################################################
    ## Compare eigenvalues
    pca.eig <- pca$sdev^2
    vars <- pca$sdev^2 
    vars <- vars/sum(vars) 
    pca.eig <- cbind(pca.eig, vars*100, cumsum(vars)*100)
    colnames(pca.eig) <- c('eigenvalue', 'percentage of variance', 'cumulative percentage of variance')
    pca.eig
    ###################################################################
    
    ###################################################################
    ## Compare plots
    par(mfrow=c(1,2))
    plot(pca)
    plot(pca, type='l')
    
    par(mfrow=c(1,1))
    plot(-pca$scores[,1], pca$scores[,2], col='red', pch=0, ylab="Dim 2", xlab="Dim 1", main='Individual factor map (PCA)\n(princomp)')
    legend("topright", c('princomp'), pch=c(0), col=c("red"), cex=0.75, bty='n')
    text(-pca$scores[,1], pca$scores[,2], labels=rownames(pca$scores), pos=4)
    abline(h=0, lty=2)
    abline(v=0, lty=2)
    ###################################################################
    
    ###################################################################
    ## Compare Plots (extended - examples)
    # biplot(pca.olympic)
    # data.frame(data[, '1500'], pca.olympic$scores[,1])
    layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow=T), widths=c(1,1), heights=c(0.05,1))
    par(mar=c(0,0,0,0))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    text(1,0,sprintf('Example Plots vs. PCA Score'),font=2, cex=1.25)
    
    par(mar=c(5,4,0.1,2))
    plot(data[,floor(ncol(data)/2)], pca$scores[,1], pch=23, bg='red', cex=2, xlab=colnames(data)[floor(ncol(data)/2)], ylab='pca score')
    plot(data[,ncol(data)], pca$scores[,2], pch=23, bg='red', cex=2, xlab=colnames(data)[ncol(data)], ylab='pca score')
    #Overall score
    plot(pca$scores[,1], data_scores, xlab='Comp. 1', pch=23, bg='red', cex=2, ylab='pca score')
    #Since the first variable (PC) seems related to speed, we should not be surprised that an increase in this variable (PC) decreases the overall decathlon score
    
    par(mar=c(5,4,4,2))
    ###################################################################
    
    ###################################################################
    #The first axis opposes athletes who are "good everywhere" like Karpov during the Olympic Games between those who are 
    #"bad everywhere" like Bourguignon during the Decastar. This dimension is particularly linked to the variables of speed 
    #and long jump which constitute a homogeneous group.
    
    #Variables linked to first principal component
    impList <- sort(pca$loadings[,1])
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
    
    #Plot variables linked to first principal component to see how chanages in varaibles contribute to score
    layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow=T), widths=c(1,1), heights=c(0.1,1,1))
    par(mar=c(0.5,0,0.5,0))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    text(1,0,sprintf('Variables linked to PC1 (princomp)'),font=2, cex=1.25)
    
    par(mar=c(5,4,0.1,2))
#     olympic_p <- data
    
    var1 <- names(impNums)[1]
    plot(data[,var1], -pca$scores[,1], xlab=var1, ylab='score')
    abline(lm(-pca$scores[,1]~data[,var1]))
    var2 <- names(impNums)[2]
    plot(data[,var2], -pca$scores[,1], xlab=var2, ylab='score')
    abline(lm(-pca$scores[,1]~data[,var2]))
    var3 <- names(impNums)[3]
    plot(data[,var3], -pca$scores[,1], xlab=var3, ylab='score')
    abline(lm(-pca$scores[,1]~data[,var3]))
    #Overall score
    plot(pca$scores[,1], data_scores, xlab='Comp. 1', ylab='pca score')
    # plot(olympic_p[,c('400')], -pca.olympic$scores[,1], xlab='400', ylab='score')
    abData <- data.frame(cbind(data_scores, pca$scores[,1]))
    abline(lm(abData[,1]~(abData[,2])))
    
    par(mar=c(5,4,4,2))
    
    ###########VARIABLES BASED ON STRENGTH###################
    #The second axis opposes athletes who are strong (variables "Discus" and "Shot.put") between those who are not. 
    #The variables "Discus", "Shot.put" and "High.jump" are not much correlated to the variables "X100m", "X400m", "X110m.hurdle" 
    #and "Long.jump". This means that strength is not much correlated to speed.
    
    #Variables linked to second principal component
    impList <- sort(pca$loadings[,2])
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
    
    #Overall score
    par(mfrow=c(1,1))
    plot(pca$scores[,2], data_scores, xlab='Comp. 2', ylab='pca score', main='PC2 vs. PCA score (princomp)')
    abData <- data.frame(cbind(data_scores, pca$scores[,2]))
    abline(lm(abData[,1]~(abData[,2])))
    
    #princomp
    layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow=T), widths=c(1,1), heights=c(0.1,1,1))
    par(mar=c(0.5,0,0.5,0))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    text(1,0,sprintf('Variables linked to PC2 (princomp)'),font=2, cex=1.25)
    
    par(mar=c(5,4,0.1,2))
#     olympic_p <- data
    
    var1 <- names(impNums)[1]    #Discus 
    plot(data[,var1], pca$scores[,2], xlab=var1, ylab='score')
    abline(lm(pca$scores[,2]~data[,var1]))
    var2 <- names(impNums)[2]     #Shotput
    plot(data[,var2], pca$scores[,2], xlab=var2, ylab='score')
    abline(lm(pca$scores[,2]~data[,var2]))
    var3 <- names(impNums)[3]
    plot(data[,var3], pca$scores[,2], xlab=var3, ylab='score')
    abline(lm(pca$scores[,2]~data[,var3]))
    var4 <- names(impNums)[4]
    plot(data[,var4], pca$scores[,2], xlab=var4, ylab='score')
    abline(lm(pca$scores[,2]~data[,var4]))
    
    par(mar=c(5,4,4,2))
    
    #At the end of this first approach, we can divide the factorial plan into four parts: fast and strong athletes (like Sebrle), 
    #slow athletes (like Casarsa), fast but weak athletes (like Warners) and slow and weak (relatively speaking!) athletes (like Lorenzo).
    ###################################################################
    
    ###################################################################
    ############## Supplementary variables #############
    data_comb <- cbind(data, data_scores, rank(-data_scores))
    colnames(data_comb) <- c(colnames(data), 'score', 'rank')
    
    #Color individuals according to the categories' center of gravity
    par(mfrow=c(1,1))
    res.pca <- princomp(data_d[,1:10], cor=T)
    catNames <- names(table(data_d[,13]))
    plot(-res.pca$scores[,1], res.pca$scores[,2],  pch=19, cex=0.7,  col=ifelse(data_d[,13]==catNames[1],'black','red'),
         main='Individual factor map (PCA)\n(princomp)', xlab='Dim 1', ylab='Dim 2')
    text(-res.pca$scores[,1], res.pca$scores[,2], rownames(res.pca$scores), cex=0.8, pos=4, col=ifelse(data_d[,13]==catNames[1],'black','red'))
    abline(h=0, lty=2)
    abline(v=0, lty=2)
    legend("topleft", c('Decastar', 'OlympicG'), text.col=c("black","red"), cex=0.75)
    
    #Qualitative supplementary variables
    #....
    #When looking at the points representing "Decastar" and "Olympic Games", we notice that this last one has higher coordinates on the 
    #first axis than the first one. This shows an evolution in the performances of the athletes. All the athletes who participated to the two 
    #competitions have then slightly better results for the Olympic Games. 
    #However, there is no difference between the points "Decastar" and "Olympic Games" for the second axis. This means that the athletes have 
    #improved their performance but did not change profile (except for Zsivoczky who went from slow and strong during the Decastar to fast and 
    #weak during the Olympic Games).
    
    ###################################################################
    
    ###################################################################
    #Graphing variables
    par(mfrow=c(1,1))
    biplot(res.pca, main='primcomp Results', col=c('white', 'black'))
    abline(h=0, lty=2)
    abline(v=0, lty=2)
    ###################################################################
    
    ###################################################################
    ## Ranking variable contribution for combined first two PCs
    impList <- res.pca$loadings[,1:2]
    impList <- sort(abs(impList[,1])+abs(impList[,2]))
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
    ###################################################################
    
    ###################################################################
    ## Combined summary plots
    #http://pbil.univ-lyon1.fr/ade4/ade4-html/olympic.html
    par(mfrow=c(2,2))
    pc.bp <- plot(res.pca, main="")
    
    pca.var.coord <- res.pca$loadings[,1:5]
    pca.var.coord[,c(1,3,4,5)] <- -pca.var.coord[,c(1,3,4,5)]
    s.corcircle(pca.var.coord)
    
    plot(data_d$Points, -res.pca$scores[,1],
         xlab='Points', ylab='PCA score')
    abline(lm(-res.pca$scores[,1]~data_d$Points))
    
    pca.coord <- -res.pca$scores[,1:5]
    pca.coord[,2] <- -pca.coord[,2]
    s.label(pca.coord, clab=0.5)
    s.arrow(2*pca.var.coord, add.p=T) 
    ###################################################################
  
  } else if (method=='FactoMineR') {
  
    ###################################################################
    ###################################################################
    ########################   FactoMineR    ##########################
    ###################################################################
    ###################################################################
    
    ########################################################$###########
    ## Compare initial plots
    par(mfrow=c(1,2))
    PCA <- PCA(data, scale.unit=T, ncp=5, graph=T)
    ###################################################################
    
    ###################################################################
    ## compare loadings
    PCA.loadings <- sweep(PCA$var$coord, 2, sqrt(PCA$eig[1:5,1]), FUN="/")
    ###################################################################
    
    ###################################################################
    ## Compare eigenvalues
    PCA$eig
    ###################################################################
    
    ###################################################################
    ## Compare plots
    par(mfrow=c(1,2))
    PCA.bp <- barplot(PCA$eig[,1], main="PCA.olympic", xlab='', names.arg=rownames(PCA$eig), ylab='Variances')
    plot(x=PCA.bp, y=PCA$eig[,1], main="PCA.olympic", xlab='', xaxt='n', ylab='Variances')
    axis(1, at=PCA.bp, labels=rownames(PCA$eig), las=2)
    lines(x=PCA.bp, y=PCA$eig[,1], type='l')
    
    par(mfrow=c(1,1))
    plot(PCA)
    legend("topright", c('FactoMineR'), pch=c(19), col=c("black"), cex=0.75, bty='n')
    ###################################################################
    
    ###################################################################
    ## Compare Plots (extended - examples)
    # biplot(pca.olympic)
    # data.frame(data[, '1500'], pca.olympic$scores[,1])
    layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow=T), widths=c(1,1), heights=c(0.05,1))
    par(mar=c(0,0,0,0))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    text(1,0,sprintf('Example Plots vs. PCA Score'),font=2, cex=1.25)
    
    par(mar=c(5,4,0.1,2))
    plot(data[,floor(ncol(data)/2)], -PCA$ind$coord[,1], pch=23, bg='red', cex=2, xlab=colnames(data)[floor(ncol(data)/2)], ylab='PCA score')
    plot(data[,ncol(data)], PCA$ind$coord[,2], pch=23, bg='red', cex=2, xlab=colnames(data)[ncol(data)], ylab='PCA score')
    #Overall score
    plot(-PCA$ind$coord[,1], data_scores, xlab='Comp. 1', pch=23, bg='red', cex=2, ylab='PCA score')
    #Since the first variable (PC) seems related to speed, we should not be surprised that an increase in this variable (PC) decreases the overall decathlon score
    
    par(mar=c(5,4,4,2))
    ###################################################################
   
    ###################################################################
    #The first axis opposes athletes who are "good everywhere" like Karpov during the Olympic Games between those who are 
    #"bad everywhere" like Bourguignon during the Decastar. This dimension is particularly linked to the variables of speed 
    #and long jump which constitute a homogeneous group.
    
    #Variables linked to first principal component
    #FactoMineR
    PCA_contrb2 <- data.frame(PCA$var$contrib)
    PCA_contrb2 <- PCA_contrb2[order(-PCA_contrb2$Dim.1),]
    rownames(PCA_contrb2)[1:length(PCA_contrb2)]
    #These tables give the correlation coefficient and the p-value of the 
    #variables which are significantly correlated to the principal dimensions. 
    #Both active and supplementary variables whose p-value is smaller than 0.05 appear.dimdesc(PCA.olympic, axes=c(1,2)) 
    charCat <- dimdesc(PCA, axes=c(1,2))$Dim.1$quanti
    names(sort(-abs(charCat[,1]))[1:length(PCA_contrb2)])
    
    #Plot variables linked to first principal component to see how chanages in varaibles contribute to score
    layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow=T), widths=c(1,1), heights=c(0.1,1,1))
    par(mar=c(0.5,0,0.5,0))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    text(1,0,sprintf('Variables linked to PC1 (FactoMineR)'),font=2, cex=1.25)
    
    par(mar=c(5,4,0.1,2))
#     olympic_p <- data
    var1 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][1]
    plot(data[,var1], PCA$ind$coord[,1], xlab=var1, ylab='score')
    abline(lm(PCA$ind$coord[,1]~data[,var1]))
    var2 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][2]
    plot(data[,var2], PCA$ind$coord[,1], xlab=var2, ylab='score')
    abline(lm(PCA$ind$coord[,1]~data[,var2]))
    var3 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][3]
    plot(data[,var3], PCA$ind$coord[,1], xlab=var3, ylab='score')
    abline(lm(PCA$ind$coord[,1]~data[,var3]))
    #Overall score
    plot(-PCA$ind$coord[,1], data_scores, xlab='Comp. 1', ylab='PCA score')
    abData <- data.frame(cbind(data_scores, -PCA$ind$coord[,1]))
    abline(lm(abData[,1]~(abData[,2])))
    # plot(olympic_p[,c('400')], PCA.olympic$ind$coord[,1], xlab='400', ylab='score')
    # abline(lm(PCA.olympic$ind$coord[,1]~olympic_p[,c('400')]))
    
    par(mar=c(5,4,4,2))
    
    ###########VARIABLES BASED ON STRENGTH###################
    #The second axis opposes athletes who are strong (variables "Discus" and "Shot.put") between those who are not. 
    #The variables "Discus", "Shot.put" and "High.jump" are not much correlated to the variables "X100m", "X400m", "X110m.hurdle" 
    #and "Long.jump". This means that strength is not much correlated to speed.
    
    #Variables linked to second principal component
    PCA_contrb2 <- data.frame(PCA$var$contrib)
    PCA_contrb2 <- PCA_contrb2[order(-PCA_contrb2$Dim.2),]
    rownames(PCA_contrb2)[1:length(PCA_contrb2)]
    #These tables give the correlation coefficient and the p-value of the 
    #variables which are significantly correlated to the principal dimensions. 
    #Both active and supplementary variables whose p-value is smaller than 0.05 appear.dimdesc(PCA.olympic, axes=c(1,2)) 
    charCat <- dimdesc(PCA, axes=c(1,2))$Dim.2$quanti
    names(sort(-abs(charCat[,1]))[1:length(PCA_contrb2)])
    
    #Overall score
    par(mfrow=c(1,1))
    plot(PCA$ind$coord[,2], data_scores, xlab='Comp. 2', ylab='PCA score', main='PC2 vs. PCA score (princomp)')
    abData <- data.frame(cbind(data_scores, PCA$ind$coord[,2]))
    abline(lm(abData[,1]~(abData[,2])))
    
    layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow=T), widths=c(1,1), heights=c(0.1,1,1))
    par(mar=c(0.5,0,0.5,0))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    text(1,0,sprintf('Variables linked to PC2 (FactoMineR)'),font=2, cex=1.25)
    
    par(mar=c(5,4,0.1,2))
#     olympic_p <- data
    
    var1 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][1]     #Discus
    plot(data[,var1], PCA$ind$coord[,2], xlab=var1, ylab='score')
    abline(lm(PCA$ind$coord[,2]~data[,var1]))
    var2 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][2]       #Shot put
    plot(data[,var2], PCA$ind$coord[,2], xlab=var2, ylab='score')
    abline(lm(PCA$ind$coord[,2]~data[,var2]))
    var3 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][3]  
    plot(data[,var3], PCA$ind$coord[,2], xlab=var3, ylab='score')
    abline(lm(PCA$ind$coord[,2]~data[,var3]))
    var4 <- rownames(PCA_contrb2)[1:length(PCA_contrb2)][4]
    plot(data[,var4], PCA$ind$coord[,2], xlab=var4, ylab='score')
    abline(lm(PCA$ind$coord[,2]~data[,var4]))
    
    par(mar=c(5,4,4,2))
    
    #At the end of this first approach, we can divide the factorial plan into four parts: fast and strong athletes (like Sebrle), 
    #slow athletes (like Casarsa), fast but weak athletes (like Warners) and slow and weak (relatively speaking!) athletes (like Lorenzo).
    ###################################################################
    
    ###################################################################
    ############## Supplementary variables #############
    #FactoMineR ONLY!!!!
    data_comb <- cbind(data, data_scores, rank(-data_scores))
    colnames(data_comb) <- c(colnames(data), 'score', 'rank')
    
    #Adding supplementary continuous variables (Rank and Score)
    par(mfrow=c(1,2))
    PCA_supp <- PCA(data_comb[,1:12], scale.unit=T, ncp=5,
                            quanti.sup=c(11,12), graph=T)
    #Winners are those who score the most and therefore have the lowest rank
    PCA_contrb2 <- data.frame(PCA_supp$var$contrib)
    PCA_contrb2 <- PCA_contrb2[order(-PCA_contrb2$Dim.1),]
    #Variables most linked to number of points
    pointVars <- rownames(PCA_contrb2)[1:4]
    pointVars
    insigVars <- rownames(PCA_contrb2[order(PCA_contrb2$Dim.1),])[1:2]
    insigVars
    
    #Color individuals according to the categories' center of gravity
    # plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13)
    #FactoMineR
    par(mfrow = c(1,2))
    res.PCA <- PCA(data_d, scale.unit=T, ncp=5, quanti.sup=c(11:12), quali.sup=13, graph=T)
    par(mfrow = c(1,1))
    plot.PCA(res.PCA, axes=c(1, 2), choix="ind", habillage=13, cex=0.8, shadow=T)
      
    #Qualitative supplementary variables
    #....
    #To see whether the categories of the supplementary variable are significantly different from each other, we can draw confidence ellipses around them.
    concat <- cbind.data.frame(data_d[,13], res.PCA$ind$coord)
    ellipse.coord <- coord.ellipse(concat, bary=T)
    plot.PCA(res.PCA, habillage=13, ellipse=ellipse.coord, cex=0.8, axes=c(1,3))
    #When looking at the points representing "Decastar" and "Olympic Games", we notice that this last one has higher coordinates on the 
    #first axis than the first one. This shows an evolution in the performances of the athletes. All the athletes who participated to the two 
    #competitions have then slightly better results for the Olympic Games. 
    #However, there is no difference between the points "Decastar" and "Olympic Games" for the second axis. This means that the athletes have 
    #improved their performance but did not change profile (except for Zsivoczky who went from slow and strong during the Decastar to fast and 
    #weak during the Olympic Games).
    
    ###################################################################
    
    ###################################################################
    #The package FactoMineR provides the function estim_ncp to calculate the best number
    #of dimensions. In this function generalized cross validation approximation (GCV) and
    #the smoothing method (Smooth) are implemented.
    
    ##ONLY WITH FACTOMINER
    estim_ncp(data, ncp.min=0, ncp.max=NULL, scale=TRUE, method="Smooth")
    estim_ncp(data, ncp.min=0, ncp.max=NULL, scale=TRUE, method="GCV")
    ###################################################################
    
    ###################################################################
    #Selecting individuals with specific features to plot
    #CAN ONLY BE DONE WITH FACTOMINER
    #Plot individuals who have a cos2>0.7
    par(mfrow=c(2,2))
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
    par(mfrow=c(1,3))
    plot(res.PCA, choix='var', shadow=T)
    plot(res.PCA, choix='var', shadow=T, select="contrib 5")
    plot(res.PCA, choix='var', shadow=T, select="cos2 5")
    ###################################################################
    
    ###################################################################
    ## Ranking variable contribution for combined first two PCs
    #FactoMineR
    dimsum <- res.PCA$var$contrib[,1]          
    for (i in 1:dim(res.PCA$var$contrib)[1]) {
      dimsum[i]<- sum(res.PCA$var$contrib[i,1:2])
    }
    sort(dimsum, decreasing=T)
    
    for (i in 1:dim(res.PCA$var$cos2)[1]) {
      dimsum[i]<- sum(res.PCA$var$cos2[i,1:2])
    }
    sort(dimsum, decreasing=T)
    ###################################################################
    
    ###################################################################
    ## Combined summary plots
    #http://pbil.univ-lyon1.fr/ade4/ade4-html/olympic.html
    par(mfrow=c(2,2))
    pc.bp <- barplot(res.PCA$eig[,1], ylab='Variances')
    axis(1, at=pc.bp, labels=rownames(res.PCA$eig), las=2)
    lines(x=pc.bp, y=res.PCA$eig[,1])
    points(x=pc.bp, y=res.PCA$eig[,1])
    
    s.corcircle(res.PCA$var)
    
    plot(data_d$Points, res.PCA$ind$coord[,1],
         xlab="Points", ylab="PCA scores")
    abline(lm(res.PCA$ind$coord[,1]~data_d$Points))
    
    s.label(res.PCA$ind$coord, clab=0.5)
    s.arrow(2*res.PCA$var$coord, add.p=T)
    ###################################################################
    
  }
  
  graphics.off()
  print(paste0('output saved in ', getwd(), '/', file))
}
