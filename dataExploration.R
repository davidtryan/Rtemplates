

## Logistic Regression Investigations

dataExploration <- function(d, tv) {
  
  #Dimension data
  numVar <- dim(d)[2]
  numObs <- dim(d)[1]
  #numObs <- nrow(d)
  
  View(head(d))
  View(tail(d, n=10))
  
  varNames <- names(data)
  
  ## SUmmary statistics for target variable
  mean(data[,tv])
  mean(data[,tv], na.rm=T)
  sd(data[,tv], na.rm=T)
  
  
  
  ############################################
  ########## SUMMARY STATISTICS ##############
  ############################################  
  
  a1 <- data.frame(sapply(d, class))
  a1$var <- rownames(a1)
  
  a2 <- data.frame(sapply(d, function(x) length(unique(x))))
  a2$var <- rownames(a2)
  
  a3 <- data.frame(sapply(d, function(x) sum(is.na(x))))
  a3$var <- rownames(a3)

  a4 <- data.frame(sapply(d, function(x) if (class(x)!="character") {min(x, na.rm=T)} else {"NA"}))
  a4$var <- rownames(a4)
  
  a5 <- data.frame(sapply(d, function(x) if (class(x)!="character") {round(mean(x, na.rm=T),2)} else {"NA"}))
  a5$var <- rownames(a5)
  
  a6 <- data.frame(sapply(d, function(x) if (class(x)!="character") {round(max(x, na.rm=T),2)} else {"NA"}))
  a6$var <- rownames(a6)
  
  a7 <- data.frame(sapply(d, function(x) if (class(x)!="character") {round(sd(x, na.rm=T),2)} else {"NA"}))
  a7$var <- rownames(a7)
  
  a <- merge (a1, a2, by='var')
  a <- merge (a, a3, by='var')
  a <- merge (a, a4, by='var')
  a <- merge (a, a5, by='var')
  a <- merge (a, a6, by='var')
  a <- merge (a, a7, by='var')
  names(a) <- c('var', 'class', 'unique', 'missing', 'min', 'mean', 'max', 'std dev')
  
  
  
  ############################################
  ############# BASIC PLOTS ##################
  ############################################  
  
  
  ###########
  #### 1 ####
  ###########
  
    #Number of variables without missing data
  sum(a$missing==0)
  
  #Variables with missing data
  aMiss <- a$missing[a$missing!=0]
  
  #Percentage of missing data range
  min(aMiss/numObs)*100
  max(aMiss/numObs)*100
  
  #Number of variables with more than 80% NAs (likely difficult to use)
  sum((a$missing/numObs)>0.8)
  sort(round(aMiss/numObs,2), decreasing=T)
  
  #Stem and leaf plot of NAs in variables
  stem(sort(round(a$missing/numObs,2), decreasing=T))
  #Distribution of NAs in variables
  hist(a$missing/numObs, xlab='% missing data', breaks=30, main='Distribution of NAs in variables')
  rug(a$missing/numObs, col='red')
  
  #Stem and leaf plot of NAs in variables (without variables that contain no missing data)
  stem(sort(round(aMiss/numObs,2), decreasing=T))
  #Distribution of NAs in variables without variables that contain no missing data
  hist(aMiss/numObs, xlab='% missing data', breaks=30, main='Distribution of NAs in variables \n (excluding variables without missing data)')
  rug(aMiss/numObs, col='red')
  
  
  ###########
  #### 2 ####
  ###########
  
  #Plots of each variable univariately along with plots relating variable to target (basic plots and summary stats)
  source("C:/Users/davidryan/Documents/DTR_Files/RExamples/RTemplates/relationPlots_2.R")
  relationPlots_2(d, tv, file="R/OutputFiles/relationtest.pdf", wordy=F)
  
  
  ###########
  #### 3 ####
  ###########
  
  #Numeric data pairs plot
  #Plot pairs in the data if number of variables range between 2 and 20
  if (numVar > 2 & numVar <= 20) {
    pairs(data_numCC,pch=19)
  }

  
  ###########
  #### 4 ####
  ###########
  
  #Plot side-by-side boxplots of the variables
  #http://rud.is/b/2014/09/10/r-version-of-an-exploratory-technique-for-visualizing-the-distributions-of-100-variables/
  par(mfrow=c(1,1))
  library(reshape2)
  library(ggplot2)

  ###NORMALIZE DATA
  singValCol <- c()
  dNum <- d[,a$var[a$class!='character']]
  
  dNum_norm <- dNum
  for (i in 1:ncol(dNum)) {
    tmp <- dNum[,i]
    if (min(tmp, na.rm=T)==max(tmp, na.rm=T)) {
      dNum_norm[,i] <- dNum[,i]
      singValCol <- c(singValCol, names(dNum[i]))
    } else {
      dNum_norm[,i] <- (tmp - min(tmp, na.rm=T)) / (max(tmp, na.rm=T)-min(tmp, na.rm=T))
    }
  }

  ranks <- names(sort(rank(sapply(colnames(dNum_norm), function(x) {
    as.numeric(quantile(dNum_norm[,x], 0.75, na.rm=T))
  }))))
  
  dNum_norm_m <- melt(as.matrix(dNum_norm))
  
  dNum_norm_m$Var2 <- factor(dNum_norm_m$Var2, ranks)
  
  gg <- ggplot(dNum_norm_m, aes(x=Var2, y=value))
  gg <- gg + geom_boxplot(fill="steelblue", notch=F, outlier.size=1)
  gg <- gg + labs(x="")
  gg <- gg + theme_bw()
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(axis.text.x=element_text(angle=-45, hjust=0.001))
  gg <- gg + ggtitle("Numeric Variables Boxplots")
  gg



  ############################################
  ############# CORRELATION ##################
  ############################################  
  
  sort(table(a$unique[a$class=='numeric']))
  as.numeric(names(table(a$unique[a$class=='numeric'])))
    
  #Taking only truly numeric variables
  dNum_T <- a$var[a$class=='numeric' & a$unique>50 & a$missing==0]    #numeric variables without missing information
  dNum_T_miss <- a$var[a$class=='numeric' & a$unique>50 & a$missing>0]    #numeric variables with missing information
  
  #Correlation calculation
  mcor <- cor(d[,dNum_T], use='complete.obs', method='pearson')
  library('gplots',quietly=T)
  my_palette <- colorRampPalette(c('red', 'white', 'forestgreen'))(n=298)
  brks <- seq(-1,1,length.out=299)
  heatmap.2(mcor, trace='none', margins=c(10,10), breaks=brks, col=my_palette, cexRow=0.75, cexCol=0.75)
  hist(mcor, breaks=50, main="Distribution of Pairwise Correlations \n Variables Without Missing Data")
  m_mcor <- melt(mcor)
  m_mcor <- m_mcor[(m_mcor$value>0.7 & m_mcor$Var1!=m_mcor$Var2),]
  m_mcor <- m_mcor[order(-m_mcor$value),]
  
  #Correlation on numeric variables with missing information
  mcor_miss <- cor(d[,dNum_T_miss], use='complete.obs', method='pearson')  
  heatmap.2(mcor_miss, trace='none', margins=c(10,10), breaks=brks, col=my_palette, cexRow=0.75, cexCol=0.75)
  hist(mcor_miss, breaks=50, main="Distribution of Pairwise Correlations \n Variables With Missing Data")
  m_mcor_miss <- melt(mcor_miss)
  m_mcor_miss <- m_mcor_miss[(m_mcor_miss$value>0.7 & m_mcor_miss$Var1!=m_mcor_miss$Var2),]
  m_mcor_miss <- m_mcor_miss[order(-m_mcor_miss$value),]
  
  #Correlation on numeric variables (all inclusive)
  mcor_Tot <- cor(d[,c(dNum_T, dNum_T_miss)], use='complete.obs', method='pearson')  
  heatmap.2(mcor_Tot, trace='none', margins=c(10,10), breaks=brks, col=my_palette, cexRow=0.75, cexCol=0.75)
  hist(mcor_Tot, breaks=50, main="Distribution of Pairwise Correlations \n All True Numeric Variables")
  m_mcor_Tot <- melt(mcor_Tot)
  m_mcor_Tot <- m_mcor_Tot[(m_mcor_Tot$value>0.7 & m_mcor_Tot$Var1!=m_mcor_Tot$Var2),]
  m_mcor_Tot <- m_mcor_Tot[order(-m_mcor_Tot$value),]
  
  
  
  ############################################
  ################### PCA  ###################
  ############################################ 
  
  pcaFit <- princomp(d[,dNum_T], cor=T)
  summary(pcaFit)
  loadings(pcaFit)
  plot(pcaFit, type='l')

  #Correlation matrix of input variables with principal components
  cm <- cor(pcaFit$scores, d[,dNum_T])
  my_palette <- colorRampPalette(c('firebrick', 'white', 'forestgreen'))(n=298)
  brks <- seq(-1,1,length.out=299)
  heatmap.2(t(cm),trace='none',margins=c(10,10),breaks=brks, col=my_palette, cexRow=0.75, cexCol=0.75)
  
  
  
  
  loadm <- as(pcaFit$loadings, 'matrix')
  dim(loadm)
  
  n <- 10   #first 10 PCs
  head(loadm[1:10, 1:n])
  
  vars <-( pcaFit$sdev^2)/(sum(pcaFit$sdev^2))
  
  rs <- abs(loadm[,1:n]) %*% vars[1:n]
  par(mar=c(10,4.1,4.1,2.1))
  barplot(sort(rs, decreasing=T), las=2, cex.names=0.7, names=rownames(rs), ylab='sum weighted eigenvalues')
  par(mar=c(5.1,4.1,4.1,2.1))
  
  df <- data.frame(rs)
  df <- df[order(-df[,1]), ,drop=F]
  names(df) <- c('abs_eigenvector_sum')
  df$rank <- 1:length(rs)
  df <- df[c(2,1)]
  
  print(df)
  
  
  
  
  
  
  
  
  ####################################################################################################################################################
    #http://www.r-bloggers.com/introduction-to-feature-selection-for-bioinformaticians-using-r-correlation-matrix-filters-pca-backward-selection/
    
    #Use CORRELATION MATRIX FILTERS to remove features with >0.70 of correlation
    install.packages("corrplot")
    library(corrplot)

#     dNum_T <- a$var[a$class=='numeric' & a$unique>50 & a$missing==0]
    dNum_T.scale <- scale(d[,dNum_T], center=TRUE, scale=TRUE)
#     dNum_T.scale <- scale(dNum_T[,!(names(dNum_T)) %in% c("Defunded")], center=TRUE, scale=TRUE)
  
    corMatMy <- cor(dNum_T.scale, use='complete.obs', method='pearson')
  
    par(mar = c(5,4,4,2)-0.5)
    corrplot(corMatMy, order="hclust")

    install.packages("caret")
    library(caret)
    highlyCor <- findCorrelation(corMatMy, 0.70)
    datMyFiltered.scale <- dNum_T.scale[,-highlyCor]
    corMatMy <- cor(datMyFiltered.scale)
    corrplot(corMatMy, order="hclust")














    install.packages("FactoMineR")
    require(FactoMineR)

    pca <- PCA(d[,dNum_T], scale.unit=TRUE, ncp=5, graph=T)
#     pca2 <- PCA(data_numCC_norm[,!(names(data_numCC_norm)) %in% singValCol], scale.unit=TRUE, ncp=5, graph=T)
#     pca3 <- PCA(data_numCC_norm[,!(names(data_numCC_norm)) %in% c(singValCol,"Defunded")], scale.unit=TRUE, ncp=5, graph=T)
    dimdesc(pca)


    library(caret)

#     data_features <- as.matrix(data_numCC_norm[,!(names(data_numCC_norm)) %in% c(singValCol,"Defunded")])
    data_features <- as.matrix(d[,dNum_T])
    data_class <- as.matrix(d[,c("Defunded")])

    dNum_T.scale <- scale(data_features, center=TRUE, scale=TRUE)

    inTrain <- createDataPartition(data_class, p=3/4, list=F)

    trainDescr <- data_features[inTrain,]
    testDescr <- data_features[-inTrain,]    

    trainClass <- data_class[inTrain]
    testClass <- data_class[-inTrain]

    descrCorr <- cor(trainDescr)
    highCorr <- findCorrelation(descrCorr, 0.70)
    trainDescr <- trainDescr[,-highCorr]
    testDescr <- testDescr[,-highCorr]
    
    install.packages("pROC")
    library("pROC", lib.loc="~/R/win-library/3.0")
    svmProfile <- rfe(x=trainDescr, y=trainClass, sizes=c(1:5), rfeControl=rfeControl(functions=caretFuncs, number=2), method="svmRadial", fit=F)

####################################################################################################################################################



  
################################################
########## ADDITIONAL OPTIONS ##################
################################################


#     install.packages("UsingR")3
#     library("UsingR", lib.loc="~/R/win-library/3.0")
#     simple.scatterplot(cars$speed, cars$dist)



# Ggobi package 


# #lattice and grid packages
# histogram(~Max.Price | Cylinders, data=Cars93)
# bwplot(~Max.Price | Cylinders, data=Cars93)
# xyplot(MPG.highway ~ Fuel.tank.capacity | Type, data=Cars93)



#http://www.mathworks.com/products/demos/statistics/mvplotdemo.html
# array of all the bivariate scatterplots between our five variables, along with a univariate histogram for each variable
#...points in each scatterplot are color-coded by...#


#Parallel coordinates plot
#....regular and standardized...
#...grayscale all and only show median and quartiles

  
}




