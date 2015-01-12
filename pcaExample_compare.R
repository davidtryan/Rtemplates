#PCA Example
##############################################################################################################################

#Creating light source data
recorders <- data.frame("X"=c(0,0,1,1), "Y"=c(0,1,1,0), row.names=c("A", "B", "C", "D"))
locs <- data.frame("X"=c(0.3, 0.5), "Y"=c(0.8, 0.2))

#Show diagram of recorders and light sources
plot(recorders, col='green', pch=0)
text(recorders[1:2,], labels=rownames(recorders)[1:2], pos=4)
text(recorders[3:4,], labels=rownames(recorders)[3:4], pos=2)
points(locs, col=c('yellow', 'red'))
text(locs, labels=colnames(locs), pos=4)

intensities <- data.frame("sine"=sin(0:99*(pi/10))+1.2, "cosine"=0.7*cos(0:99*(pi/15))+0.9)

#assume that the amount of light each recorder picks up decays exponentially with the
#distance from the light source to the recorder. Assume taht the lights combine
#linearly (sensors record a simple sum of the amount of light they receive from each source)

dists <- matrix(nrow=dim(locs)[1], ncol=dim(recorders)[1], dimnames=list(NULL, row.names(recorders)))

for (i in 1:dim(dists)[2]) {
  dists[,i]=sqrt((locs$X-recorders$X[i])^2
                 + (locs$Y-recorders$Y[i])^2)
}

set.seed(500)

recorded.data <- data.frame(jitter(as.matrix(intensities)%*%as.matrix(exp(-2*dists)),amount=0))

#Scatterplot of the data collected
plot(recorded.data, main="Recorded Data [2 source(s)]")     #reveals strong correlations among the variables (since recorders are recording the same data with different weights)
#Correlation matrix
round(cor(recorded.data), 2)      #A and D corr is strongest, followed by B and C (A and D receive from red ligth and less from yellow; B and C are opposite)
#Recording from D is essentially a copy of the data from A
#PCA Principle 1:  In general high correlation between variables is a telltale sign of high redundancy in the data

#time series plot of data as recorded at each sensor
plot.ts(recorded.data, main='as.ts(recorded data)')
#PCA Principle 2: The most important dynamics are the ones with the largest variance


##############################################################################################################################
##############################################################################################################################
##############################################################################################################################

#To Perform PCA
#PCA takes data and rewrites it in terms of new variables so that the new data has
#all the information from the original data but redundancy has been removed and it has been organized
#such that the most important variables are listed first
#NOTE: New data should have low or zero correlation between pairs of distinct variables
#NOTE: Sort new variables in descending variance to sort in order of descending importance

###############################################################
#Obtain data in a matrix
Xoriginal <- t(as.matrix(recorded.data))

#Center the data so taht the mean of each row is 0
rm <- rowMeans(Xoriginal)
X <- Xoriginal-matrix(rep(rm, dim(Xoriginal)[2]), nrow=dim(Xoriginal)[1])

#Calculate P
A <- X %*% t(X)
E <- eigen(A, TRUE)
P <- t(E$vectors)

#Find the enw data and standard deviations of the principal components
newdata <- P %*% X
sdev <- sqrt(diag((1/(dim(X)[2]-1)*P %*% A %*% t(P))))

#Print st devs
#Print PC loadings
cat("Standard deviations:\n",sdev)
E$vectors

barplot((sdev)^2)
barplot(sdev/sdev[1])


plot.ts(t(newdata)[,1:2])
#Plot intensities which make up the actual signal from inside the box
plot.ts(intensities)
#Plot the recorded data
plot.ts(recorded.data)
#New data is not a perfect replication of the actual signal sources, but instead better than the recorded data

#Since PC1 appears to upside down in previous plot, reverse the sign (negative of an eigenvector is the eigenvector)
plot.ts(cbind(-1*t(newdata)[,1], -1*t(newdata)[,2]))


#Data Management
#Since we only keep the first two PCs, we can just keep that data and the P rotation matrix.
#That cuts our dataset from 400 points (See recorded.data) to 216 points (P and newdata[1:2,]); this is a 46% savings in data!
#Reconstructing X (N=PX, P^-1=t(P), X = t(P)N)
Xreconst <- t(newdata) %*% P
max(t(X)-Xreconst)
min(t(X)-Xreconst)
Xreconst2 <- t(newdata)[,1:2] %*% P[1:2,]
max(t(X)-Xreconst2)
min(t(X)-Xreconst2)

plot.ts(recorded.data)
plot.ts(Xreconst)
plot.ts(Xreconst2)

###############################################################
pr <- prcomp(recorded.data)
pr
plot(pr)
#Look for a sharp drop in PC when deciding whether or not to reduce dataset dimensionality.
#Here, keep PC1 and 2 and discard other two (this reflects the fact that there were 2 light sources)

#Use this to choose a tolerance that will force prcomp() to keep just the components we want
#Anything between the height of the second and third bar will work (i.e. 0.1)
barplot(pr$sdev/pr$sdev[1])

#Run prcomp again but with the tolerance of 0.1 (this returns only PCs whose sdev is > 10% of the sdev of the first PC)
pr2 <- prcomp(recorded.data, tol=0.1)
pr2
#Plot new data in a time series
plot.ts(pr2$x)
#Plot intensities which make up the actual signal from inside the box
plot.ts(intensities)
#Plot the recorded data
plot.ts(recorded.data)
#New data is not a perfect replication of the actual signal sources, but instead better than the recorded data

#Since PC1 appears to upside down in previous plot, reverse the sign (negative of an eigenvector is the eigenvector)
plot.ts(cbind(-1*pr2$x[,1], pr2$x[,2]))


#If no components are discarded, a perfect reconstruction (od) can be made of the original data (recorded.data).  
#If the smallest components are discarded, the reconstruction (od2) is not exact, but is very good.
od <- pr$x %*% t(pr$rotation)
min(t(X)-od)
max(t(X)-od)
od2 <- pr2$x %*% t(pr2$rotation)
min(t(X)-od2)
max(t(X)-od2)

plot.ts(recorded.data)
plot.ts(od)
plot.ts(od2)

###############################################################
prcp <- princomp(recorded.data)
prcp
prcp$loadings[1:4,]
plot(prcp)
#Look for a sharp drop in PC when deciding whether or not to reduce dataset dimensionality.
#Here, keep PC1 and 2 and discard other two (this reflects the fact that there were 2 light sources)

#Use this to choose a tolerance that will force prcomp() to keep just the components we want
#Anything between the height of the second and third bar will work (i.e. 0.1)
barplot(prcp$sdev/pr$sdev[1])

#Run prcomp again but with the tolerance of 0.1
prcp2 <- princomp(recorded.data, tol=0.1)
prcp2
prcp2$loadings[1:4,]
#Plot new data in a time series
plot.ts(prcp2$scores[,1:2])
#Plot intensities which make up the actual signal from inside the box
plot.ts(intensities)
#Plot the recorded data
plot.ts(recorded.data)
plot.ts(cbind(-1*prcp2$scores[,1], -1*prcp2$scores[,2]))
#New data is not a perfect replication of the actual signal sources (intensities), but instead better than the recorded data

#If no components are discarded, a perfect reconstruction (od) can be made of the original data (recorded.data).  
#If the smallest components are discarded, the reconstruction (od2) on first two PCs is not exact, but is very good (quite close but smoothed out).
odcp <- prcp$scores %*% t(prcp$loadings)
min(t(X)-odcp)
max(t(X)-odcp)
#Code for reconstructing X (X=Nt(P) or X=pr$x%*%t(pr$rotation))
odcp2 <- prcp2$scores[,1:2] %*% t(prcp2$loadings[,1:2])
min(t(X)-odcp2)
max(t(X)-odcp2)

plot.ts(recorded.data)
plot.ts(odcp)
plot.ts(odcp2)
