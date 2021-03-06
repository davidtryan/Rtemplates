#' @title Plot data.frame to PDF
#' @description Plots every column of a data.frame as an individual plot (one plot per page)
#'  in a PDF file.  
#' 
#' @param df data.frame to plot.
#' @param file string - name of the PDF that will be created
#' @param wordy - boolean \code{TRUE} or \code{FALSE}.  Sequentially prints status of each chart to the console.
#' Could be useful for large data.frames.
#'
#' @return PDF file of plots
#' @export
#' 
#' @examples
#' \dontrun{
#' plotdf(df=mtcars, file='mtcars_plots.pdf')
#' }

relationPlots <- function(df, df_T, file='output.pdf', wordy=F){
  pdf(file)
  df_T_data <- df[,df_T]
  df <- df[,!(names(df)) %in% df_T]  
  
  prevSplit <- 0
  
  for(i in 1:ncol(df)){
    if(wordy==T) print(i)
    if((class(df[,i]) %in% c('numeric', 'integer')) & length(unique(df[,i]))>15) { 
      if (i>1) {close.screen(all = TRUE)} 
      split.screen(c(2,1))
      plotScat(df[,i], df_T_data, vn=names(df)[i], tn=df_T, singPlot=0)
      plotNum(df[,i], vn=names(df)[i], singPlot=0, tn=df_T)
      prevSplit <- 1
    } else if ((class(df[,i]) %in% c('numeric', 'integer')) & length(unique(df[,i]))<=15) { 
      if (i>1) {close.screen(all = TRUE)} 
      split.screen(c(2,1))
      plotScat(df[,i], df_T_data, vn=names(df)[i], tn=df_T, singPlot=0)
      plotChar(df[,i], vn=names(df)[i], singPlot=0, tn=df_T)
      prevSplit <- 1
    } else { 
      if (i>0) {close.screen(all = TRUE)} 
      coord <- matrix(c(c(0, 1, 0, 0.6), c(0, 1, 0.6, 1)), byrow=T, ncol=4)
      split.screen(coord)
      plotChar(df[,i], vn=names(df)[i], singPlot=1, tn=df_T)
      prevSplit <- 0
    }
  }
  graphics.off()
  print(paste0('charts saved in ', getwd(), '/', file))
}


##################################################################
## Helper functions for plotdf (not included in Rsenal package)
##################################################################

plotScat <- function(x, y, vn, tn, singPlot) {
  if (dim(table(x))<=4 & dim(table(y))==2) {
    split.screen(c(2,2), screen=1)
    par(mar=c(5,4,4,2))
    screen(3)
    par(mar=c(2,2,2,1))
    plot(x, y, xlab=vn, ylab=tn)
    screen(4)
    par(mar=c(2,2,2,1))
    jitterScat(x,y,vn,tn)
    grid()
    screen(5)
    par(mar=c(2,2,2,1))
    stackedBar(x,y,vn,tn)
    screen(6)
    par(mar=c(2,2,2,1))
    sideBar(x,y,vn,tn)
    mtext(sprintf("Plots of %s vs %s", vn, tn), outer=T, cex=1)
  } else if (dim(table(x))<=15 & dim(table(y))==2) {
    split.screen(c(1,2), screen=1)
    par(mar=c(3,2,2,1))
    screen(3)
    par(mar=c(3,2,2,1))
    plot(x, y, xlab=vn, ylab=tn)
    screen(4)
    par(mar=c(3,2,2,1))
    stackedBar(x,y,vn,tn)
    legend("topright", rownames(table(y, x)), pch=15, col=c("darkblue","red"), cex=0.75, bty='n')
  } else {
    screen(1)
    par(mar=c(4,4,4,1))
    if (singPlot==0) {
      plot(x,y,xlab=vn,ylab=tn)
    } else {
      plot(x,y,xlab=vn,ylab=tn, main=sprintf("Scatterplot of %s vs %s"))
    }
  }
}

stackedBar <- function(x, y, vn, tn) {
  barplot(table(y,x), xlab=vn, ylab=tn, col=c("darkblue","red"))
}

sideBar <- function(x,y,vn,tn) {
  barplot(table(y,x), beside=T, col=c("darkblue","red"), xlab=vn, ylab=tn)
  legend("topright", rownames(table(y, x)), pch=15, col=c("darkblue","red"), cex=0.75, bty='n')
}

jitterScat <- function(x,y,vn,tn) {
  plot(jitter(x), jitter(y), xlab=vn, ylab=tn)
}


plotNum <- function(x, singPlot, tn, ...) {
  if (singPlot==0) {
    screen(2)
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    p2<-hist(x, main=ptitle, breaks=30, xlab=ptitle, col='grey')
    rug(x, col='red')
    
    #######################
    op <- par(fig = c(0.55,0.95,0.125,0.525), new = TRUE)
    par(mfrow=c(1,1),mar=c(5,4,4,2))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    mtext(sprintf("Plots of %s vs %s", ptitle, tn), outer=F, cex=1.0, line=3, side=3, font=2)
    par(op)
    ########################
    
    op <- par(fig = c(0.55,0.95,0.125,0.525), new = TRUE)
    
    p1<-plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='')
    
    text(x=0.015, y=0.8, label='type:', pos=4, font=2, cex=0.4)
    text(x=0.015, y=0.7, label='# of unique values:', pos=4, font=2, cex=0.4)
    text(x=0.015, y=0.6, label='% NA:', pos=4, font=2, cex=0.4)
    text(x=0.015, y=0.5, label='min:', pos=4, font=2, cex=0.4)
    text(x=0.015, y=0.4, label='median:', pos=4, font=2, cex=0.4)
    text(x=0.015, y=0.3, label='mean:', pos=4, font=2, cex=0.4)
    text(x=0.015, y=0.2, label='max:', pos=4, font=2, cex=0.4)
    
    text(x=0.5, y=0.8, label=class(x), pos=4, cex=0.4)
    text(x=0.5, y=0.7, label=length(unique(x)), pos=4, cex=0.4)
    text(x=0.5, y=0.6, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4, cex=0.4)
    text(x=0.5, y=0.5, label=min(x, na.rm=T), pos=4, cex=0.4)
    text(x=0.5, y=0.4, label=median(x, na.rm=T), pos=4, cex=0.4)
    text(x=0.5, y=0.3, label=mean(x, na.rm=T), pos=4, cex=0.4)
    text(x=0.5, y=0.2, label=max(x, na.rm=T), pos=4, cex=0.4)
    
    box(lwd=1)
    par(op)
    par(mfrow=c(1,1),mar=c(5,4,4,2))
    
  } else {
    par(mfrow=c(3, 2))
    par(oma=c(1,2,2,2))
    layout(matrix(c(2,1,2,2,2,2), 3, 2, byrow=T), widths=c(1,1), heights=c(1,1,1))
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    
    p1<-plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='')
    
    text(x=0.2, y=0.8, label='type:', pos=4, font=2)
    text(x=0.2, y=0.7, label='# of unique values:', pos=4, font=2)
    text(x=0.2, y=0.6, label='% NA:', pos=4, font=2)
    text(x=0.2, y=0.5, label='min:', pos=4, font=2)
    text(x=0.2, y=0.4, label='median:', pos=4, font=2)
    text(x=0.2, y=0.3, label='mean:', pos=4, font=2)
    text(x=0.2, y=0.2, label='max:', pos=4, font=2)
    
    text(x=0.4, y=0.8, label=class(x), pos=4)
    text(x=0.4, y=0.7, label=length(unique(x)), pos=4)
    text(x=0.4, y=0.6, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4)
    text(x=0.4, y=0.5, label=min(x, na.rm=T), pos=4)
    text(x=0.4, y=0.4, label=median(x, na.rm=T), pos=4)
    text(x=0.4, y=0.3, label=mean(x, na.rm=T), pos=4)
    text(x=0.4, y=0.2, label=max(x, na.rm=T), pos=4)
    
    p2<-hist(x, main=ptitle, breaks=30, xlab=ptitle, col='grey')
    rug(x, col='red')
  }
  
  return(list(p1, p2))
}


plotChar <- function(x, singPlot, tn, ...) {
  if (singPlot==0) { 
    screen(2) 
    
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    tab <- sort(table(x), decreasing=T)[1:min(length(unique(x)),50)]
    tabmiss <- round(sum(tab, na.rm=T)/length(x)*100, 1)
    
    p2 <- barplot(tab, las=2, cex.names=0.6, col='dodgerblue', xlab = ptitle,
                  main=paste(ptitle, ': ', tabmiss, '% of data shown', sep=''))
    
    #######################
    op <- par(fig = c(0.55,0.95,0.125,0.525), new = TRUE)
    par(mfrow=c(1,1),mar=c(5,4,4,2))
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    mtext(sprintf("Plots of %s vs %s", ptitle, tn), outer=F, cex=1.0, line=3, side=3, font=2)
    par(op)
    ########################
    
    op <- par(fig = c(0.6,0.95,0.225,0.525), new = TRUE)
    
    p1 <- plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='')
    text(x=0, y=0.8, label='type:', pos=4, font=2, cex=0.4)
    text(x=0, y=0.5, label='# of unique values:', pos=4, font=2, cex=0.4)
    text(x=0, y=0.2, label='% NA:', pos=4, font=2, cex=0.4)
    
    text(x=0.65, y=0.8, label=class(x), pos=4, cex=0.4)
    text(x=0.65, y=0.5, label=length(unique(x)), pos=4, cex=0.4)
    text(x=0.65, y=0.2, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4, cex=0.4)
    
    box(lwd=1)
    par(op)
    par(mfrow=c(1,1),mar=c(5,4,4,2))
    
  } else {
    screen(2)
    par(mar=c(5.1,4.1,4.1,2.1))
    
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    tab <- sort(table(x), decreasing=T)[1:min(length(unique(x)),50)]
    tabmiss <- round(sum(tab, na.rm=T)/length(x)*100, 1)
    
    p1 <- plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='', main=ptitle)
    text(x=0.35, y=0.8, label='type:', pos=4, font=2, cex=0.65)
    text(x=0.35, y=0.6, label='# of unique values:', pos=4, font=2, cex=0.65)
    text(x=0.35, y=0.4, label='% NA:', pos=4, font=2, cex=0.65)
    
    text(x=0.55, y=0.8, label=class(x), pos=4, cex=0.65)
    text(x=0.55, y=0.6, label=length(unique(x)), pos=4, cex=0.65)
    text(x=0.55, y=0.4, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4, cex=0.65)
    
    screen(1)
    par(mar=c(7,4,0.75,1))
    p2 <- barplot(tab, las=2, cex.names=0.4, col='dodgerblue')
    mtext(paste(ptitle, ': ', tabmiss, '% of data shown', sep=''), outer=F, cex=1.25, line=1, side=3, font=2)
    par(mar=c(5.1,4.1,4.1,2.1))    
  }
  
  return(list(p1, p2))
}

