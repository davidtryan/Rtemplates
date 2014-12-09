
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
  
  for(i in 1:ncol(df)){
    if(wordy==T) print(i)
    if((class(df[,i]) %in% c('numeric', 'integer')) & length(unique(df[,i]))>15) { 
      screen(1)
      plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
      split.screen(c(2,1))
      plotScat(df[,i], df_T_data, vn=names(df)[i], tn=df_T)
      plotNum(df[,i], vn=names(df)[i], singPlot=0)
      close.screen(all = TRUE)    # exit split-screen mode
    } else if ((class(df[,i]) %in% c('numeric', 'integer')) & length(unique(df[,i]))<=15) { 
      screen(1)
      plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
      split.screen(c(2,1), screen=1)
      plotScat(df[,i], df_T_data, vn=names(df)[i], tn=df_T)
      plotChar(df[,i], vn=names(df)[i], singPlot=0)
      close.screen(all = TRUE)    # exit split-screen mode
    } else { 
      plotChar(df[,i], vn=names(df)[i], singPlot=1)
    }
  }
  graphics.off()
  print(paste0('charts saved in ', getwd(), '/', file))
}



##################################################################
## Helper functions for plotdf (not included in Rsenal package)
##################################################################

plotScat <- function(x, y, vn, tn) {
#   par(mfrow = c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))
  if (dim(table(x))<=4 & dim(table(y))==2) {
    split.screen(c(2,2), screen=1)
#     par(mfrow = c(1,1), mar=c(5,4,4,2), oma=c(0,0,0,0))
    par(mar=c(5,4,4,2))
#     par(mfrow=c(2,2),mar=c(5,4,2,2), oma=c(0,0,3,0))
    screen(3)
    par(mar=c(3,2,2,1))
    plot(x, y, ylab=tn)
    screen(4)
    par(mar=c(3,2,2,1))
    jitterScat(x,y,vn,tn)
    grid()
    screen(5)
    par(mar=c(3,2,2,1))
    stackedBar(x,y,vn,tn)
    screen(6)
    par(mar=c(3,2,2,1))
    sideBar(x,y,vn,tn)
#     if (length(unique(df[,i]))<=15) {
#       plotChar(df[,i], vn=names(df)[i])
#     } else {
#       plotNum(df[,i], vn=names(df)[i])
#     }
    mtext(sprintf("Plots of %s vs %s", vn, tn), outer=T, cex=1)
  } else if (dim(table(x))<=15 & dim(table(y))==2) {
    split.screen(c(1,2), screen=1)
    par(mar=c(3,2,2,1))
#     par(mfrow=c(1,2),mar=c(5,4,2,2), oma=c(0,0,3,0))
    screen(3)
    par(mar=c(3,2,2,1))
    plot(x, y, xlab=vn, ylab=tn)
    screen(4)
    par(mar=c(3,2,2,1))
    stackedBar(x,y,vn,tn)
#     if (length(unique(df[,i]))<=15) {
#       plotChar(df[,i], vn=names(df)[i])
#     } else {
#       plotNum(df[,i], vn=names(df)[i])
#     }
    mtext(sprintf("Plots of %s vs %s", vn, tn), outer=TRUE, cex=1.5)
  } else {
    screen(1)
    par(mar=c(4,4,4,1))
#     par(mfrow=c(1,1))
    plot(x,y,xlab=vn,ylab=tn, main=sprintf("Scatterplot of %s vs %s", vn, tn))
#     if (length(unique(df[,i]))<=15) {
#       plotChar(df[,i], vn=names(df)[i])
#     } else {
#       plotNum(df[,i], vn=names(df)[i])
#     }
  }
}

stackedBar <- function(x, y, vn, tn) {
  barplot(table(y,x), xlab=vn, ylab=tn, col=c("darkblue","red"))
}

sideBar <- function(x,y,vn,tn) {
  barplot(table(y,x), beside=T, col=c("darkblue","red"), xlab=vn, ylab=tn)
  legend("topright", rownames(table(y, x)))
}

jitterScat <- function(x,y,vn,tn) {
  plot(jitter(x), jitter(y), xlab=vn, ylab=tn)
}




plotNum <- function(x, singPlot, ...) {
  if (singPlot==0) {
    screen(2)
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    p2<-hist(x, main=ptitle, breaks=30, xlab=ptitle, col='grey')
    rug(x, col='red')
    
    #######################
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
    ########################
    
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





## example
## plotNum(rnorm(1000)^2, vn='ssds')


plotChar <- function(x, singPlot, ...) {
  if (singPlot==0) { 
    screen(2) 
    par(mar=c(5,4,4,1))
    
    
#     par(mfrow=c(3, 2))
#     layout(matrix(c(1,1,2,2,2,2), 3, 2, byrow=T), widths=c(1,1), heights=c(1,1,1))
    
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    tab <- sort(table(x), decreasing=T)[1:min(length(unique(x)),50)]
    tabmiss <- round(sum(tab, na.rm=T)/length(x)*100, 1)

#     par(mar=c(10,4.1,4.1,2.1))
    p2 <- barplot(tab, las=2, cex.names=0.6, col='dodgerblue',
                  main=paste(ptitle, ': ', tabmiss, '% of data shown', sep=''))
#     par(mar=c(5.1,4.1,4.1,2.1))
    
    #######################
    op <- par(fig = c(0.6,0.9,0.225,0.525), new = TRUE)

    p1 <- plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='')
    text(x=0.01, y=0.8, label='type:', pos=4, font=2, cex=0.4)
    text(x=0.01, y=0.5, label='# of unique values:', pos=4, font=2, cex=0.4)
    text(x=0.01, y=0.4, label='% NA:', pos=4, font=2, cex=0.4)
    
    text(x=0.3, y=0.8, label=class(x), pos=4, cex=0.4)
    text(x=0.3, y=0.5, label=length(unique(x)), pos=4, cex=0.4)
    text(x=0.3, y=0.4, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4, cex=0.4)
    
    box(lwd=1)
    par(op)
    par(mfrow=c(1,1),mar=c(5,4,4,2))
    
  } else {
    par(mfrow=c(3, 2))
    layout(matrix(c(1,1,2,2,2,2), 3, 2, byrow=T), widths=c(1,1), heights=c(1,1,1))  
    
    ptitle <- ifelse(is.null(list(...)$vn), '', list(...)$vn)
    tab <- sort(table(x), decreasing=T)[1:min(length(unique(x)),50)]
    tabmiss <- round(sum(tab, na.rm=T)/length(x)*100, 1)
    
    p1 <- plot(0:1, 0:1, col='white', yaxt='n', ylab = '', xaxt='n', xlab='', main=ptitle)
    text(x=0.2, y=0.8, label='type:', pos=4, font=2)
    text(x=0.2, y=0.7, label='# of unique values:', pos=4, font=2)
    text(x=0.2, y=0.6, label='% NA:', pos=4, font=2)
    
    text(x=0.4, y=0.8, label=class(x), pos=4)
    text(x=0.4, y=0.7, label=length(unique(x)), pos=4)
    text(x=0.4, y=0.6, label=paste0(round(sum(is.na(x))/length(x)*100, 4), '%'), pos=4)
    
    par(mar=c(10,4.1,4.1,2.1))
    p2 <- barplot(tab, las=2, cex.names=0.6, col='dodgerblue',
                  main=paste(ptitle, ': ', tabmiss, '% of data shown', sep=''))
    par(mar=c(5.1,4.1,4.1,2.1))
  }
  
  return(list(p1, p2))
}