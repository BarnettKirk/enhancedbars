enhancedbarsAlpha <- function(dataframe,resp,treat,meandf,sedf,colpal=palette(), ylabel=NULL, maintitle=NULL,xlabel=NULL, 
                         yadj=-1.7,ylimit=range(resp[!is.na(resp) & is.finite(resp)]), ...){
  
  plot((1:nrow(meandf)),meandf[,2], ylim=ylimit, 
      cex.axis=1.2, cex=1.2, xlim=c(0.5,nrow(meandf)+0.5),type='n', ylab="",xaxt='n', xlab="",main=maintitle)

  for(i in 1:nrow(meandf)){
    rect(i-0.3,0,i+0.3,meandf[i,sapply(meandf, is.numeric)], col=alpha(colpal[i],alpha=0.2), ...)
  }
  
  for(i in 1:nrow(meandf)){
    points(jitter(rep(i,nrow(dataframe[treat==levels(treat)[i],]))), 
           jitter(resp[treat==levels(treat)[i]]), col=alpha(colpal[i],alpha=0.7), pch=19)
  }
  
  for(i in 1:nrow(meandf)){
    arrows(i, meandf[i,sapply(meandf, is.numeric)], i, meandf[i,sapply(meandf, is.numeric)]+sedf[i,sapply(meandf, is.numeric)], 
           length = 0.1, angle = 90)
    arrows(i, meandf[i,sapply(meandf, is.numeric)], i, meandf[i,sapply(meandf, is.numeric)]-sedf[i,sapply(meandf, is.numeric)], 
           length = 0.1, angle = 90)
  }
  axis(side=1, labels=xlabel, at=c(1:nrow(meandf)), cex=1.2)
  mtext(ylabel, side=2, cex=1.2, padj=-1.7)
  
}

enhancedbarsDen <- function(dataframe,resp,treat,meandf,sedf,colpal=palette(), ylabel=NULL, maintitle=NULL,xlabel=NULL, 
                              yadj=-1.7,ylimit=range(resp[!is.na(resp) & is.finite(resp)]), density=NULL, angle=NULL, ...){
  
  plot((1:length(meandf)),meandf, ylim=ylimit, 
       cex.axis=1.2, cex=1.2, xlim=c(0.5,length(meandf)+0.5),type='n', ylab="",xaxt='n', xlab="",main=maintitle)
  
  for(i in 1:length(meandf)){
    rect(i-0.3,0,i+0.3,meandf[i], col=colpal[i], density=density[i], angle=angle[i], ...)
  }
  
  for(i in 1:length(meandf)){
    points(jitter(rep(i,nrow(dataframe[treat==levels(treat)[i],]))), 
           jitter(resp[treat==levels(treat)[i]]), bg=alpha(colpal[i],alpha=0.7), pch=21)
  }
  
  for(i in 1:length(meandf)){
    arrows(i, meandf[i], i, meandf[i]+sedf[i], 
           length = 0.1, angle = 90)
    arrows(i, meandf[i], i, meandf[i]-sedf[i], 
           length = 0.1, angle = 90)
  }
  axis(side=1, labels=xlabel, at=c(1:length(meandf)), cex=1.2)
  mtext(ylabel, side=2, cex=1.2, padj=-1.7)
  
}