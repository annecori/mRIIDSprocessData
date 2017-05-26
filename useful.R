install_github("reconhub/distcrete")
install_github("reconhub/projections")
install_github("reconhub/incidence")
library(projections)
library(incidence)
# function for projections and plot
################################################################
################################################################
get_projection <- function(T_proj , T_sim  , new_i  , res ){
  projection <- list()
  for (i in 1:length(T_proj)){
    # samples for R
    f <- which(res$R$dates %in% new_i$dates[T_proj[i]])
    shape <- res$R$`Mean(R)`[f]^2 / res$R$`Std(R)`[f]^2
    scale <- res$R$`Std(R)`[f]^2 / res$R$`Mean(R)`[f]
    R_samples <- rgamma(1e3,shape=shape,scale=scale)
    
    projection[[i]] <- project(x = new_i[1:T_proj[i]], R = R_samples , si = SI_Distr, n_sim = 1e3, n_days = T_sim,
                               R_fix_within = TRUE)
  }
  return(projection)
}

################################################################
################################################################
daily_to_weekly <- function(x){
  if(length(x)/7 != round(length(x)/7)) {
    warning('x is not a multiple of 7')
    stop()
  }
  out <- colSums(matrix(x,7,length(x)/7))
  return(out)
}

################################################################
################################################################
get_plot_weekly <- function(new_i, T_proj, projection, res ){
  
  # convert into weekly icidences
  
  weekly_incidence <- daily_to_weekly(new_i$counts[1:(floor(length(new_i$counts)/7)*7)])
  weekly_dates <- new_i$dates[seq(7, length(new_i$counts),by = 7)]
  weekly_proj <-list()
  for (i in 1:length(T_proj)){
    temp <- matrix(NA,T_sim/7,1e3)
    for (j in 1:1e3) {
      temp[,j] <- daily_to_weekly(projection[[i]][,j])
    }
    weekly_proj[[i]] <- temp
  }
  
  
  # plot(new_i[1:(T_proj+T_sim)], proj = project_1)
  
  col.scenarios <- c("Sc1"="#4f81bd","Sc2"="#9bbb59",
                     "Sc3"="#c0504d","Sc3"="#e67e00")
  col.scenarios.transp <- paste(col.scenarios,"55",sep="")
  
  
  ########################
  ## ploting  #########
  ######################
  par(oma=c(2,1.2,.5,0.1),mar=c(1.7,3,2,0.5))
  mat <- matrix(c(1,1,1,2,2,2,2,2,2,2,2,2),3,3,byrow = TRUE)
  layout(mat)
  
  plot(res$R$dates,res$R$`Mean(R)`,
       type='l',lwd=2,
       col=col.scenarios[1],
       xlim=c(min(new_i$dates),max(new_i$dates)),
       ylim=c(0,5),
       xlab='',ylab='',
       main='',
       xaxt  = 'n',yaxt='n',bty='n')
  xlab <- seq(min(new_i$dates),max(new_i$dates),length.out = 8)
  axis(side = 1, at = xlab,labels = xlab)
  axis(side = 2, at = c(0,1,2,3,4))
  
  mtext("R",2,3,outer=FALSE,las=0,adj=.5)
  
  #95%
  polygon(c(res$R$dates,rev(res$R$dates)),
          c(res$R$`Quantile.0.025(R)`,rev(res$R$`Quantile.0.975(R)`)),
          col = col.scenarios.transp[1],border=NA)
  # critical
  lines(res$R$dates,rep(1,length(res$R$dates)),lty=2,lwd=1.5,
        col='red')
  # time points
  for (i in 1:length(T_proj)){
    f <- which(res$R$dates %in% new_i$dates[T_proj[i]])
    lines(rep(res$R$dates[f],2),c(0,6.2),
          lty=5,col = col.scenarios[i],lwd=1.5)
  }

  plot(weekly_dates-3.5,weekly_incidence,
       pch=1,lwd=2,
       col='grey',
       xlim=c(min(new_i$dates),max(new_i$dates)),
       ylim=c(0,max(weekly_incidence)*1.1),
       xlab='',ylab='',
       main='',
       xaxt  = 'n',yaxt='n',bty='n')
  xlab <- seq(min(new_i$dates),max(new_i$dates),length.out = 8)
  axis(side = 1, at = xlab,labels = xlab)
  axis(side = 2, at = seq(0,800,length.out = 5))
  
  mtext("Incidence (weekly)",2,3,outer=FALSE,las=0,adj=.5)
  mtext("Date", 1, 2.5, outer=FALSE,padj=0,adj=.5) 
  for (j in 1:length(T_proj)){
    
    f<- ((T_proj[j]-time_window)/7+1):(T_proj[j]/7)
    points(weekly_dates[f]-3.5,
           weekly_incidence[f],
           pch=1,lwd=2,
           col=col.scenarios[j])
    f<- (T_proj[j]/7+1):(T_proj[j]/7+T_sim/7)
    points(weekly_dates[f]-3.5,
           weekly_incidence[f],
           pch=19,lwd=2,
           col=col.scenarios[j])
    
    #95%
    pred_95 <- apply(weekly_proj[[j]],1,quantile,c(.5,.025,.975))
    x<-weekly_dates[f]-3.5
    lines(x,pred_95[1,],
          type='l',lwd=2, col=col.scenarios[j])
    
    #95%
    polygon(c(x,rev(x)),
            c(pred_95[2,],rev(pred_95[3,])),
            col = col.scenarios.transp[j],border=NA)
    
    lines(rep(x[1]-time_window-3.5,2),c(0,max(weekly_incidence)*1.2),
          lty=3,col = col.scenarios[j],lwd=1.5)
    
    lines(rep(x[1]-3.5,2),c(0,max(weekly_incidence)*1.2),
          lty=5,col = col.scenarios[j],lwd=1.5)
  }
  
  # legend("topleft",c("Data","Fitted","IQR"),
  #            col=c(col.scenarios[i],col.scenarios[i],
  #                  col.scenarios.transp[i]),
  #        lty=c(-1,1,-1),pch=c(19,-1,15),pt.cex=c(1,1,2),
  #        lwd=c(2,2,1),cex=0.8,bty="n")
  legend("topleft",c("Data","Fitted","95%CrI"),
         col=c(col.scenarios[1],col.scenarios[1],
               col.scenarios.transp[1]),
         lty=c(-1,1,-1),pch=c(19,-1,15),pt.cex=c(1,1,2),
         lwd=c(2,2,1),cex=0.8,bty="n")
}
