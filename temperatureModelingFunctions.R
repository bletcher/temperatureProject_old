riverLabeller <- function(var, value){
  value <- as.character(value)
  if (var=="site") { 
    value[value=="1"] <- "WB"
    value[value=="2"] <- "OL"
    value[value=="3"] <- "OS"
    value[value=="4"] <- "Is"
  }
  return(value)
}

riverLabeller2 <- function(var, value){
  value <- as.character(value)
  if (var=="site") { 
    value[value=="WEST BROOK"] <- "WB"
    value[value=="WB JIMMY"] <- "OL"
    value[value=="WB MITCHELL"] <- "OS"
    value[value=="WB OBEAR"] <- "Is"
  }
  return(value)
}

# this works for modes with no interactions, but not for those with them
# deosn't seems all that useful anyway because a linear model is fine for thses data
predSim <- function( nSim=100, model, data ){
  
  # predictive simulation for model checking
  # simulated parameter estimates
  
  s <- sim( model,nSim ) # gets 100 parameter estimates and sigmas from the model
  sC <- coef(s)
  sSigma <- s@sigma #sigma method doesn't work
  
  p <- array(NA,c(nrow(data),nSim))
  pWError <- p 
  etArray <- data.matrix(cbind(rep(1,nrow(data)),data[,fixCov])) # this call to data works well for modes with no interactions, not sure how to get the interactions in the data
  for(i in 1:nrow(data)){
    
    p[i,] <- sC %*% etArray[i,]
    pWError[i,]  <- rnorm(nSim,p[i,],sSigma)
    
  }
  pRowMeans <- apply(p,1,mean)
  pRowSDs <- apply(p,1,sd)
  
  pWErrorRowMeans <- apply(pWError,1,mean)
  pWErrorRowSDs <- apply(pWError,1,sd)
  
  gP <- 
    ggplot(cbind(pRowMeans,data), aes(pRowMeans,temp)) +
    geom_point()+
    geom_abline(intercept=0, slope=1,color='white')
    #ggtitle('data')
  
  gPWError <- 
    ggplot(cbind(pWErrorRowMeans,data), aes(pWErrorRowMeans,temp)) +
    geom_point()+
    geom_abline(intercept=0, slope=1,color='white')
  
  return( list(pRowMeans=pRowMeans,pRowSDs=pRowSDs,
               pWErrorRowMeans=pWErrorRowMeans,pWErrorRowSDs=pWErrorRowSDs,
               gP=gP,gPWError=gPWError) )
  
}


# leave out one site validation
validateModel <- function ( model, d ) {
  f=model$call
  siteList <- unique(d$site)
  v <- data.frame(s=NA,y=NA,n=NA,rmse=NA,biasMean=NA,biasSD=NA,
                  sigma=NA,lat=NA,lon=NA)
  g <- list()
  i=0
  for(s in siteList){
    
    # run model without site = s
    withOut <- d[d$site != s,]    
    m <- lm( f, data=withOut )
    
    yearList <- unique( d$year[d$site == s ] )
    
    for(y in yearList){
     
      # data for the site/year combo
      with <- d[d$site == s & d$year == y,]
      # predict temp for missing site/year based on all the other sites
      p <- cbind(with,pred=predict(m,with))
      
      i=i+1
      g[[i]] <- ggplot(p[,c('temp','pred')],aes(temp,pred))+
        geom_point()+
        geom_smooth(method='lm') +
        geom_abline(intercept=0,slope=1) +
        ggtitle(paste(as.character(s), as.character(y),sep='_'))
      
      p$bias <- p$pred-p$temp
# #      v <- rbind(v,c(as.character(s), as.character(y), nrow(p), 
#                      sqrt(sum(p$bias^2, na.rm=T)/nrow(p)),
#                      mean(p$bias,na.rm=T),
#                      sd(p$bias,na.rm=T),
#                      summary(m)$sigma,
#                      unique(p$Latitude),unique(p$Longitude))) 
      v <- rbind(v,data.frame(s=as.character(s), 
                              y=y, n=nrow(p), 
                              rmse=sqrt(sum(p$bias^2, na.rm=T)/nrow(p)),
                              biasMean=mean(p$bias,na.rm=T),
                              biasSD=sd(p$bias,na.rm=T),
                              sigma=summary(m)$sigma,
                              lat=unique(p$Latitude),lon=unique(p$Longitude)))
           
    }
  }
  v <- v[-1,] #get rid of first dummy row
  v$biasMeanAbs <- abs(v$biasMean)
  v$biasMeanDir <- ifelse( v$biasMean>0,1,-1 )
  return( list(v=v,g=g) )
}

makeBiasMap <- function (d) {
  library(ggmap)
  map.center <- geocode("Hartford, CT")
  baseMap <- qmap(c(lon=map.center$lon, lat=map.center$lat), source="google", zoom=8)
  map <- baseMap + geom_point(aes(x=as.numeric(lon), y=as.numeric(lat), 
                                  size=as.numeric(biasMeanAbs),
                                  color=factor(biasMeanDir)), 
                              data=d$v)
  return(map)
}
