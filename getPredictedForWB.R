## Fill in missing data for WB
#Some days are missing for the WB data. Develop a simple predictive model to fill in missing data based on relationships
#between trib and WB temperatures


#look at raw data first
ggplot( e, aes(dOY,temp) )+
  geom_point()+
  facet_grid(site~year)

#get data in wide format so we can plot rivers against each other
eWide <- dcast( e[,c('date','temp','site','year','dOY')], dOY+year+date~site, value.var='temp' )
names(eWide) <- c('dOY','year','date','WBJIMMY','WBMITCHELL','WBOBEAR','WESTBROOK')

#merge in airTemp and flow
eWide <- merge( x=eWide, y=e[ e$site == 'WEST BROOK',c('date','airTemp','flow'), ],all.x=T)

#pairwise river graphs of temperature
ggplot( eWide[eWide$year %in% 2002:2010,], aes(WBMITCHELL,WBJIMMY) )+
  geom_point( aes(color=dOY) )+
  geom_abline(slope=1)+
  #geom_smooth(method='lm')+
  facet_wrap(~year)
#pairwise river graphs of temperature
ggplot( eWide[eWide$year %in% 2002:2010,], aes(WESTBROOK,WBJIMMY) )+
  geom_point( aes(color=dOY) )+
  geom_abline(slope=1)+
  #geom_smooth(method='lm')+
  facet_wrap(~year)

#pairs(eWide)
#ggpairs(eWide, columns=4:7, diag=list(continuous="density", discrete="bar"), axisLabels="show") 

#  lm() models for predicting WB temps from trib temps
# m3a <- lm( WESTBROOK ~ WBJIMMY , data=eWide )  
# m3b <- lm( WESTBROOK ~ WBJIMMY + factor(year), data=eWide )  
# m3c <- lm( WESTBROOK ~ WBJIMMY * factor(year), data=eWide )
# m3d <- lm( WESTBROOK ~ WBJIMMY * factor(year) + flow, data=eWide )
# m3e <- lm( WESTBROOK ~ WBJIMMY * factor(year) * flow , data=eWide )
# m3f <- lm( WESTBROOK ~ WBJIMMY * factor(year) * flow + airTemp, data=eWide )
m3g <- lm( WESTBROOK ~ WBJIMMY * factor(year) * flow * airTemp, data=eWide )

#AIC(m3a,m3b,m3c,m3d,m3e,m3f,m3g) #m3g is by far the best

#try other streams besides JIMMY
#m4g <- lm( WESTBROOK ~ WBMITCHELL * factor(year) * flow*airTemp, data=eWide )
#m5g <- lm( WESTBROOK ~ WBOBEAR * factor(year) * flow*airTemp, data=eWide )
#r^2s, OBear=0.9863, Mitchell=0.988, Jimmy=0.989, go with Jimmy for predictions

# get dates where wb temp=NA and we have trib data 2002-2013
predictListWB <- eWide[is.na(eWide$WESTBROOK) & eWide$year>2002 & eWide$year<2013,c('date','WBJIMMY','year','flow','airTemp')]
predictedWaterTempWB <- predict(m3g,predictListWB)
preds <- cbind(predictListWB,predictedWaterTempWB)
preds$site='WEST BROOK'
e <- merge( x=e, y=preds[,c('site','date','predictedWaterTempWB')], all.x=T)

# waterTempWPred is WB water temps with predicted temps filled in when WB temp=NA
e$temp <- ifelse( !is.na(e$predictedWaterTempWB),e$predictedWaterTempWB,e$temp )

rm(eWide)

#scale flow
#e$flowScaled <- scale(e$flow)

# #check WB temps with predicted
# ggplot( e, aes(dOY,waterTempWPred) )+
#   geom_point()+
#   geom_point(aes(dOY,waterTemp), color='red') +
#   facet_grid(site~year)
# 
# ggplot( e[e$year==2000,], aes(dOY,waterTempWPred) )+
#   geom_point() +
#   geom_line() +
#   geom_point(aes(dOY,airTemp), color='red') +
#   geom_line(aes(dOY,airTemp), color='red') +
#   facet_grid(site~year)
