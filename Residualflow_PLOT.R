setwd("C://Users//Sward//OneDrive - NTNU//Research NTNU//Hydra_hp_Q_hr_2010_2018_all_v01_59 min time step//Hove_before")
library(hydroTSM)

d = read_excel("Hove.xlsx",na = "-9999")
d1 = as.xts(d[,1],order.by = seq.POSIXt(as.POSIXct("1967/01/01", format("%Y/%m/%d")),as.POSIXct("1970/12/31" ,format("%Y/%m/%d")),"1 day"))
t1 = read_excel("166.20_Lakshola.xlsx",na = "-9999")
names(t1)[2] = "Q"
t1 = as.xts(t1$Q,order.by = t1$Time)
t1$Q  = as.numeric(t1$Q)
t2 = t1["20100101/20191231"]
t2 = subdaily2daily(t2,mean,na.rm = T)
t2 = as.xts(t2)
RES = d1
RES[RES<0]=0
ep = endpoints(RES,on = "months")
dmon = period.apply(RES,INDEX = ep,FUN = quantile,probs=.05,na.rm=T)
dmon= fortify(dmon)
dmon$m = substring(dmon$Index,6,7)
dmon = dmon[,-1]
names(dmon) = c("Q","m")
Summ = Rmisc::summarySE(dmon, measurevar="Q",groupvars=c("m"),na.rm = T)
# monthly average 
#summer mean 
mean(Summ[c(5:9),3],na.rm = T)
#Winter mean 
mean(Summ$Q[-c(5:9)],na.rm = T)
dmon= subdaily2monthly(RES,quantile,probs=.05,na.rm = T)
# FROM MAY TO SEPTEMBER 
dmon= fortify(dmon)
dmon$m = substring(dmon$Index,6,7)
#Mean monthly 
summers = dmon %>%  group_by(m)
tt =summers %>%  summarise( mean(dmon,na.rm = T))
#summer mean 
mean(tt$ss[c(5:9)])
#Winter mean 
mean(tt$ss[-c(5:9)],na.rm = T)

dmon$Index= substring(dmon$Index,6,7) %>% 
  as.factor()

#ploting 
library(ggalt)
ggplot(dmon,aes(x=Index,y=dmon,group=Year,color = Year))+ geom_xspline(spline_shape =1)+labs(y="Q m3/s",x="Monthly Average") #+facet_zoom(ylim = c(0,10))+
  
ggsave("Residual.png",dpi = 2000)
dev.off()

