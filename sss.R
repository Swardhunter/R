library(ggplot2)
library(plotly)
library(reshape2)
library(ggforce)
library(dplyr)
library(ggpubr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(ggpmisc)
library(stringi)
library(tidyverse)

#Preparing dataframe for operation
T1 = as.data.frame(D[,c(1,2,3)])

#Making column for years 
T1$Year = substr(T1$Time,1,4)
#Removing Year string from the time column
T1$Time = substr(T1$Time,6,18)
#Melting two powerplants into one column
T1_1 = melt(T1,id.vars = c("Year","Time"))
#Casting the powerplants for each year as column and time as row 
T2 = dcast(T1_1,Time~variable+Year,value.var = "value")
T1$Year = as.factor(T1$Year)

D = Einunna
T2= NULL
D$TEST = NA


for (i in 2:length(D$Einunna_R)) {

   sub = D$Einunna_R[i]-D$Einunna_R[i-1]
   D$TEST[i] = sub
   }
   
     

y= NULL

for (i in D$Einunna_R) {
   
   TMP = i+1 - i
   y = rbind(y,TMP)
   
}


TEST2= NULL
for (i in D$Einunna_R) {
   TEST2[i] = i+1 - i
}



setwd("C:/Users/Sward/OneDrive - NTNU/Environmental Engineering and Managment/Master Thesis/HydroFlex/Results/")
Water_Needed$Time= format(Water_Needed$Time, format="%H:%M:%S")
D = reshape2::melt(D,id = "TIME")

d2$Region <- sub("", "Region ", d2$Region)
 d2 = merge(D,N, by.x="variable", by.y = "FID",all.x=T)
d3 = filter(d2,d2$Type=="S")

d2$Type = str_replace(d2$Type,"S","Shallow")

d2$TIME = as.POSIXct(d2$TIME,format="%H:%M:%S")
d2$TIME= format(d2$TIME, format="%H:%M:%S")

 theme_set(theme_grey(base_size=15))
 ggplot(data=d2,aes(x=TIME,y=value, group =variable,color = Type )) + geom_line() + facet_wrap(.~Region, scales="free")
 
 
 ggplot(data=d2,aes(x=TIME,y=value, group =variable,color = Type )) + geom_line(size=1) + facet_wrap(.~Region, scales="free")+ theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8)
     ,legend.position = "bottom", ) +labs(y=("Depth (m)"), x=("Time"))
 
 ggsave("First Simulation Depth Restuls.png",width = 180 , height = 180 , units = "mm", dpi = 600)
 

 AVG = reshape2::melt(Average, id.vars ="HydroFlex changing time (min)")
 
ggplot(AVG,aes(value,`HydroFlex changing time (min)`,color =variable)) +geom_point() + geom_smooth(method = "lm",se=F,formula = y~x+I(x^2)+I(x^3)+I(x^4))+
theme(,legend.position = c(.95, .95),legend.justification = c("right", "top")) +labs( x=("Deatwering rate (cm/hr)"))+ geom_vline(aes(xintercept=10))
   
names(AVG)[2] = "Type"  

ggplot(AVG,aes(value,`HydroFlex changing time (min)`,color =Type)) +geom_point() + stat_smooth(method = "lm",se=F,formula = y~x+I(x^2)+I(x^3)+I(x^4),fullrange =T )+xlim(0,70) +
   theme(,legend.position = c(.95, .95),legend.justification = c("right", "top")) +labs( x=("Dewatering rate (cm/hr)"))+ geom_vline(aes(xintercept=10))+scale_x_continuous(breaks = seq(from = 0,to =70 ,by=10))
ggsave("Correlation of simulation.png",width = 180 , height = 180 , units = "mm", dpi = 600)

library(viridis)
library(hrbrthemes)

 WN = reshape2::melt(Water_Needed,id.vars = "Time")
 WN$Time = as.POSIXct(WN$Time,format="%H:%M:%S")
 names(WN[2])="Type"
ggplot(WN,aes(x=Time,y= value,color=variable))+ geom_line(size=1)+theme(legend.position = "bottom")+labs( y=("Q m^3/s"))
ggsave("Water needed.png",width = 180 , height = 90 , units = "mm", dpi = 600)


d1 = D[,c(1,2)]
names(d1)[2]="tyholt"
if(!is.na(DATA[i,2]) & !is.na(DATA[i,3]))
  
d1 = as.data.frame(d1)
d2= vector()

#if(t[!is.na(t)][[i]])

for (i in 1:length(d1[ ,2])) {
  if(i==1){d2[1]=0}
   else if (d1[!is.na(d1)][i]==0 & d1[!is.na(d1)][i-1]!=0){
      print("SHUTDOWN")
      d2[i]=1}
   else {
      print("FUCK")
      d2[i]=0}
      }
d1 =cbind(d1,d2)
 d2 = filter(d1,V3==1)
test = function(t1){
     d2 = vector()
   for (i in 1:length(t1)) {
      if(i==1){d2[1]=0}
    else  if (t1[!is.na(t1)][i]==0 & t1[!is.na(t1)][i-1]!=0){
         #print("SHUTDOWN")
         d2[i]=1}
      else {
         #print("FUCK OFF")
         d2[i]=0}
   }
   return(d2)
}  
t = test(as.data.frame(D$`21`)) 
d1$t = data.frame(d1,t)  
  df <- do.call("rbind",mylist)     
   d1$df = cbind(d1,df)   

   
T2    =test(D$`21`)   
for (i in 2:length(D)) {
   D[,i]= xts(D[,i],order.by = D[,1])
   s = subdaily2daily(D[,i],test)
}

test3 = xts(D$`21`,order.by = D$Time)
test2 = subdaily2daily(test3,sum(test()),na.rm = T)
  test(test3)
  
  
  
  
library(hydroTSM)  
  
  
  
  
  
 
  