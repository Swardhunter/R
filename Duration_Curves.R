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
library(openxlsx)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggalt)
library(scales)
library(grid)
library(hydroTSM)
library(ggpattern)
library(png)
library(grid)
library(ggplot2)
library(cowplot)
library(magick)



setwd("E:/Users/Sward/OneDrive - NTNU/Research NTNU")


bxplot <- readPNG(system.file("img", "boxplot.png", package="png"))
N = read_excel("N_DL.xlsx")
HP1_DAILY <- read_excel("dataresults_v2.xlsx",sheet = "HP1_Daily",guess_max = 15000000)
names(HP1_DAILY)[1] = "Time"

HP1 = HP1_DAILY[,c(-1)]
FDC = fdc(HP1,plot = F)
FDC = as.data.frame(FDC)
HP1 = melt(HP1)
FDC = melt(FDC)

d = cbind(HP1,FDC)
d= d[,-3]
names(d)= c("HP.NUMBER","HP1","FDC")
#D4$HPnumber <- sub("FDC_", "",D4$HPnumber)
d= d[complete.cases(d[,c(2,3)]),]

N = read_excel("Flow Ramping Chrac.xlsx",sheet = "Hourly_Data_Analysis (3.1)",na = c("-9999"))

D= merge(d,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
D.R = filter(D,`Outlet Location`=="R")
D.R$Restrictions = sub("Unknown","No",D.R$Restrictions)
D.Res = filter(D.R ,`HP Type`=="Reservoir")
D.RoR = filter(D.R ,`HP Type`=="RoR")

theme_set(theme_bw(base_size=15))
# HP TYPE 
ggplot(data=D.Res,aes(x=round(FDC,2),y=log10(HP1+1)))+ 
  stat_summary(geom="line", fun.y  =mean,  )+
  scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "HP Type")

ggplot(data=D.R,aes(x=round(FDC,2),y=log10(HP1+1),group = `HP Type`,color = `HP Type`))+ 
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), fill="gray")+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "HP Type")
 


ggsave("Duration_HP1_Daily_log(x+1)_HP TYPE.png",dpi= 2000)    


set.seed(1)      # for reproducible example
time <- 1:25
df   <- data.frame(time,
                   pop=rnorm(100*length(time), mean=10*time/(25+time)))

library(ggplot2)
ggplot(df, aes(x=time, y=pop))+ 
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), fill="lightblue")+
  stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun.y=mean, color="red")

#LOG+1 PLOT 
ggplot(data=D.R,aes(x=FDC,y=log10(HP1+1), group =`HP Type`,color =`HP Type` ))+ geom_smooth(se = T) +scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "HP Type")
ggsave("Duration_HP1_Daily_log(x+1)_HP TYPE.png",dpi= 2000)     
#hp number 
TEST = filter(D.Res, HP.NUMBER ==c(375,3,383))
ggplot(data=TEST,aes(x=FDC,y=log10(HP1+1), group =`HP.NUMBER`,color =`Restrictions`))+geom_line() +scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "Restrictions")
ggsave("Duration_HP1_Daily_log(x+1)_HP_RoR_Restrictions.png",dpi= 2000)    


#LOG PLOT 
ggplot(data=D,aes(x=FDC,y=(HP1), group =`HP.NUMBER`,color =`HP Type` ))+geom_line() +scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="Log HP1 (Magintude)",trans = "log10",breaks = c(0,0.0001,0.001,0.01,0.1,1,10),labels = c("0","0.0001","0.001","0.01","0.1","1","10"))+theme(legend.position ="bottom",)
ggsave("Duration_HP1_Daily_log(x)_HP TYPE.png",dpi= 2000)     
#Normal Scale
  ggplot(data=D,aes(x=FDC,y=HP1, group =`HP.NUMBER`,color = `HP Type` )) +geom_line()+
    scale_x_continuous(limits=c(0,1),name= "Time %")+
    scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position ="bottom")
  ggsave("HP1_Duration_HP_TYP.png",dpi= 2000)
# LEGEND HP.NUMBER GROUP HP_TYPE
  ggplot(data=D.R,aes(x=FDC,y=log10(HP1+1), group =`HP.NUMBER`,color =`HP Type` ))+geom_line() +scale_x_continuous(name= "Time %")+
    scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "HP Type")
  ggsave("Duration_HP1_Daily_log(x+1)_HP TYPE.png",dpi= 2000)     
   ggsave("HP1_Duration_HP_TYP.png",dpi= 900) 
  
#MAX DOWN RAMPING PLOTS 
   #Adjusting the box plot whiskers to 95/5 percentile 
   f <- function(x) {
     r <- quantile(x, probs = c(.05, 0.25, 0.5, 0.75, 0.95),na.rm = T)
     names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
     r
   }
   g <- function(x) {
     r <- c(quantile(x, probs = c(.05, 0.25, 0.5, 0.75),na.rm = T),0.00461+(1.31*quantile(x, probs = c( 0.95),na.rm = T)))
     names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
     r
   }
   
   o <- function(x) {
     subset(x, x < quantile(x,.05,na.rm = T) | quantile(x,.95,na.rm = T) < x)
   }
   
   
   
  maxdn <- read_excel("dataresults_v2.xlsx",sheet = "Max_Dn",guess_max = 15000000)
  str(maxdn)
  maxdn$`87`= as.numeric(maxdn$`87`)
  names(maxdn)[1]= "dts"
  d2 = melt(maxdn,id.vars = "dts")
  names(d2)[2]= "HP.NUMBER"
  D = read_excel("Newdatacombined.xlsx",na = c("-9999","-9999.00"),guess_max = 175390)
  D =D[!duplicated(D$Time),]
  D[D<0]<-NA
 D$`334`=as.numeric(D$`334`) 
    
Maxq = as.data.frame(lapply(D[,-1],quantile,.99,na.rm=T)) 
names(Maxq) <- sub("X", "", names(Maxq))
MAXQ = melt(Maxq)
names(MAXQ) = c("HP.NUMBER","MAXQ")
d3= merge(d2,MAXQ,by= "HP.NUMBER",all.x = T) 
d3$value[d3$value>0]=NA
 #Max Q from the data 
 d3$Normalized_Max_DN = -(d3$value/d3$MAXQ )
 

d4 = dcast(d3[,c(1,2,5)],dts~HP.NUMBER,sum)
d4 = d4[,-1]
d4[d4>1]=1
P1 = melt(d4,id.vars = NULL,variable.name = "HP.NUMBER")
P1 = na.omit(P1)


Ranking = lapply(d4,quantile,.95,na.rm = T)
Ranking = as.data.frame(do.call("rbind",Ranking))
Ranking$HP.NUMBER = row.names(Ranking)
P1 = merge(P1,Ranking,by = "HP.NUMBER")

N_AGD = filter(N, N$`Batch Source`== "ECO")
P1_AGD = filter(P1, HP.NUMBER  %in% N_AGD$Number)
P1_AGD$HP.NUMBER = reorder(P1_AGD$HP.NUMBER,-1*as.numeric(as.character(P1_AGD$`95%`)))
P1_AGD  = merge(P1_AGD,N_AGD, by.x = "HP.NUMBER",by.y = "Number")

ggplot(P1_AGD, aes(HP.NUMBER,value,color = Restrictions,pattern =`HP Type`)) + 
  stat_summary(fun.data=f, geom="boxplot_pattern",     pattern_density = 0.35,pattern_spacing = .01) + 
  stat_summary(fun = o, geom="point",size = .05)   +   
  stat_summary(fun.data = f, geom = "errorbar")+ labs(y="Peaking Intensity" , x="HP Number")+theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8), legend.position ="bottom",
                                                                                                   legend.justification = c("left", "top"),
                                                                                             legend.box.just = "left")


N_R = filter(N, `Outlet Location`== "River") 
N_FL = filter(N, `Outlet Location`!= "River") 
P1_R =  filter(P1, HP.NUMBER  %in% N_R$vannkrvNr)
P1_R  = merge(P1_R,N_R, by.x = "HP.NUMBER",by.y = "vannkrvNr" )
P1_R_REST = filter(P1_R,Restrictions_ORR =="Yes")
P1_R_NOR = filter(P1_R,Restrictions_ORR !="Yes")

P1_R$HP.NUMBER = reorder(P1_R$HP.NUMBER,-1*as.numeric(as.character(P1_R$`95%`)))
P1_R$Restrictions_ORR = as.factor(P1_R$Restrictions_ORR)
P1_R$value = round(P1_R$value,digits = 2)
#Daily
ggplot(P1_R, aes(HP.NUMBER,value,color = `Hp Type`,pattern =Restrictions_ORR)) + 
  stat_summary(fun.data =g, geom = "errorbar",color = "black",size = 1,position = "dodge" )+
  stat_summary(fun.data = f, geom = "boxplot_pattern", pattern_density = 0.35,pattern_spacing = .01) +
  stat_summary(fun = o, geom = "point", size = .5)   +
  stat_summary(fun.data = f, geom = "errorbar") +
  labs(y = "Peaking Intensity" , x = "HP Number", color = "HP Typology",pattern = "Restrictions") +
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      size = 12
    ),axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    legend.justification = c("left", "top"),
    legend.box.just = "left",legend.title = element_text(size = 12),legend.text = element_text(size = 12)
  )+ guides(color=guide_legend(nrow=2,byrow=TRUE),pattern=guide_legend(nrow=2,byrow=TRUE))

ggsave("Fig10_option1.png",dpi = 2000,width = 210,height = 150,units = "mm")

P1_FL =  filter(P1, HP.NUMBER  %in% N_FL$vannkrvNr)
P1_FL  = merge(P1_FL,N_FL, by.x = "HP.NUMBER",by.y = "vannkrvNr")
P1_FL$HP.NUMBER = reorder(P1_FL$HP.NUMBER,-1*as.numeric(as.character(P1_FL$`95%`)))
P1_FL$value = round(P1_FL$value,digits = 2)
ggplot(P1_FL, aes(HP.NUMBER,value,color = `Hp Type`,pattern =`Outlet Location`)) + 
  stat_summary(fun.data =g, geom = "errorbar",color = "gray")+
  stat_summary(fun.data=f,geom="boxplot_pattern",     pattern_density = 0.35,pattern_spacing = .01) + 
  stat_summary(fun = o, geom="point",size = .05)   +   
  stat_summary(fun.data = f, geom = "errorbar")+ labs(y="Peaking Intensity" , x="HP Number", color = "HP Typology")+theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 8), legend.position ="bottom",
                                                                                                                         legend.justification = c("left", "top"),
                                                                                                                      legend.box.just = "left")

ggsave("Daily_Max_RoC_Boxplot_FL_with_Regression.png",dpi = 2000,width= 297,height = 210 ,units = "mm")   

N_R = filter(N, `Outlet Location`== "River")
P1_R =  filter(P1, HP.NUMBER  %in% N_R$vannkrvNr)
P1_R  = merge(P1_R,N_R, by.x = "HP.NUMBER",by.y ="vannkrvNr")
P1_ROR = filter(P1_R , `HP Type` == "RoR")
P1_R = P1_R[complete.cases(P1_R$value),]
P1_R$HP.NUMBER = reorder(P1_R$HP.NUMBER,-1*as.numeric(as.character(P1_R$`95%`)))
`%notin%` <- Negate(`%in%`)
P1_FL = filter(P1 ,HP.NUMBER  %notin% N_R$vannkrvNr)
N_FL = filter(N, `Outlet Location`!= "R")
P1_FL  = merge(P1_FL,N_FL, by.x = "HP.NUMBER",by.y = "vannkrvNr")
P1_FL$HP.NUMBER = reorder(P1_FL$HP.NUMBER,-1*as.numeric(as.character(P1_FL$`95%`)))

Median_Daily = as.data.frame(as.numeric(sapply(d4, median,na.rm=T)))
Median_Daily$HP.NUMBER = names(d4)
names(Median_Daily)[1]= "MEDIAN"
Median_Daily$`90` = Ranking$`95%`



P1_R$Restrictions = sub("Unknown","No",P1_R$Restrictions)
P1_FL$Restrictions = sub("Unknown","No",P1_FL$Restrictions)

#Hourly Plots
names(P1_R)[15]= "Restrictions"
names(P1_R)[7]= "HP Typology"

P1_R$value= round(P1_R$value,digits = 2)
Hourly_R_PLOT = ggplot(P1_R, aes(HP.NUMBER,value,pattern = Restrictions,color =`HP Typology`)) + 
  stat_summary(fun.data=f, geom="boxplot_pattern",     pattern_density = 0.35,pattern_spacing = .01) + 
  stat_summary(fun = o, geom="point",size = .5)   +   
  stat_summary(fun.data = f, geom = "errorbar")+ labs(y="Peaking Intensity" , x="HP Number")+
  theme(axis.text.x = element_text(angle = 75, hjust = 1,size = 12), axis.text.y = element_text(size = 12),legend.position ="bottom",
                                                      legend.justification = c("left", "top"),
        legend.key.width=unit(5,"mm"),legend.title = element_text(size = 12),legend.box.just = "left",legend.text = element_text( size=12))+
        guides(color=guide_legend(nrow=2,byrow=TRUE))
  
img <- readPNG("boxplot.png")

Hourly_R_PLOT#+
annotation_custom(rasterGrob(img, x = .965, y = .875, height = .25, width = .07, interpolate=TRUE))

ggsave("fig3_Hourly_Max-dn-rate_rivers_15txt_12axis_12legendtext_option1_NOBOXPLOT.png",width = 210,height = 150,units = "mm" ,dpi = 2000)


names(P1_FL)[7]= "HP Typology"
P1_FL$value = round(P1_FL$value,digits = 2)
ggplot(P1_FL, aes(HP.NUMBER,value,color = `Outlet Location`,pattern =`HP Typology`)) + 
  stat_summary(fun.data=f, geom="boxplot_pattern",     pattern_density = 0.35,pattern_spacing = .01) + 
  stat_summary(fun = o, geom="point",size = .5)   + stat_summary(fun.data = f, geom = "errorbar")+   
  labs(y="Peaking Intensity" , x="HP Number")+theme(axis.text.y = element_text(size = 12),axis.text.x = element_text(angle = 45, hjust = 1,size = 12), legend.position ="bottom",
                                                    legend.justification = c("left", "top"),
                                                    legend.box.just = "left")+
 guides(color=guide_legend(nrow=2,byrow=TRUE),pattern=guide_legend(nrow=2,byrow=TRUE))

ggsave("Max_RoC_Decrease_Boxplot_R_DAILY_95.png",width = 300, units = "mm" ,dpi = 2000)
ggsave("fig3b_Hourly_Max-dn-rate_FL_15txt_10axis_12legendtext_option1.png",width = 210,height = 150,units = "mm", dpi = 2000)

#plotting medians and 95 tile for daily vs hourly 

TT = merge(Median_Daily,Median_Hourly,by = "HP.NUMBER")
TT = merge(TT,N,by.x =  "HP.NUMBER", by.y = "Number")
names(TT)= c("HP.NUMBER","Med_DL","95_Dl","Med_HR","95_HR","90_Daily","75_Daily","75_Hourly","90_Hourly")

ggplot(TT,aes(y= `95_HR`,x = `95_Dl`)) + geom_point(aes( shape = `Outlet Location` , color =Restrictions)) + geom_smooth(aes(y= `95_HR`,x = `95_Dl`),method = "lm", formula = FORM, color = "blue", type = "dashed") + labs(x = "95-tile Daily" , y = "95-tile Hourly") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "left", label.y.npc = 0.95,
               formula = FORM, parse = TRUE, size = 4)
  
  
ggplot(TT,aes(y= `90_Hourly`,x = `90_Daily`)) + geom_point(aes( shape = `Outlet Location` , color =Restrictions)) + geom_smooth(aes(),method = "lm", formula = FORM, color = "blue", type = "dashed") + labs(y = "90-tile Hourly" , x = "90-tile Daily") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.x.npc = "left", label.y.npc = 0.95,
               formula = FORM, parse = TRUE, size = 4)


FORM = y ~ x
scatter.smooth(x=TT$Med_HR, y=TT$Med_DL, main="Hourly ~ Daily Median") 
scatter.smooth(x=TT$`95_HR`, y=TT$`95_Dl`, xlab = "Hourly 95-tile",ylab = "Daily 95-tile") 
scatter.smooth(x=TT$`90_Hourly`, y=TT$`90_Daily`, xlab = "Hourly 90-tile",ylab = "Daily 90-tile") 

 text(1,1,labels = paste("R2",cor(TT$`95_HR`,TT$`95_Dl`)))

 plot(fitdist(TT$`75_Daily`,"norm"))
 plot(fitdist(TT$`75_Hourly`,"norm"))
 
 
 M1 = lm(`95_Dl`~`95_HR`,data = TT)
 M2 =
 
 
 
FDC_maxdn = (fdc(d4,plot=F))
FDC_maxdn = as.data.frame(FDC_maxdn)
FDC_maxdn = melt(FDC_maxdn,id.vars = NULL)
names(FDC_maxdn)= c("HP.NUMBER","FDC")
d5 = cbind(Normalized_Max_DN,FDC_maxdn)
d5 = d5[,-3]
#asssigning HP prop.
D= merge(d5,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
D.R = filter(D,`Outlet Location`=="R")
D.R= D.R[complete.cases(D.R[,c(2,3)]),]
D.Res = filter(D.R,`HP Type`=="Reservoir")
D.RoR = filter(D.R,`HP Type`=="RoR")

#plotting
ggplot(data=D.R,aes(x=FDC,y=(value), group =`HP.NUMBER`,color =`HP Type` ))+geom_line()+scale_x_continuous(name= "Time %",limits = c(0,1))+#facet_zoom(ylim = c(0.75,1))
  scale_y_continuous(name="Normalized Max RoC",limits = c(0,1))+theme(legend.position ="bottom",)
ggsave("Max_RoC_dec_Normalized-by-maxQ-adjusted_HPTYPE_.png",dpi= 2000)  

# Plotting Roc WITH MEAN AND CI

D.R= D.R[complete.cases(D.R[,c(2,3)]),]

ggplot(data=D.R,aes(x=Quantile,y=(value),group = `HP Type`,color = `HP Type`))+ 
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), fill="gray")+
  stat_summary(geom="line", fun.y=mean ,linetype="dashed")+scale_x_discrete(name= "Time %")+
  scale_y_continuous(name="Peaking Intensity",limits = c(0,1))+theme(legend.position = "bottom")+labs(color = "HP Type")
ggsave("Peaking_Intesity_Duration_HP_Type.png",dpi = 2000)
D.R$value[D.R$value>1] = 1
  



ggplot(data=D.Res,aes(x=FDC,y=(value), group =`HP.NUMBER`,color =`HP.NUMBER` ))+geom_line()+scale_x_continuous(name= "Time %",limits = c(0,1))+
  scale_y_continuous(name="Normalized Max RoC",limits = c(0,1))+theme()+labs(color = "HP Number")
ggsave("Max_RoC_dec_Normalized-by-maxQ-adjusted_Res_.png",dpi= 2000)   

ggplot(data=D.RoR,aes(x=FDC,y=(value), group =`HP.NUMBER`,color =`HP.NUMBER` ))+geom_line()+scale_x_continuous(name= "Time %",limits = c(0,1))+
  scale_y_continuous(name="Normalized Max RoC",limits = c(0,1))+theme()+labs(color = "HP Number")
ggsave("Max_RoC_dec_Normalized-by-maxQ-adjusted_RoR_.png",dpi= 2000)     


ggplot(data=D.Res,aes(x=FDC,y=(value), group =`HP.NUMBER`,color =`Restrictions` ))+geom_line()+scale_x_continuous(name= "Time %",limits = c(0,1))+
  scale_y_continuous(name="Normalized Max RoC",limits = c(0,1))+theme(legend.position = "bottom")
ggsave("Max_RoC_dec_Normalized-by-maxQ-adjusted_Reservoir_Restrictions.png",dpi= 2000)     


#Quantiles 
qq= as.data.frame(sapply(d4,quantile,seq(1,0,-.1),na.rm=T)) 
  qq[qq>1] = 1 
  qq$Quantile = row.names(qq)
  qq =melt(qq,id.vars = "Quantile")  
  names(qq) = c("Quantile","HP.NUMBER","value")
  D= merge(qq,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
  D.R = filter(D,`Outlet Location`=="R")
  

  D.R$Quantile = factor(D.R$Quantile,levels =c("100%","90%","80%","70%","60%","50%","40%","30%","20%","10%","0%"))
  
  ggplot(data=D.R,aes(x=Quantile,y=value, group =`HP.NUMBER`,color =`HP Type` ))+geom_xspline(spline_shape =.5)+scale_x_discrete(name= "Time %")+
    scale_y_continuous(name="Peaking Intensity",limits = c(0,1))+theme(legend.position ="bottom")
  ggsave("Max_RoC_Quantile.png",dpi= 2000)  
  
  
names(qq) = c("")
L1 =as.data.frame(unlist(test))







mylist= list()
for (i in 1:ncol(qq)) {
  s= qq[,i]
        for (j in 1:nrow(qq)) {
       t= s[j]/s[7]
         d[j]=cbind(t) 
          mylist[[i]] = d
      
      }  
  }
     
     
df <- do.call("cbind",mylist) %>% 
  as.data.frame()
names(df )= names(as.data.frame(qq))
#rownames(df) = qq$Quantile
df$Quantile = row.names(qq)
df2= melt(df,id.vars = "Quantile")
names(df2) = c("Quantile","HP.NUMBER","value")
D= merge(df2,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
D.R = filter(D,`Outlet Location`=="R")
D.R$Quantile=factor(D.R$Quantile,levels = c("0%",  "10%" ,"25%"    ,  "50%"  ,"75%","90%"  ,"100%"))

D.RES = filter(D.R,`HP Type` =="Reservoir")
D.RES$HP.NUMBER = as.character(D.RES$HP.NUMBER)

D.RES = arrange(D.RES,as.numeric(as.character(D.RES$HP.NUMBER) ))
D.RES$HP.NUMBER = reorder(D.RES$HP.NUMBER,as.numeric(as.character(D.RES$HP.NUMBER)))

TT = filter(D.RES,HP.NUMBER == 3)
ggplot(data=TT,aes(x=Quantile,y=(value), group =HP.NUMBER,color =`HP.NUMBER`))+geom_xspline(spline_shape =1)+scale_x_discrete(name= "Time %")+
  scale_y_continuous(name="%HP1")+theme(legend.position =)+labs(color = "HP Type")
ggsave("HP1_Normalized_percent_RESERVOIR.png",dpi= 2000)  


#HP1& ANNUAL 
HP1_ANNUAL <- read_excel("Hourly_Data_Restults_Combined_R.xlsx",sheet = "HP1_Annual",guess_max = 15000000)
HP2_ANNUAL<- read_excel("Hourly_Data_Restults_Combined_R.xlsx",sheet = "HP2_Annual(Need_checking)_DONE",guess_max = 15000000)

HP1_A = melt(HP1_ANNUAL,id.vars = "Year")
HP2_A = melt(HP2_ANNUAL,id.vars = "Year")
HP_A = merge(HP1_A,HP2_A,by = c("Year","variable"))
names(HP_A)[c(2,3,4)]= c("HP.NUMBER","HP1","HP2")
HP_A= merge(HP_A,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
HP_A = arrange(HP_A,as.numeric(as.character(HP_A$HP.NUMBER) ))
HP_A$HP.NUMBER = reorder(HP_A$HP.NUMBER,as.numeric(as.character(HP_A$HP.NUMBER)))

#HP1 & 2 OUTLET LOCATIN 
P1= ggplot(data=HP_A,aes(x=`Outlet Location`,y=(HP1),  fill=`Restrictions`))+geom_boxplot()+
  scale_y_continuous(name="HP1",limits = c(0,3))
P2 = ggplot(data=HP_A,aes(x=`Outlet Location`,y=(HP2),  fill=`Restrictions`))+geom_boxplot()+
  scale_y_continuous(name="HP2",limits = c(0,40))
ggarrange(P1,P2,ncol = 1,nrow = 2,common.legend = TRUE, legend = "bottom")
ggsave("HP1_2_ANNUAL_OUTLET LOCATION .png",dpi= 2000)  


#HP1_2 FOR RIVER POWER PLANTS 
HP_R = filter(HP_A,`Outlet Location`=="R")
ggplot(data=HP_R,aes(x=HP.NUMBER,y=(HP1),  fill=`Restrictions`))+geom_boxplot()+
  scale_y_continuous(name="HP1",limits = c(0,3))+theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(.85, 1),legend.background = element_rect(fill="gray", 
 size=.5, linetype="solid",colour ="black"),legend.justification = c("left", "top"))+labs(x="HP Number")
ggsave("HP1_ANNUAL_River.png",width = 297,height = 210,units = "mm",dpi= 2000)

ggplot(data=HP_R,aes(x=HP.NUMBER,y=(HP2),  fill=`Restrictions`))+geom_boxplot()+
  scale_y_continuous(name="HP2",limits = c(0,20))+theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(.85, 1),legend.background = element_rect(fill="gray", 
                                                                                                                                                                      size=.5, linetype="solid",colour ="black"),legend.justification = c("left", "top"))+labs(x="HP Number")
ggsave("HP2_ANNUAL_River.png",width = 297,height = 210,units = "mm",dpi= 2000)

#box plot max RoC decrease

maxdn <- read_excel("Hourly_Data_Restults_Combined_R.xlsx",sheet = "Max_Down_DAILY",guess_max = 15000000)
str(maxdn)
maxdn$`87`= as.numeric(maxdn$`87`)
d2 = melt(maxdn,id.vars = "dts")
names(d2)[2]= "HP.NUMBER"
D = merge(d2,N, by.x = "HP.NUMBER",by.y = "Number",all.x = T)
D$value = -1*D$value
D.R = filter(D,`Outlet Location`=="R")
D.R$HP.NUMBER = reorder(D.R$HP.NUMBER,as.numeric(as.character(D.R$HP.NUMBER)))

ggplot(data=D,aes(x=`Outlet Location`,y=value,  fill=`Restrictions`))+geom_boxplot()+
  scale_y_continuous(name="Max RoC (m3/s/h)",limits = c(0,300))+
  theme(legend.position = c(.85, 1),legend.background = element_rect(fill="gray", 
  size=.5, linetype="solid",colour ="black"),legend.justification = c("left", "top"))
ggsave("Max_RoC_Dec_Outlet.png",dpi= 2000) 



ggplot(data=D.R,aes(x=HP.NUMBER,y=value, fill=`Restrictions`))+geom_boxplot()+
  scale_y_continuous(name="Max RoC (m3/s/h)",limits = c(0,200))+theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = c(.85, 1),legend.background = element_rect(fill="gray", 
  size=.5, linetype="solid",colour ="black"),legend.justification = c("left", "top"))+labs(x="HP Number")
ggsave("Max_RoC_Dec_River.png",width = 297,height = 210,units = "mm",dpi= 2000)




#COSH tool processing 

D = read_excel("Newdatacombined.xlsx",na = c("-9999","-9999.00"),guess_max = 175390)
D= D[,-1]
D$Time = as.character(D$Time)
for (i in 2:length(D)) {
  s = write.xlsx(D[,c(1,i)],paste(names(D)[i],".xlsx"))
  print(paste(names(D)[i],".xlsx"))
}
#Quantile curves from COSH
Max_RoC = read_excel("Flow Ramping Chrac.xlsx",sheet = "Duration_Curves_Max_RoC",guess_max = 107000)  
d2 = dcast(d1,Type~HP+variable,sum)

d1 = melt(Max_RoC,id.vars = c("HP","Type"))
D= merge(d1,N,by.x = "HP",by.y = "Number",all.x = T)
D.R= filter(D,`Outlet Location`=="R")
d3$Type <- sub("Min",1, d3$Type)
d3$Type <- sub("Max",20, d3$Type)

d3$Type <- sub("10th percentile",4, d3$Type)
d3$Type <- sub("25th percentile",7, d3$Type)
d3$Type <- sub("Median",10, d3$Type)
d3$Type <- sub("75th percentile",13, d3$Type)
d3$Type <- sub("90th percentile",17, d3$Type)


ggplot(data=d3,aes(x=Type,y=(Normalized_Max_DN), group =HP,color =`HP Type`))+geom_smooth(se=F,size=0.5)+scale_x_continuous(limits=c(0, 20),name= "%Time",breaks=c(1,4,7,10,13,17,20),labels=c("1" = "Min", "4" = "10th percentile","7" = "25th percentile","10" = "Median","13" = "75th percentile","17" = "90th percentile","20" = "Max"))+
  scale_y_continuous(name="Noramlized Max RoC")+theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10),legend.position ="bottom")+labs(color = "HP Type")
ggsave("Max_RoC_Normalized_COSH.png",dpi= 2000)  


d1 = melt(Max_RoC,id.vars = c("HP","Type"))
pp = as.factor(as.character(unique((d1$HP))))

d1 = filter(d1,HP=="3"|HP=="78")
mylist= list()
mylist2 = list()
test = NULL 
for (i in 1:length(pp)) {
  p = filter(d1,HP==pp[i])
  r = as.factor(unique(p$variable))
  for (j in  1:length(r)) {
    s = filter(p,variable == r[j])
    for (k in 1:nrow(s)) {
      t= s$value[k]/max(s$value)
      test[k]= t
      mylist[[j]]= test
    #df <- do.call("cbind",mylist) %>% melt %>% as.data.frame()
   
    #mylist2[[i]]=df[,3]
        }
  }
  
}






df <- do.call("rbind",mylist2) %>% melt %>% as.data.frame()
 df =  do.call("cbind",df) #%>% 
  as.data.frame() %>% 
  melt
d1 = cbind(d1,df[,3])
df$Quantile = d1$Type
names(df)= names(as.data.frame(d1))



test = D.Res %>%  group_by(`FDC`)
tt =test %>%  summarise(ss = mean(HP1,na.rm = T))
ggplot(data=tt,aes(x=FDC,y=ss))+ geom_point()+ geom_line()




# Duration curve using the quantiles 
QQ =  sapply(HP1_DAILY[,-1],FUN =  quantile,seq(1,0,-0.1),na.rm=T)
HP1 = melt(QQ)
names(HP1)= c("Quantile","HP.NUMBER","HP1")
#D4$HPnumber <- sub("FDC_", "",D4$HPnumber)
#d= d[complete.cases(d[,c(2,3)]),]

N = read_excel("Flow Ramping Chrac.xlsx",sheet = "Hourly_Data_Analysis (3.1)",na = c("-9999"))

D= merge(HP1,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
D.R = filter(D, `Outlet Location`=="R")
library(plyr)
D.R$HP1 = log10(D.R$HP1+1)
TEST = Rmisc::summarySE(D.R, measurevar="value",groupvars=c("`HP Type`","Quantile"),na.rm = T)
            
 #PLot mean with CI  
ggplot(data=D.R,aes(x=(Quantile),y=value,group = `HP Type`,color= `HP Type`))+ 
  geom_ribbon(aes(ymin=value-ci, ymax=value+ci),fill="gray")+
  geom_xspline(spline_shape =.25,linetype = "dashed",size =1 )+scale_x_discrete(name= "Time %",labels = as.character(seq(0,100,10)))+
  scale_y_continuous(name="Normalized RoC")+theme(legend.position = "bottom")+labs(color = "HP Type")
ggsave("RoC_Mean_ci.png",dpi=2000)

#Plot quantile 
ggplot(data=D.R,aes(x=as.character(Quantile),y=HP1,group = `HP Type`,color= `HP Type`))+ 
  geom_line(aes(group = `HP Type`),spline_shape =1,linetype = "dashed",size =1 )#+scale_x_discrete(name= "Time %",labels = as.character(seq(0,100,10)))+
  scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "HP Type")
ggsave("HP1+MEAN+CI final.png",dpi=2000)

#RoC
QQ =  sapply(d4,FUN =  quantile,seq(0,1,0.01),na.rm=T)
FDC = fdc(QQ,plot = F)

FDC = as.data.frame(FDC)
ROC = melt(QQ)
FDC = melt(FDC)

d = cbind(ROC,FDC)
d= d[,c(2,3,5)]
names(d)= c("HP.NUMBER","HP1","FDC")
#D4$HPnumber <- sub("FDC_", "",D4$HPnumber)
d= d[complete.cases(d[,c(2,3)]),]

N = read_excel("Flow Ramping Chrac.xlsx",sheet = "Hourly_Data_Analysis (3.1)",na = c("-9999"))

D= merge(d,N,by.x = "HP.NUMBER",by.y = "Number",all.x = T)
D.R = filter( D, `Outlet Location` == "R")

TEST = Rmisc::summarySE(D.R, measurevar="HP1",groupvars=c("`HP Type`","FDC"))


ggplot(data=TEST,aes(x=FDC,y=HP1,color = `HP Type`))+ 
  geom_ribbon(aes(ymin=HP1-ci, ymax=HP1+ci),fill="gray")+
  geom_xspline(spline_shape =.25)+scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="Normalized Max RoC",limits = c(-0.1,1))+theme(legend.position = "bottom")+labs(color = "HP Type")
ggsave("RoC_Duration_Mean_CI.png",dpi = 2000)
ggplot(data=D,aes(x=FDC,y=log10(HP1+1),color = `HP Type`))+ 
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), fill="gray")+
  stat_summary(geom="line", fun.y  =mean ,linetype="solid")+
  scale_x_continuous(name= "Time %")+
  scale_y_continuous(name="HP1 (Magintude)")+theme(legend.position = "bottom")+labs(color = "HP Type")


