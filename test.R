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
setwd("C:/Users/Sward/OneDrive - NTNU/Research NTNU")
D = read_excel("Newdatacombined.xlsx",na = c("-9999","-9999.00"),guess_max = 175390)
N = read_excel("Flow Ramping Chrac.xlsx",sheet = "Hourly_Data_Analysis (3.1)",na = c("-9999"))


D$`334` = as.numeric(D$`334`)
D[D<0]=NA
D_XTS = as.xts(D[,-1],order.by = D$Time)

d1 = D$`306`
d1 = d1$`21`
d2= vector()

#if(t[!is.na(t)][[i]])
# WORKS FINALLY!! DO NOT CHANGE ANY SHIT IN THIS
d1[is.na(d1)]=-9999
for (i in 1:length(d1)) {
  if(i==1){d2[1]=0}
  else if (!is.na(d1[i])==T &  d1[i]==0& d1[i-1]!=0){
    #print("SHUTDOWN")
    d2[i]=1}
  else {
    #print("")
    d2[i]=0}
  print(paste(sum(d2),"ShutDowns"))
}

#For the function 
t1_df = data.frame(date=index(D_XTS$`306`), coredata(D_XTS$`306`))
t1_vec = as.numeric(as.character(t1_df[,2]))
t1_vec[is.na(t1_vec)]=-9999
d2 = vector()
for (i in 1:length(t1_vec)) {
  if(i==1){d2[1]=0}
  else if (!is.na(t1_vec[i])==T &  t1_vec[i]==0 & t1_vec[i-1]!=0){
    print("SHUTDOWN")
    d2[i]=1}
  else {
    #print("NO SHUTDOWN")
    d2[i]=0}
}
d2_xts= xts(d2,order.by=t1_df$date)



d1 =cbind(d1,d2)



test = function(t1){
  t1_df = data.frame(date=index(t1), coredata(t1))
  t1_vec = as.numeric(as.character(t1_df[,2])) 
  d2 = vector()
  for (i in 1:length(t1)) {
    if(i==1){d2[1]=0}
    else if (!is.na(t1_vec[i])==T&  t1_vec[i]==0& t1_vec[i-1]!=0){
      print("SHUTDOWN")
      d2[i]=1}
    else {
      print("NO  SHUTDOWN")
      d2[i]=0}
  }
  d2_xts= xts(d2,order.by=t1_df$date)
  return(d2_xts)
}
Shu
t1 = to.period(test3,period = "days",drop.ti)
indexFormat(test2)<-"%Y-%m-%d"

t1 = split(temps, f = "days")
temps_avg <- lapply(X = t1, FUN = test)
rbind(temps_avg)



ep = endpoints(test3,on="days",k=1)
T2 = period.apply(test3,INDEX = ep,FUN = sum(test))










t1 = split.xts(test3,f="days",k=1)
t2 = lapply(t1,test)
t3 = do.call(rbind,t2)
t4=subdaily2annual(t3,sum,na.rm = T)
View(t4)

t2 = t1[[1]]
t2 = test(t2)





#For testing

d1[is.na(d1)]=-9999
for (i in 1:length(d1)) {
  if(i==1& d1[1]==0){d2[1]=1}
  else if (i==1& d1[1]!=0){d2[1]=0}
  else if (!is.na(d1[i])==T &  d1[i]==0& d1[i-1]!=0){
    #print("SHUTDOWN")
    d2[i]=1}
  else {
    #print("")
    d2[i]=0}
  print(paste(sum(d2),"ShutDowns"))
}
# duration 
d1= D$`21`
d1[is.na(d1)]=-9999
for (i in 1:length(d1)) {
   if(i==1& d1[1]==0){d2[1]=1}
  
}
t1 = split.xts(D_XTS,f="year",k=1)
t = t1[[15]][,1]
 t= t[[1]]
 
 
 s2=vector()
 t1_df = data.frame(date=index(t), coredata(t))
 t1_vec = as.numeric(as.character(t1_df[,2])) 
 t1_vec[t1_vec<0]= NA
 t1_vec[is.na(t1_vec)]=-9999
 d2 = vector()
for (i in 1:length(t1_vec)) {
  if (!is.na(t1_vec[i])==T & t1_vec[i]==0){
    #print("SHUTDOWN")
    s2[i]=1}
    else  {s2[i]=0}
  
  
}
 

d4 = vector()
 test2 = 0
 test = vector()
 for (i in 1:length(s2)) {
  if (s2[i]==1) {
    test2=test2+1
    if (s2[i+1]==0 & !is.na(s2[i+1])==T ) {test[i]=test2
    test2=0
        print( paste("shutdown ends at",t1_df[i,1]))
      next}
    else if(is.na(s2[i+1])==T) {break}
  }
 }
d5 = mean(test,na.rm = T)
 
 
# Function making for duration of shutdown 
 d_shutdown = function(t){
 t1_df = data.frame(date=index(t), coredata(t))
 t1_vec = as.numeric(as.character(t1_df[,2])) 
 t1_vec[t1_vec<0]= NA
 t1_vec[is.na(t1_vec)]=-9999
 d2 = vector()
 for (i in 1:length(t1_vec)) {
   if (!is.na(t1_vec[i])==T & t1_vec[i]==0){
     d2[i]=1}
   else  {d2[i]=0}
    }
 
 d1 = 0
 d3 = vector()
 for (i in 1:length(d2)) {
   if (d2[i]==1) {
      d1=d1+1
     if (d2[i+1]==0 & !is.na(d2[i+1])==T ) {d3[i]=d1
     d1=0
     print( paste("shutdown ends at",t1_df[i,1]))
     next}
     else if(is.na(s2[i+1])==T) {break}
   }
 }
# Ending the function 
 return(mean(d3,na.rm = T))
 } 
 
 
 
 # TESTING
 t1 = split.xts(D_XTS[,c(1:10)],f="year",k=1)
 t2 = list()
 for (i in 1:length(t1)) {
   s= sapply(t1[[i]],Duration_Shutdown,simplify = TRUE, USE.NAMES = TRUE)
   t2[[i]]=s 
 }
 t4 = as.data.frame(do.call("rbind",t2),row.names = unique(format(index(D_XTS),"%Y")))
 
 
 
 
 
 
 
 dt = D_XTS[,c(1,2)]
 dt$`3` = as.numeric(as.character(dt$`3`))
 DT2 = split.xts(dt,f="day",k=1)
 
 
HP1_DAILY =  function(t1){
 t1 = as.numeric(as.character(t1))
   hp1 <- (max(t1,na.rm = T) - min(t1,na.rm = T))/mean(t1,na.rm = T)
   return(hp1)
}

HP2_DAILY = function(t1){
  t1 = as.numeric(as.character(t1))
hp2 = vector()
findmax= vector()
for (i in 2:length(t1)) {
  if(is.na(t1[i])|is.na(t1[i-1])){ 
    hp2[i-1] <- NA
  }
  else{
    hp2[i-1] <- abs((t1[i]-t1[i-1])/1)  
    findmax[i-1] <- (t1[i]-t1[i-1])  
  }
}
  reshp2 <- quantile(na.omit(hp2),prob=.90)
  maxDrop<- min(na.omit(findmax[is.finite(findmax)]))  
  maxDrop[!is.finite(maxDrop)] =NA
  
  maxUp <- max(na.omit(findmax[is.finite(findmax)]))   
  maxUp[!is.finite(maxUp)] =NA
  
   return(c(reshp2,maxDrop,maxUp))
   }






TTT = sapply(DT2,HP1_DAILY,simplify = T,USE.NAMES = TRUE)
TTT = as.xts(TTT,order.by = index())

ep = endpoints(dt,on = "days")
TTT = list()
 for (i in 1:ncol(dt)) {
  TTT[[i]] = period.apply(dt[,i],INDEX = ep,FUN = HP1_DAILY)
    #period.apply(dt[,i],INDEX = ep,FUN = HP1_DAILY)

}
tt = do.call("cbind",TTT)

period.apply(dt[,2],INDEX = ep,FUN = HP1_DAILY)

















TEST = t1[[sample(1250:6500,1)]][,sample(1:89,1)] 

TEST = t1[[sample(1500:7305,1)]][,61]
TEST[(TEST-quantile(TEST,.99,na.rm = T))/quantile(TEST,.99,na.rm = T)< -.995]= 0
plot(c(1:24),coredata(TEST),"b")






t1 = D_XTS$`191`

# Creating Function for 1 year / 10% Max Q as threshold 
RoC = function(t1){
  storage.mode(t1) <- "numeric"
  t1[t1<0]=NA
  #Max Q throughout the whole time series
  qmax = as.numeric(quantile(coredata(t1),.99,na.rm = T))
  t1[(t1/qmax < .01)]= 0
  #Env. Flow 
   if (length(Res.Flow$`Base-Flow_winter`[Res.Flow$vannkrvNr == names(t1)])==0) {print("WARNING NO WINTER.ENV FOUND")
     RS_WINTER =0} else {RS_WINTER =as.numeric(Res.Flow$`Base-Flow_winter`[Res.Flow$vannkrvNr == names(t1)])
     }
  
  
  if (length(Res.Flow$`Base flow_summer`[Res.Flow$vannkrvNr == names(t1)])==0  ) {print("WARNING NO SUMMER.ENV FOUND")
    RS_SUMMER =0} else  {RS_SUMMER =as.numeric(Res.Flow$`Base flow_summer`[Res.Flow$vannkrvNr == names(t1)])
    }
  
  
   #List with the name of the indicator 
  ind = c("Q","d_Q","d_Q_inc","dur_inc","d_Q_dec","dur_dec","Roc_Inc","Roc_Dec","Q_Start","Q_End","percent_Max","Flow Ratio Summer","Flow Ratio Winter")
  `%notin%` <- Negate(`%in%`)
    d_Q = vector()
    ts = vector()
    d_Q_inc = vector()
    dur_inc = vector()
    d_Q_dec = vector()
    dur_dec = vector()
    d_Qsum_inc =0
    t_sum_inc = 0
    d_Qsum_dec =0
    t_sum_dec = 0
    for (i in 2:length(t1)) {
      d_Q[i] =  coredata(t1[i])-coredata(t1[i-1])
      ts[i] = 1
    }
    #Ignoring d_Q ~ 0 to avoid extending the duration of a ramping event 
    d_Q[(abs(d_Q)/qmax )<.05] =0
    #Calculating RoC, Duration of increase 
    for (j in 1:length(d_Q)) {
      if (d_Q[j] > 0 & is.na(d_Q[j])==F | ((!d_Q[j]> 0 & is.na(d_Q[j])==F) &  ((d_Q[j+1]>0 & is.na(d_Q[j+1])== F ) & (d_Q[j-1]>0)& is.na(d_Q[j-1])== F &&length(d_Q[i-1]) == T ))) {
        d_Qsum_inc = d_Qsum_inc + (d_Q[j])
        t_sum_inc = t_sum_inc + ts[j]
        #print(d_Qsum_inc)
        if ((d_Q[j+1]<0 | is.na(d_Q[j+1])==T|d_Q[j+1]==0) &(d_Q[j+2]<0 | is.na(d_Q[j+2])==T|d_Q[j+2]==0)){d_Q_inc[j]=d_Qsum_inc 
        dur_inc[j] = t_sum_inc
        d_Qsum_inc = 0 
        t_sum_inc = 0
        next
        }
      }
      else {
        d_Q_inc[j] = NA
        dur_inc[j] = NA
      }
    }
    
    #Calculating RoC, Duration of decrease 
    for (j in 1:length(d_Q)) {
      
      if (d_Q[j] < 0 & is.na(d_Q[j])==F | ((!d_Q[j] < 0 & is.na(d_Q[j])==F) &  ((d_Q[j+1]< 0 & is.na(d_Q[j+1])==F) & (d_Q[j-1]<0 & is.na(d_Q[j-1])==F)&&length(d_Q[i-1]) == T ))) {
             d_Qsum_dec = d_Qsum_dec - (d_Q[j])
        t_sum_dec = t_sum_dec + ts[j]
        #print(d_Qsum_dec)
        if ( (d_Q[j+1]>0 | is.na(d_Q[j+1])==T|d_Q[j+1]==0) & (d_Q[j+2]>0 | is.na(d_Q[j+2])==T|d_Q[j+2]==0) ) {d_Q_dec[j]=(d_Qsum_dec) 
        dur_dec[j] = t_sum_dec
        d_Qsum_dec = 0 
        t_sum_dec = 0
        next
        }
      }
      else {
        d_Q_dec[j] = NA
        dur_dec[j] = NA
      }
    }
   
    dd = data.frame(coredata(t1),d_Q,d_Q_inc,dur_inc,d_Q_dec,dur_dec)
    dd = as.xts(dd,order.by = index(t1))
    
    # Removing negative values in D_Q inc&dec ( Extracting severe reversed  peak hidden inside long peak )
    dd_fix = vector()
    dd_fixt = vector()
    dur_fixt =vector()
    for (i in length(d_Q_inc):1) {
      if (d_Q_inc[i] < 0 & is.na(d_Q_inc[i]) == F)
      {d_Q_inc[i] = NA 
      dur_inc[i]= NA 
      for (j in i:1) {
        if (d_Q[j] < 0 & is.na(d_Q[j]) == F) {
          dd_fix[j] = d_Q[j] 
          dd_fixt[j]= as.character(index(t1[j]),format("%Y/%m/%d %H:%M"))
          dur_fixt[j] = 1
          break
        } else {
          dd_fix[i] = NA
          dd_fixt[i] = NA
          dur_fixt[i]= NA
        }
        
      }
      } 
    }
    
    dd_fixt = as.POSIXct(dd_fixt,format("%Y/%m/%d %H:%M ") )
    d_q_fix = as.xts(data.frame(na.omit(dd_fix),na.omit(dur_fixt)),order.by =na.omit(dd_fixt))
    dd$d_Q_dec[index(d_q_fix)]= -d_q_fix$na.omit.dd_fix.    
    dd$dur_dec[index(d_q_fix)]= d_q_fix$na.omit.dur_fixt.    
    
    
   
    # Removing negative values in D_Q inc&dec ( Extracting severe reversed  peak hidden inside long peak )
    dd_fix = vector()
    dd_fixt = vector()
    dur_fixt =vector()
    for (i in length(d_Q_dec):1) {
      if (d_Q_dec[i] < 0 & is.na(d_Q_dec[i]) == F)
      {d_Q_dec[i] = NA 
      dur_dec[i]= NA 
      for (j in i:1) {
        if (d_Q[j] < 0 & is.na(d_Q[j]) == F) {
          dd_fix[j] = d_Q[j] 
          dd_fixt[j]= as.character(index(t1[j]),format("%Y/%m/%d %H:%M"))
          dur_fixt[j] = 1
          break
        } else {
          dd_fix[i] = NA
          dd_fixt[i] = NA
          dur_fixt[i]= NA
        }
        
      }
      } 
    }
    
    dd_fixt = as.POSIXct(dd_fixt,format("%Y/%m/%d %H:%M ") )
    d_q_fix = as.xts(data.frame(na.omit(dd_fix),na.omit(dur_fixt)),order.by =na.omit(dd_fixt))
    dd$d_Q_inc[index(d_q_fix)]= -d_q_fix$na.omit.dd_fix.    
    dd$dur_inc[index(d_q_fix)]= d_q_fix$na.omit.dur_fixt.    
    
    dd = data.frame(coredata(t1),d_Q,d_Q_inc,dur_inc,d_Q_dec,dur_dec)
    dd = as.xts(dd,order.by = index(t1))
    
    
    Mean.Roc_Dec = d_Q_dec/dur_dec
    Mean.Roc_Inc = d_Q_inc/dur_inc
   # Roc_Dec[Roc_Dec/qmax <.1]=NA
   # Roc_Inc[Roc_Inc/qmax <.1]=NA
    # Assigning Q end 
    Q_End = vector()
    for (i in 1:nrow(t1)) {
      if  (!is.na(Mean.Roc_Dec[i]==T)|(!is.na(Mean.Roc_Inc[i]==T))) {
        Q_End[i] = t1[i]
      }
      else {Q_End[i]= NA}
    }
    #Q_start 
    Q_Start = vector()
    for (i in 1:nrow(t1)) {
      if (!is.na(Q_End[i])==T & !is.na(Mean.Roc_Dec[i])==T) {
        Q_Start[i] = Q_End[i] + d_Q_dec[i]
      }
      else if (!is.na(Q_End[i])==T & !is.na(Mean.Roc_Inc[i])==T) {
        Q_Start[i] = Q_End[i] - d_Q_inc[i]
      }
      else {Q_Start[i] = NA}
    }
    
    # Removing negative values from Q start ( because of the exclusion of d_q <5%)
    Q_Start[Q_Start<0]=0
    
    dd = cbind(dd,Mean.Roc_Inc,Mean.Roc_Dec,Q_Start,Q_End)
    
    
    #Threshold for removing Roc  (Q + base flow )
    #tts = (dd$Q_End[.indexmon(dd) %in% c(04:08)]+RS_SUMMER)
    #ttw = (dd$Q_End[.indexmon(dd) %notin% c(04:08)]+RS_WINTER)
    #ttD = rbind(tts,ttw)
    
    #tts = (dd$Q_Start[.indexmon(dd) %in% c(04:08)]+RS_SUMMER)
    #ttw = (dd$Q_Start[.indexmon(dd) %notin% c(04:08)]+RS_WINTER)
    #ttI = rbind(tts,ttw)
    
    
    
    #dd$Roc_Dec[dd$Roc_Dec < ttD] = NA
   # dd$Roc_Inc[dd$Roc_Inc < ttI] = NA 
    #Removing Q start & end for NA RoC values 
   # dd$Q_Start[is.na(dd$Roc_Inc) ==T & is.na(dd$Roc_Dec) ==T ] = NA
  #  dd$Q_End[is.na(dd$Roc_Inc) ==T & is.na(dd$Roc_Dec) ==T ] = NA
    
 #   Roc_Dec = coredata(dd$Roc_Dec)
 #   Roc_Inc = coredata(dd$Roc_Inc)
    percent_Max = vector()
    for (i in 1:length(d_Q_dec)) {
      if (!is.na(d_Q_dec[i]==T)) {
        percent_Max[i] = d_Q_dec [i] /qmax
      }
      else if (!is.na(d_Q_inc[i]==T)){
        percent_Max[i] = d_Q_inc [i] /qmax
      }
      else {percent_Max[i] = NA}
    }
    TT = cbind(dd,percent_Max)
    
   
  #Flow Ratio 
    FR_DEC_Sum = TT$d_Q_dec  [.indexmon(TT) %in% c(04:08)]/((TT$Q_End[.indexmon(TT) %in% c(04:08)]) +RS_SUMMER )
    FR_DEC_Wint= TT$d_Q_dec[.indexmon(TT) %notin% c(04:08)]/((TT$Q_End[.indexmon(TT) %notin% c(04:08)]) +RS_WINTER )
  TT$FFR_Decrease = rbind(FR_DEC_Sum,FR_DEC_Wint)
  #Setting bigger FFR classes than 10 to >10 
  #TT$FFR_Decrease[TT$FFR_Decrease>10]= 10.1
  
  FR_INC_Sum = TT$d_Q_inc[.indexmon(TT) %in% c(04:08)]/((TT$Q_Start[.indexmon(TT) %in% c(04:08)]) +RS_SUMMER )
  FR_INC_Wint = TT$d_Q_inc[.indexmon(TT) %notin% c(04:08)]/((TT$Q_Start[.indexmon(TT) %notin% c(04:08)]) +RS_SUMMER )
  TT$FFR_Increase = rbind(FR_INC_Sum,FR_INC_Wint)
  #TT$FFR_Increase[TT$FFR_Increase>10]= 10.1
  names(TT)[1] = sub("X","",names(TT)[1])
  return(TT) 
}


Num_Peaks_FR = function(t1){
  t2 = split.xts(t1,"years")
  ep = endpoints(t1 , on = "years")
  Ann.Mean  = period.apply(t1$d_Q_dec,INDEX = ep,FUN=mean,na.rm = T)
  Ann.Mean[is.infinite(Ann.Mean) ==T] = NA
  
  Ann.Max   = period.apply(t1$d_Q_dec,INDEX = ep,FUN=max,na.rm = T)
  Ann.Max[is.infinite(Ann.Max) ==T] = NA
  
  Ann.Min   = period.apply(t1$d_Q_dec,INDEX = ep,FUN=min,na.rm = T)
  Ann.Min[is.infinite(Ann.Min) ==T] = NA
  
  `FR_1.5` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease<1.5) & !is.na(TT$FFR_Decrease)==T] 
    TT3 = length(TT2$d_Q_dec)
    `FR_1.5`[i] = TT3
  }
  
  `FR_1.5-3` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >=1.5 & TT$FFR_Decrease < 3) & !is.na(TT$FFR_Decrease)==T] 
    TT3 = length(TT2$d_Q_dec)
    `FR_1.5-3`[i] = TT3
  }
  
  `FR_3-5` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >=3 & TT$FFR_Decrease < 5) & !is.na(TT$FFR_Decrease)==T] 
    TT3 = length(TT2$d_Q_dec)
    `FR_3-5`[i] = TT3
  }
  `FR_5-10` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >=5 & TT$FFR_Decrease <= 10) & !is.na(TT$FFR_Decrease)==T] 
    TT3 = length(TT2$d_Q_dec)
    `FR_5-10`[i] = TT3
  }
  `FR_>10` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >10) & !is.na(TT$FFR_Decrease)==T] 
    TT3 = length(TT2$d_Q_dec)
    `FR_>10`[i] = TT3
  }
  
  Missingvalues= vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][,1]
    Missingvalues[i] = sum(is.na(TT))/length(TT)
  }
  HrPercent= vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][,1]
    HrPercent[i] = length(TT[TT!=0 &!is.na(TT)==T])/length(TT)
  }
  
  Missingvalues = round(Missingvalues,2)
  HrPercent     = round(HrPercent,2)
  Numberpeaks =data.frame(unique(format(index(t1),"%Y")),`FR_1.5`,`FR_1.5-3`,`FR_3-5`, `FR_5-10`, `FR_>10`,coredata(Ann.Mean),coredata(Ann.Max),coredata(Ann.Min),Missingvalues,HrPercent)
  names(Numberpeaks) = c("Year","FR_1.5","FR_1.5-3","FR_3-5","FR_5-10","FR_>10","Ann.Mean","Ann.Max","Ann.Min","%Missingvalues","%Production")
  Numberpeaks = Numberpeaks[complete.cases(Numberpeaks$Ann.Mean),]
  return(Numberpeaks)
}

tes  = Num_Peaks_FR(t1)

t1 = RoC(D_XTS$`191`)
tes  = Num_Peaks_FR(t1)
t2_h = Num_Peaks_FR(t1)
tttt = Num_Peaks(t1)

# Number of Decrease peaks in specific Quantile range 
test2 = test[(test$percent_Max>=.5) & !is.na(test$Roc_Dec)==T] 
test2 = test2 [!is.na(test2$Roc_Dec)==T]
length(test2$Roc_Dec)

Num_Peaks_Dec = function(t1){
  t2 = split.xts(t1,"years")
  ep = endpoints(t1 , on = "years")
  Ann.Mean  = period.apply(t1$d_Q_dec,INDEX = ep,FUN=mean,na.rm = T)
  Ann.Mean[is.infinite(Ann.Mean) ==T] = NA
  
  Ann.Max   = period.apply(t1$d_Q_dec,INDEX = ep,FUN=max,na.rm = T)
  Ann.Max[is.infinite(Ann.Max) ==T] = NA
  
  Ann.Min   = period.apply(t1$d_Q_dec,INDEX = ep,FUN=min,na.rm = T)
  Ann.Min[is.infinite(Ann.Min) ==T] = NA
  
  `50%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.5) & !is.na(TT$d_Q_dec)==T] 
    TT3 = length(TT2$d_Q_dec)
    `50%`[i] = TT3
  }
  
  `20%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.20) & !is.na(TT$d_Q_dec)==T] 
    TT3 = length(TT2$d_Q_dec)
    `20%`[i] = TT3
  }
  
  `25%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.25) & !is.na(TT$d_Q_dec)==T] 
    TT3 = length(TT2$d_Q_dec)
    `25%`[i] = TT3
  }
  
  `10%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.10) & !is.na(TT$d_Q_dec)==T] 
    TT3 = length(TT2$d_Q_dec)
    `10%`[i] = TT3
  }
  
  `75%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.75) & !is.na(TT$d_Q_dec)==T] 
    TT3 = length(TT2$d_Q_dec)
    `75%`[i] = TT3
  }
  
  `90%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.90) & !is.na(TT$d_Q_dec)==T] 
    TT3 = length(TT2$d_Q_dec)
    `90%`[i] = TT3
  }
  Missingvalues= vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][,1]
    Missingvalues[i] = sum(is.na(TT))/length(TT)
  }
  Missingvalues = round(Missingvalues,2)
  Numberpeaks =data.frame(unique(format(index(t1),"%Y")),`10%`,`20%`,`25%`,`50%`,`75%`,`90%`,coredata(Ann.Mean),coredata(Ann.Max),coredata(Ann.Min),Missingvalues)
  names(Numberpeaks) = c("Year","10%","20%","25%","50%","75%","90%","Ann.Mean","Ann.Max","Ann.Min","%Missingvalues")
  Numberpeaks = Numberpeaks[complete.cases(Numberpeaks$Ann.Mean),]
  return(Numberpeaks)
}
Num_Peaks_Inc = function(t1){
  t2 = split.xts(t1,"years")
  ep = endpoints(t1 , on = "years")
  Ann.Mean  = period.apply(t1$d_Q_inc,INDEX = ep,FUN=mean,na.rm = T)
  Ann.Mean[is.infinite(Ann.Mean) ==T] = NA
  
  Ann.Max   = period.apply(t1$d_Q_inc,INDEX = ep,FUN=max,na.rm = T)
  Ann.Max[is.infinite(Ann.Max) ==T] = NA
  
  Ann.Min   = period.apply(t1$d_Q_inc,INDEX = ep,FUN=min,na.rm = T)
  Ann.Min[is.infinite(Ann.Min) ==T] = NA
  
  `50%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.5) & !is.na(TT$d_Q_inc)==T] 
    TT3 = length(TT2$d_Q_inc)
    `50%`[i] = TT3
  }
  
  `20%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.20) & !is.na(TT$d_Q_inc)==T] 
    TT3 = length(TT2$d_Q_inc)
    `20%`[i] = TT3
  }
  
  `25%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.25) & !is.na(TT$d_Q_inc)==T] 
    TT3 = length(TT2$d_Q_inc)
    `25%`[i] = TT3
  }
  
  `10%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.10) & !is.na(TT$d_Q_inc)==T] 
    TT3 = length(TT2$d_Q_inc)
    `10%`[i] = TT3
  }
  
  `75%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.75) & !is.na(TT$d_Q_inc)==T] 
    TT3 = length(TT2$d_Q_inc)
    `75%`[i] = TT3
  }
  
  `90%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max>=.90) & !is.na(TT$d_Q_inc)==T] 
    TT3 = length(TT2$d_Q_inc)
    `90%`[i] = TT3
  }
  Missingvalues= vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][,1]
    Missingvalues[i] = sum(is.na(TT))/length(TT)
  }
  Missingvalues = round(Missingvalues,2)
  Numberpeaks =data.frame(unique(format(index(t1),"%Y")),`10%`,`20%`,`25%`,`50%`,`75%`,`90%`,coredata(Ann.Mean),coredata(Ann.Max),coredata(Ann.Min),Missingvalues)
  names(Numberpeaks) = c("Year","10%","20%","25%","50%","75%","90%","Ann.Mean","Ann.Max","Ann.Min","%Missingvalues")
  Numberpeaks = Numberpeaks[complete.cases(Numberpeaks$Ann.Mean),]
  return(Numberpeaks)
}


t1 = RoC(D_XTS$`557`)
t2_h = Num_Peaks_Dec(t1)
tttt = Num_Peaks_Inc(t1)




plot(seq(from = 1,to =0 ,by=-0.1),quantile(t1$Roc_Dec,seq(from = 0,to =1 ,by=0.1),na.rm = T),"b")
lines(seq(from = 1,to =0 ,by=-0.1),quantile(t1$Roc_Inc,seq(from = 0,to =1 ,by=0.1),na.rm = T))


t2 = t1["2011"]

test = t2[[11]][,1]
sum(is.na(test))/length()
test2 = test[[5]]

t2[[1]]

test = subdaily2daily()



T1 = D$`38`
T1 = as.xts(T1,order.by = D$Time)
T1 = as.xts(subdaily2daily(T1,FUN = mean,na.rm = T))
names(T1) = "38"


T1_D = RoC(T1)
T2D = Num_Peaks_FR(T1_D)
