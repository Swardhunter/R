library(rlist)

D = read_excel("Daily Time Series.xlsx",na = c("-9999.00","-9999"),guess_max = 175320)
D$`22` = as.numeric(D$`22`)
D$`403` = as.numeric(D$`403`)
D$`102` = as.numeric(D$`102`)
D$`254` = as.numeric(D$`254`)

#Importing data
setwd("E:/OneDrive - NTNU/Research NTNU")

N = read_excel("N.xlsx",
               na = c("-9999"))

D_XTS = as.xts(D[,-1],order.by = D$Date)


DailyShutDown = function(t1) {
  storage.mode(t1) <- "numeric"
  t1[t1 < 0] = NA
  t1core= as.numeric(coredata(t1))
  qmax = as.numeric(quantile(t1core,.99,na.rm = T))
  t1core[(t1core - quantile(t1core, .99, na.rm = T)) / quantile(t1core, .99, na.rm = T) < -.99] = 0
  Cshutdown = vector()
  for (i in 1:length(t1core)) {
    if (!is.na(t1core[i])==T & t1core[i]==0) {Cshutdown[i]=1
    }
    else  {Cshutdown[i]=0}
   # else {Cshutdown[i]= NA}
  }
  #Counter
  dd = 0
  #Vector for storing the counter
  duration = vector()
  for (i in 1:length(Cshutdown)) {
    if (!is.na(Cshutdown[i])==T && Cshutdown[i] == 1) {
      dd = dd + 1
      if (Cshutdown[i + 1] == 0 && !is.na(Cshutdown[i + 1]) == T) {
        duration[i] = dd
        dd = 0
        print(paste("shutdown ends at",index(t1[i])))
        next
      }
      else if (is.na(Cshutdown[i + 1]) == T) {
        break
      }
    }
    else {duration[i]=NA}
  }
Cshutdown = as.xts(Cshutdown,order.by = index(t1))
duration = as.xts(duration,order.by = index(t1))
C25 = vector()
for (i in 1:length(t1)) {
  if (!is.na(t1core[i])==T && t1core[i]<=.25*qmax) {C25[i]=1
  }
  else   {C25[i]=0}

}
#Counter
dd = 0
#Vector for storing the counter
duration25 = vector()
for (i in 1:length(C25)) {
  if (!is.na(C25[i])==T && C25[i] == 1) {
    dd = dd + 1
    if (C25[i + 1] == 0 && !is.na(C25[i + 1]) == T) {
      duration25[i] = dd
      dd = 0
      print(paste("shutdown ends at",index(t1[i])))
      next
    }
    else if (is.na(C25[i + 1]) == T) {
      break
    }
  }
  else {duration25[i]=NA}
}
C25 = as.xts(C25,order.by = index(t1))
duration25 = as.xts(duration25,order.by = index(t1))
Final = data.frame(index((daily2annual(t1,mean,na.rm = T))),daily2annual(Cshutdown,sum,na.rm = T),daily2annual(duration,mean,na.rm = T),daily2annual(C25,sum,na.rm = T),daily2annual(duration25,mean,na.rm = T))
names(Final)= c("Year","Number of Shutdown days","Mean Annual Duration of Shutdown in Days","Number of days <25%","Mean annual duration <25%") 
Final = Final[complete.cases(Final$`Mean annual duration <25%`),]
return(Final)
}

TEST = list()
for (i in 1:ncol(D_XTS)) {
  TEST[[i]] = DailyShutDown(D_XTS[,i])
}
names(TEST)= names(D_XTS)


DailyShutDown(D_XTS$`22`)

RiversN = filter(N,`Outlet Location`=="River")
Others=  filter(N,`Outlet Location`!="River")


#Filter Lists by their names 
library(purrr)
Rivers=TEST %>% 
  keep(names(TEST) %in%RiversN$vannkrvNr )
Fj.Lake = TEST %>% 
  keep(names(TEST) %in% Others$vannkrvNr)


write.xlsx(Rivers,"DailyRivers Shutdown Analysis.xls")
write.xlsx(Fj.Lake,"DailyFL Shutdown Analysis.xls")
