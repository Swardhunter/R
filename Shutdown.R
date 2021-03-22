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

#Importing data
setwd("C:/Users/Sward/OneDrive - NTNU/Research NTNU")
D = read_excel("Newdatacombined.xlsx",
               na = c("-9999", "-9999.00"),
               guess_max = 175390)
N = read_excel("N.xlsx",
               na = c("-9999"))

D$`334` = as.numeric(D$`334`)
D[D < 0] = NA
D_XTS = as.xts(D[, -1], order.by = D$Time)
D_XTS_D = as.xts(subdaily2daily(D_XTS, mean, na.rm = T))

#Function for counting the shutdowns
C_shutdown = function(t1) {
  #Switching the xts to dataframe with date and value
  t1_df = data.frame(date = index(t1), coredata(t1))
  #Vector for the dischrage value
  t1_vec = as.numeric(as.character(t1_df[, 2]))
  # Removing - values
  t1_vec[t1_vec < 0] = NA
  #Removing Noise in the data (value - 99%-tile/99%-tile) < (-.99)  == 0)
  t1_vec[(t1_vec - quantile(t1_vec, .99, na.rm = T)) / quantile(t1_vec, .99, na.rm = T) < -.99] = 0
  #Ignoring NA values (It is annoying to deal with it in the if statments )
  t1_vec[is.na(t1_vec)] = -9999
  #Vector for storing the shutdowns counts
  d2 = vector()
  
  for (i in 1:length(t1)) {
    if (i == 1 & t1_vec[1] == 0) {
      d2[1] = 1
    }
    else if (i == 1 & t1_vec[1] != 0) {
      d2[1] = 0
    }
    else if (!is.na(t1_vec[i]) == T &
             t1_vec[i] == 0 & t1_vec[i - 1] != 0) {
      #print("SHUTDOWN")
      d2[i] = 1
    }
    else {
      #print("")
      d2[i] = 0
    }
  }
  #Switching the output back to an axts object
  d2_xts = xts(d2, order.by = index(t1))
  print(paste(sum(d2_xts), "ShutDowns"))
  return(d2_xts)
}

#Function for getting the duration
D_shutdown = function(t) {
  #Switching the xts to dataframe with date and value
  t1_df = data.frame(date = index(t), coredata(t))
  #Vector for the dischrage value
  t1_vec = as.numeric(as.character(t1_df[, 2]))
  #Removing - Values
  t1_vec[t1_vec < 0] = NA
  #Removing Noise in the data (value - 99%-tile/99%-tile) < (-.99)  == 0)
  t1_vec[(t1_vec - quantile(t1_vec, .99, na.rm = T)) / quantile(t1_vec, .99, na.rm = T) < -.99] = 0
  #Ignoring NA values (It is annoying to deal with it in the if statments )
  t1_vec[is.na(t1_vec)] = -9999
  #Vector for storing 0 values
  d2 = vector()
  for (i in 1:length(t1_vec)) {
    if (!is.na(t1_vec[i]) == T & t1_vec[i] == 0) {
      d2[i] = 1
    }
    else  {
      d2[i] = 0
    }
  }
  #Counter
  d1 = 0
  #Vector for storing the counter
  d3 = vector()
  for (i in 1:length(d2)) {
    if (d2[i] == 1) {
      d1 = d1 + 1
      if (d2[i + 1] == 0 & !is.na(d2[i + 1]) == T) {
        d3[i] = d1
        d1 = 0
        print(paste("shutdown ends at", t1_df[i, 1]))
        next
      }
      else if (is.na(d2[i + 1]) == T) {
        break
      }
    }
  }
  #Returning the mean of the duration (Could be adjusted if median or other quantiles is needed)
  return(mean(d3, na.rm = T))
}




#Splitting by YEARS for each power plant (output is lists for each power plant for each year)
t1 = split.xts(D_XTS, f = "year", k = 1)



#Applying the shutdown funciton
t2 = list()
for (i in 1:length(t1)) {
  s = sapply(t1[[i]], C_shutdown, simplify = TRUE, USE.NAMES = TRUE)
  t2[[i]] = s
}
#Converting lists into XTS object again
t4 = as.xts(do.call("rbind", t2), order.by = index(D_XTS))
#Summing to annual number of shutdowns
t5 = subdaily2annual(t4, sum)
View(t5)
#Hourly to daily ( Not needed )
t6 = subdaily2daily(t2, sum)
#Exporting output
write.xlsx(
  as.data.frame(t5),
  "Shutdowns_Counts.xlsx",
  col.names = T,
  row.names = T
)



#Duration of the shut down
t2 = list()
for (i in 1:length(t1)) {
  s = sapply(t1[[i]], D_shutdown, simplify = TRUE, USE.NAMES = TRUE)
  t2[[i]] = s
}
#Getting the output to dataframe with the mean year
t4_duration = as.data.frame(do.call("rbind", t2), row.names = unique(format(index(D_XTS), "%Y")))
write.xlsx(
  t4,
  "Mean_Annual_Duration_Shutdown.xlsx",
  col.names = TRUE,
  row.names = TRUE
)


#OlD script transformed (HP1,HP2,MAX RATE OF CHANGE)

#HP1_Daily
HP1_DAILY =  function(t1) {
  t1 = as.numeric(as.character(t1))
  hp1 <-
    (max(t1, na.rm = T) - min(t1, na.rm = T)) / mean(t1, na.rm = T)
  return(hp1)
}

#HP2,Max Up, Max Drop
HP2_DAILY = function(t1) {
  t1 = as.numeric(as.character(t1))
  hp2 = vector()
  findmax = vector()
  for (i in 2:length(t1)) {
    if (is.na(t1[i]) | is.na(t1[i - 1])) {
      hp2[i - 1] <- NA
      findmax[i - 1] = NA
    }
    else{
      hp2[i - 1] <- abs((t1[i] - t1[i - 1]) / 1)
      findmax[i - 1] <- (t1[i] - t1[i - 1])
    }
  }
  reshp2 <- quantile(na.omit(hp2), prob = .90)
  maxDrop <- min(findmax[findmax < 0], na.rm = T)
  #Infinity values existed because some days doesnt have any values ( max (NA)= infinity)
  maxDrop[!is.finite(maxDrop)] = NA
  
  maxUp <- max(findmax[findmax > 0], na.rm = T)
  maxUp[!is.finite(maxUp)] = NA
  
  return(c(reshp2, maxDrop, maxUp))
}

#Getting the index for hour to days
ep = endpoints(D_XTS, on = "days")

#HP1_DAILY
HP1 = list()
for (i in 1:ncol(D_XTS)) {
  HP1[[i]] = period.apply(D_XTS[, i], INDEX = ep, FUN = HP1_DAILY)
}
HP1 = do.call("cbind", HP1)
names(HP1) = names(D_XTS)


# HP2_DAILY
HP2 = list()
for (i in 1:ncol(D_XTS)) {
  HP2[[i]] = period.apply(D_XTS[, i], INDEX = ep, FUN = HP2_DAILY)
}

#Getting HP2_DAILY
HP2_D = list()
for (i in 1:length(HP2)) {
  HP2_D[[i]] = HP2[[i]][, 1]
}
HP2_D = do.call("cbind", HP2_D)
names(HP2_D) = names(D_XTS)

#Max Down
Max_Dn = list()
for (i in 1:length(HP2)) {
  Max_Dn[[i]] = HP2[[i]][, 2]
}
Max_Dn = do.call("cbind", Max_Dn)
names(Max_Dn) = names(D_XTS)
#Max Up
Max_Up = list()
for (i in 1:length(HP2)) {
  Max_Up[[i]] = HP2[[i]][, 3]
}
Max_Up = do.call("cbind", Max_Up)
names(Max_Up) = names(D_XTS)

#HP1 ANNUAL (MEDIAN)

HP1_Annual = daily2annual(HP1, median, na.rm = T)
HP2_Annual = daily2annual(HP2_D, median, na.rm = T)
View(HP1_Annual)
View(HP2_Annual)



# List containing all the results
Results = list(
  as.data.frame(HP1),
  as.data.frame(HP2_D),
  as.data.frame(Max_Dn),
  as.data.frame(Max_Up),
  as.data.frame(HP1_Annual),
  as.data.frame(HP2_Annual),
  as.data.frame(t4),
  t4_duration
)

#Export results
write.xlsx(
  Results,
  file = "dataresults_v2.xlsx",
  sheetName = c(
    "HP1_Daily",
    "HP2_Daily",
    "Max_Dn",
    "Max_Up",
    "HP1_Annual",
    "HP2_Annual",
    "Shutdowns Count",
    "Shutdown duration"
  ),
  col.names = TRUE,
  row.names = TRUE
)






#Data
setwd("C:/Users/Sward/OneDrive - NTNU/Research NTNU")
D = read_excel("Newdatacombined.xlsx",
               na = c("-9999", "-9999.00"),
               guess_max = 175390)
N = read_excel("N.xlsx",
               na = c("-9999"))
#Min Flow
Res.Flow = read_excel(
  "Base_flow_levels.xlsx",
  sheet = "Complete Data",
  na = c("-9999", "-9999.00"),
  guess_max = 175390
)
Res.Flow$`Base flow_summer` = as.numeric(Res.Flow$`Base flow_summer`)
Res.Flow$`Base-Flow_winter` = as.numeric(Res.Flow$`Base-Flow_winter`)
#Importing Data
N = filter(N, `Outlet Location` == "R")
N = merge(N,
          Res.Flow,
          by.x = "Number",
          by.y = "vannkrvNr",
          all.x = T)
N = data.frame(N$Number,
               N$`HP Type`,
               N$`Base flow_summer`,
               N$`Base-Flow_winter`)
names(N) = c("Number", "HP Type", "Base flow_summer", "Base-Flow_winter")
d = melt(D, id.vars = "Time")
d = filter(d, variable %in%   N$Number[N$`HP Type` == "Reservoir"] &
             variable %in% Res.Flow$vannkrvNr)
d$value = as.numeric(d$value)
d = dcast(d, Time ~ variable, value.var = "value")
D_XTS = as.xts(D[, -1], order.by = D$Time)

D2 = as.xts(data.frame(D$`383`, D$`260`, D$`181`), order.by = D$Time)
D_XTS = cbind(D_XTS, D2)
names(D_XTS) = sub("D..", "", names(D_XTS))

D_XTS[D_XTS < 0] = NA




#NEW SCRIPT FOR THE RoC FFR RATIO WORKS IF ONLY Res.Flow  IS INSERTED Otherwise it issue a warning there is no env.FLOW
RoC = function(t1) {
  storage.mode(t1) <- "numeric"
  t1[t1 < 0] = NA
  #Max Q throughout the whole time series
  qmax = as.numeric(quantile(coredata(t1), .99, na.rm = T))
  t1[(t1 / qmax < .01)] = 0
  #Env. Flow
  if (length(Res.Flow$`Base-Flow_winter`[Res.Flow$vannkrvNr == names(t1)]) ==
      0) {
    print("WARNING NO WINTER.ENV FOUND")
    RS_WINTER = 0
  } else {
    RS_WINTER = as.numeric(Res.Flow$`Base-Flow_winter`[Res.Flow$vannkrvNr == names(t1)])
  }
  
  
  if (length(Res.Flow$`Base flow_summer`[Res.Flow$vannkrvNr == names(t1)]) ==
      0) {
    print("WARNING NO SUMMER.ENV FOUND")
    RS_SUMMER = 0
  } else  {
    RS_SUMMER = as.numeric(Res.Flow$`Base flow_summer`[Res.Flow$vannkrvNr == names(t1)])
  }
  
  
  #List with the name of the indicator
  ind = c(
    "Q",
    "d_Q",
    "d_Q_inc",
    "dur_inc",
    "d_Q_dec",
    "dur_dec",
    "Roc_Inc",
    "Roc_Dec",
    "Q_Start",
    "Q_End",
    "percent_Max",
    "Flow Ratio Summer",
    "Flow Ratio Winter"
  )
  `%notin%` <- Negate(`%in%`)
  d_Q = vector()
  ts = vector()
  d_Q_inc = vector()
  dur_inc = vector()
  d_Q_dec = vector()
  dur_dec = vector()
  d_Qsum_inc = 0
  t_sum_inc = 0
  d_Qsum_dec = 0
  t_sum_dec = 0
  for (i in 2:length(t1)) {
    d_Q[i] =  coredata(t1[i]) - coredata(t1[i - 1])
    ts[i] = 1
  }
  #Ignoring d_Q ~ 0 to avoid extending the duration of a ramping event
  d_Q[(abs(d_Q) / qmax) < .05] = 0
  #Calculating RoC, Duration of increase
  for (j in 1:length(d_Q)) {
    if (d_Q[j] > 0 &
        is.na(d_Q[j]) == F |
        ((!d_Q[j] > 0 &
          is.na(d_Q[j]) == F) &
         ((d_Q[j + 1] > 0 &
           is.na(d_Q[j + 1]) == F) &
          (d_Q[j - 1] > 0) & is.na(d_Q[j - 1]) == F &&
          length(d_Q[i - 1]) == T
         ))) {
      d_Qsum_inc = d_Qsum_inc + (d_Q[j])
      t_sum_inc = t_sum_inc + ts[j]
      #print(d_Qsum_inc)
      if ((d_Q[j + 1] < 0 |
           is.na(d_Q[j + 1]) == T |
           d_Q[j + 1] == 0) &
          (d_Q[j + 2] < 0 |
           is.na(d_Q[j + 2]) == T | d_Q[j + 2] == 0)) {
        d_Q_inc[j] = d_Qsum_inc
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
    if (d_Q[j] < 0 &
        is.na(d_Q[j]) == F |
        ((!d_Q[j] < 0 &
          is.na(d_Q[j]) == F) &
         ((d_Q[j + 1] < 0 &
           is.na(d_Q[j + 1]) == F) &
          (d_Q[j - 1] < 0 & is.na(d_Q[j - 1]) == F) &&
          length(d_Q[i - 1]) == T))) {
      d_Qsum_dec = d_Qsum_dec - (d_Q[j])
      t_sum_dec = t_sum_dec + ts[j]
      #print(d_Qsum_dec)
      if ((d_Q[j + 1] > 0 |
           is.na(d_Q[j + 1]) == T |
           d_Q[j + 1] == 0) &
          (d_Q[j + 2] > 0 |
           is.na(d_Q[j + 2]) == T | d_Q[j + 2] == 0)) {
        d_Q_dec[j] = (d_Qsum_dec)
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
  
  dd = data.frame(coredata(t1), d_Q, d_Q_inc, dur_inc, d_Q_dec, dur_dec)
  dd = as.xts(dd, order.by = index(t1))
  
  # Removing negative values in D_Q inc&dec ( Extracting severe reversed  peak hidden inside long peak )
  dd_fix = vector()
  dd_fixt = vector()
  dur_fixt = vector()
  for (i in length(d_Q_inc):1) {
    if (d_Q_inc[i] < 0 & is.na(d_Q_inc[i]) == F)
    {
      d_Q_inc[i] = NA
      dur_inc[i] = NA
      for (j in i:1) {
        if (d_Q[j] < 0 & is.na(d_Q[j]) == F) {
          dd_fix[j] = d_Q[j]
          dd_fixt[j] = as.character(index(t1[j]), format("%Y/%m/%d %H:%M"))
          dur_fixt[j] = 1
          break
        } else {
          dd_fix[i] = NA
          dd_fixt[i] = NA
          dur_fixt[i] = NA
        }
        
      }
    }
  }
  
  dd_fixt = as.POSIXct(dd_fixt, format("%Y/%m/%d %H:%M "))
  d_q_fix = as.xts(data.frame(na.omit(dd_fix), na.omit(dur_fixt)), order.by =
                     na.omit(dd_fixt))
  dd$d_Q_dec[index(d_q_fix)] = -d_q_fix$na.omit.dd_fix.
  dd$dur_dec[index(d_q_fix)] = d_q_fix$na.omit.dur_fixt.
  
  
  
  # Removing negative values in D_Q inc&dec ( Extracting severe reversed  peak hidden inside long peak )
  dd_fix = vector()
  dd_fixt = vector()
  dur_fixt = vector()
  for (i in length(d_Q_dec):1) {
    if (d_Q_dec[i] < 0 & is.na(d_Q_dec[i]) == F)
    {
      d_Q_dec[i] = NA
      dur_dec[i] = NA
      for (j in i:1) {
        if (d_Q[j] < 0 & is.na(d_Q[j]) == F) {
          dd_fix[j] = d_Q[j]
          dd_fixt[j] = as.character(index(t1[j]), format("%Y/%m/%d %H:%M"))
          dur_fixt[j] = 1
          break
        } else {
          dd_fix[i] = NA
          dd_fixt[i] = NA
          dur_fixt[i] = NA
        }
        
      }
    }
  }
  
  dd_fixt = as.POSIXct(dd_fixt, format("%Y/%m/%d %H:%M "))
  d_q_fix = as.xts(data.frame(na.omit(dd_fix), na.omit(dur_fixt)), order.by =
                     na.omit(dd_fixt))
  dd$d_Q_inc[index(d_q_fix)] = -d_q_fix$na.omit.dd_fix.
  dd$dur_inc[index(d_q_fix)] = d_q_fix$na.omit.dur_fixt.
  
  dd = data.frame(coredata(t1), d_Q, d_Q_inc, dur_inc, d_Q_dec, dur_dec)
  dd = as.xts(dd, order.by = index(t1))
  
  
  Mean.Roc_Dec = d_Q_dec / dur_dec
  Mean.Roc_Inc = d_Q_inc / dur_inc
  # Roc_Dec[Roc_Dec/qmax <.1]=NA
  # Roc_Inc[Roc_Inc/qmax <.1]=NA
  # Assigning Q end
  Q_End = vector()
  for (i in 1:nrow(t1)) {
    if (!is.na(Mean.Roc_Dec[i] == T) | (!is.na(Mean.Roc_Inc[i] == T))) {
      Q_End[i] = t1[i]
    }
    else {
      Q_End[i] = NA
    }
  }
  #Q_start
  Q_Start = vector()
  for (i in 1:nrow(t1)) {
    if (!is.na(Q_End[i]) == T & !is.na(Mean.Roc_Dec[i]) == T) {
      Q_Start[i] = Q_End[i] + d_Q_dec[i]
    }
    else if (!is.na(Q_End[i]) == T & !is.na(Mean.Roc_Inc[i]) == T) {
      Q_Start[i] = Q_End[i] - d_Q_inc[i]
    }
    else {
      Q_Start[i] = NA
    }
  }
  
  # Removing negative values from Q start ( because of the exclusion of d_q <5%)
  Q_Start[Q_Start < 0] = 0
  
  dd = cbind(dd, Mean.Roc_Inc, Mean.Roc_Dec, Q_Start, Q_End)
  
  
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
    if (!is.na(d_Q_dec[i] == T)) {
      percent_Max[i] = d_Q_dec [i] / qmax
    }
    else if (!is.na(d_Q_inc[i] == T)) {
      percent_Max[i] = d_Q_inc [i] / qmax
    }
    else {
      percent_Max[i] = NA
    }
  }
  TT = cbind(dd, percent_Max)
  
  
  #Flow Ratio
  FR_DEC_Sum = TT$d_Q_dec  [.indexmon(TT) %in% c(04:08)] / ((TT$Q_End[.indexmon(TT) %in% c(04:08)]) +
                                                              RS_SUMMER)
  FR_DEC_Wint = TT$d_Q_dec[.indexmon(TT) %notin% c(04:08)] / ((TT$Q_End[.indexmon(TT) %notin% c(04:08)]) +
                                                                RS_WINTER)
  TT$FFR_Decrease = rbind(FR_DEC_Sum, FR_DEC_Wint)
  #Setting bigger FFR classes than 10 to >10
  #TT$FFR_Decrease[TT$FFR_Decrease>10]= 10.1
  
  FR_INC_Sum = TT$d_Q_inc[.indexmon(TT) %in% c(04:08)] / ((TT$Q_Start[.indexmon(TT) %in% c(04:08)]) +
                                                            RS_SUMMER)
  FR_INC_Wint = TT$d_Q_inc[.indexmon(TT) %notin% c(04:08)] / ((TT$Q_Start[.indexmon(TT) %notin% c(04:08)]) +
                                                                RS_SUMMER)
  TT$FFR_Increase = rbind(FR_INC_Sum, FR_INC_Wint)
  #TT$FFR_Increase[TT$FFR_Increase>10]= 10.1
  names(TT)[1] = sub("X", "", names(TT)[1])
  return(TT)
}

#Classification of the FFR class
Num_Peaks_FR = function(t1) {
  t2 = split.xts(t1, "years")
  ep = endpoints(t1 , on = "years")
  Ann.Mean  = period.apply(t1$d_Q_dec,
                           INDEX = ep,
                           FUN = mean,
                           na.rm = T)
  Ann.Mean[is.infinite(Ann.Mean) == T] = NA
  
  Ann.Max   = period.apply(t1$d_Q_dec,
                           INDEX = ep,
                           FUN = max,
                           na.rm = T)
  Ann.Max[is.infinite(Ann.Max) == T] = NA
  
  Ann.Min   = period.apply(t1$d_Q_dec,
                           INDEX = ep,
                           FUN = min,
                           na.rm = T)
  Ann.Min[is.infinite(Ann.Min) == T] = NA
  
  `FR_1.5` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease < 1.5) & !is.na(TT$FFR_Decrease) == T]
    TT3 = length(TT2$d_Q_dec)
    `FR_1.5`[i] = TT3
  }
  
  `FR_1.5-3` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >= 1.5 &
                TT$FFR_Decrease < 3) & !is.na(TT$FFR_Decrease) == T]
    TT3 = length(TT2$d_Q_dec)
    `FR_1.5-3`[i] = TT3
  }
  
  `FR_3-5` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >= 3 &
                TT$FFR_Decrease < 5) & !is.na(TT$FFR_Decrease) == T]
    TT3 = length(TT2$d_Q_dec)
    `FR_3-5`[i] = TT3
  }
  `FR_5-10` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease >= 5 &
                TT$FFR_Decrease <= 10) &
               !is.na(TT$FFR_Decrease) == T]
    TT3 = length(TT2$d_Q_dec)
    `FR_5-10`[i] = TT3
  }
  `FR_>10` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$FFR_Decrease > 10) & !is.na(TT$FFR_Decrease) == T]
    TT3 = length(TT2$d_Q_dec)
    `FR_>10`[i] = TT3
  }
  
  Missingvalues = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][, 1]
    Missingvalues[i] = sum(is.na(TT)) / length(TT)
  }
  HrPercent = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][, 1]
    HrPercent[i] = length(TT[TT != 0 &
                               !is.na(TT) == T]) / length(TT)
  }
  
  Missingvalues = round(Missingvalues, 2)
  HrPercent     = round(HrPercent, 2)
  Numberpeaks = data.frame(
    unique(format(index(t1), "%Y")),
    `FR_1.5`,
    `FR_1.5-3`,
    `FR_3-5`,
    `FR_5-10`,
    `FR_>10`,
    coredata(Ann.Mean),
    coredata(Ann.Max),
    coredata(Ann.Min),
    Missingvalues,
    HrPercent
  )
  names(Numberpeaks) = c(
    "Year",
    "FR_1.5",
    "FR_1.5-3",
    "FR_3-5",
    "FR_5-10",
    "FR_>10",
    "Ann.Mean",
    "Ann.Max",
    "Ann.Min",
    "%Missingvalues",
    "%Production"
  )
  Numberpeaks = Numberpeaks[complete.cases(Numberpeaks$Ann.Mean), ]
  
  #Flow Ramping Ratio Annual Classification
  #High Ecological status class
  Numberpeaks$YearRate[Numberpeaks$FR_1.5 < 10 &
                         Numberpeaks$FR_1.5 > 0] = "H"
  Numberpeaks$YearRate[Numberpeaks$FR_1.5 >= 10 &
                         Numberpeaks$FR_1.5 < 20] = "H"
  Numberpeaks$YearRate[Numberpeaks$`FR_1.5-3` < 10 &
                         Numberpeaks$`FR_1.5-3` > 0] = "H"
  
  #Good Ecological status class
  Numberpeaks$YearRate[Numberpeaks$FR_1.5 >= 20] = "G"
  Numberpeaks$YearRate[Numberpeaks$`FR_1.5-3` >= 10 &
                         Numberpeaks$FR_1.5 < 150] = "G"
  Numberpeaks$YearRate[Numberpeaks$`FR_3-5` < 20 &
                         Numberpeaks$`FR_3-5` > 0] = "G"
  Numberpeaks$YearRate[Numberpeaks$`FR_5-10` < 10 &
                         Numberpeaks$`FR_5-10` > 0] = "G"
  
  #Moderate Ecological Status class
  Numberpeaks$YearRate[Numberpeaks$`FR_1.5-3` >= 150] = "M"
  Numberpeaks$YearRate[Numberpeaks$`FR_3-5` >= 20 &
                         Numberpeaks$`FR_3-5` < 100] = "M"
  Numberpeaks$YearRate[Numberpeaks$`FR_5-10` >= 10 &
                         Numberpeaks$`FR_5-10` < 20] = "M"
  Numberpeaks$YearRate[Numberpeaks$`FR_>10` < 10 &
                         Numberpeaks$`FR_>10` > 0] = "M"
  
  #Bad Ecological Status class
  Numberpeaks$YearRate[Numberpeaks$`FR_3-5` >= 100 &
                         Numberpeaks$`FR_3-5` < 200] = "B"
  Numberpeaks$YearRate[Numberpeaks$`FR_5-10` >= 20 &
                         Numberpeaks$`FR_5-10` < 200] = "B"
  Numberpeaks$YearRate[Numberpeaks$`FR_>10` >= 10 &
                         Numberpeaks$`FR_>10` < 50] = "B"
  
  #Severe Ecological Status class
  Numberpeaks$YearRate[Numberpeaks$`FR_3-5` >= 200] = "P"
  Numberpeaks$YearRate[Numberpeaks$`FR_5-10` >= 200] = "P"
  Numberpeaks$YearRate[Numberpeaks$`FR_>10` >= 50] = "P"
  return(Numberpeaks)
}

#Number of peaks classified to Max Q
Num_Peaks_Dec = function(t1) {
  t2 = split.xts(t1, "years")
  ep = endpoints(t1 , on = "years")
  Ann.Mean  = period.apply(t1$d_Q_dec,
                           INDEX = ep,
                           FUN = mean,
                           na.rm = T)
  Ann.Mean[is.infinite(Ann.Mean) == T] = NA
  
  Ann.Max   = period.apply(t1$d_Q_dec,
                           INDEX = ep,
                           FUN = max,
                           na.rm = T)
  Ann.Max[is.infinite(Ann.Max) == T] = NA
  
  Ann.Min   = period.apply(t1$d_Q_dec,
                           INDEX = ep,
                           FUN = min,
                           na.rm = T)
  Ann.Min[is.infinite(Ann.Min) == T] = NA
  
  `50%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .5) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `50%`[i] = TT3
  }
  
  `20%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .20) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `20%`[i] = TT3
  }
  
  `25%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .25) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `25%`[i] = TT3
  }
  
  `10%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .10) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `10%`[i] = TT3
  }
  
  `75%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .75) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `75%`[i] = TT3
  }
  
  `90%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .90) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `90%`[i] = TT3
  }
  `95%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .95) & !is.na(TT$d_Q_dec) == T]
    TT3 = length(TT2$d_Q_dec)
    `95%`[i] = TT3
  }
  Missingvalues = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][, 1]
    Missingvalues[i] = sum(is.na(TT)) / length(TT)
  }
  Missingvalues = round(Missingvalues, 2)
  Numberpeaks = data.frame(
    unique(format(index(t1), "%Y")),
    `10%`,
    `20%`,
    `25%`,
    `50%`,
    `75%`,
    `90%`,
    `95%`,
    coredata(Ann.Mean),
    coredata(Ann.Max),
    coredata(Ann.Min),
    Missingvalues,
    names(t1[, 1])
  )
  names(Numberpeaks) = c(
    "Year",
    "10%",
    "20%",
    "25%",
    "50%",
    "75%",
    "90%",
    "95%",
    "Ann.Mean",
    "Ann.Max",
    "Ann.Min",
    "%Missingvalues",
    "HP.NUMBER"
  )
  Numberpeaks = Numberpeaks[complete.cases(Numberpeaks$Ann.Mean), ]
  return(Numberpeaks)
}
Num_Peaks_Inc = function(t1) {
  t2 = split.xts(t1, "years")
  ep = endpoints(t1 , on = "years")
  Ann.Mean  = period.apply(t1$d_Q_inc,
                           INDEX = ep,
                           FUN = mean,
                           na.rm = T)
  Ann.Mean[is.infinite(Ann.Mean) == T] = NA
  
  Ann.Max   = period.apply(t1$d_Q_inc,
                           INDEX = ep,
                           FUN = max,
                           na.rm = T)
  Ann.Max[is.infinite(Ann.Max) == T] = NA
  
  Ann.Min   = period.apply(t1$d_Q_inc,
                           INDEX = ep,
                           FUN = min,
                           na.rm = T)
  Ann.Min[is.infinite(Ann.Min) == T] = NA
  
  `50%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .5) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `50%`[i] = TT3
  }
  
  `20%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .20) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `20%`[i] = TT3
  }
  
  `25%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .25) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `25%`[i] = TT3
  }
  
  `10%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .10) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `10%`[i] = TT3
  }
  
  `75%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .75) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `75%`[i] = TT3
  }
  
  `90%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .90) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `90%`[i] = TT3
  }
  `95%` = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]]
    TT2 = TT[(TT$percent_Max >= .95) & !is.na(TT$d_Q_inc) == T]
    TT3 = length(TT2$d_Q_inc)
    `95%`[i] = TT3
  }
  Missingvalues = vector()
  for (i in 1:length(t2)) {
    TT = t2[[i]][, 1]
    Missingvalues[i] = sum(is.na(TT)) / length(TT)
  }
  Missingvalues = round(Missingvalues, 2)
  Numberpeaks = data.frame(
    unique(format(index(t1), "%Y")),
    `10%`,
    `20%`,
    `25%`,
    `50%`,
    `75%`,
    `90%`,
    `95%`,
    coredata(Ann.Mean),
    coredata(Ann.Max),
    coredata(Ann.Min),
    Missingvalues,
    names(t1[, 1])
  )
  names(Numberpeaks) = c(
    "Year",
    "10%",
    "20%",
    "25%",
    "50%",
    "75%",
    "90%",
    "95%",
    "Ann.Mean",
    "Ann.Max",
    "Ann.Min",
    "%Missingvalues",
    "HP.NUMBER",
    
  )
  Numberpeaks = Numberpeaks[complete.cases(Numberpeaks$Ann.Mean), ]
  return(Numberpeaks)
}


#Applying functions
T1 = list()
for (i in 1:ncol(D_XTS)) {
  T1[[i]] =  RoC(D_XTS[, i])
  
}


T2 = lapply(T1, Num_Peaks_FR)
T2 = lapply(T1, Num_Peaks_Dec)
names(T2) = names(D_XTS)
for (i in 1:length(T2)) {
  row.names(T2[[i]]) = names(D_XTS)
  
}

#Merging the results into 1 dataframe
TT = Reduce(function(x, y)
  merge(x, y, all = TRUE), T2)
TT = melt(TT, id.vars = c("HP.NUMBER", "Year"))
TT$Year = as.Date(ISOdate(TT$Year, 1, 1))
TT = merge(TT, N, by.x = "HP.NUMBER", by.y = "vannkrvNr")
TT = merge(TT, Ranking, by = "HP.NUMBER")
TT$HP.NUMBER =  reorder(TT$HP.NUMBER, -1 * as.numeric(as.character(TT$`95%`)))

TT_R = filter(TT, `Outlet Location` == "River")
TT_FL = filter(TT, `Outlet Location` != "River")

#Plotting the trend of the annual number of peaks
ggplot(TT_FL,
       aes(
         x  = Year,
         y = value,
         shape = variable,
         color = `Outlet Location`
       )) + geom_point() + facet_wrap(. ~ HP.NUMBER) + labs(y = "Annual No. Peaks", shape = "Annual No. Peaks >") +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    size = 8
  ),
  legend.position = "bottom")


#Hourly to daily data
D_XTS_D = as.xts(subdaily2daily(D_XTS, mean, na.rm = T))
#Applying functions
T1_D = list()
for (i in 1:ncol(D_XTS_D)) {
  T1_D[[i]] =  RoC(D_XTS_D[, i])
  
}


T2_D = lapply(T1_D, Num_Peaks_FR)
write.xlsx(T2_D, "FFR results_D.xlsx", sheetName = names(D_XTS_D))


#Duration curves AND SOME PLOTTING
DR = list()
for (i in 1:length(T1)) {
  DR[[i]] = T1[[i]][, 12]
}
DR = do.call("cbind", DR)
names(DR) = names(D_XTS)

DR[DR > 15] = 15
#Quantiles AND SOME PLOTTING
qq = lapply(DR, quantile, seq(1, 0, by = -0.1), na.rm = T)
qq2 = do.call("cbind", qq)
qq2 = melt(qq2)
names(qq2) = c("Quantile", "HP.NUMBER", "value")
D = merge(qq2,
          N,
          by.x = "HP.NUMBER",
          by.y = "Number",
          all.x = T)
ggplot(data = D,
       aes(
         x = Quantile,
         y = value,
         group = `HP.NUMBER`,
         color = `HP Type`
       )) + geom_xspline(spline_shape = .5) + scale_x_discrete(name = "Time %", labels = as.character(seq(0, 100, 10))) +
  scale_y_continuous(name = "FRR Level") + theme(legend.position = "bottom")






# SOME PLOTTING
DR_M = melt(fortify(DR), id.vars = "Index")
DR_M$value[DR_M$value > 15] = 15
DR_FDC =  fdc(DR, plot = F)
DR_FDC = melt(DR_FDC)
DR_FDC = DR_FDC[, -1]
DR_M = cbind(DR_FDC$Var2, DR_FDC$value, DR_M$value)
DR_M = as.data.frame(DR_M)
names(DR_M) = c("HP.NUMBER", "FDC", "value")
N = read_excel("Flow Ramping Chrac.xlsx",
               sheet = "Hourly_Data_Analysis (3.1)",
               na = c("-9999"))

D = merge(DR_M,
          N,
          by.x = "HP.NUMBER",
          by.y = "Number",
          all.x = T)
D.R = filter(D, `HP Type` == "Reservoir")
theme_set(theme_bw(base_size = 15))
# HP TYPE PLOTTING
ggplot(data = D.R,
       aes(
         x = FDC,
         y = value,
         group = HP.NUMBER,
         color = Restrictions
       )) +  geom_line() +
  scale_y_continuous(limits = c(0, 15)) + theme(legend.position = "bottom") +
  labs(y = "FRR level", color = "Restrictions")
ggsave("FRR_DURATION.png", dpi = 2000)





#X-Y Hourly-Daily data CORRELATION
for (i in 1:length(T2_D)) {
  names(T2_D[[i]])[c(2:6)] = sub("Daily", "", names(T2[[i]])[c(2:6)])
}


T2_COMB = list()
for (i in 1:length(T2_D)) {
  T2_COMB[[i]] = merge(
    melt(
      T2[[i]][, c(1:6)],
      id.vars = "Year",
      variable.name = "FR_Hour",
      value.name = "Hourly_Value"
    ),
    melt(
      T2_D[[i]][, c(1:6)],
      id.vars = "Year",
      variable.name = "FR_Daily",
      value.name = "Daily_Value"
    ) ,
    by.x = c("Year", "FR_Hour"),
    by.y = c("Year", "FR_Daily"),
    all = T
  )
  
}



T2_BIND  = do.call("rbind", T2_COMB)

FR3_5 = filter(T2_COMB[[62]], T2_COMB[[62]][, 2] == "FR_3-5")
FR5_10 = filter(T2_BIND, T2_BIND$FR_Hour == "FR_5-10")
FR10 = filter(T2_BIND, T2_BIND$FR_Hour == "FR_>10")
plot(FR3_5$Hourly_Value, FR3_5$Daily_Value)
MFR3_5 = lm(FR3_5$Hourly_Value ~ FR3_5$Daily_Value)
BC = boxcox_trans(FR3_5$Hourly_Value ~ FR3_5$Daily_Value)



ggplot(test , aes(Hourly_Value, Daily_Value)) + geom_point() + geom_smooth()

scatter.smooth(x = T2_COMB[[4]][, 3], y = T2_COMB[[4]][, 4], main = "Hourly ~ Daily")


t2_comb_filtered
for (i in 1:length(T2_COMB)) {
  T2_COMB[[i]][T2_COMB[, 2] == "FR_1.5", ] = NA
}
T2_COMB

Correlation = vector()
for (i in 1:length(T2_COMB)) {
  Correlation[i] = cor(T2_COMB[[i]][, 3], T2_COMB[[i]][, 4])
}
Correlation = data.frame(Correlation, names(D_XTS))
names(Correlation)[2] = "HP Number"


for (i in 1:length(T2_COMB)) {
  scatter.smooth(
    x = T2_COMB[[i]][, 3],
    y = T2_COMB[[i]][, 4] ,
    main = paste("Hourly ~ Daily-", names(D_XTS_D[, i]))
  )
}


M1 = list()
for (i in 1:length(T2_COMB)) {
  M1[[i]] = T2_COMB[[i]]
  
}



# Function for delta Q for the daily data
DQ = function(t1) {
  d_Q = vector()
  coredata(t1) = as.numeric(coredata(t1))
  for (i in 2:length(t1)) {
    d_Q[i] =  (coredata(t1[i])) - coredata(t1[i - 1])
    
  }
  return(d_Q)
}
D_XTS = as.xts((D_Daily[, -1]), order.by = D_Daily$Date)
coredata(D_XTS) = as.numeric(as.character(coredata(D_XTS)))

D_XTS_D = lapply(D_Daily, DQ)
D_XTS_D =  do.call("cbind", D_XTS_D)
D_XTS_D = as.data.frame(D_XTS_D)
D_XTS_D$dts = index(D_Daily)
#dECREASE ONLY
D_DN = D_XTS_D[D_XTS_D < 0, ]


#Combining the all the daily data available
D_Daily <-
  read_excel(
    "Daily Time Series.xlsx",
    guess_max = 177500,
    na = c("#N/A", "-9999.00", "-9999", "#DIV/0!")
  )
str(D_Daily)
DD2 = cbind(
  D_XTS$`23`,
  D_XTS$`44`,
  D_XTS$`78`,
  D_XTS$`141`,
  D_XTS$`162`,
  D_XTS$`177`,
  D_XTS$`181`,
  D_XTS$`191`,
  D_XTS$`239`,
  D_XTS$`21`,
  D_XTS$`537`,
  D_XTS$`306`,
  D_XTS$`331`,
  D_XTS$`334`,
  D_XTS$`391`,
  D_XTS$`455`,
  D_XTS$`751`,
  D_XTS$`848`,
  D_XTS$`1168`,
  D_XTS$`1182`
)
names(DD2) = sub("X", "", names(DD2))


D_XTS = cbind(D_XTS, DD2)

D_Daily$`22` = as.numeric(D_Daily$`22`)
D_Daily$`403` = as.numeric(D_Daily$`403`)
D_Daily$`102` = as.numeric(D_Daily$`102`)
D_Daily$`254` = as.numeric(D_Daily$`254`)

D_Daily = transform(D_Daily, RDateTime = strptime(Date, "%Y-%m-%d"))
D_Daily = as.xts(D_Daily[, -1], order.by = as.POSIXct(TEST, format = "%Y-%m-%d"))
D_Daily = cbind(D_Daily, DD2)
names(D_Daily) = sub("X", "", names(D_Daily))
