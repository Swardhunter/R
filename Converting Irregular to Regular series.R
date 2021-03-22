setwd("C:/Users/Sward/OneDrive - NTNU/Research NTNU")
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lubridate)
library(openxlsx)

D = X2_283_Skjak
names(X2_443_Braskereidfoss_R)[1] = "Time"

D$Time = as.POSIXct(D$Time,format("%Y/%m/%d %H:%M"))

d2 = transform(D,Time=strptime(Time, "%Y/%m/%d %H:%M"))

x = seq.POSIXt(as.POSIXlt("1966/01/01 12:00:00", format("%Y/%m/%d %H:%M:%S")),as.POSIXlt("2019/12/31 23:00" ,format("%Y/%m/%d %H:%M")),"1 hour")

X = data.frame(x)
names(X)  = c("Time")
d = merge(x = X , y = X2_443_Braskereidfoss_R , by =  'Time', all.x = TRUE)
d  = d [!duplicated(d$Time),]



Output = data.frame(Bingsfoss_LR,Dokka_L,Einunna_R,Fjaermfossen,Funnefoss_LR,Harpfoss,Kolsvik,Kongsvinger_LR,Lopet_R,Mesna,Raanaafoss_LR,
                    Reinset,Savalen_R,Skjefstadfoss_LR,Vinje)


write.xlsx(d, file="output.xlsx",  row.names=TRUE)

library(readxl)
write.xlsx(d, file="SKJAK.xlsx", sheetName="hr", row.names=TRUE)
write.xlsx(Bingsfoss, file="Bingsfoss.xlsx", sheetName="Bingsfoss",append=TRUE, row.names=TRUE)
write.xlsx(Braskereidfoss, file="Braskereidfoss.xlsx", sheetName="Braskereidfoss",append=TRUE, row.names=TRUE)
write.xlsx(Dokka, file="Dokka.xlsx", sheetName="Dokka",append=TRUE, row.names=TRUE)
write.xlsx(Einunna_R, file="Einunna_R.xlsx", sheetName="Einunna_R",append=TRUE, row.names=TRUE)
write.xlsx(Funnefoss, file="Funnefoss.xlsx", sheetName="Funnefoss",append=TRUE, row.names=TRUE)
write.xlsx(Kongsvinger, file="Kongsvinger.xlsx", sheetName="Kongsvinger",append=TRUE, row.names=TRUE)
write.xlsx(Lopet, file="Lopet.xlsx", sheetName="Lopet",append=TRUE, row.names=TRUE)
write.xlsx(Mesna, file="Mesna.xlsx", sheetName="Mesna",append=TRUE, row.names=TRUE)
write.xlsx(Raanaafoss, file="Raanaafoss.xlsx", sheetName="Raanaafoss",append=TRUE, row.names=TRUE)
write.xlsx(Reinset, file="Reinset.xlsx", sheetName="Reinset",append=TRUE, row.names=TRUE)
write.xlsx(Savalen, file="Savalen.xlsx", sheetName="Savalen",append=TRUE, row.names=TRUE)
write.xlsx(Skjefstadfoss, file="Skjefstadfoss.xlsx", sheetName="Skjefstadfoss",append=TRUE, row.names=TRUE)
write.xlsx(Vinje, file="Vinje.xlsx", sheetName="Vinje",append=TRUE, row.names=TRUE)





