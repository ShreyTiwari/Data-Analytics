rm(list=ls())

library('plyr')
library('plotly')
library('dplyr')

rain<-read.csv("C:\\Users\\shail\\Downloads\\RainfallData.csv")

rain['YYYYMM']<-as.Date(paste0(as.character(rain$YYYYMM), '01'), format='%Y%m%d')
colnames(rain)[1] <- "Timeline"

####Please note that gaps in the graph have been connected via extrapolation
karnataka<-subset(rain,Description=='Karnataka' & !is.na(Timeline))
tn<-subset(rain,Description=='Tamil Nadu' & !is.na(Timeline))
ker<-subset(rain,Description =='Kerala' & !is.na(Timeline))
aru<-subset(rain,Description =='Arunachal Praesh' & !is.na(Timeline))
bih<-subset(rain,Description =='Bihar' & !is.na(Timeline))
mad<-subset(rain,Description =='Madhya Pradesh' & !is.na(Timeline))
mah<-subset(rain,Description =='Maharashtra' & !is.na(Timeline))
ori<-subset(rain,Description =='Orissa' & !is.na(Timeline))
wes<-subset(rain,Description =='West Bengal' & !is.na(Timeline))

plot_ly(karnataka, x = ~Timeline, y = ~as.numeric(as.character(karnataka$Value)), type = 'scatter', mode = 'lines',name = 'Karnataka',line = list(color = '#5DA5DA', width = 3),connectgaps = TRUE)%>%
  add_trace(y = ~as.numeric(as.character(tn$Value)), name = 'Tamil Nadu', line = list(color = '#FAA43A', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(ker$Value)), name = 'Kerala', line = list(color = 'Green', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(aru$Value)), name = 'Arunachal Pradesh', line = list(color = 'Red', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(bih$Value)), name = 'Bihar', line = list(color = 'grey', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(mad$Value)), name = 'Madhya Pradesh', line = list(color = 'Orange', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(mah$Value)), name = 'Maharashtra', line = list(color = 'Yellow', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(ori$Value)), name = 'Orissa', line = list(color = 'Violet', width = 3))%>%
  add_trace(y = ~as.numeric(as.character(wes$Value)), name = 'West Bengal', line = list(color = 'Cyan', width = 3))%>%
layout(
  title = "Rainfall in Indian States",
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(
          count = 3,
          label = "3 Months",
          step = "month",
          stepmode = "backward"),
        list(
          count = 6,
          label = "6 Months",
          step = "month",
          stepmode = "backward"),
        list(
          count = 1,
          label = "1 Year",
          step = "year",
          stepmode = "backward"),
        list(step = "all",label="1997-2016"))),
    
    rangeslider = list(type = "date")),
  
  
  yaxis = list(karnataka['Value'],title = "Rainfall in cm"),
  updatemenus = list(
    list(
      y = 0.7,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label = "Karnataka"),
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label = "Tamil Nadu"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label = "Kerala"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)),
             label = "Arunachal Pradesh"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE)),
             label = "Bihar"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)),
             label = "Madhya Pradesh"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)),
             label = "Maharashtra"),
        list(method = "restyle",
             args = list("visible", list(FALSE, FALSE, FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE)),
             label = "Orissa"),
        list(method = "restyle",
             args = list("visible", list(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE, FALSE, TRUE)),
             label = "West Bengal")
        
        #Add lists for other states, True, false
        
        
      ))
  )
)