#Question 2

library('plyr')
library('plotly')
library('dplyr')


#Reading Dataset
tn <- TNAgri

#Group by crop name and area
group_tn<- tn%>% group_by(Crop) %>% arrange(Crop,Area)
#Sum up the area for each crop
x<-(aggregate(group_tn$Area, by=list(Category=group_tn$Crop),FUN=sum))
x<-(setNames(x,c("Crop","Area")))
x <- x[order(x$Area),] 

#search for Total foograin in the dataset. 
#You will fill that only for 2 years it's there, and it can't be considered as a crop
#So i felt we have to filter that out, and then take the highest 5

x<-filter(x,Crop!="Total foodgrain")

top_five_all<-tail(x,5)
plot_ly(top_five_all, labels = ~Crop, values = ~Area, type = 'pie',
        marker = list(colors=c("#F15854", "#DECF3F" ,"#60BD68" ,"#FAA43A","#5DA5DA"))) %>%
  layout(title = 'Top 5 Crops in Tamil Nadu Based on Area 1997-2013',
         xaxis = list(showgrid = TRUE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


years <- unique(tn['Crop_Year'])
rice<-subset(tn,Crop=="Rice")
rice<-aggregate(rice$Area,by=list(Category=rice$Crop_Year),FUN=sum)
rice<-(setNames(rice,c("Year","Area")))

gn<-subset(tn,Crop=="Groundnut")
gn<-aggregate(gn$Area,by=list(Category=gn$Crop_Year),FUN=sum)
gn<-(setNames(gn,c("Year","Area")))

sg<-filter(tn,Crop=="Sugarcane")
sg<-aggregate(sg$Area,by=list(Category=sg$Crop_Year),FUN=sum)
sg<-(setNames(sg,c("Year","Area")))

#Need to write for loop for generic case
#Right now it's just manually filling NA and connecting the previous points
#Please tell me if there's a better method
jow<-filter(tn,Crop=="Jowar")
jow<-aggregate(jow$Area,by=list(Category=jow$Crop_Year),FUN=sum)
jow<-(setNames(jow,c("Year","Area")))
jow<-rbind(jow,c('2010',NA))
jow<-rbind(jow,c('2012',NA))
jow <- jow[order(jow$Year),]

ur<-filter(tn,Crop=="Urad")
ur<-aggregate(ur$Area,by=list(Category=ur$Crop_Year),FUN=sum)
ur<-(setNames(ur,c("Year","Area")))


plot_ly(rice, x = ~Year, y = ~Area, type = 'scatter', mode = 'lines',name = 'Rice',line = list(color = '#5DA5DA', width = 3))%>%
  add_trace(y = ~gn$Area, name = 'Groundnut', line = list(color = '#FAA43A', width = 3))%>%
  add_trace(y = ~sg$Area, name = 'Sugarcane', line = list(color = '#60BD68', width = 3))%>%
  add_trace(y = ~jow$Area, name = 'Jowar', line = list(color = '#DECF3F', width = 3),connectgaps = TRUE)%>%
  add_trace(y = ~ur$Area, name = 'Urad', line = list(color = '#F15854', width = 3))%>%
  layout(title = 'Top 5 Crops in Tamil Nadu Based on Area 1997-2013 - Trends')


