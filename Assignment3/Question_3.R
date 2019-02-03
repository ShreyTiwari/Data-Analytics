#Question 3:

# Gun violence is a growing crime especially in the United States due to more liberal Gun Laws than in other 
# parts of the world. 'GunViolence' dataset contains details of this crime. Provide a suitable visualisation
# of the same accounting for the distribution of the crimes across the country.

#The dataset has many attributes. First we drop the unnecessary ones.
#The columns are dropped based on many factors:
# 1. Relevance to the Question asked.
# 2. Can they be represented Visually?
# 3. This there enough information present in that column( Mostly empty? ).

rm(list = ls())

ds <- GunViolence
ds <- subset(ds, select = c(2:8, 15, 16, 18, 19))



###Using Plotly to visualize the data###
library(plotly)
library(dplyr)

#Geo Styling
g <- list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showland = TRUE,
          landcolor = toRGB("gray95"),
          subunitcolor = toRGB("gray85"),
          countrycolor = toRGB("gray85"),
          countrywidth = 0.5,
          subunitwidth = 0.5
         )

p1 <- plot_geo(ds, lat = ~latitude, lon = ~longitude) %>%
  #add_markers(
  # text = ~paste(date, state, paste("Killed:", n_killed), paste("Injured:", n_injured), incident_characteristics, sep = "<br />"), 
  # size = I(8),
  # hoverinfo = "text"
  #)%>%
  layout(
    title = '<br />Gun Crimes In The US', geo = g
  )

#Setting API credentials
Sys.setenv("plotly_username"="ShreyTiwari")
Sys.setenv("plotly_api_key"="Y9vIEVgM1xunNQByAO9Y")

# Creating a shareable link to the chart.
chart_link1 = api_create(p1, filename = "crimes")
chart_link1

#Bar Chart for crimes in the various states.
p2 <- plot_ly(count(ds, state), x = ~state, y = ~n)%>%
  layout(title="Barplot for different states.",  
         xaxis=list(title="State", showgrid = F),
         yaxis=list(title="Frequency", showgrid = F))

#Creating a shareable link to the chart
chart_link2 = api_create(p2, filename = "Distribution")
chart_link2

#Other inferences from the dataset are possible but here we limit ourselves to the question asked.