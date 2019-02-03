
df = read.csv("C:\\Users\\shail\\OneDrive\\Documents\\sem5\\Data_Analytics\\Data Analytics\\Assignments\\Assignment3\\NewYearResolution15.csv",header = TRUE,sep=",")

library('plyr')
library('plotly')

####WORDCLOUD##########
install.package("tm")
install.package("SnowBallC")
install.package("RColorBrewer")
install.package("RColorBrewer")
library("tm")
library(SnowBallC)
library("RColorBrewer")
library(wordcloud)
df.Corpus = Corpus(VectorSource(df$text))
df.Clean = tm_map(df.Corpus,PlainTextDocument)
df.Clean = tm_map(df.Corpus,tolower)
df.Clean = tm_map(df.Clean,removeNumbers)
df.Clean = tm_map(df.Clean,removePunctuation)
df.Clean = tm_map(df.Clean,stripWhitespace)
df.Clean = tm_map(df.Clean,removeWords,stopwords("english"))
df.Clean = tm_map(df.Clean,removeWords,c("fuck","shit"))
wordcloud(df.Clean,max.words=200,random.color=TRUE,random.order=FALSE,rt.per=0.35,colors=brewer.pal(8,"Dark2"))



l2 = ddply(df,.(Resolution_Category),summarize,freq=(length(Resolution_Category)))

##pie chart of resolution Categories vs tweets####

p1 <- plot_ly(l2, labels = ~Resolution_Category, values = ~freq, type = 'pie') %>%
  layout(title = 'Resolution Categories',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

###Bar plot of tweets vs resolution categories(female,male)
lm = subset(df,gender=="male")
lm = ddply(lm,.(Resolution_Category),summarize,freq=(length(Resolution_Category)))
lf = subset(df,gender=="female")
lf=ddply(lf,.(Resolution_Category),summarize,freq=(length(Resolution_Category)))
p2 <- plot_ly(lm, x = ~Resolution_Category, y = ~lm$freq, type = 'bar',name = "males")%>%
  add_trace(y = ~lf$freq, name = 'females') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'stack')





