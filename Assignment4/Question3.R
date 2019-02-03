#Question 3

#Reading the datasets:
df16 = read.csv("C:\\Users\\shail\\OneDrive\\Documents\\sem5\\Data_Analytics\\Data Analytics\\Assignments\\Assignment4\\Data2016.csv")
df15 = read.csv("C:\\Users\\shail\\OneDrive\\Documents\\sem5\\Data_Analytics\\Data Analytics\\Assignments\\Assignment4\\Data2015.csv")
df17 = read.csv("C:\\Users\\shail\\OneDrive\\Documents\\sem5\\Data_Analytics\\Data Analytics\\Assignments\\Assignment4\\Data2017.csv")

#Finding the common countries:
commonCountries = intersect(intersect(df15$Country,df16$Country),df17$Country)

#Splitting the data into testing and training sets:
train = subset(rbind(df15,df16),Country %in% commonCountries)
test = subset(df17,Country %in% commonCountries)

#Building the models:
model1 = lm(formula =Happiness.Score  ~ Economy..GDP.per.Capita. +Family + year + Health..Life.Expectancy., data = train)
model2 = lm(formula =Happiness.Score  ~ Economy..GDP.per.Capita. + year + Health..Life.Expectancy., data = train)

#Predicting the values for the testing data: 
pred1 = predict(model1,test)
pred2 = predict(model2,test)

#Defining function for RMS error
rmsError <- function(v1,v2){
  vd = v1 - v2
  vd2 = vd^2
  
  return(sqrt(sum(vd2)/length(vd2)))
}

#Errors
e1 = rmsError(pred1,test$Happiness.Score)
e2 = rmsError(pred2,test$Happiness.Score)

#Output:
print(e1)
print(e2)