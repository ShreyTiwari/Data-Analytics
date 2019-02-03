#Question 4

#Packages:
install.packages("caret")
library("caret")
install.packages("e1071")
library("e1071")

#Reading Data
df = read.csv("C:\\Users\\shail\\OneDrive\\Documents\\sem5\\Data_Analytics\\Data Analytics\\Assignments\\Assignment4\\School_Data.csv")

#Filtering:
noOfPassedCandidates = nrow(subset(df,df$Pass == 1))
noOfFailedCandidates = nrow(subset(df,df$Pass == 0))
failedCandidates = subset(df,df$Pass == 0)
print(noOfPassedCandidates-noOfFailedCandidates)
upsampledFailedCandidates = failedCandidates[sample(nrow(failedCandidates),(noOfPassedCandidates-noOfFailedCandidates),replace=TRUE),]


newdf = rbind(df,upsampledFailedCandidates)
y<-"Pass"
x<-colnames(newdf)
x2<-x[2:length(x)-1]

params = "Pass~Day1+Day2+Day3+Day4+Day5+Senior+Class_Prefect+Athlete+popularity"
model1 = glm(params,data=newdf,family=binomial(link="logit"))
summary(model1)

##from the summary we observe that the significant components are Day1,Day3,Day4,Athlete and popularity
print("from the summary we observe that the significant components are Day1,Day3,Day4,Athlete and popularity")

## AIC value mentioned in summary reis 876.3
params2 = "Pass~Day1+Day3+Day4+Athlete+popularity"
model2 = glm(params2,data=newdf,family=binomial(link="logit"))
summary(model2)

##AIC value mentioned in summary is 870.9
print("AIC value mentioned in summary is 870.9")

##As the AIC for the second model is lower(870.9<876.3), we can say that it is a better model than the first one 
print("As the AIC for the second model is lower(870.9<876.3), we can say that it is a better model than the first one")

test = read.csv("C:\\Users\\shail\\OneDrive\\Documents\\sem5\\Data_Analytics\\Data Analytics\\Assignments\\Assignment4\\Test - Sheet1.csv")
colnames(test) = colnames(newdf)[2:11]

res1 = predict(model1,test,type="response")
res2 = predict(model2,test,type="response")
model1Pred = as.factor(as.integer(as.logical(res1>0.5)))
model2Pred = as.factor(as.integer(as.logical(res2>0.5)))

CM1 = confusionMatrix(model1Pred,as.factor(test$Pass),dnn=c("Pass","Fail"))
CM2 = confusionMatrix(model2Pred,as.factor(test$Pass),dnn=c("Pass","Fail"))