#SATYAM SHIVAM SUNDARAM
#01FB16ECS344
#ASSIGNMENT 2
rm(list = ls())
library(dplyr)
library(readr)
library(splitstackshape)

setwd('~Users\exfmti\Desktop\Semester5\DataAnalytics\Assignments\Assignment2')
dataframe <- read_csv("./kc_house_data.csv")
df<-data
#Question 1

#a)The factors to be considered while choosing a sampling method - 
#Cost in applying the method
#importanc of the decision
#sample size considerations
#Accuracy and precision out of the sample
#Target data population
#resource constraints


#b)
#1)Simple Random Sampling
simple_random_sample <- sample_n(df,as.integer(0.75*nrow(df)))
View(simple_random_sample)

#2)Systematic Sampling
i<-1
last_row <- nrow(df)
sys_sample <- filter(df,bedrooms==-1)
while(i<=last_row){
  sys_sample <- rbind(sys_sample,df[i,])
  i=i+4;
}
View(sys_sample)

#3)Clustered Sampling
#maybe we need to use kmeans

content <- c(names(df))
total<-sort(unique(df$bedrooms))
cluster <- filter(df,bedrooms==-1)

i<-0
for(i in total){
  samp <- filter(df,bedrooms==i)
  this_size <- nrow(samp)
  k <- as.integer(this_size*0.6)
  if(this_size == 1){
    sam <- sample_n(samp,this_size);
  }else{
    sam <- sample_n(samp,k);
  }
  cluster <-rbind(cluster,sam)
}

View(cluster)

#4)Stratified Sampling

strat_sample <- stratified(df,c("floors"),0.7)
View(strat_sample)


#PLOTS 

plot(simple_random_sample$price)
plot(sys_sample$price)
plot(cluster$price)
plot(strat_sample$price)
plot(df$price)


#most inaccurate
#Cluster Sampling



#sampling errors
#A sampling error expresses the difference between
#results for the sample and estimated results for the population.
#It is a problem in the way that members of a population are
#selected for research or data collection, which impacts the validity
#of results. 


#sampling bias
#A systematic error due to non-random sampling from a population, 
#causing some contents of population less likely to be included in the sample

#example_from_this_dataset
#.....


#C)
#Undersampling and Oversampling are techniques used to adjust the class
#distribution of a data set

#undersampling
  #Cluster
  #Cluster centroids is a method that replaces cluster of samples 
  #by the cluster centroid of a K-means algorithm, where the number
  #of clusters is set by the level of undersampling.
  
  #Tomek links
  
  #Tomek links remove unwanted overlap between classes where majority
  #class links are removed until all minimally distanced nearest 
  #neighbor pairs are of the same class


#oversampling
  #SMOTE
  #SMOTE: Synthetic Minority Over-sampling Technique
  #The adaptive synthetic sampling approach, or ADASYN algorithm, 
  #builds on the methodology of SMOTE.
  
  #ADASYN
  #ADASYN uses a weighted distribution for different minority class
  #examples according to their level of difficulty in learning, where
  #more synthetic data is generated for minority class examples that
  #are harder to learn.






#Question 2


#a) rounding on/off as required

grades <- sort(unique(df$grade))
grades <- (grades/max(grades))*100;
grades <- round(grades,0)
grades

#using normalization
ng <- unique(df$grade)
ng.stand <- scale(ng)
ng.stand <- sort(ng.stand)
View(ng.stand*10)
nor_grade = scale(df$grade)

#END_a


#b) standardization
df <- dataframe
price <- df$price
bed <- df$bedrooms
liv <- df$sqft_living
lot <- df$sqft_lot
grade <- df$grade
above <- df$sqft_above
base <- df$sqft_basement

price.stand <- scale(price)

bed.stand <- scale(bed)

liv.stand <- scale(liv);

lot.stand <- scale(lot)

grade.stand <- scale(grade)

above.stand <- scale(above)

base.stand <- scale(base)

limi = c(0:4)

#normal
par(new=FALSE)
plot(density(rnorm(nrow(df),mean = 0,sd = 1)),col = "blue",pch=123,lwd = 2,xlim = c(-4,4),ylim = c(0,5))

par(new = TRUE)
plot(density(price.stand),col = "red",lwd = 2,xlim = c(-4,4),ylim=c(0,5))

par(new = TRUE)
plot(density(bed.stand),col = "green",lwd = 2,xlim = c(-4,4),ylim = c(0,5))

par(new = TRUE)
plot(density(liv.stand),col = "yellow",lwd = 2,xlim = c(-4,4),ylim = c(0,5))

par(new = TRUE)
plot(density(lot.stand),col = "magenta",lwd = 2,xlim = c(-4,4),ylim = c(0,5))

par(new = TRUE)
plot(density(grade.stand),col = "pink",lwd = 2,xlim = c(-4,4),ylim = c(0,5))

par(new = TRUE)
plot(density(above.stand),col = "grey",lwd = 2,xlim = c(-4,4),ylim = c(0,5))

par(new = TRUE)
plot(density(base.stand),col = "orage",lwd = 2,xlim = c(0,3),xlim = c(-4,4),ylim = c(0,5))



#significance of standard normal



#c kurtosis and skewness
library(e1071)

kurtosis(price.stand)
skewness(price.stand)

kurtosis(bed.stand)
skewness(bed.stand)

kurtosis(liv.stand)
skewness(liv.stand)

kurtosis(lot.stand)
skewness(lot.stand)

kurtosis(grade.stand)
skewness(grade.stand)

kurtosis(above.stand)
skewness(above.stand)

kurtosis(base.stand)
skewness(base.stand)



#d
#non-scaled values
mli = mean(liv)
mlo = mean(lot)
mab = mean(above)
plot(sort(c(mli,mlo,mab)),ylim = c(-1,max(c(mli,mlo,mab))),xlab = "NULL",ylab = "F")

#distance is very large, huge standard deviation

#scaled values
par(new = TRUE)
mli.stand = mean(liv.stand)
mlo.stand = mean(lot.stand)
mab.stand = mean(above.stand)
plot(sort(c(mli.stand,mlo.stand,mab.stand)),ylim = c(-1,max(c(mli,mlo,mab))),xlab = "NULL",ylab = "F")
#distance is ~0 between the means

#the data is normalized and scaled down to the same mean



#Question 3
cov_mat <- matrix(nrow=ncol(df)-2,ncol = ncol(df)-2)
for(i in 1:ncol(cov_mat))
  for(j in 1:ncol(cov_mat))
    cov_mat[i,j] = 0;
 
View(cov_mat)

num_cols <- ncol(df)
avg <- matrix(0, ncol = num_cols-2, nrow = 1)
na <- colnames(df)
for(i in 3:num_cols){
  avg[i-2] = mean(df[[i]])
}
View(avg)

mean_adj_data = df
mean_adj_data[2] = NULL
mean_adj_data[1] = NULL
View(mean_adj_data)

for(i in 1:nrow(mean_adj_data)){
  for(j in 1:(ncol(mean_adj_data)-1)){
    mean_adj_data[i,j] = mean_adj_data[i,j]-avg[j]
  }
}
View(mean_adj_data)


for(i in 1:ncol(mean_adj_data)){
  for(j in i:ncol(mean_adj_data)){
    con_mat[i,j] = con_mat[i,j]+mean_adj_data[i,i]*mean_adj_data[i,j]
    con_mat[j,i] = con_mat[i,j]
  }
}

View(con_mat)
