#Shrey Manoj Tiwari
#01FB16ECS368
#Assignment: 2

#importing dataset and storing it in another variable
dataframe <- kc_house_data
#modifying dataframe to for easier use.
colnames(dataframe) <- c("id", "date", "price", "bedrooms", "bathrooms", 
                         "sqft_living", "sqft_lot", "floors", "waterfront", "view", 
                         "condition", "grade", "sqft_above", "sqft_basement", "yr_built", 
                         "yr_renovated", "zipcode", "lat", "long", "sqft_living15", "sqft_lot15")
dataframe <- dataframe[-1,]
#now dataframe is in standard format.

#Question 1:

#Subquestion 1
#Answer written in the blue book.

#Subquestion 2
#Simple Random Sample:
#Finding out size.
size <- floor(0.75*nrow(dataframe))
#Using random sampling to generate simple_random_sample of size = 'size' 
srs <- dataframe[sample(nrow(dataframe), size),]
#Plotting 
plot(srs$price)
hist(as.integer(srs$price))

#Systematic Sample:
#A list to tell which values to select and which ones to reject.
l = c(T, F, F, F)
#Using concept of recycling in R we get our systematic sample
ss <- dataframe[l,]
#Plotting graph. 
plot(ss$price)
hist(as.integer(ss$price))

#Cluster Sampling
#Getting the clusters.
clusters <- unique(dataframe$bedrooms)
#Performing simple random sample to get sample containing 60% on the clusters
n <- floor(0.6*length(clusters))
sampled_clusters <- sample(clusters, n)
#Selecting the datapoints belonging to sampled clusters
i <- 0
cs <- c(names(dataframe))
for(i in sampled_clusters)
{
  #Subset the dataframe and append to variable cs
  x <- subset(dataframe, bedrooms == as.integer(i))
  cs <- rbind(cs, x)
}
#Plotting graph.
plot(cs$price)
hist(as.integer(cs$price))

#Stratified Sampling:
#importing library to use predefined function.
library(splitstackshape)
#Using the function to get sample.
strats <- stratified(dataframe , c("floors"), 0.7)
#Plotting graph.
plot(strats$price)
hist(as.integer(strats$price))

#Answers to other subquestions in bluebook.

#Sampling error:
mean(as.integer(srs$bedrooms))
mean(as.integer(dataframe$bedrooms))
#Here we see some difference between the values


#Question 2

#Subquestion 1
#Finding the range of grades
min <- min(as.integer(dataframe$grade))
max <- max(as.integer(dataframe$grade))
range <- max - min
#Mapping it to the range 0-10
MapFunction <- function(x)
{
  new <- ((as.integer(x) - min)/range) * 10
  return(new)
}

#Subquestion 2
#Converting to zero mean and standard deviation of 1 using scale function
stand_price <- scale(as.integer(dataframe$price))
stand_bedrooms <- scale(as.integer(dataframe$bedrooms))
stand_sqft_living <- scale(as.integer(dataframe$sqft_living))
stand_sqft_lot <- scale(as.integer(dataframe$sqft_lot))
stand_grade <- scale(as.integer(dataframe$grade))
stand_sqft_above <- scale(as.integer(dataframe$sqft_above))
stand_sqft_basement <- scale(as.integer(dataframe$sqft_basement))

#Plotting graphs
plot(density(stand_price), lwd = 2, main = "Scaled Price")
plot(density(stand_bedrooms), lwd = 2, main = "Scaled Bedrooms")
plot(density(stand_sqft_living), lwd = 2, main = "Scaled sqft_living")
plot(density(stand_sqft_lot), lwd = 2, main = "Scaled sqft_lot")
plot(density(stand_grade), lwd = 2, main = "Scaled Grade")
plot(density(stand_sqft_above), lwd = 2, main = "sqft_above")
plot(density(stand_sqft_basement), lwd = 2, main = "sqft_basement")
     
#Subquestion 3
#importing library to find skew and kurtosis
library(e1071)

skewness(stand_price)
kurtosis(stand_price)

skewness(stand_bedrooms)
kurtosis(stand_bedrooms)

skewness(stand_sqft_living)
kurtosis(stand_sqft_living)

skewness(stand_sqft_lot)
kurtosis(stand_sqft_lot)

skewness(stand_grade)
kurtosis(stand_grade)

skewness(stand_sqft_above)
kurtosis(stand_sqft_above)

skewness(stand_sqft_basement)
kurtosis(stand_sqft_basement)
#inference in blue book

#Subquestion 4
mean_liv <- mean(as.integer(dataframe$sqft_living))
mean_lot <- mean(as.integer(dataframe$sqft_lot))
mean_abo <- mean(as.integer(dataframe$sqft_above))
#Distances between these is large

mean_sliv <- mean(as.integer(stand_sqft_living))
mean_slot <- mean(as.integer(stand_sqft_lot))
mean_sabo <- mean(as.integer(stand_sqft_above))
#Distances between these is small
#Inference in blue book




#Question 3

#Theory answers written in the blue book
