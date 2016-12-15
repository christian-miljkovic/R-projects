### Import in the libraries and data that you are going to use ###
library(class)
library('rpart')
library(ggmap)
library(magrittr)
library(ggplot2)

streeteasy_data = read.csv("/Users/christianmiljkovic/Desktop/Data_Analytics/app/streeteasy_data.csv")
naked_data = read.csv("/Users/christianmiljkovic/Desktop/Data_Analytics/app/naked_apartments.csv")

### Part 1 - Clean the Data ###

## clean up the data by removing apartments that do not have all the data
streeteasy_data = streeteasy_data[-which(streeteasy_data$Detailcell.value==""),]

# remove furnished because if that shows up in the first detail cell then the rest of the details will
#not match up with the ones that start with beds as their first detail cell
streeteasy_data = streeteasy_data[-which(streeteasy_data$Firstdetail.value=="Furnished"),]
streeteasy_data = streeteasy_data[-which(streeteasy_data$Firstdetail.value=="studio"),]

#convert the data into numbers so that you can use it for the regression

streeteasy_data$Firstdetail.value = gsub(" beds","",streeteasy_data$Firstdetail.value)
streeteasy_data$Firstdetail.value = gsub(" bed","",streeteasy_data$Firstdetail.value)
streeteasy_data$Firstdetail.value = as.numeric(streeteasy_data$Firstdetail.value)
colnames(streeteasy_data)[11] = "Beds"

streeteasy_data$Detailcell.value = gsub(" baths","",streeteasy_data$Detailcell.value)
streeteasy_data$Detailcell.value = gsub(" bath","",streeteasy_data$Detailcell.value)
streeteasy_data$Detailcell.value = as.numeric(streeteasy_data$Detailcell.value)
streeteasy_data = streeteasy_data[!is.na(streeteasy_data$Detailcell.value),]
colnames(streeteasy_data)[12] = "Baths"

streeteasy_data$Lastdetail.value = gsub(" ft\xb2","",streeteasy_data$Lastdetail.value)
streeteasy_data$Lastdetail.value = gsub(",","",streeteasy_data$Lastdetail.value)
streeteasy_data$Lastdetail.value = as.numeric(streeteasy_data$Lastdetail.value)
colnames(streeteasy_data)[13] = "Sqft"

streeteasy_data$Price = gsub("[[:punct:]]", "", streeteasy_data$Price)
streeteasy_data$Price = as.numeric(streeteasy_data$Price)

streeteasy_data = streeteasy_data[-which(streeteasy_data$Detailsinfo.link==""),]
colnames(streeteasy_data)[15] = "Location"

#convert the bouroughs to numbers so that you can use this information for statistical models
streeteasy_data$Location = gsub('Upper West Side',1,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('West Village',2,streeteasy_data$Location, fixed = TRUE) 
streeteasy_data$Location = gsub('Tribeca',3,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Central Harlem',4,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Washington Heights',5,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Battery Park City',6,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Chelsea',7,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('East Village',8,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Financial District',9,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Greenwich Village',10,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Midtown',11,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Murray Hill',12,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Soho',13,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Upper East Side',14,streeteasy_data$Location, fixed = TRUE)
streeteasy_data$Location = gsub('Lincoln Square',15,streeteasy_data$Location, fixed = TRUE)






### Part 2 - Set up the Machine Learning ###

traindata = streeteasy_data[1:737, ]
testdata = streeteasy_data[738:787, -10]


# use the CART model for machine learning
model = rpart(Price ~ Beds + Baths + Sqft + Location, data=traindata, method ="anova",control =rpart.control(minsplit = 10))

#test a model with different predictor variables
model2 = rpart(Price ~ Beds + Baths + Sqft, data=traindata, method ="anova",control =rpart.control(minsplit = 10))

plot(model,margin=.1, main = "Rent Prediction")
text(model)

prediction = predict(model,newdata = testdata, type="vector")
prediction = as.vector(prediction)


#the second model
plot(model2,margin=.1, main = "Rent Prediction Streeteasy")
text(model2)

prediction2 = predict(model2,newdata = testdata, type="vector")
prediction2 = as.vector(prediction2)

#compare the results to see how accurate the mdoel is
comparison = streeteasy_data[c(737:787),10]

#find the difference between the two to see how far off
diff_prediction =  prediction - comparison
mean_diff = mean(diff_prediction)


#find the difference between the two to see how far off
diff_prediction2 =  prediction2 - comparison
mean_diff2 = mean(diff_prediction2)




### Part 3 #### 
### Clean up the Naked Apartments Data Set ###
naked_data$Price = gsub("[[:punct:]]", "", naked_data$Price)
naked_data$Bedroom = gsub("BR","",naked_data$Bedroom)
naked_data$Bathroom = gsub("BA","",naked_data$Bathroom)
naked_data$Price = as.numeric(naked_data$Price)
naked_data$Bedroom = as.numeric(naked_data$Bedroom)
naked_data$Bathroom = as.numeric(naked_data$Bathroom)


#convert the bouroughs to numbers so that you can use this information for statistical models
naked_data$Location = gsub('Upper West Side',1,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('West Village',2,naked_data$Location, fixed = TRUE) 
naked_data$Location = gsub('Tribeca',3,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Central Harlem',4,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Washington Heights',5,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Battery Park City',6,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Chelsea',7,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('East Village',8,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Financial District',9,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Greenwich Village',10,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Midtown',11,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Murray Hill',12,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Soho',13,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Upper East Side',14,naked_data$Location, fixed = TRUE)
naked_data$Location = gsub('Lincoln Square',15,naked_data$Location, fixed = TRUE)

#get rid of the apartments that are not found in the streeteasy_data
naked_data$Location = as.numeric(naked_data$Location)
naked_data = naked_data[!is.na(naked_data$Location),]

#find the square feet
naked_data$Amenities.description = gsub("[[:alpha:]]","",naked_data$Amenities.description)
naked_data$Amenities.description = gsub("[[:punct:]]","",naked_data$Amenities.description)
naked_data$Amenities.description = gsub("[[:space:]]","",naked_data$Amenities.description)

naked_data$Mapmarker.value = gsub("[[:alpha:]]","",naked_data$Mapmarker.value)
naked_data$Mapmarker.value = gsub("[[:punct:]]","",naked_data$Mapmarker.value)


naked_data = naked_data[-which(naked_data$Amenities.description==""), ]
naked_data$Amenities.description = as.numeric(naked_data$Amenities.description)
colnames(naked_data)[15] = "Sqft"



### Part 4 Set up the Machine Learning for the Naked Apartments Data Set###

traindata_naked = naked_data[51:651, ]
testdata_naked = naked_data[1:50, -7]


# use the CART model for machine learning
model_naked = rpart(Price ~ Bedroom + Bathroom + Location + Sqft, data=traindata_naked, method ="anova",control =rpart.control(minsplit = 10))

plot(model_naked,margin=.1, main = "Rent Prediction Naked Apartments")
text(model_naked)

prediction_naked = predict(model_naked, newdata = testdata_naked, type="vector")
prediction_naked = as.vector(prediction_naked)

#compare the results to see how accurate the mdoel is
comparison_naked = naked_data[c(1:50),7]

#find the difference between the two to see how far off
diff_prediction_naked =  prediction_naked - comparison_naked
mean_diff_naked = mean(diff_prediction_naked)


### create a function that calculates the predicted price using the three models ###

my_prediction = function(bedrooms,bathrooms,sqft,location) {
  
  #create the test data
  test_data = as.data.frame(streeteasy_data[1,])
  test_data_naked = as.data.frame(naked_data[1,])
  
  #change to the input values
  test_data_naked[1,c(9,10,11,14)] = c(bedrooms,bathrooms,location,sqft)
  
  #create the model
  model_naked = rpart(Price ~ Bedroom + Bathroom + Location + Sqft, data=traindata_naked, method ="anova",control =rpart.control(minsplit = 10))
  
  #create a vector for predictions of the data set in order to compare
  prediction_naked = predict(model_naked, newdata = test_data_naked, type="vector")
  prediction_naked = as.vector(prediction_naked)
  
  return(prediction_naked)
  
}

a = c(mean_diff_naked,mean_diff2,mean_diff)

barplot(a,main="Mean Difference in Predicited and Actual Prices", 
       names.arg=c("Model 1", "Model 2", "Model 3"), ylim = c(-1000,1000))

## create a line graph of the predicted and actual prices using the most effective model
plot(prediction,type = "o",col = "red", xlab = "Apartments", ylab = "Price", 
     main = "Predicted vs Actual Prices")

lines(comparison, type = "o", col = "blue")

