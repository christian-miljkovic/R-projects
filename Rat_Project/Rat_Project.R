#read in all of the neccesary data from the rat database
restaurant_inspection = read.csv("/Users/christianmiljkovic/Desktop/Data Analytics/Rat_Project/Restaurant_Inspection.csv",header = FALSE)
rodent_inspection = read.csv("/Users/christianmiljkovic/Desktop/Data Analytics/Rat_Project/Rodent_Inspection.csv")
rat_sightings = read.csv("/Users/christianmiljkovic/Desktop/Data Analytics/Rat_Project/sandyrelated.csv")

#import all of the packages that you are going to use
library(lubridate)

#####Part 1.a #######

#convert all the string representations of the dates into actual dates

rodent_inspection$INSPECTION_DATE = parse_date_time(rodent_inspection$INSPECTION_DATE, orders="mdy HM", truncated = 3)

#create a data set with just active rodent sightings
active_sightings = rodent_inspection[which(rodent_inspection$RESULT == "Active Rat Signs"),]

# create a table with the years and months of active sightings
sighting_table = table(month(active_sightings$INSPECTION_DATE),year(active_sightings$INSPECTION_DATE))

#convert the table to a data frame to access items easily

sighting_frame = as.data.frame(sighting_table)

colnames(sighting_frame) = c("Month","Year","Frequency")

# create a line plot from the year 2010 to 2015
plot(c(1:61),sighting_frame$Frequency[25:85], xlab="Months Since 2010", ylab="Rat Sightings")

#### Part 1.b #####

# create a table using the entire rodent_inspection data set that only has dates
# and the results of each inspection

rodent_table = table(month(rodent_inspection$INSPECTION_DATE),year(rodent_inspection$INSPECTION_DATE))

# turn the table into a data frame to access items easier
rodent_frame = as.data.frame(rodent_table)

colnames(rodent_frame) = c("Month","Year","Frequency")

#find the effeciency of inspections 
effeciency = (sighting_frame$Frequency[25:85])/(rodent_frame$Frequency[61:121]) * 100

# create a line plot from the year 2010 to 2015 showing the effeciency of inspections
plot(c(1:61),effeciency, xlab="Months Since 2010", ylab="Inspection Effeciency",main = "How Effective Inspections Are")


#### Part 1.c #####

#using the rodent_inspection2 table which is organized by active rat sightings
#find the top 10 zip codes with rat sightings

#get the frequencies of zip codes and sort them in decreasing order

temp_table = table(active_sightings$ZIP_CODE)

temp_table = sort(temp_table,decreasing = TRUE)

# convert to data frame so you can easily access numbers

zip_frame = as.data.frame(temp_table)

colnames(zip_frame) = c("ZipCode","Frequency")

#get only the top ten zip codes

zip_frame = zip_frame[c(1:10),]

zip_frame


### Part 2.a ####

#create a table that only has the zip code frequencies prior to 2012 using 
#the already clean version of rodent_inspection2

table_prior = active_sightings[which(year(active_sightings$INSPECTION_DATE) < 2012),]

#get the frequencies of zip codes and sort them in decreasing order

table_prior = table(table_prior$ZIP_CODE)

table_prior = sort(table_prior,decreasing = TRUE)

# convert to data frame so you can easily access numbers

prior_frame = as.data.frame(table_prior)

colnames(prior_frame) = c("ZipCode","Frequency")

#get only the top twenty zip codes

prior_frame = prior_frame[c(1:20),]

prior_frame

# do the same as the above but for after 2012

table_post = active_sightings[which(year(active_sightings$INSPECTION_DATE) > 2012),]

#get the frequencies of zip codes and sort them in decreasing order

table_post = table(table_post$ZIP_CODE)

table_post = sort(table_post,decreasing = TRUE)

# convert to data frame so you can easily access numbers

post_frame = as.data.frame(table_post)

colnames(post_frame) = c("ZipCode","Frequency")

#get only the top twenty zip codes

post_frame = post_frame[c(1:20),]

post_frame


### now do the same however use the data from the rat_sightings data frame 
#this means convert all of the string dates to date types and get a data frame 
# of just active rodent sightings to then proceed

rat_sightings$Created.Date = mdy_hm(rat_sightings$Created.Date)

#make remove all of the inspections that did not involve active rodents
rat_sightings2 = rat_sightings[which(rat_sightings$Descriptor=="Rat Sighting"),]

#get the frequencies of zip codes and sort them in decreasing order

rat_table = table(rat_sightings2$Incident.Zip)

rat_table = sort(rat_table,decreasing = TRUE)

# convert to data frame so you can easily access numbers

rat_frame = as.data.frame(rat_table)

colnames(rat_frame) = c("ZipCode","Frequency")

#get only the top twenty zip codes

rat_frame = rat_frame[c(1:20),]

rat_frame


### Part 3 ###

##Step 1##

#generate a dataset which, for each month-year-zipcode combination that
# contains the likelihood that a rodent inspection yields active rat sightings

#use effeciency from Part 1.a to create a data set with zip codes and month-year in one row
# and effeciency

inspection_frame = as.data.frame(effeciency)
inspection_frame = cbind(sighting_frame$Month[25:85],sighting_frame$Year[25:85],inspection_frame)
colnames(inspection_frame) = c("Month","Year","Effeciency")


#Merge these data with the restaurant violation data set so that each row 
#in the restaurant database also now contains average rat sightings for the 
#zip code in which the restaurant is located

inspection_zipcode_total = table(month(rodent_inspection$INSPECTION_DATE),year(rodent_inspection$INSPECTION_DATE),
                                               rodent_inspection$ZIP_CODE)

inspection_zipcode_total = as.data.frame(inspection_zipcode_total)

inspection_zipcode_total = inspection_zipcode_total[which(inspection_zipcode_total$Freq != 0), ]

colnames(inspection_zipcode_total) = c("Month","Year","ZipCode","Frequency")

inspection_zipcode_total = inspection_zipcode_total[which(inspection_zipcode_total$ZipCode != 0), ]

#do the same for active rat sightings now

active_zipcode_total = table(month(active_sightings$INSPECTION_DATE),year(active_sightings$INSPECTION_DATE),
                                     active_sightings$ZIP_CODE)
active_zipcode_total = as.data.frame(active_zipcode_total)

active_zipcode_total = active_zipcode_total[which(active_zipcode_total$Freq != 0), ]

colnames(active_zipcode_total) = c("Month","Year","ZipCode","Frequency")

active_zipcode_total = active_zipcode_total[which(active_zipcode_total$ZipCode != 0), ]

#merge the data sets together 
merged_sightings = merge(inspection_zipcode_total, active_zipcode_total, by = c("Month","Year","ZipCode"))

#now calculate the average right sighting based upon zip code and date
merged_sightings$AvgSighting = (merged_sightings$Frequency.y/merged_sightings$Frequency.x)

#get rid of the extra columns 

merged_sightings = merged_sightings[,-c(4:5)]

##Step 2##

#Merge these data with the restaurant violation data set so that each row in
#the restaurant database also now contains average rat sightings for the zip code in which the 
#restaurant is located,for the month and year that the restaurant was inspected

#convert the string representations of the dates to actual date objects
restaurant_inspection$INSPECTION.DATE = parse_date_time(restaurant_inspection$V9, "mdy")

#create the year and month columns so that you can merge easily with the other data set
restaurant_inspection$Month = month(restaurant_inspection$INSPECTION.DATE)
restaurant_inspection$Year = year(restaurant_inspection$INSPECTION.DATE)

#now merge the two data frames by month, year, and zip code

restaurant_inspection = merge(restaurant_inspection, merged_sightings, all.x= TRUE, by.x = c("V6", "Month", "Year"), by.y = c("ZipCode","Month","Year"))

#now change the qualitative data of inspection code to either 1,0's so that we can perform
#statistical regression on them

restaurant_inspection$Rodent_Violation = ifelse(restaurant_inspection$V11 == "04L" | restaurant_inspection$V11 == "04K", 1,0)

restaurant_inspection$Month = months(restaurant_inspection$INSPECTION.DATE)
restaurant_inspection$Year = as.factor(restaurant_inspection$Year)

##Step 3##

regression = glm(Rodent_Violation ~ AvgSighting + Month + Year, data = restaurant_inspection)
summary(regression)







