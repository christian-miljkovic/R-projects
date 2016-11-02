dating_data = read.csv("/Users/christianmiljkovic/Downloads/Speed Dating Data.csv")

#data for men 

men_data = dating_data[which(dating_data$gender==1),]


#data for women

women_data = dating_data[which(dating_data$gender==0),]

#find the mean for each activity for men in every column and put them in a single vector

mean_all = colMeans(women_data[51:67],na.rm=TRUE)

#create the graph

barplot(mean_all,cex.names=0.5,cex.axis=0.5,ylim = c(0,8),main="What Activities Will Help You Get A Girlfriend")



