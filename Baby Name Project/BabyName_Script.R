test = read.csv("baby_names/yob1880.txt")

babynames = as.data.frame(NULL) #baby name data frame 
file_years = c(1880:2014) #this vector is the one that holds the file dates 

for(i in file_years) {
  temp_year = toString(i)
  temp_file = c("baby_names/yob",temp_year,".txt")
  a = paste(temp_file, collapse="")
  read_year = read.csv(a, header=FALSE)
  
  #create the data frame for each csv file
  temp_frame = as.data.frame(read_year)
  temp_frame$"year" = temp_year
  babynames = rbind(temp_frame,babynames)
}

#change the babynames columns
names(babynames) = c("Name","Gender","Count","Year")

#create a new data frame to hold your name's data
myname = babynames[which(babynames$Name=="Christian" & babynames$Gender=="M"), ]

plot(myname$Year,myname$Count,xlab="Year",ylab="Number of Babies",main="Number of Babies Named Christian Since 1880")

babygirls = babynames[which(babynames$Gender=="F"),]

female_count = as.data.frame(table(babygirls$Year))

names(female_count) = c("Year","Count")

plot(female_count$Year,female_count$Count,xlab="Year",ylab="Number of Female Babies",main="Growth in number of unique girl names by year" ) 

toptennames = function(year, gender) {
  ttn = babynames[which(babynames$Gender==gender & babynames$Year==year),]
  top_names = ttn[order(ttn$Count, decreasing=TRUE),]
  return(top_names[(1:10),])
}


print(toptennames(1880, "F"))

print(toptennames(2014, "F"))

male = babynames[which(babynames$Gender=="M" & babynames$Year==2014),]
male = cbind(male[order(male$Count, decreasing=TRUE),], c(1:nrow(male)))

female = babynames[which(babynames$Gender=="F"  & babynames$Year==2014),]
female = cbind(female[order(female$Count, decreasing=TRUE),], c(1:nrow(female)))

names(male)[5] = "rank"
names(female)[5] = "rank"

mf = merge(male, female, by=c("Name", "Year"), suffixes=c(".m", ".f"))
head(mf)

mf = mf[order(pmax(mf$rank.m, mf$rank.f)),]
head(as.data.frame(unique(mf[, "name"])))
