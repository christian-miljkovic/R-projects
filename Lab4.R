#Group: Christian Miljkovic, Zachary Fineberg, Suchit Sadana


#import the libraries that you need 
library(RColorBrewer)
library(NLP)
library(ggplot2)
library(wordcloud)
library(tm)
library(plyr)
library(slam)
library(SnowballC)


yelp_review = as.data.frame(read.csv("/Users/christianmiljkovic/Desktop/Data Analytics/Lab4/YelpReviews_20k.csv"))



qplot(yelp_review$stars,data = yelp_review,geom="histogram",binwidth = .3, xlab = "Stars", 
      main = "Ratings Count", ylab="Total Count")

agg_business = aggregate(yelp_review, by=list(yelp_review$business_id) , FUN = length)

mean_reviews_business = mean(agg_business$review_id)

# now do the same as the above but for a user

agg_user = aggregate(yelp_review, by=list(yelp_review$user_id) , FUN = length)

mean_reviews_user = mean(agg_user$review_id)

# now do the same for businesses that are marked as good for lunch

good_for_lunch_business = yelp_review[which(yelp_review$GoodforLunch=="TRUE"),]
not_for_lunch_business = yelp_review[which(yelp_review$GoodforLunch=="FALSE"),]

good_lunch_avg = mean(good_for_lunch_business$stars)

not_for_lunch_avg = mean(not_for_lunch_business$stars)


###Step 2####

corp = VCorpus(VectorSource(yelp_review$text))
tmp = corp



corp = tm_map(corp, removePunctuation)
corp = tm_map(corp, removeNumbers)
corp = tm_map(corp, stripWhitespace)
corp = tm_map(corp, content_transformer(removeWords), stopwords("SMART"), lazy = T)
corp = tm_map(corp, content_transformer(tolower), lazy = T)
corp = tm_map(corp, content_transformer(stemDocument), lazy = T)





dtm = DocumentTermMatrix(corp)


m = as.matrix(dtm)
attri = colSums(m)
ord = order(attri, decreasing = T)
freq.term = attri[ord[1:1000]]
paste("Top 15 words in all reviews:")
freq.term[1:15]



wordcloud(names(freq.term), freq.term,  max.words = 100, random.order = F, color = brewer.pal(6, "Dark2"))



### Step 3 ###

cols = ncol(dtm)
paste("number of unique terms in DTM:",cols)
dtms = removeSparseTerms(dtm, .990)
m = as.matrix(dtms)


yelp$GFL = yelp$GoodforLunch == "True"
yelp$GoodforLunch = yelp$GFL
yelp$GFL = ifelse(yelp$GoodforLunch == T, 1,0)


corr <- cor(yelp$GFL, m)
sel <- order(abs(corr),decreasing=T)<=200
subset = colnames(corr)[sel]


pos = order(corr, decreasing = T)[1:20]
posNames = colnames(corr)[pos]
posVals = corr[pos]
neg = order(corr)[1:20]


negNames = as.vector(colnames(corr)[neg])
negVals = corr[neg]
myColors = c(rep("Darkgreen",20), rep("Darkred",20))
names = c(posNames, negNames)
vals = c(posVals, abs(negVals))
wordcloud(names, vals, random.order = F, colors = myColors, ordered.colors = T)


foo = as.data.frame(m[,subset])
foo$GFL = yelp$GFL
train = foo[(1:(length(foo[,1])*.8)),]
test = foo[(length(foo[,1]):(length(foo[,1]) *.8 + 1)), ]
model = glm(GFL~., data=train, family=binomial)
coef <- coef(model)[-1]


positive.terms<- coef[coef>0]
topPositive<- sort(positive.terms,decreasing=T)[1:15]
negative.terms <- coef[coef<0]
topNegative <-sort(negative.terms)[1:15]

myColors = c(rep("Darkgreen",15), rep("Darkred",15))
names = c(names(topPositive), names(topNegative))
wordcloud(names, c(topPositive, abs(topNegative)), random.order = F, colors = myColors, ordered.colors = T)


train$predictVal = predict(model, type = "response")
train$predict = train$predictVal > .5
mean(train$GFL == train$predict)

test$predictVal = predict(model, test, type = "response")
test$predict = test$predictVal > .6
mean(test$GFL == test$predict)
