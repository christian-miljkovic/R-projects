#group Christian Miljkovic, Zachary Fineberg, Suchit Sadan

#load in the libraries that you need to use
library(rpart)


### part 1 ###


#load in the data
train = read.csv("/Users/christianmiljkovic/Downloads/LoanStats3c.csv", skip = 1)
test = read.csv("/Users/christianmiljkovic/Downloads/LoanStats3d.csv", skip = 1)

#remove empty rows at the bottom:
train = train[1:(ncol(test) -1),]
test = test[1:(ncol(train) -1),]


### part 2 ###


train$highgrade = ifelse(train$grade == "A" | train$grade == "B",1,0)

percentHigh = nrow(train[train$highgrade == 1,])/nrow(train)

#t-test for above and below median income:
median_income = median(train$annual_inc)
above_med_income = ifelse(train$annual_inc>median_income,"Above","Below")
t.test(train$highgrade~above_med_income)

#t-test for above or below median loan amount
median_loan = median(train$loan_amnt)
above_med_loan = ifelse(train$loan_amnt > median_loan,"Above", "Below")
t.test(train$highgrade~above_med_loan)

#t-test for home ownership status:
home_owner = ifelse(train$home_ownership == "RENT", "Rents", "Doesn't rent")
t.test(train$highgrade~home_owner)

##########
# part 3 #
#########

#run glm regression:
fit1 = glm(highgrade~annual_inc + home_ownership + loan_amnt, data = train)
summary(fit1)

#predict values and find the optimal threshold:
train$predict_val = predict(fit1, type = "response")
train$predict = (train$predict_val >.6)
accuracy = mean(train$predict == train$highgrade)
accuracy


random = runif(nrow(test), min=0, max=1)
train$benchmark1 = random>=.5
mean(train$benchmark1 == train$highgrade)


#test against benchmark of all 0's:
benchmark2 = rep(0, nrow(train))
mean(benchmark2 == train$highgrade)

##########
# part 4 #
#########


fit2 = rpart(highgrade~annual_inc + home_ownership + loan_amnt, data = train, method = "class")
z = predict(fit2, type="class")
paste("The machine learning based classifier has an accuracy of", round(mean(z == train$highgrade),4), "while the regression based approach has an accuracy of", round(accuracy,4))


### part 5 ###


test$highgrade = ifelse(test$grade == "A" | test$grade == "B",1,0)

#predict using regression:
predict_val = predict(fit1, test, type = "response")
predict = (predict_val > .5)
test_accuracy1 = mean(test$highgrade == predict)

#print out the result
test_accuracy1


#predict using classifier:
predict_val2 = predict(fit2, test, type = "class")
test_accuracy2 = mean(test$highgrade == predict_val2)

#print out the 2nd result
test_accuracy2

#benchmark accuracy of random assignment:
rand = (runif(nrow(test), min = 0, max = 1) >= .5)
acc = mean(rand == test$highgrade)

#print out the result
acc


#benchmark accuracy of all 0's:
zero = rep(0,nrow(test))
acc2 = mean(zero == test$highgrade)

#print out the second result
acc2
