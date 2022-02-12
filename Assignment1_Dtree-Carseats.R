library(ISLR)
data("Carseats")
attach(Carseats)
View(Carseats)
?Carseats
summary(Carseats)
names(Carseats)

Carseats$Target<- as.factor(ifelse(Sales>8, "High", "Low"))
data<-Carseats[-1] #removing the sales column (first column)
View(data)

#data has all columns along with the target variable and without the sales

#partitioning

set.seed(123)
#sample(5, 10, replace = TRUE) to illustrate how sample function works

indx<- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
indx
nrow(data)
train<- data[indx==1,]
train
test<- data[indx==2,]
test
View(train)
nrow(train)/nrow(data) # not exact 80% as we have sampled but close cause of randomness/sampling

library("rpart")

# for supervised models it is basically the same. We are just specifying the response and predict variable.
#Below code doesn't mean we are running regression
myFormula = Target~.
mytree<- rpart(myFormula, data = train)
## Important to learn and read this output:
#40% are predicted in a wrong class. 130 are wrongly classified
#130/325 =  40%
# * means terminal node
train%>% 
  group_by(Target)%>%
  summarise(n())
# This is basically the root node-..How many High and Low we have in the training data

print(mytree)
library(rpart.plot)
rpart.plot(mytree)

pred_train<-predict(mytree, data = train, type = "class") # use without class to see the probability
pred_train
# now compare this predict class

mean(train$Target !=pred_train)
#### Now on test instances:

pred_test<-predict(mytree, newdata = test, type = "class")
mean(test$Target != pred_test)

## Other argument can be changed:
mytree2<- rpart(myFormula, data = train, parms = list(split = "information"), 
               control = rpart.control(minsplit = 10, minbucket = 5, cp = -1))
# minisplit== cannot split unless there are these many instances. Minimum instances to split further
# minibucket== minimum instances in the terminal node
# Cp parameter = -1 meaning tree grows to full (for every new addition of terminal node the error goes down hence
# tree grows full. Error = X + #terminalnodes*Cp)

rpart.plot(mytree2)

# performance on test and train

pred_train2<-predict(mytree2, data = train, type = "class")
# now compare this predict class

mean(train$Target !=pred_train2)
#### Now on test instances:

pred_test2<-predict(mytree2, newdata = test, type = "class")
mean(test$Target != pred_test2)

#
# Grow tree fully- overfit, don't let tree grow- underfit
# Find the sweet spot with a balance

## next step- Check with validation data set as well--
summary(mytree)

###############\
#DECISION RULES:
#mytree Rule 52
#If ShelveLoc = Medium or Bad and and Price < 105.5 and CompPrice < 124.5 and Price < 92.5 then Sales is high
#Support
20/325
# 0.061

#Confidence
16/20
# 0.80
