print("welcome the future data scientist - vaishali sagar")

##multiple regression###
data<- read.csv("50_Startups.csv")
View(data)

str(data)
##converting catagorical data into factor numbers

data$State= factor(data$State,
                   levels = c("New York","California","Florida"),
                   labels = c("1",'2','3'))
##spliting the dataset#########
split<- sample.split(data$Profit,
                     SplitRatio = 0.8)
split

set.seed(123)
library(caTools)
train<- subset(data,
               split==TRUE)
test<- subset(data,
              split==FALSE)

##starting with the regression process
#model for trainn set
regressor<- lm(formula = Profit~. , data= train)
summary(regressor)##significant variable<- R.D.Spend

regressor<- lm(formula = Profit~ R.D.Spend , data= train)
summary(regressor)#
##test set
predict<- predict(regressor, newdata= test)
predict

  #####Backward Elimination
regressor<- lm(formula = Profit~ R.D.Spend +Administration +Marketing.Spend + State , data= data)
summary(regressor)
#eliminating state
regressor<- lm(formula = Profit~ R.D.Spend +Administration +Marketing.Spend , data= data)
summary(regressor)
#eliminating administration
regressor<- lm(formula = Profit~ R.D.Spend +Marketing.Spend , data= data)
summary(regressor)

##############################################################################################

                PRACTISE

sal<- read.csv("50_Startups.csv")
View(sal)

sal$State<- factor(sal$State,
                   levels = c("New York","California","Florida"),
                   labels = c("1","2","3"))
library(caTools)
set.seed(123)
split<- sample.split(sal$Profit,
                     SplitRatio = 2/3)
split
train<- subset(sal, split==TRUE)
test<- subset(sal, split== FALSE)

#fitting the model for train set
regressor<- lm(Profit~. ,
               data= train)

summary(regressor)
#R.D.spend***
#predicitng for test set
predict<- predict(regressor, newdata= test)
predict
####backward elimination in multiple regression

regressor<- lm(formula = Profit~R.D.Spend +Administration +Marketing.Spend + State,
               data = sal)
summary(regressor)

regressor<- lm(formula = Profit~R.D.Spend +Administration +Marketing.Spend ,
               data = sal)
summary(regressor)

regressor<- lm(formula = Profit~R.D.Spend +Marketing.Spend ,
               data = sal)
summary(regressor)
