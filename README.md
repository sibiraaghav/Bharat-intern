# Bharat-intern
# Titanic Survival Prediction

install.packages("caret")
install.packages("tidyverse")
install.packages("randomForest")
titanic=read.csv("C:/Users/sibir/Downloads/Titanic-Dataset.csv")
head(titanic)
tail(titanic)
View(titanic)
summary(titanic)
str(titanic)
colnames(titanic)


# To check for missing values in the dataset
sum(is.na(titanic))
sum(is.na(titanic$Age))

# Filling the missing values of age with the mean value of rest of the ages
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age,na.rm = TRUE)
head(titanic)
sum(is.na(titanic$Age))

# remove the column "Cabin" as it has missing characters
titanic=subset(titanic,select = -Cabin)

# changing to factor variables
titanic$Survived=factor(titanic$Survived,levels = c(0,1),labels = c("No","Yes"))
titanic$Name=as.factor(titanic$Name)
titanic$Sex=as.factor(titanic$Sex)
titanic$Ticket=as.factor(titanic$Ticket)
titanic$Embarked=as.factor(titanic$Embarked)
str(titanic)
head(titanic)


# Exploratory Data Analysis
# Gender representation
d1=data.frame(sex=c("male","female"),count=c(sum(titanic$Sex=="male"),sum(titanic$Sex=="female")))
pct=round((d1$count/sum(d1$count))*100)
pie(d1$count,labels=paste(d1$sex,sep=" ",pct,"%"),main="Male Vs Female",col=rainbow(length(d1$sex))) 

library(tidyverse)
g1=ggplot(titanic, aes(x="", y=Embarked, fill=Embarked)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0) + ggtitle("Embarkation")
g2=ggplot(titanic, aes(x="", y=Survived, fill=Survived)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0) + ggtitle("Survival representation")
g3=ggplot(titanic, aes(x="", y=SibSp, fill=SibSp)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0) + ggtitle("Siblings & Spouse")
g4=ggplot(titanic, aes(x="", y=Parch, fill=Parch)) + geom_bar(stat="identity", width=1)  + coord_polar("y", start=0) + ggtitle("Parch")
install.packages("gridExtra")
library(gridExtra)
grid.arrange(g1,g2,g3,g4,ncol=2)

Age_grp=cut(titanic$Age,seq(0,80,10),include.lowest = TRUE)
df2=data.frame(titanic$Age,titanic$Sex,Age_grp)
g5=ggplot(df2, aes(x=Age_grp, fill=titanic.Sex)) + geom_bar(position="dodge") + ggtitle("Age Wise distribution") + xlab("Age Group") + ylab("Count")
df3=data.frame(titanic$Age,titanic$Survived,Age_grp)
g6=ggplot(df3, aes(x=Age_grp, fill=titanic.Survived)) + geom_bar(position="dodge") + ggtitle("Age Wise Survival") + xlab("Age Group") + ylab("Count")
grid.arrange(g5,g6,ncol=2)

g7=ggplot(data = titanic) + geom_bar(mapping = aes(x = Pclass,fill=Survived)) + ggtitle("PClass Wise Survival") + xlab("Pclass") + ylab("Count")
g8=ggplot(data = titanic) + geom_bar(mapping = aes(x = Sex,fill=Survived)) + ggtitle("Gender Wise Survival") + xlab("Sex") + ylab("Count") 
g9=ggplot(data = titanic) + geom_bar(mapping = aes(x = Embarked,fill=Survived)) + ggtitle("Embarkation Wise Survival") + xlab("Emabrked") + ylab("Count")
g10=ggplot(data = titanic) + geom_bar(mapping = aes(x = Parch,fill=Survived)) + ggtitle("Parch Wise Survival") + xlab("Parch") + ylab("Count")
grid.arrange(g7,g8,g9,g10,ncol=2)


# Splitting training and testing data
obs=nrow(titanic)
split=round(obs*0.8)
train=titanic[1:split,]
test=titanic[(split+1):nrow(titanic),]
dim(train)
dim(test)


# Using Random Forest algorithm to predict the model
library(randomForest)
set.seed(123)
model=randomForest(formula=Survived~Sex+Fare+Age+Pclass+SibSp+Embarked+Parch,data=train,ntree=500)
model
model$importance
varImpPlot(model,sort=T,n.var=10)
library(caret)
confusionMatrix(predict(model,test),test$Survived)


# Using Logistic Regression Model
logisfit=glm(formula = titanic$Survived~Sex+Age+Pclass+Parch+Fare+Embarked+SibSp,family = 'binomial',data = titanic)
logisfit
summary(logisfit)
logistrain=predict(logisfit,type='response')
logistrain
tapply(logistrain, titanic$Survived, mean)
logispred=predict(logisfit,newdata=test,type='response')
logispred
summary(logispred)
test[logispred<=0.5,"Survivepred"]="No"
test[logispred>0.5,"Survivepred"]="Yes"
View(test)
confusionMatrix(table(test$Survived,test$Survivepred),positive = "No")

# Using Decision tree algorithm for prediction
library(rpart)
library(rpart.plot)
titanic2=data.frame(titanic,Age_grp)
View(titanic2)
tree=rpart(Survived~Age_grp+Sex+Embarked+Pclass+Parch,titanic)
pred=data.frame(Age_grp=c("(20,30]"),Sex=c("female"),Embarked=c("C"),Pclass=c(1),Parch=c(1))
result=predict(tree,pred)
result
rpart.plot(tree)
