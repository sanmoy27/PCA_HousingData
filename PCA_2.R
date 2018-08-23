cat("\014")
library(MASS)
library(caTools)
library(ISLR)
library(glmnet)
library(caret)
library(class)
library(gridExtra)
library(ggplot2)
library(gridExtra)
library(plotly)

readUrl<-url("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
houseData<-read.table(readUrl)
houseData<-as.data.frame(houseData)
colnames(houseData) <- c("CRIM", "ZN", "INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

dim(houseData)

str(houseData)
glimpse(houseData)

colnames(houseData)

head(houseData, 3)

detectNAs<-function(x){
  return(sum(is.na(x)))
}
lapply(houseData, detectNAs)

summary(houseData)

# Perform PCA
pcaHouses <- prcomp(scale(houseData[,-14]))
scoresHouses <- pcaHouses$x

pcaHousesEig <- pcaHouses$sdev^2 
pcaHousesEig

# Fit lm using the first 3 PCs
modHouses <- lm(houseData$MEDV ~ scoresHouses[,1:3])
summary(modHouses)

# Fit full model
modHousesFull <- lm(MEDV ~ ., data=houseData)
summary(modHousesFull)

p1<-ggplot(houseData, aes(x=MEDV, y=predict(modHouses))) + geom_point() + geom_smooth( method="lm", se=FALSE)+
  labs(title="PCA") +
  labs(x="Observed MEDV") +
  labs(y="Predicted MEDV")

p2<-ggplot(houseData, aes(x=MEDV, y=predict(modHousesFull))) + geom_point() + geom_smooth( method="lm", se=FALSE)+
  labs(title="Full Model") +
  labs(x="Observed MEDV") +
  labs(y="Predicted MEDV")

grid.arrange(p1,p2, nrow=1, ncol=2)
