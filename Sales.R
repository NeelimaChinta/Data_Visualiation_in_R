library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)
library(ggvis)
library(caret)
library(klaR)
library(e1071)

df<-read.csv("C:/Users/neeli/Downloads/supermarket_sales.csv")

head(df,5)

str(df)

summary(df)

dim(df)

cat("Complete Cases No Null ", sum(complete.cases(df)))

cat('Total Missing Values -> ', sum(is.na(df)))

plot(df)

df %>% ggvis(~Unit.price, ~Quantity, fill = ~Payment) %>% layer_points()

hist(df$Total)
hist(df$Rating)

ggplot(df,aes(x=gross.income))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Gross Income")+
  theme_classic()

grp<-df %>%
  group_by(Product.line) %>%
  count()
ggplot(grp,aes(x=Product.line, y=n))+geom_bar(stat='identity')+labs(title='Quantity')

ggplot(df,aes(x=Unit.price ,y=Quantity))+geom_point()+labs(title="Unit price vs quantity")

cor(df$Total, df$gross.income)
x=levels(df$Product.line)
print(x[1])
cor(df[df$Product.line==x[1],1:4])
print(x[2])
cor(df[df$Product.line==x[2],1:4])
print(x[3])

cr <- cor(df[7:10])
corrplot(cr)

relation<-lm(df$Total~df$gross.income)
plot(df$Total,df$gross.income,col = "blue",main = "Total and Gross Income", abline(lm(df$Total~df$gross.income)),cex = 1.3,pch = 16,xlab = "Total",ylab = "gross.income")


split=0.60
trainIndex <- createDataPartition(df$Payment, p=split, list=FALSE)
data_train <- df[ trainIndex,]
data_test <- df[-trainIndex,]
model <- naiveBayes(Payment ~ ., data = data_train)
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
predictions
confusionMatrix(predictions, as.factor(y_test))
