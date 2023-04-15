library(caret)
library(ggplot2)

sample <- read.csv("C:\\Users\\Patryk\\Downloads\\BlogFeedback\\blogData_train.csv", header =  FALSE)

colnames(sample)
summary(sample[51:54])
sapply(sample[51:54],sd)

#install.packages("moments")
library(moments)
skew <- apply(sample[51:54], 2, skewness)
print(skew)

correlations <- cor(sample[51:54])
print(correlations)

sample2<-sample[51:54]
par(mfrow=c(1,4))
for(i in 1:4) {
  hist(sample2[,i], main=names(sample2)[i])
}

par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(sample2[,i]), main=names(sample2)[i])
}


par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(sample2[,i], main=names(sample2)[i])
}

library(caret)
library(ggplot2)
install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
corrplot(correlations, method="circle")


train <- read.csv("C:\\Users\\Patryk\\Downloads\\BlogFeedback\\blogData_train.csv", header =  FALSE)
test <- read.csv("C:\\Users\\Patryk\\Downloads\\BlogFeedback\\blogData_test-2012.02.01.00_00.csv", header =  FALSE)

summary(train)

idx <- createDataPartition(train$V281,
                           p=0.7, list=F)
trn <- data.frame(train[idx,])

tst <- data.frame(test[-idx,])

ggplot(mapping=aes(alpha=0.4)) +
  geom_density(aes(V281, fill="red"), trn) +
  geom_density(aes(V281, fill="blue"), tst) +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 2)) +
  theme_gray()

model <- lm(V281 ~ ., data = trn)

summary(model)

p <- predict(model, newdata = tst)

error <- p - tst$V281

sqrt(mean(error^2))

model <- train(
  V281 ~ ., train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)
model
