#rm(list = ls())
library(ggcorrplot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(moments)
library(caTools)
library(MASS)
library(Metrics)
library(leaps)
library(olsrr)
library(ISLR)




load("Diabetes.RData")
head(Diabetes)
View(Diabetes)
dim(Diabetes)
sum(is.na(Diabetes))
summary(Diabetes)

# data visulaization and doing scatterplots

color <- c("red", "blue", "orange")
pairs(Diabetes[,1:5], pch = 19, cex = 0.5, col = color[Diabetes$group])


#Next we will split our data set into train and testing 
set.seed(1)

splitting_size = floor(0.80 *nrow(Diabetes))
splitting_size

split_data <- sample(seq_len(nrow(Diabetes)), size = splitting_size)
train_data <- Diabetes[split_data,]
test_data <- Diabetes[-split_data,]
View(train_data)
View(test_data)

#next we are gonna normalize our dataset
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}

norm_train = as.data.frame(lapply(train_data[1:5], normalize))
norm_test = as.data.frame(lapply(test_data[1:5], normalize))
View(norm_train)
View(norm_test)

# check the skewness of the data 
skewness(norm_train)
#would like to to if each feature is normally distributed or not. If it's now we need to transform it.
skewness(norm_train$relwt, na.rm = TRUE)
ggdensity(norm_train, x = "relwt", fill = "yellow", title = "relative weight density") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")

skewness(norm_train$glufast, na.rm = TRUE)
ggdensity(norm_train, x = "glufast", fill = "blue", title = "fasting plasma glucose level density") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")

skewness(norm_train$glutest, na.rm = TRUE)
ggdensity(norm_train, x = "glutest", fill = "yellow", title = "test plasma glucose level density") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")

skewness(norm_train$instest, na.rm = TRUE)
ggdensity(norm_train, x = "instest", fill = "blue", title = "plasma insulin density") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")

skewness(norm_train$sspg, na.rm = TRUE)
ggdensity(norm_train, x = "sspg", fill = "blue", title = "staddy state plasma glucose density") + scale_x_continuous() + stat_overlay_normal_density(color = 'red', linetype = "dashed")


#now we do sqrt transformation to transform the data into normal distribution
norm_train$glufast <- sqrt(norm_train$glufast)
skewness(norm_train$glufast)
norm_train$glufast <- sqrt(norm_train$glufast)
skewness(norm_train$glufast)

norm_train$glutest <- sqrt(norm_train$glutest)
skewness(norm_train$glutest)
norm_train$glutest <- sqrt(norm_train$glutest)
skewness(norm_train$glutest)

norm_train$instest <- sqrt(norm_train$instest)
skewness(norm_train$instest)

skewness(norm_train)

# next we see normal distribution for test data and do transformation if needs to
skewness(norm_test)

norm_test$glufast <- sqrt(norm_test$glufast)
skewness(norm_test$glufast)

norm_test$glufast <- sqrt(norm_test$glufast)
skewness(norm_test$glufast)

norm_test$glutest <- sqrt(norm_test$glutest)
skewness(norm_test$glutest)
norm_test$glutest <- sqrt(norm_test$glutest)
skewness(norm_test$glutest)

norm_test$instest <- sqrt(norm_test$instest)
skewness(norm_test$instest)

norm_test$sspg <- sqrt(norm_test$sspg)
skewness(norm_test$sspg)

skewness(norm_test)

View(norm_train)
View(norm_test)




#get 6th column from train data
View(train_data)
category_train = train_data[,6]
View(category_train)

#get 6th column from test data
View(test_data)
category_test = test_data[,6]
View(category_test)

#next we would like to add the categorical to the norm_train data
new_train = cbind(norm_train, category_train)
View(new_train)

#next we would like to add the categorical_test to the norm_test data 

new_test = cbind(norm_test, category_test)
View(new_test)

########################## LDA ##########################
# computing LDA 
lda_model = lda(category_train~., data = new_train)
lda_model

#make training set prediction
lda_predict_train = predict(lda_model, new_train)
lda_predict_train

par(mar=c(0.1,0.1,0.1,0.1))

ldahist(data = lda_predict_train$x[,1], g = new_train$category_train)
ldahist(data = lda_predict_train$x[,2], g = new_train$category_train)

#making testing set prediction using lda model 
lda_predict_test = predict(lda_model, new_test)
lda_predict_test

ldahist(data = lda_predict_test$x[,1], g = new_test$category_test)
ldahist(data = lda_predict_test$x[,2], g = new_test$category_test)


#seeing how Lda did on train data 
graph_data_train = cbind(new_train, lda_predict_train$x)
View(graph_data_train)
ggplot(graph_data_train, aes(LD1,LD2)) + geom_point(aes(color = category_train))


# seeing how Lda did on test data by seeing plot
graph_data_test = cbind(new_test, lda_predict_test$x)
View(graph_data_test)
ggplot(graph_data_test, aes(LD1,LD2)) + geom_point(aes(color = category_test ))


#Next like to know accuracy 
#mean(lda_predict_train$class == new_train$category_train)
mean(lda_predict_test$class == new_test$category_test)


######### QDA #########

qda_model = qda(category_train~., data = new_train)
qda_model

qda_predict_test = predict(qda_model, new_test)
qda_predict_test


mean(qda_predict_test$class == new_test$category_test)




########### 3rd question #######
View(train_data)
lda_model3 = lda(group~., data = train_data)
lda_model3

qda_model3 = qda(group~., data = train_data)
qda_model3

relwt = c(1.86)
glufast = c(184)
glutest = c(68)
instest = c(122)
sspg = c(544)

df = data.frame(relwt, glufast, glutest, instest, sspg)
View(df)
predict(lda_model3, df)
predict(qda_model3, df)


#########################################################################
########################## Question -2 ############################
?Weekly

names(Weekly)
View(Weekly)
dim(Weekly)
summary(Weekly)
sum(is.na(Weekly))

plot_histogram(Weekly)

coloring <- c("blue", "orange")
pairs(Weekly[,2:8], pch = 19, cex = 0.5, col = coloring[Weekly$Direction])


# I would like to see how many up(positive) and down(negative) return on a given week
count_up_down = table(Weekly$Direction)
count_up_down

par(mar=c(3,3,3,3))
colors = c("red", "blue")
col = colors

pie(count_up_down,labels = count_up_down, main ="up and down-given week",col=colors)
box()


# I would like to see which years has more up and down on a given week

years_up <- table(Weekly$Year[Weekly$Direction == 'Up'])
years_up

barplot_up <- barplot(years_up, main = "Years_Up ", col = "red", xlab = "Years" , ylim = c(0, 50), ylab = "count")                               
text(x = barplot_up,  y = years_up+ 5, labels = years_up)

years_down <- table(Weekly$Year[Weekly$Direction == 'Down'])
years_down

barplot_down <- barplot(years_down, main = "Years_Down ", col = "yellow", xlab = "Years" , ylim = c(0, 40), ylab = "count")                               
text(x = barplot_down,  y = years_down+ 3, labels = years_down)


########## question-2(b)############
library(tidyverse)
library(caret)

New_weekly = Weekly[c(-1, -8)]
View(New_weekly)


logistic_model = glm(Direction~., data = New_weekly, family = binomial)
summary(logistic_model)

############## question - 2(c)#########
predict_log = predict(logistic_model,type = "response")
predict_log
summary(predict_log)

tapply(predict_log, New_weekly$Direction, mean)

predicted.classes1 <- ifelse(predict_log > 0.5, "Up", "Down")
predicted.classes1

conf_table = table(New_weekly$Direction, predicted.classes1)
conf_table


############# question - 2(d) ###########

train_weekly = Weekly[Weekly$Year< 2009, ]
View(train_weekly)
dim(train_weekly)

#train_weekly = train_weekly[c(-1)]
#View(train_weekly)

test_weekly = Weekly[Weekly$Year>2008, ]
View(test_weekly)
dim(test_weekly)

weekly_model = glm(Direction~Lag2, data = train_weekly, family = binomial)
summary(weekly_model)

weekly_predict = predict(weekly_model, newdata = test_weekly, type = "response")
weekly_predict

tapply(weekly_predict, test_weekly$Direction, mean)


predicted.classes <- ifelse(weekly_predict > 0.5, "Up", "Down")
predicted.classes

conf_table_week = table(test_weekly$Direction, predicted.classes)
conf_table_week

# Model accuracy
mean(predicted.classes == test_weekly$Direction)

################ 2(e) ###############

lda_model_weekly = lda(Direction~Lag2, data = train_weekly)
lda_model_weekly

#make training set prediction
lda_predict_weekly = predict(lda_model_weekly, test_weekly)
lda_predict_weekly

ldahist(data = lda_predict_weekly$x[,1], g = test_weekly$Direction)

#confusion matrix
lda.class.week = lda_predict_weekly$class
lda.class.week

table(lda.class.week, test_weekly$Direction)


#Next like to know accuracy 

mean(lda_predict_weekly$class == test_weekly$Direction)

################ 2(f) ###############


knn_train = train_weekly[c(3)]
View(knn_train)

knn_test = test_weekly[c(3)]
View(knn_test)


knn_week = knn(knn_train, knn_test, cl = train_weekly$Direction, k=1)
knn_week

knn_conf <- table(knn_week, test_weekly$Direction)
knn_conf

mean(knn_week == test_weekly$Direction) #finding accuracy



