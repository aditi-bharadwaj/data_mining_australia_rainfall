#Reading the data
read.csv("D:/Minning project/Data Set/weatherAUS.csv")->Aus_Rainfall
View(Aus_Rainfall)
summary(Aus_Rainfall$RainToday[Aus_Rainfall$RainToday == 'Yes'])
prop.table(table(Aus_Rainfall$RainToday))
#viewing the column names
colnames(Aus_Rainfall)
#running the summary and checking the NA values
summary(Aus_Rainfall)
#checking the structure of every variable
str(Aus_Rainfall)
#checking if any special character
sapply(Aus_Rainfall, function(x) sum(grepl("^\\s*$", x)))
#checking the count of NA values
count=sapply(Aus_Rainfall,function(x) sum(is.na(x)))
count
#plotting missing values
library(visdat)
vis_miss(Aus_Rainfall,warn_large_data = FALSE)

barplot(count, 
        horiz = TRUE, 
        cex.names = 1,
        cex.axis = 0.9,
        axes = TRUE,
        las = 2,
        sub = "(%)",
        border =NA,
        col= "grey",
        main = "PROPORTION OF MISSING VALUES", xlab="Percentage"
)
par(mar=c(5,5.3,4,2))
#dropping the missing values of the dependent column
Aus_Rainfall <- Aus_Rainfall[!is.na(Aus_Rainfall$RainToday),]
summary(Aus_Rainfall)
prop.table(table(Aus_Rainfall$RainToday))
#ploting the missing values after removing missing values from dependent variable
vis_miss(Aus_Rainfall,warn_large_data = FALSE)

barplot(count, 
        horiz = TRUE, 
        cex.names = 1,
        cex.axis = 0.9,
        axes = TRUE,
        las = 2,
        sub = "(%)",
        border =NA,
        col= "grey",
        main = "PROPORTION OF MISSING VALUES", xlab="Percentage"
)
#Dropping the column Date 
library(dplyr)
Aus_Rainfall<- Aus_Rainfall %>% select(-Date)
summary(Aus_Rainfall)

#applying the chi sqaure test for checking the corelation of categorical value
chi<- names(which(sapply(Aus_Rainfall, class) == "factor"))

#Performing Chi-Square n categorical variables:

chi1=Aus_Rainfall[,chi]

for(i in 2:length(chi)-1){
  print(i)
  chi3=table(chi1[,5],chi1[,i])
  mosaicplot(chi3,shade = TRUE, las=2,
             y = chi[i],main = "Rain_today")
  print(chi[i])
  print(chisq.test(chi3,simulate.p.value = TRUE))
}
#replacing missing values with mean
Aus_Rainfall$MinTemp = ifelse(is.na(Aus_Rainfall$MinTemp), 
                                  ave(Aus_Rainfall$MinTemp, FUN = function(x) mean(x, na.rm = TRUE)),
                                  Aus_Rainfall$MinTemp) 
Aus_Rainfall$MaxTemp = ifelse(is.na(Aus_Rainfall$MaxTemp), 
                              ave(Aus_Rainfall$MaxTemp, FUN = function(x) mean(x, na.rm = TRUE)),
                              Aus_Rainfall$MaxTemp) 
Aus_Rainfall$Evaporation = ifelse(is.na(Aus_Rainfall$Evaporation), 
                              ave(Aus_Rainfall$Evaporation, FUN = function(x) mean(x, na.rm = TRUE)),
                              Aus_Rainfall$Evaporation) 
Aus_Rainfall$Sunshine = ifelse(is.na(Aus_Rainfall$Sunshine), 
                              ave(Aus_Rainfall$Sunshine, FUN = function(x) mean(x, na.rm = TRUE)),
                              Aus_Rainfall$Sunshine) 
Aus_Rainfall$WindGustSpeed = ifelse(is.na(Aus_Rainfall$WindGustSpeed), 
                               ave(Aus_Rainfall$WindGustSpeed, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$WindGustSpeed) 
Aus_Rainfall$Cloud3pm = ifelse(is.na(Aus_Rainfall$Cloud3pm), 
                               ave(Aus_Rainfall$Cloud3pm, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Cloud3pm) 
Aus_Rainfall$Cloud9am = ifelse(is.na(Aus_Rainfall$Cloud9am), 
                               ave(Aus_Rainfall$Cloud9am, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Cloud9am) 
Aus_Rainfall$WindSpeed3pm = ifelse(is.na(Aus_Rainfall$WindSpeed3pm), 
                               ave(Aus_Rainfall$WindSpeed3pm, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$WindSpeed3pm) 
Aus_Rainfall$WindSpeed9am = ifelse(is.na(Aus_Rainfall$WindSpeed9am), 
                               ave(Aus_Rainfall$WindSpeed9am, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$WindSpeed9am) 
Aus_Rainfall$Temp3pm = ifelse(is.na(Aus_Rainfall$Temp3pm), 
                               ave(Aus_Rainfall$Temp3pm, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Temp3pm) 
Aus_Rainfall$Temp9am = ifelse(is.na(Aus_Rainfall$Temp9am), 
                               ave(Aus_Rainfall$Temp9am, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Temp9am) 
Aus_Rainfall$Pressure9am = ifelse(is.na(Aus_Rainfall$Pressure9am), 
                               ave(Aus_Rainfall$Pressure9am, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Pressure9am) 
Aus_Rainfall$Pressure3pm = ifelse(is.na(Aus_Rainfall$Pressure3pm), 
                               ave(Aus_Rainfall$Pressure3pm, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Pressure3pm) 
Aus_Rainfall$Humidity9am = ifelse(is.na(Aus_Rainfall$Humidity9am), 
                               ave(Aus_Rainfall$Humidity9am, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Humidity9am) 
Aus_Rainfall$Humidity3pm = ifelse(is.na(Aus_Rainfall$Humidity3pm), 
                               ave(Aus_Rainfall$Humidity3pm, FUN = function(x) mean(x, na.rm = TRUE)),
                               Aus_Rainfall$Humidity3pm) 
summary(Aus_Rainfall)


#Dropping the columns which is not usefull
library(dplyr)
Aus_Rainfall<- Aus_Rainfall %>% select(-WindGustDir, -WindDir9am, -WindDir3pm)
summary(Aus_Rainfall)
#converting the categorical values in numerical
Aus_Rainfall$RainToday <- ifelse(Aus_Rainfall$RainToday=="Yes", 1, 0)
Aus_Rainfall$RainToday= factor(Aus_Rainfall$RainToday)
Aus_Rainfall$RainTomorrow <- ifelse(Aus_Rainfall$RainTomorrow=="Yes", 1, 0)
Aus_Rainfall$RainToday= factor(Aus_Rainfall$RainTomorrow)
View(Aus_Rainfall)
table(Aus_Rainfall$RainToday)
# Scatterplot
library(tidyverse)
library(ggplot2)
gg <- ggplot(Aus_Rainfall, aes(x=MaxTemp, y=MinTemp)) + 
  geom_point(aes(col=Location)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(-9,35)) + 
  ylim(c(-5, 50)) + 
  labs(subtitle="MinTemp Vs MaxTemp", 
       y="MinTemp", 
       x="MaxTemp", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)
# Scatterplot
gg <- ggplot(Aus_Rainfall, aes(x=MaxTemp, y=MinTemp)) + 
  geom_point(aes(col=RainToday)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(-9,35)) + 
  ylim(c(-5, 50)) + 
  labs(subtitle="MinTemp Vs MaxTemp", 
       y="MinTemp", 
       x="MaxTemp", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)
#Checking the Corellation
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$MinTemp)
cor(Aus_Rainfall$MinTemp, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$MaxTemp)
cor(Aus_Rainfall$MaxTemp, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$Sunshine)
cor(Aus_Rainfall$Sunshine, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$Evaporation)
cor(Aus_Rainfall$Evaporation, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$WindGustSpeed)
cor(Aus_Rainfall$WindGustSpeed, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$Rainfall)
cor(Aus_Rainfall$Rainfall, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$RISK_MM)
cor(Aus_Rainfall$RISK_MM, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$RainTomorrow)
cor(Aus_Rainfall$RainTomorrow, as.numeric(Aus_Rainfall$RainToday))
cor(as.numeric(Aus_Rainfall$RainToday),Aus_Rainfall$Humidity9am)
cor(Aus_Rainfall$Humidity9am, as.numeric(Aus_Rainfall$RainToday))

# Encoding target variable as factor
Aus_Rainfall$RainToday <- as.factor(Aus_Rainfall$RainToday)

#dividing the data into test and train
library(caTools)
set.seed(123)
split = sample.split(Aus_Rainfall$RainToday, SplitRatio = 0.75)
training_set = subset(Aus_Rainfall, split == TRUE)
test_set = subset(Aus_Rainfall, split == FALSE)
Aus_Rainfall$RainToday <- as.factor(Aus_Rainfall$RainToday)
#library(cobalt)
#library(VCA)
#Applying sampaling technique
library(ROSE)
both <- ovun.sample(RainToday ~ MinTemp+MaxTemp+Evaporation+Sunshine+Rainfall+WindGustSpeed+Humidity9am,
                    data = training_set, method = "both", p = 0.5, seed = 222, N = 105591)
summary(both)
#library(DMwR)
#balanced.data <- SMOTE(RainToday ~ MinTemp+MaxTemp+Evaporation+Sunshine+Rainfall+WindGustSpeed+Humidity9am, 
 #                      training_set, perc.over = 4800, k = 5, perc.under = 00)
#as.data.frame(table(balanced.data$RainToday))
#applying Svm
library(e1071)
library(caret)
classifier = svm(formula = RainToday ~ MinTemp+MaxTemp+Evaporation+Sunshine+Rainfall+WindGustSpeed+Humidity9am,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test set results
y_predsvm = predict(classifier, newdata = test_set[-18])
y_predsvm
# Making the Confusion Matrix
class(y_predsvm)
class(test_set)
class(RainToday)
cm =table(test_set$RainToday,y_predsvm)
cm
library(caret)
confusionMatrix(table(test_set$RainToday, y_predsvm), mode = 'everything')
#plotting Roc Curve
library(pROC)
library(ggplot2)
rocobj <- roc(as.numeric(test_set$RainToday), as.numeric(y_predsvm))
g <- ggroc(rocobj)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve svm") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")
#Applying svm Radial
library(e1071)
classifier = svm(formula = RainToday ~ MinTemp+MaxTemp+Evaporation+Sunshine+Rainfall+WindGustSpeed+Humidity9am,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')
# Predicting the Test set results
y_predsvmradial = predict(classifier, newdata = test_set[-18])
y_predsvmradial
# Making the Confusion Matrix
cm = table(test_set$RainToday, y_predsvmradial)
cm
library(caret)
confusionMatrix(table(test_set$RainToday, y_predsvmradial),mode = 'everything')
#Plotting Roc curve for svm radial
library(pROC)
library(ggplot2)
rocobj1 <- roc(as.numeric(test_set$RainToday), as.numeric(y_predsvmradial))
g <- ggroc(rocobj1)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Svm Radial") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")

#Applying decission tree
library(rpart)
classifier = rpart(formula = RainToday ~ MinTemp+MaxTemp+Evaporation+Sunshine+Rainfall+WindGustSpeed+Humidity9am,
                   data = training_set)
#############################
# Predicting the Test set results
y_preddecissiontree = predict(classifier, newdata = test_set[-18], type = 'class')
y_preddecissiontree
# Making the Confusion Matrix
cm = table(test_set$RainToday, y_preddecissiontree)
cm
library(caret)
confusionMatrix(table(test_set$RainToday, y_preddecissiontree), mode = 'everything')
library(rpart)
rpart.plot::rpart.plot(classifier, box.palette="RdBu", shadow.col="gray", nn=TRUE)
#Plotting Roc curve for decission tree
library(pROC)
library(ggplot2)
rocobj2 <- roc(as.numeric(test_set$RainToday), as.numeric(y_preddecissiontree))
g <- ggroc(rocobj2)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve decission tree") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")


#Applying Naive Bayes
library(e1071)
classifier = naiveBayes(x = training_set[-18],
                        y = training_set$RainToday)
# Predicting the Test set results
y_prednaivebayes = predict(classifier, newdata = test_set[-18])
y_prednaivebayes
# Making the Confusion Matrix
cm = table(test_set$RainToday, y_prednaivebayes)
cm
library(caret)
confusionMatrix(table(test_set$RainToday, y_prednaivebayes), mode='everything',)
# Plotting ROC
library(pROC)
library(ggplot2)
rocobj3 <- roc(as.numeric(test_set$RainToday), as.numeric(y_prednaivebayes))
g <- ggroc(rocobj3)
g  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Naive Bayes") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="blue", linetype="dashed")
#Roc curve comparision
g2 <- ggroc(list(y_predsvm=rocobj, y_predsvmradial=rocobj1, y_prednaivebayes=rocobj3))
g2
g2 <- ggroc(list(Svm=rocobj, SvmRadial=rocobj1, NaiveBayes=rocobj3),size=1.5)
g2  + xlab("FPR") + ylab("TPR") + theme_minimal() + ggtitle("ROC Curve Comparison") +
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed")

# Making the Confusion Matrix
cm = table(test_set$RainToday, y_pred)
cm
