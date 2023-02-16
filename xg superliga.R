### Eksamen ###
library(readr)
library(tidyverse)
library(randomForest)
library(tree)
library(caTools)
library(rpart)
library(rpart.plot)


setwd("Documents/Dataanalyse/2. Semester/Eksamen/OneDrive_1_31-03-2022")

#### Importerer alle datasæt direkte ####

shots_superliga_s2122_wyscout_main_info <- read_delim("shots_superliga_s2122_wyscout_main_info.csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

shots_superliga_s2122_wyscoutshots <- read_delim("shots_superliga_s2122_wyscoutshots.csv", 
                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)


#### Opgave 1.1 #### 

## Indlæsning af skud Superliga sæson 21/22 ##

#Sætter de to datasæt med skud data sammen

skud_superliga<-merge(shots_superliga_s2122_wyscout_main_info, shots_superliga_s2122_wyscoutshots, by= "eventId")

#### Opgave 1.2 ####
#Laver et subset med alle mål 
mål_superliga<-subset(skud_superliga, skud_superliga$shot_isGoal=="TRUE")

#Laver subset med alle mislykkede målskud#
skud_ikke_mål<-subset(skud_superliga, skud_superliga$shot_isGoal=="FALSE")

## Lokation x for mål## 
location_x<-mål_superliga$location_X
x_table<-table(location_x)
x_table<-as.data.frame(x_table)
mean_x<-mean(location_x)

## Lokation y for mål## 
location_y<-mål_superliga$location_Y
y_table<-table(location_y)
y_table<-as.data.frame(y_table)
mean_y<-mean(location_y)

## Lokation x for skud ## 
location_x_shot<-skud_superliga$location_X
x_shot_table<-table(location_x_shot)
x_shot_table<-as.data.frame(x_shot_table)

mean_x_shot<-mean(location_x_shot)

## Lokation y for skud ## 
location_y_shot<-skud_superliga$location_Y
y_shot_table<-table(location_y_shot)
y_shot_table<-as.data.frame(y_shot_table)

mean_y_shot<-mean(location_y_shot)

## Distance til mål ##
mean_dist<-mean(x8_distance)

## Vinkel til mål ##
mean_vinkel<-mean(x9_vinkel)

#### Opgave 1.4 ####

### Definerer Y-variablen ###
y_variabel<-skud_superliga$shot_isGoal
y_variabel<-ifelse(y_variabel=="TRUE", 1,0)
y_variabel<-as.factor(y_variabel)

### Laver X-variabler ###

## X1 Kropsdel ## 
x1_bodypart<-skud_superliga$shot_bodyPart

## X3 matchperiod ##
x3_machperiod<-skud_superliga$matchPeriod

## X4 Location X ##
x4_locationX<-skud_superliga$location_X

## X5 Location Y ## 
x5_locationY<-skud_superliga$location_Y

## X6 Team formation ## 
x6_teamformation<-skud_superliga$teamFormation

## X7 Opponent team formation ## 
x7_opteamformation<-skud_superliga$opponentTeamFormation

## X8 Distance til mål ##
q<-120-(skud_superliga$location_X)
qq<-q^2
qqq<-40-(skud_superliga$location_X)
qqqq<-qqq^2
x8_distance<-sqrt(qq+qqqq)

## X9 Vinkel ##
w<-atan((8*q)/qq+(qqqq - (8/2)^2))*180/pi
x9_vinkel<-w

#Laver er datasæt med X variabler
X_variabler<-data.frame(x1_bodypart, x3_machperiod, x4_locationX, x5_locationY, x6_teamformation, x7_opteamformation, x8_distance, x9_vinkel)

#Samler i et datasæt 
datasæt<-data.frame(y_variabel, X_variabler)
datasæt<-na.omit(datasæt)

### Opdel i træning og test ### 
## 75% af datasættet er test
data1 = sort(sample(nrow(datasæt)*.75))

#creating training data set by selecting the output row values
train<-datasæt[data1,]

#creating test data set by not selecting the output row values
test<-datasæt[-data1,]

### Random Forest ###
#Kører random forest model
model<-randomForest(y_variabel ~ ., data = train)

#Laver plot over variabler 
vigtige_variabler<-varImpPlot(model)
importance(model)

# Laver forudsigelser med modellen # 
prediction<-predict(model, newdata=test)

#Confusion matrix# 
table(prediction, test$y_variabel)

### Classification Tree ### 
model2<-tree(y_variabel~.-y_variabel, data=datasæt, subset=unlist(train))
plot(model2)
text(model2, pretty = 0)

#Tjek om der skal prunes 
cv.model2 <- cv.tree(model2, FUN = prune.misclass)
cv.model2

par(mfrow = c(1, 2))
plot(cv.model2$size, cv.model2$dev, type = "b")
plot(cv.model2$k, cv.model2$dev, type = "b")

## Pruning ## 
prune.model2 <- prune.misclass(model2, best = 7)
plot(prune.model2)
text(prune.model2, pretty = 0)

## Prediction ## 
tree.pred <- predict(prune.model2, test, type = "class")
table(tree.pred, test$y_variabel)

