### Eksamen 2. semester ###
###Opgave 2###

library(readr)
library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)

setwd("Documents/Dataanalyse/2. Semester/Eksamen/OneDrive_1_31-03-2022")
path<-"Documents/Dataanalyse/2. Semester/Eksamen/"

#### 2.1 ####

### Indlæsning af data ###

## Bundligaer ##
bot_wyscout_shots_new <- read_delim("bot_wyscout_shots_new.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

bot_wyscout_main_info_new <- read_delim("bot_wyscout_main_info_new.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

bot_wyscout_possession_new <- read_delim("bot_wyscout_possession_new.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Samler dataen i en dataframe #
bund_liste<-list(bot_wyscout_main_info_new, bot_wyscout_possession_new, bot_wyscout_shots_new)
bundliga<-bund_liste %>% reduce(full_join, by='eventId')

## Midligaer ##
mid_wyscout_main_info <- read_delim("mid_wyscout_main_info.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

mid_wyscout_possession <- read_delim("mid_wyscout_possession.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

mid_wyscout_shots <- read_delim("mid_wyscout_shots.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Samler dataen i en dataframe #
mid_liste<-list(mid_wyscout_main_info, mid_wyscout_possession, mid_wyscout_shots)
midliga<-mid_liste %>% reduce(full_join, by='eventId')
midliga<-midliga %>% select (-"teamId", -"category")

## Topligaer ##
top_wyscout_main_info <- read_delim("top_wyscout_main_info.csv", 
                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

top_wyscout_possession <- read_delim("top_wyscout_possession.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

top_wyscout_shots <- read_delim("top_wyscout_shots.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
 

# Samler dataen i en dataframe #
top_liste<-list(top_wyscout_main_info, top_wyscout_possession, top_wyscout_shots)
topliga<-top_liste %>% reduce(full_join, by='eventId')
topliga<-topliga %>% select (-"teamId", -"category")

# Samler alle ligaer i en dataframe #
alle_ligaer<-rbind(bundliga, midliga, topliga)


#### 2.2 ####

### Beskrivende statistik ### 

#Laver et subset med alle mål 
mål_alle_ligaer<-subset(alle_ligaer, alle_ligaer$shot_isGoal=="TRUE")

#Laver subset med alle mislykkede målskud#
skud_ikke_mål<-subset(alle_ligaer, alle_ligaer$shot_isGoal=="FALSE")

## Kropsdele og mål ##
body_part_mål<-mål_alle_ligaer$shot_bodyPart
table(body_part_mål)

body_part_shot<-alle_ligaer$shot_bodyPart
table(body_part_shot)

## Type af skud ## 
shot_type_goal<-mål_alle_ligaer$typePrimary
table(shot_type_goal)

shot_type<-alle_ligaer$typePrimary
table(shot_type)

## Halvleg ## 
halvleg_mål<-mål_alle_ligaer$matchPeriod
table(halvleg_mål)

halvleg_skud<-alle_ligaer$matchPeriod
table(halvleg_skud)

## Lokation x for mål## 
location_x<-mål_alle_ligaer$location_X
x_table<-table(location_x)
x_table<-as.data.frame(x_table)

mean_x<-mean(location_x)

## Lokation y for mål## 
location_y<-mål_alle_ligaer$location_Y
y_table<-table(location_y)
y_table<-as.data.frame(y_table)
mean_y<-mean(location_y)

## Lokation x for skud ## 
location_x_shot<-skud_ikke_mål$location_X
x_shot_table<-table(location_x_shot)
x_shot_table<-as.data.frame(x_shot_table)

mean_x_shot<-mean(location_x_shot)

## Lokation y for skud ## 
location_y_shot<-skud_ikke_mål$location_Y
y_shot_table<-table(location_y_shot)
y_shot_table<-as.data.frame(y_shot_table)

mean_y_shot<-mean(location_y_shot)

## Team formation ##

#Mål#
team_formation_goal<-mål_alle_ligaer$teamFormation
team_formation_goal_df<-table(team_formation_goal)
team_formation_goal_df<-as.data.frame(team_formation_goal_df)

#Ekspoter dataframe#
write.csv(team_formation_goal_df, "team_formation_goal_df.csv") 

#Ikke mål# 
team_formation_nogoal<-skud_ikke_mål$teamFormation
team_formation_nogoal_df<-table(team_formation_nogoal)
team_formation_nogoal_df<-as.data.frame(team_formation_nogoal_df)

#Ekspoter dataframe#
write.csv(team_formation_nogoal_df, "team_formation_nogoal_df.csv") 

## Opponent team formation ##

#Mål#
oteam_formation_goal<-mål_alle_ligaer$opponentTeamFormation
oteam_formation_goal_df<-table(oteam_formation_goal)
oteam_formation_goal_df<-as.data.frame(oteam_formation_goal_df)

#Ekspoter dataframe#
write.csv(oteam_formation_goal_df, "oteam_formation_goal_df.csv") 

#Ikke mål# 
oteam_formation_nogoal<-skud_ikke_mål$opponentTeamFormation
oteam_formation_nogoal_df<-table(oteam_formation_nogoal)
oteam_formation_nogoal_df<-as.data.frame(oteam_formation_nogoal_df)

#Ekspoter dataframe#
write.csv(oteam_formation_nogoal_df, "oteam_formation_nogoal_df.csv")

## shot_goalZone ## 
mål_goalzone<-mål_alle_ligaer$shot_goalZone
table(mål_goalzone)

ikkemål_goalzone<-skud_ikke_mål$shot_goalZone
goalzone<-table(ikkemål_goalzone)

write.csv(goalzone, "goalzone.csv")

#### 2.3 ####
### Possession / Opspil ### 

###Possession attack flank ###

## Bundliga ## 
flank_bund<-bundliga$possessionAttack_flank
table(flank_bund)

## Midliga ## 
flank_mid<-midliga$possessionAttack_flank
table(flank_mid)

## Topliga ## 
flank_top<-topliga$possessionAttack_flank
table(flank_top)

### Mål fra opspil ###

## Bundliga ##
pos_mål_bund<-bundliga$possessionAttack_withGoal
table(pos_mål_bund)

## Midliga ##
pos_mål_mid<-midliga$possessionAttack_withGoal
table(pos_mål_mid)

## Topliga ##
pos_mål_top<-topliga$possessionAttack_withGoal
table(pos_mål_top)

### X lokation i opspil ###

## Bundliga ##
x_start_pos_bund<-bundliga$possessionStartLocation_X
mean(x_start_pos_bund)

x_slut_pos_bund<-bundliga$possessionEndLocation_X
mean(x_slut_pos_bund)

## Midliga ##
x_start_pos_mid<-midliga$possessionStartLocation_X
mean(x_start_pos_mid)

x_slut_pos_mid<-midliga$possessionEndLocation_X
mean(x_slut_pos_mid)

## Topliga ##
x_start_pos_top<-topliga$possessionStartLocation_X
mean(x_start_pos_top)

x_slut_pos_top<-topliga$possessionEndLocation_X
mean(x_slut_pos_top)

### Y lokation i opspil ###

## Bundliga ##
y_start_pos_bund<-bundliga$possessionStartLocation_Y
mean(y_start_pos_bund)

y_slut_pos_bund<-bundliga$possessionEndLocation_Y
mean(y_slut_pos_bund)

## Midliga ##
y_start_pos_mid<-midliga$possessionStartLocation_Y
mean(y_start_pos_mid)

y_slut_pos_mid<-midliga$possessionEndLocation_Y
mean(y_slut_pos_mid)

## Topliga ##
y_start_pos_top<-topliga$possessionStartLocation_Y
mean(y_start_pos_top)

y_slut_pos_top<-topliga$possessionEndLocation_Y
mean(y_slut_pos_top)

#### 2.4 ####
library(randomForest)
library(tree)
set.seed(1)

### Definerer Y-variablen ###
y_variabel<-alle_ligaer$shot_isGoal
y_variabel<-ifelse(y_variabel=="TRUE", 1,0)
y_variabel<-as.factor(y_variabel)

### Laver X-variabler ###

## X1 Kropsdel ## 
x1_bodypart<-alle_ligaer$shot_bodyPart

## X2 Skudtype ##
x2_shottype<-alle_ligaer$typePrimary

## X3 matchperiod ##
x3_machperiod<-alle_ligaer$matchPeriod

## X4 Location X ##
x4_locationX<-alle_ligaer$location_X

## X5 Location Y ## 
x5_locationY<-alle_ligaer$location_Y

## X6 Team formation ## 
x6_teamformation<-alle_ligaer$teamFormation

## X7 Opponent team formation ## 
x7_opteamformation<-alle_ligaer$opponentTeamFormation

## X8 Distance til mål ##
q<-120-(alle_ligaer$location_X)
qq<-q^2
qqq<-40-(alle_ligaer$location_X)
qqqq<-qqq^2
x8_distance<-sqrt(qq+qqqq)

## X9 Vinkel ##
w<-atan((8*q)/qq+(qqqq - (8/2)^2))*180/pi
x9_vinkel<-w
                  
#Laver er datasæt med X variabler
X_variabler<-data.frame(x1_bodypart, x2_shottype, x3_machperiod, x4_locationX, x5_locationY, x6_teamformation, x7_opteamformation, x8_distance, x9_vinkel)

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
class_tree<-rpart(y_variabel~.-y_variabel, data=train, minsplit = 1, 
                  minbucket = 120)

summary(class_tree)
rpart.plot(class_tree)
