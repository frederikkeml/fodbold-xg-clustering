### Eksamen 2. Semester ###
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dendextend)
library(factoextra)

setwd("Documents/Dataanalyse/2. Semester/Eksamen/OneDrive_1_31-03-2022")

#### Opgave 3.1 ####
### CLustering på afleveringer ###

## Indlæser afleveringer fra Superligaen ###
passes_superliga_s2122_wyscout_main_info <- read_csv("passes_superliga_s2122_wyscout_main_info.csv")

passes_superliga_s2122_wyscoutpasses <- read_csv("passes_superliga_s2122_wyscoutpasses.csv")

# Samler dataen i en dataframe #
afleveringer_liste<-list(passes_superliga_s2122_wyscout_main_info, passes_superliga_s2122_wyscoutpasses)
afleveringer_superliga<-afleveringer_liste %>% reduce(full_join, by='eventId')

afleveringer_superliga<-na.omit(afleveringer_superliga)

#Udvælger de variabler jeg vil bruge

afleveringer_udvalgte<-afleveringer_superliga %>% select(10 |11|25|26|27|31|32)
afleveringer_matrix<-as.matrix(afleveringer_udvalgte)

#Hieraisk clustering med euclidean metode
cluster_afleveringer<- hclust(dist(afleveringer_matrix), method = "average")

#Finder der hvor den skal skæres til 
hc.out <- hclust(dist(afleveringer_matrix))

#Cutter 
cut_hcluster<- cutree(hc.out, 7)

#Laver en tabel med eventID + cluster nummer 
aflv_cluster<-table(cut_hcluster, afleveringer_superliga$eventId)
aflv_cluster1<-subset(aflv_cluster, aflv_cluster$f)


#### 3.1 Brøndby spiller ####

Brøndby_afleveringer<-subset(afleveringer_superliga, afleveringer_superliga$teamId=="7453")

#Tjekker hvor ofte spillerne optræder
table(Brøndby_afleveringer$playerName)

#Laver et datasæt med s. Rosteds afleveringer
Rosted_afleveringer<-subset(Brøndby_afleveringer, Brøndby_afleveringer$playerName=="S. Rosted")
RO_afleveringer_udvalgte<-Rosted_afleveringer %>% select(10 |11|25|26|27|31|32)
Rosted_matrix<-as.matrix(RO_afleveringer_udvalgte)

Rosted_ID<-Rosted_afleveringer$eventId

#Hieraisk clustering med euclidean metode
hcluster_rosted<- hclust(dist(Rosted_matrix), method = "average")

#Finder der hvor den skal skæres til 
hc.out_rosted <- hclust(dist(Rosted_matrix))

#Cutter 
cut_rosted<- cutree(hc.out_rosted, k = 7)

#Laver en tabel med eventID + cluster nummer 
Rosted_cluster<-table(cut_rosted, Rosted_ID)
Rosted_cluster<-as.data.frame(Rosted_cluster)
# !!! 
Rosted_cluster<-subset(Rosted_cluster, Rosted_cluster$Freq=="1")
Rosted_cluster<-Rosted_cluster[,1:2]

colnames(Rosted_cluster)<-c("cluster","eventId")

#Samler det hele i én dataframe 
Rosted_list<-list(Rosted_afleveringer, Rosted_cluster)
Rosted_data<-Reduce(function(x, y) merge(x, y, all=TRUE), Rosted_list) 


#### 3.2 ####
## Importerer skud fra Superligaen ##
shots_superliga_s2122_wyscout_main_info <- read_delim("shots_superliga_s2122_wyscout_main_info.csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

shots_superliga_s2122_wyscoutshots <- read_delim("shots_superliga_s2122_wyscoutshots.csv", 
                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Samler dataen i en dataframe #
skud_liste<-list(shots_superliga_s2122_wyscout_main_info, shots_superliga_s2122_wyscoutshots)
skud_superliga<-skud_liste %>% reduce(full_join, by='eventId')

#Udvælger de variabler jeg vil bruge
skud_udvalgte<-skud_superliga %>% select(10 |11 |25|26)
#Laver kolonne 25 & 26 numeriske (0= false, 1= true)
skud_udvalgte$shot_isGoal<-ifelse(skud_udvalgte$shot_isGoal=="TRUE", 1,0)
skud_udvalgte$shot_onTarget<-ifelse(skud_udvalgte$shot_onTarget=="TRUE", 1,0)
skud_udvalgte<-na.omit(skud_udvalgte)

#K-means clustering
#Finder hvor mange clustre
fviz_nbclust(skud_udvalgte, kmeans, method = "wss")

#Laver k-means med 3 clustrer (optimale)
set.seed(2)
km.skud <- kmeans(skud_udvalgte, 3)

#Ser hvordan det er fordelt
km.skud$cluster
table(km.skud$cluster)

#Laver plot
fviz_cluster(km.skud, data = skud_udvalgte)

#Finder gennemsnit per cluster
aggregate(skud_udvalgte, by=list(cluster=km.skud$cluster), mean)

#add cluster assigment to original data
final_data_skud <- cbind(skud_udvalgte, cluster = km.skud$cluster)

#view final data
head(final_data_skud)


#### 3.3 ####
### Clustering på spilleres skud og afleveringer ###
## Dataen ligger under opggave 3.1 & 3.2 ##
## afleveringer_superliga + skud_superliga ##
spillerdata_liste<-list(afleveringer_liste, skud_liste)
spiller_data<-Reduce(function(afleveringer_superliga, skud_superliga) merge(afleveringer_superliga, skud_superliga, all=TRUE), spillerdata_liste)

## Laver dataen om til at være for spillere istedet for eventid ##
#Laver summary på variavblerne jeg ønsker at anvende 
location_x<-spiller_data %>%
  group_by(playerName) %>%
  summarize(mean_locationx = mean(location_X, na.rm = TRUE))

location_y<-spiller_data %>%
  group_by(playerName) %>%
  summarize(mean_locationy = mean(location_Y, na.rm = TRUE))

passlength<-spiller_data %>%
  group_by(playerName) %>%
  summarize(mean_passlength = mean(passLength, na.rm = TRUE))

passangle<-spiller_data %>%
  group_by(playerName) %>%
  summarize(mean_passangle = mean(passAngle, na.rm = TRUE))

passendlocation_x<-spiller_data %>%
  group_by(playerName) %>%
  summarize(mean_passendlocation_x = mean(pass_EndLocation_X, na.rm = TRUE))

passendlocation_y<-spiller_data %>%
  group_by(playerName) %>%
  summarize(mean_passendlocation_y = mean(pass_EndLocation_Y, na.rm = TRUE))

#Laver kolonne 26, 36 & 37 numeriske (0= false, 1= true)
spiller_data$shot_isGoal<-ifelse(spiller_data$shot_isGoal=="TRUE", 1,0)
spiller_data$shot_onTarget<-ifelse(spiller_data$shot_onTarget=="TRUE", 1,0)
spiller_data$passAccurate<-ifelse(spiller_data$passAccurate=="TRUE", 1,0)

shot_isgoal<-spiller_data %>%
  group_by(playerName) %>%
  summarize( shot_isgoal = mean(shot_isGoal, na.rm = TRUE))

shot_ontarget<-spiller_data %>%
  group_by(playerName) %>%
  summarize(shot_ontarget = mean(shot_onTarget, na.rm = TRUE))

pass_isaccurate<-spiller_data %>%
  group_by(playerName) %>%
  summarize(pass_isaccuraye = mean(passAccurate, na.rm = TRUE))

#Samler alle variablerne 
spiller_variabler_liste<-list(location_x, location_y, passlength, passendlocation_x, passendlocation_y, shot_isgoal, shot_ontarget, pass_isaccurate)
spiller_variabler<-spiller_variabler_liste %>% reduce(full_join, by='playerName')
spiller_variabler<-na.omit(spiller_variabler)

#Udvælger de variabler jeg vil bruge
udvalgte_variabler<-spiller_variabler %>% select(-1)

### K-means clustering ###
#Finder hvor mange clustre
fviz_nbclust(udvalgte_variabler, kmeans, method = "wss")

#Laver k-means med 4 clustrer (optimale)
set.seed(2)
km.spillere <- kmeans(udvalgte_variabler, 4)

#Ser hvordan det er fordelt
km.spillere$cluster
table(km.spillere$cluster)
km.spillere$centers
#Laver plot
fviz_cluster(km.spillere, data = udvalgte_variabler)

#Finder gennemsnit per cluster
aggregate(udvalgte_variabler, by=list(cluster=km.spillere$cluster), mean)

#add cluster assigment to original data
final_data_spillere<- cbind(spiller_variabler, cluster = km.spillere$cluster)

## Cluster 2 ##
cluster2<-subset(final_data_spillere, final_data_spillere$cluster=="2")

