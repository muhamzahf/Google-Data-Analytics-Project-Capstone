##Install and Load Libraries
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("sqldf")

library(tidyverse)
library(skimr)
library(janitor)
library(sqldf)


#upload dataset
activity<-read.csv("dailyActivity_merged.csv")
calories<-read.csv("dailyCalories_merged.csv")
sleep_day<-read.csv("sleepDay_merged.csv")
intensities<-read.csv("dailyIntensities_merged.csv")
weight_log<-read.csv("weightLogInfo_merged.csv")


head(activity)
colnames(activity)
head(calories)
colnames(calories)
head(sleep_day)
colnames(sleep_day)
head(intensities)
colnames(intensities)
head(weight_log)
colnames(weight_log)
```

#5 data frame have same  'ID' field
#activity,calories and intensities have similar column. use SQL to join data table
activity2<-activity %>% select(Id,ActivityDate,Calories)
head(activity2)
sql_check1<-sqldf('select * from activity2 intersect select * from calories')
head(sql_check1)
nrow(sql_check1)
activity3<-activity %>% select(Id,ActivityDate,SedentaryMinutes,LightlyActiveMinutes,FairlyActiveMinutes,VeryActiveMinutes,SedentaryActiveDistance,LightActiveDistance,ModeratelyActiveDistance,VeryActiveDistance)
head(activity3)
sql_check2<-sqldf('select * from activity3 intersect select * from intensities')
head(sql_check2)
nrow(sql_check2)

#activity table has observation of both calories and intensities table
#only 3 dataset activity,sleep day,weight log used for analysis
#clean data
n_distinct(activity$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log$Id)
```

##number of observation
nrow(activity)
nrow(sleep_day)
nrow(weight_log)


##Statistic
activity %>% select(TotalSteps,TotalDistance,SedentaryMinutes,VeryActiveMinutes) %>% summary()
sleep_day %>% select(TotalSleepRecords,TotalMinutesAsleep,TotalTimeInBed) %>% summary()
weight_log %>% select(WeightPounds,BMI) %>% summary()


##visualization
#plot the relationship graph between the steps taken in a day and sedentary minutes and steps taken in a day and calories
ggplot(activity,mapping = aes(x=TotalSteps,y=SedentaryMinutes,color=Calories))+geom_point()
ggplot(activity,mapping = aes(x=TotalSteps,y=Calories))+geom_point()+geom_smooth(formula = 'y~x',method = lm)


##theres a large spread towards the lower amounts.let make a residuals or the differences between the observed values and the estimated vlaue.
calories.lm<-lm(Calories~TotalSteps,data = activity)
calories.res<-resid(calories.lm)
plot(activity$TotalSteps,calories.res,ylab="Residuals",xlab = "Total Steps",main = "Calories Burned")
abline(0,0)

##plot density of the residuals
plot(density(calories.res))

##checking for normality
qqnorm(calories.res)
qqline(calories.res)


##sleep time in the bed
ggplot(sleep_day,mapping = aes(x=TotalMinutesAsleep,y=TotalTimeInBed))+geom_point()


##combined dataset
combined<-merge(sleep_day, activity, by="Id")
head(combined)
nrow(combined)
n_distinct(combined$Id)


##Visialization Plot The Graph
ggplot(combined,mapping = aes(x=VeryActiveMinutes,y=Calories))+geom_point()+geom_smooth(formula = y~x,method = lm)
ggplot(combined,mapping = aes(x=FairlyActiveMinutes,y=Calories))+geom_point()+geom_smooth(formula = y~x,method = lm)
