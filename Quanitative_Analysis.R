library(rstatix)
library(ggplot2)
library(dplyr)

#FORMAT DATA-----------------------------------------------------------------------
#(outputs: formatted data(light & audio), data_by_gem(light,audio & gem), data_dual_task(light & audio(dual only)))

#read csv
my_quant_data48 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/all_data_48.csv")

#format data
cidx <- c(1,3,7,11:17,105:111)
my_quant_data48 <- my_quant_data48[,cidx]
formatted_data <- data.frame(0, nrow = 48*2, ncol = 18);
subIdx <- seq(1, 288, by = 6)
for (sub in 1:48) {
  #select data for this subject
  thisIdx = subIdx[sub]:(subIdx[sub]+5);
  thisData = my_quant_data48[thisIdx,]
  #separate data by dual and single task
  trueIdx = which(thisData$audio == 'True')
  falseIdx = which(thisData$audio == 'False')
  #enter into new matrix
  formatted_data[(sub*2)-1,1] = sub; #subject number
  formatted_data[sub*2,1] = sub; #subject number
  formatted_data[(sub*2)-1,4:17] = colMeans(thisData[falseIdx,4:17]) #single task means
  formatted_data[sub*2,4:17] = colMeans(thisData[trueIdx,4:17]) #dual task means
  formatted_data[(sub*2)-1,18] = 0 #single 
  formatted_data[sub*2,18] = 1 #dual
  formatted_data[(sub*2)-1,3] = 'single'; 
  formatted_data[sub*2,3] = 'dual'; 
  #re-code day and night
  if (thisData[1,2] == 'day') {
    formatted_data[(sub*2)-1,2] = 'Day'
    formatted_data[sub*2,2] = 'Day'
  } else if (thisData[1,2] == 'night') {
    formatted_data[(sub*2)-1,2] = 'Night'
    formatted_data[sub*2,2] = 'Night'
  }
}
colnames(formatted_data) <- colnames(my_quant_data48) #rename columns
colnames(formatted_data)[18] <- "audioPlotCode" #rename columns
formatted_data$light<-as.factor(formatted_data$light) #set as a factor
formatted_data$audio<-as.factor(formatted_data$audio) #set as a factor
formatted_data$audioPlotCode<-as.factor(formatted_data$audioPlotCode) #set as a factor
formatted_data$participant_number<-as.factor(formatted_data$participant_number) #set as a factor

#format the data for analysis of gem type
cidx <- c(1:6,11:16,18)
thisData <- formatted_data[,cidx]
data_by_gem <- as.data.frame(matrix(0, nrow = 48*6, ncol = 8));
colnames(data_by_gem) <- c('participant_number','light','audio','audioPlotCode','gemType','accuracy','detectionAccuracy','discriminationAccuracy')
#gem in long format
data_by_gem1 <- thisData %>%
  gather(key = "gemType", value = "accuracy", physicalAccuracy, virtualAccuracy, floatingAccuracy)
data_by_gem2  <- thisData %>%
  gather(key = "gemTpye2", value = "detectionAccuracy", physicalDetectionAccuracy, virtualDetectionAccuracy, floatingDetectionAccuracy)
data_by_gem3  <- thisData %>%
  gather(key = "gemType3", value = "discriminationAccuracy", physicalDiscriminationAccuracy, virtualDiscriminationAccuracy, floatingDiscriminationAccuracy)

data_by_gem[,1:6] = data_by_gem1[,c(1:3,10:12)]
data_by_gem[,7] = data_by_gem2[,12]
data_by_gem[,8] = data_by_gem3[,12]
data_by_gem <- data_by_gem %>% mutate(gemType = recode(gemType,
                              "physicalAccuracy" = "Physical",
                              "virtualAccuracy" = "Virtual",
                              "floatingAccuracy" = "Floating"))
data_by_gem$gemType<-as.factor(data_by_gem$gemType) #set as a factor
#separate data for day and night plots
dayIdx = which(data_by_gem$light == 'Day')
nightIdx = which(data_by_gem$light == 'Night')
data_by_gem_day <- data_by_gem[dayIdx,]
data_by_gem_day$audioPlotCode<-as.factor(data_by_gem_day$audioPlotCode) #set as a factor
data_by_gem_night <- data_by_gem[nightIdx,]
data_by_gem_night$audioPlotCode<-as.factor(data_by_gem_night$audioPlotCode) #set as a factor

#Format data for audio RT and accuracy analysis
#select only dual audio condition
audioIdx = which(formatted_data$audio == 'dual')
data_dual_task <- formatted_data[audioIdx,]
row.names(data_dual_task) <- NULL

#ANALYSES-----------------------------------------------------------------------
## Average RT audio condition
#Plot data
audioRtPlot <- ggplot(data_dual_task, aes(light,rt_mean_per_trial,fill = light))
audioRtPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  geom_point(aes(x = light), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  coord_cartesian(ylim=c(0,4))+
  labs(y= "RT (s)", x = "Lighting")+
  scale_x_discrete(labels=c("1"="Day","2"="Night"))+
  scale_fill_brewer(palette="Paired",)+theme_minimal()

#Independent samples t-test
rtMeanPerTrial.ttest <- data_dual_task %>%
  t_test(rt_mean_per_trial ~ light, var.equal = TRUE) %>%
  add_significance()
rtMeanPerTrial.ttest

## Average accuracy audio condition
#Plot data
audioAccuracyPlot <- ggplot(data_dual_task, aes(light,audioAccuracy,fill = light))
audioAccuracyPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  geom_point(aes(x = light), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  coord_cartesian(ylim=c(.5,1))+
  labs(y= "p(correct)", x = "Lighting")+
  scale_x_discrete(labels=c("1"="Day","2"="Night"))+
  scale_fill_brewer(palette="Paired")+theme_minimal()

#Independent samples t-test
audioAccuracy.ttest <- data_dual_task %>%
  t_test(audioAccuracy ~ light, var.equal = TRUE) %>%
  add_significance()
audioAccuracy.ttest

## Distance Traveled
#Plot data
distanceTraveledPlot <- ggplot(formatted_data, aes(light,distanceTraveled,fill = audioPlotCode))
distanceTraveledPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(200,600))+
  geom_point(aes(x = light), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "Distance (m)", x = "Lighting",fill = "Audio")+
  scale_x_discrete(labels=c("day"="Day","night"="Night"))+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

# Two-way mixed ANOVA
distanceTraveled.aov <- anova_test(
  data = formatted_data, dv = distanceTraveled, wid = participant_number,
  between = light, within = audio
)
get_anova_table(distanceTraveled.aov)

## Head Rotation
#Plot data
totalRotationPlot <- ggplot(formatted_data, aes(light,totalRotation,fill = audioPlotCode))
totalRotationPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(100,300))+
  geom_point(aes(x = light), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "Accumulated Quaternion Distance Norm", x = "Lighting", fill = "Audio")+
  scale_x_discrete(labels=c("day"="Day","night"="Night"))+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

# Two-way mixed ANOVA
totalRotation.aov <- anova_test(
  data = formatted_data, dv = totalRotation, wid = participant_number,
  between = light, within = audio
)
get_anova_table(totalRotation.aov)

# Pairwise comparison for the simple main effect of audio 
totalRotation.pwc <- formatted_data %>%
  pairwise_t_test(
    totalRotation ~ audio, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
totalRotation.pwc

## Elapsed Time
#Plot data
totalSessionTimePlot <- ggplot(formatted_data, aes(light,totalSessionTime,fill = audioPlotCode))
totalSessionTimePlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(200,700))+
  geom_point(aes(x = light), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "Total Time (s)", x = "Lighting",fill = "Audio")+
  scale_x_discrete(labels=c("day"="Day","night"="Night"))+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

# Two-way mixed ANOVA
totalSessionTime.aov <- anova_test(
  data = formatted_data, dv = totalSessionTime, wid = participant_number,
  between = light, within = audio
)
get_anova_table(totalSessionTime.aov)

##Gem type analysis
#accuracy

#plot day data
gemAccuracyDayPlot <- ggplot(data_by_gem_day, aes(gemType,accuracy,fill = audioPlotCode))
gemAccuracyDayPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0.5,1))+
  geom_point(aes(x = gemType), shape = 21, position = position_jitterdodge(jitter.width = 0.5, jitter.height=0, dodge.width=0.9)) +
  labs(y= "p(correct)", x = "Gem Type",title="Day",fill = "Audio")+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()
#plot night data
gemAccuracyNightPlot <- ggplot(data_by_gem_night, aes(gemType,accuracy,fill = audioPlotCode))
gemAccuracyNightPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0.5,1))+
  geom_point(aes(x = gemType), shape = 21, position = position_jitterdodge(jitter.width = 0.5, jitter.height=0, dodge.width=0.9)) +
  labs(y= "p(correct)", x = "Gem Type",title="Night",fill = "Audio")+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

#3-way mixed ANOVA
gemAccuracy.aov <- anova_test(
  data = data_by_gem, dv = accuracy, wid = participant_number, between = light,
  within = c(audio,gemType)
)
get_anova_table(gemAccuracy.aov)

# Pairwise comparison for the simple main effect of gemType
gemAccuracy.pwc <- data_by_gem %>%
  pairwise_t_test(
    accuracy ~ gemType, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
gemAccuracy.pwc

#detections
gemDetectionDayPlot <- ggplot(data_by_gem_day, aes(gemType,detectionAccuracy,fill = audioPlotCode))
gemDetectionDayPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0.5,1))+
  geom_point(aes(x = gemType), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(detections)", x = "Gem Type",title="Day",fill = "Audio")+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

gemDetectionNightPlot <- ggplot(data_by_gem_night, aes(gemType,detectionAccuracy,fill = audioPlotCode))
gemDetectionNightPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0.5,1))+
  geom_point(aes(x = gemType), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(detections)", x = "Gem Type",title="Night",fill = "Audio")+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

#3-way mixed ANOVA
gemDetectionAccuracy.aov <- anova_test(
  data = data_by_gem, dv = detectionAccuracy, wid = participant_number, between = light,
  within = c(audio,gemType)
)
get_anova_table(gemDetectionAccuracy.aov)

# Pairwise comparison for the simple main effect of gemType
gemDetectionAccuracy.pwc <- data_by_gem %>%
  pairwise_t_test(
    detectionAccuracy ~ gemType, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
gemDetectionAccuracy.pwc

#discriminations
gemDiscrimDayPlot <- ggplot(data_by_gem_day, aes(gemType,discriminationAccuracy,fill = audioPlotCode))
gemDiscrimDayPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0.5,1))+
  geom_point(aes(x = gemType), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(discriminations)", x = "Gem Type",title="Day",fill = "Audio")+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

gemDiscrimNightPlot <- ggplot(data_by_gem_night, aes(gemType,discriminationAccuracy,fill = audioPlotCode))
gemDiscrimNightPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0.5,1))+
  geom_point(aes(x = gemType), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(discriminations)", x = "Gem Type",title="Night",fill = "Audio")+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

#3-way mixed ANOVA
gemDiscriminationAccuracy.aov <- anova_test(
  data = data_by_gem, dv = discriminationAccuracy, wid = participant_number, between = light,
  within = c(audio,gemType)
)
get_anova_table(gemDiscriminationAccuracy.aov)

# Pairwise comparison for the simple main effect of gemType
gemDiscriminationAccuracy.pwc <- data_by_gem %>%
  pairwise_t_test(
    discriminationAccuracy ~ gemType, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
gemDiscriminationAccuracy.pwc