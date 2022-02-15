library(rstatix)
library(ggplot2)
library(dplyr)

#FORMAT DATA-----------------------------------------------------------------------
#outputs: formatted data(light, audio & audio_order), data_by_gem(light, audio, gem & audio_order)
#read csv
my_quant_data48 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/all_data_48.csv")

#format data
cidx <- c(1,3,7,11:17,105:111)
my_quant_data48 <- my_quant_data48[,cidx]
formatted_data <- as.data.frame(matrix(0, nrow = 48*2, ncol = 19));
subIdx <- seq(1, 288, by = 6)
for (sub in 1:48) {
  #select data for this subject
  thisIdx = subIdx[sub]:(subIdx[sub]+5);
  thisData = my_quant_data48[thisIdx,]
  #separate data by dual and single task
  trueIdx = which(thisData$audio == 'True')
  falseIdx = which(thisData$audio == 'False')
  if (thisData[1,3] == 'True'){
    audioOrder = 'First'; #audio first
  } else if (thisData[1,3] == 'False'){
    audioOrder = 'Second'; #audio second
  }
  #enter into new matrix
  formatted_data[(sub*2)-1,1] = sub; #subject number
  formatted_data[sub*2,1] = sub; #subject number
  formatted_data[(sub*2)-1,4:17] = colMeans(thisData[trueIdx,4:17]) #dual task means
  formatted_data[sub*2,4:17] = colMeans(thisData[falseIdx,4:17]) #single task means
  formatted_data[(sub*2)-1,3] = 'Dual'; 
  formatted_data[sub*2,3] = 'Single'; 
  formatted_data[(sub*2)-1,19] = 0 #single 
  formatted_data[sub*2,19] = 1 #dual
  formatted_data[(sub*2)-1,18] = audioOrder; #audio order
  formatted_data[sub*2,18] = audioOrder; #audio order
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
colnames(formatted_data)[18] <- "audio_order"
colnames(formatted_data)[19] <- "audioPlotCode"
formatted_data$light<-as.factor(formatted_data$light) #set as a factor
formatted_data$audio<-as.factor(formatted_data$audio) #set as a factor
formatted_data$audioPlotCode<-as.factor(formatted_data$audioPlotCode) #set as a factor
formatted_data$participant_number<-as.factor(formatted_data$participant_number) #set as a factor
formatted_data$audio_order<-as.factor(formatted_data$audio_order) #set as a factor
formatted_data <- formatted_data[-c(3,4,17,18,29,30,49,50), ]#2,9,15,25

#format the data for analysis of gem type
cidx <- c(1:6,11:16,18:19)
thisData <- formatted_data[,cidx]
data_by_gem <- as.data.frame(matrix(0, nrow = 44*6, ncol = 9));
colnames(data_by_gem) <- c('participant_number','light','audio','audio_order','audioPlotCode','gemType','accuracy','detectionAccuracy','discriminationAccuracy')

data_by_gem1 <- thisData %>%
  gather(key = "gemType", value = "accuracy", physicalAccuracy, virtualAccuracy, floatingAccuracy)
data_by_gem2  <- thisData %>%
  gather(key = "gemTpye2", value = "detectionAccuracy", physicalDetectionAccuracy, virtualDetectionAccuracy, floatingDetectionAccuracy)
data_by_gem3  <- thisData %>%
  gather(key = "gemType3", value = "discriminationAccuracy", physicalDiscriminationAccuracy, virtualDiscriminationAccuracy, floatingDiscriminationAccuracy)

data_by_gem[,1:7] = data_by_gem1[,c(1:3,10:13)]
data_by_gem[,8] = data_by_gem2[,13]
data_by_gem[,9] = data_by_gem3[,13]
data_by_gem <- data_by_gem %>% mutate(gemType = recode(gemType,
                                                       "physicalAccuracy" = "Physical",
                                                       "virtualAccuracy" = "Virtual",
                                                       "floatingAccuracy" = "Floating"))
data_by_gem$gemType<-as.factor(data_by_gem$gemType) #set as a factor

#separate data for day and night plots
dayIdx = which(data_by_gem$light == 'Day')
nightIdx = which(data_by_gem$light == 'Night')
data_by_gem_day <- data_by_gem[dayIdx,]
data_by_gem_night <- data_by_gem[nightIdx,]
data_by_gem$audio<-as.factor(data_by_gem$audio) #set as a factor
data_by_gem$participant_number<-as.factor(data_by_gem$participant_number) #set as a factor
data_by_gem$audio_order<-as.factor(data_by_gem$audio_order) #set as a factor
data_by_gem$light<-as.factor(data_by_gem$light) #set as a factor
data_by_gem$audioPlotCode<-as.factor(data_by_gem$audioPlotCode) #set as a factor
#ANALYSES-----------------------------------------------------------------------
## Distance Traveled
#three-way mixed ANOVA
distanceTraveled.aov <- anova_test(
  data = formatted_data, dv = distanceTraveled, wid = participant_number, between = c(light,audio_order),
  within = audio)
get_anova_table(distanceTraveled.aov)

# Effect of task at each audio order
distanceTraveled.oneway <- formatted_data %>%
  group_by(audio_order) %>%
  anova_test(dv = distanceTraveled, wid = participant_number, within = c(audio)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
distanceTraveled.oneway

distanceTraveled.pwc <- formatted_data %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    distanceTraveled ~ audio, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
distanceTraveled.pwc

#Plot data significant findings
distanceTraveledPlot <- ggplot(formatted_data, aes(audio_order,distanceTraveled,fill = audioPlotCode))
distanceTraveledPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(200,600))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "Distance (m)", x = "Audio Order", fill = 'Audio')+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

## Head Rotation
#three-way mixed ANOVA
totalRotation.aov <- anova_test(
  data = formatted_data, dv = totalRotation, wid = participant_number,
  between = c(light,audio_order), within = audio
)
get_anova_table(totalRotation.aov)

# Effect of task at each audio order
totalRotation.oneway <- formatted_data %>%
  group_by(audio_order) %>%
  anova_test(dv = totalRotation, wid = participant_number, within = c(audio)) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
totalRotation.oneway

totalRotation.pwc <- formatted_data %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    totalRotation ~ audio, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
totalRotation.pwc

#Plot data significant findings
totalRotationPlot <- ggplot(formatted_data, aes(audio_order,totalRotation,fill = audioPlotCode))
totalRotationPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0,300))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "Accumulated Quaternion Distance Norm", x = "Audio Order", fill = 'Audio')+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

##Elapsed time
# three-way mixed ANOVA
totalSessionTime.aov <- anova_test(
  data = formatted_data, dv = totalSessionTime, wid = participant_number,
  between = c(light,audio_order), within = audio
)
get_anova_table(totalSessionTime.aov)

# Effect of task at each audio order
totalSessionTime.oneway <- formatted_data %>%
  group_by(audio_order) %>%
  anova_test(dv = totalSessionTime, wid = participant_number, within = audio) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
totalSessionTime.oneway

totalSessionTime.pwc <- formatted_data %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    totalSessionTime ~ audio, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
totalSessionTime.pwc

#Plot data significant findings
totalSessionTimePlot <- ggplot(formatted_data, aes(audio_order,totalSessionTime,fill = audioPlotCode))
totalSessionTimePlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(200,700))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "Total Time (s)", x = "Audio Order", fill = 'Audio')+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

##Gem type analysis
#accuracy
#four-way mixed ANOVA
gemAccuracy.aov <- anova_test(
  data = data_by_gem, dv = accuracy, wid = participant_number, between = c(light,audio_order),
  within = c(audio,gemType)
)
get_anova_table(gemAccuracy.aov)

# Effect of task at each audio order
accuracyTaskOrder.pwc <- data_by_gem %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    accuracy ~ audio, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
accuracyTaskOrder.pwc

#Plot data significant findings
accuracyTaskOrderPlot <- ggplot(data_by_gem, aes(audio_order,accuracy,fill = audioPlotCode))
accuracyTaskOrderPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(correct)", x = "Audio Order", fill = 'Audio')+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

# Effect of gem at each audio order
accuracyGemOrder.pwc <- data_by_gem %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    accuracy ~ gemType, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
accuracyGemOrder.pwc

#Plot data significant findings
accuracyGemOrderPlot <- ggplot(data_by_gem, aes(audio_order,accuracy,fill = gemType))
accuracyGemOrderPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(correct)", x = "Audio Order", fill = 'Gem Type')+
  scale_fill_brewer(palette="Paired")+theme_minimal()

##detections
#4-way mixed ANOVA
#four-way mixed ANOVA
gemDetectionAccuracy.aov <- anova_test(
  data = data_by_gem, dv = detectionAccuracy, wid = participant_number, between = c(light,audio_order),
  within = c(audio,gemType)
)
get_anova_table(gemDetectionAccuracy.aov)

# Effect of task at each audio order
detectionAccuracyTaskOrder.pwc <- data_by_gem %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    detectionAccuracy ~ audio, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
detectionAccuracyTaskOrder.pwc

#Plot data significant findings
detectionAccuracyTaskOrderPlot <- ggplot(data_by_gem, aes(audio_order,detectionAccuracy,fill = audioPlotCode))
detectionAccuracyTaskOrderPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(detections)", x = "Audio Order", fill = 'Audio')+
  scale_fill_brewer(palette="Paired",labels=c("0"="Single","1"="Dual"))+theme_minimal()

# Effect of gem type at each audio order
detectionAccuracyGemOrder.pwc <- data_by_gem %>%
  group_by(audio_order) %>%
  pairwise_t_test(
    detectionAccuracy ~ gemType, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
detectionAccuracyGemOrder.pwc

#Plot data significant findings
detectionAccuracyGemOrderPlot <- ggplot(data_by_gem, aes(audio_order,accuracy,fill = gemType))
detectionAccuracyGemOrderPlot + geom_bar(position = 'dodge', stat = 'summary', fun = 'mean') +
  geom_errorbar(stat = 'summary', position=position_dodge(.9), width = 0.2) +
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x = audio_order), shape = 21, position = 
               position_jitterdodge(jitter.width = 0.5, jitter.height=0, 
                                    dodge.width=0.9)) +
  labs(y= "p(detections)", x = "Audio Order", fill = 'Gem Type')+
  scale_fill_brewer(palette="Paired")+theme_minimal()

##discriminations
#four-way mixed ANOVA
gemDiscrimAccuracy.aov <- anova_test(
  data = data_by_gem, dv = discriminationAccuracy, wid = participant_number, between = c(light,audio_order),
  within = c(audio,gemType)
)
get_anova_table(gemDiscrimAccuracy.aov)

# no significant interactions with audio order
