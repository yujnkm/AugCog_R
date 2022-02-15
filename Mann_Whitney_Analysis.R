install.packages("coin")
library("coin")
library("dplyr")

#read csv
my_qual_data48 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/all_data_48.csv")

#format data
ridx <- seq(1, 288, by = 6)
cidx <- c(1,3,66:75,89:99,101:103)
my_qual_data48 <- my_qual_data48[ridx,cidx]
row.names(my_qual_data48) <- NULL
for (idx in 1:length(my_qual_data48$light)) {
  if (my_qual_data48[idx,2] == "day") {
    my_qual_data48[idx,2] = 1;
  } else if (my_qual_data48[idx,2] == "night")
    my_qual_data48[idx,2] = 2;
}
my_qual_data48$light <- as.factor(my_qual_data48$light)

#24 Mann-Whitney U tests w. Z approximation output
MW.results <- data.frame(c(1:24*0),c(1:24*0),c(1:24*0))
colnames(MW.results) <- c('Z', 'p', 'effectSize')
for (idx in 3:26){
  mwu <- wilcox_test(my_qual_data48[,idx] ~ my_qual_data48$light)
  Z = as.numeric(statistic(mwu, type="standardized"))
  p = pvalue(mwu)
  effect_size <- Z^2/sqrt(48)
  rownames(MW.results)[idx-2] = colnames(my_qual_data48[idx])
  p <- round(p, digits = 4)
  if (p < 0.05 && p > 0.01) {
    p <- paste(c(p, "*"), collapse = "")
  } else if (p < 0.01 && p > 0.001) {
    p <- paste(c(p, "**"), collapse = "")
  } else if (p < 0.001) {
    p <- paste(c(p, "***"), collapse = "")
  }
  MW.results[idx-2,1] <- round(Z, digits = 2)
  MW.results[idx-2,2] <- p
  MW.results[idx-2,3] <- round(effect_size, digits = 2)
}

#run one analysis
mwu_post_comfortable <- wilcox_test(post_comfortable ~ light, data = my_qual_data48, distribution = "exact")
Z_post_comfortable = as.numeric(statistic(mwu_post_comfortable, type="standardized"))
p_post_comfortable = pvalue(mwu_post_comfortable)
effect_size_post_comfortable <- Z_post_comfortable^2/sqrt(48)

#format data for 24 sub data set
my_qual_data24 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/Post-Questionnaire24Set.csv")
cidx <- c(2:4,6:9,11:15,29:39,41:42,44)
my_qual_data24 <- my_qual_data24[,cidx]
row.names(my_qual_data24) <- NULL
for (idx in 1:length(my_qual_data24$day_night)) {
  if (my_qual_data24[idx,2] == "Day") {
    my_qual_data24[idx,2] = 1;
  } else if (my_qual_data24[idx,2] == "Night")
    my_qual_data24[idx,2] = 2;
}
my_qual_data24$day_night <- as.factor(my_qual_data24$day_night)
colnames(my_qual_data24)[2] <- c('light')
my_qual_data24$light <- as.factor(my_qual_data24$light)

#24 Mann-Whitney U tests w. Z approximation output
MW.results24 <- data.frame(c(1:24*0),c(1:24*0),c(1:24*0))
colnames(MW.results24) <- c('Z', 'p', 'effectSize')
for (idx in 3:26){
  mwu <- wilcox_test(my_qual_data24[,idx] ~ my_qual_data24$light)
  Z = as.numeric(statistic(mwu, type="standardized"))
  p = pvalue(mwu)
  effect_size <- Z^2/sqrt(24)
  rownames(MW.results24)[idx-2] = colnames(my_qual_data24[idx])
  p <- round(p, digits = 4)
  if (p < 0.05 && p > 0.01) {
    p <- paste(c(p, "*"), collapse = "")
  } else if (p < 0.01 && p > 0.001) {
    p <- paste(c(p, "**"), collapse = "")
  } else if (p < 0.001) {
    p <- paste(c(p, "***"), collapse = "")
  }
  MW.results24[idx-2,1] <- round(Z, digits = 2)
  MW.results24[idx-2,2] <- p
  MW.results24[idx-2,3] <- round(effect_size, digits = 2)
}