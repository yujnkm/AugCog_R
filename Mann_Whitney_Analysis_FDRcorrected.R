install.packages("coin")
library("coin")
library("dplyr")

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
MW.results24 <- data.frame(c(1:24*0),c(1:24*0),c(1:24*0),c(1:24*0))
colnames(MW.results24) <- c('Z', 'p', 'p.adjust', 'effectSize')
for (idx in 3:26){
  mwu <- wilcox_test(my_qual_data24[,idx] ~ my_qual_data24$light)
  Z = as.numeric(statistic(mwu, type="standardized"))
  p = pvalue(mwu)
  effect_size <- Z^2/sqrt(24)
  rownames(MW.results24)[idx-2] = colnames(my_qual_data24[idx])
  MW.results24[idx-2,1] <- round(Z, digits = 2)
  MW.results24[idx-2,2] <- p
  MW.results24[idx-2,4] <- round(effect_size, digits = 2)
}

MW.results24$p.adjust <- p.adjust(MW.results24$p, method = 'fdr', n = length(MW.results24$p))
MW.results24$p <- round(MW.results24$p, digits = 4)
MW.results24$p.adjust <- round(MW.results24$p.adjust, digits = 4)
for (idx in 1:24){
  if (MW.results24$p[idx] < 0.05 && MW.results24$p[idx] > 0.01) {
    MW.results24$p[idx] <- paste(c(MW.results24$p[idx], "*"), collapse = "")
  } else if (MW.results24$p[idx] < 0.01 && MW.results24$p[idx] > 0.001) {
    MW.results24$p[idx] <- paste(c(MW.results24$p[idx], "**"), collapse = "")
  } else if (MW.results24$p[idx] < 0.001) {
    MW.results24$p[idx] <- paste(c(MW.results24$p[idx], "***"), collapse = "")
  }
  if (MW.results24$p.adjust[idx] < 0.05 && MW.results24$p.adjust[idx] > 0.01) {
    MW.results24$p.adjust[idx] <- paste(c(MW.results24$p.adjust[idx], "*"), collapse = "")
  } else if (MW.results24$p.adjust[idx] < 0.01 && MW.results24$p.adjust[idx] > 0.001) {
    MW.results24$p.adjust[idx] <- paste(c(MW.results24$p.adjust[idx], "**"), collapse = "")
  } else if (MW.results24$p.adjust[idx] < 0.001) {
    MW.results24$p.adjust[idx] <- paste(c(MW.results24$p.adjust[idx], "***"), collapse = "")
  }
}

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
MW.results48 <- data.frame(c(1:24*0),c(1:24*0),c(1:24*0),c(1:24*0))
colnames(MW.results48) <- c('Z', 'p', 'p.adjust', 'effectSize')
for (idx in 3:26){
  mwu <- wilcox_test(my_qual_data48[,idx] ~ my_qual_data48$light)
  Z = as.numeric(statistic(mwu, type="standardized"))
  p = pvalue(mwu)
  effect_size <- Z^2/sqrt(48)
  rownames(MW.results48)[idx-2] = colnames(my_qual_data48[idx])
  MW.results48[idx-2,1] <- round(Z, digits = 2)
  MW.results48[idx-2,2] <- p
  MW.results48[idx-2,4] <- round(effect_size, digits = 2)
}

MW.results48$p.adjust <- p.adjust(MW.results48$p, method = 'fdr', n = length(MW.results48$p))
MW.results48$p <- round(MW.results48$p, digits = 4)
MW.results48$p.adjust <- round(MW.results48$p.adjust, digits = 4)
for (idx in 1:24){
  if (MW.results48$p[idx] < 0.05 && MW.results48$p[idx] > 0.01) {
    MW.results48$p[idx] <- paste(c(MW.results48$p[idx], "*"), collapse = "")
  } else if (MW.results48$p[idx] < 0.01 && MW.results48$p[idx] > 0.001) {
    MW.results48$p[idx] <- paste(c(MW.results48$p[idx], "**"), collapse = "")
  } else if (MW.results48$p[idx] < 0.001) {
    MW.results48$p[idx] <- paste(c(MW.results48$p[idx], "***"), collapse = "")
  }
  if (MW.results48$p.adjust[idx] < 0.05 && MW.results48$p.adjust[idx] > 0.01) {
    MW.results48$p.adjust[idx] <- paste(c(MW.results48$p.adjust[idx], "*"), collapse = "")
  } else if (MW.results48$p.adjust[idx] < 0.01 && MW.results48$p.adjust[idx] > 0.001) {
    MW.results48$p.adjust[idx] <- paste(c(MW.results48$p.adjust[idx], "**"), collapse = "")
  } else if (MW.results48$p.adjust[idx] < 0.001) {
    MW.results48$p.adjust[idx] <- paste(c(MW.results48$p.adjust[idx], "***"), collapse = "")
  }
}