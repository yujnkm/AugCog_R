#Questionnaire analysis
library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)

#load data with 24 subjects
my_qual_data24 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/Post-Questionnaire24Set.csv")

#22 variables without trial_length_guess and number_gems_guess
model24 <- lm(cbind(enjoyment,comfortable,stable,convincing,walk_through_objects,ease_controller,audio_audibility,fatigue,disoriented_during,disoriented_after,sufficient_light,real_object_visibility,virtual_object_visibility,virtual_obj_brightness_absolute,virtual_obj_brightness_relative,likelihood_walk_into_physical,found_all_gems,gem_categorization_difficulty,audio_task_difficulty,emphasis_what_task,performance_over_time,recommend) ~ day_night, my_qual_data24)
Manova(model24, test.statistic = "Wilks")

#all 24 variables
model24 <- lm(cbind(enjoyment,comfortable,stable,convincing,walk_through_objects,ease_controller,audio_audibility,fatigue,disoriented_during,disoriented_after,sufficient_light,real_object_visibility,virtual_object_visibility,virtual_obj_brightness_absolute,virtual_obj_brightness_relative,likelihood_walk_into_physical,found_all_gems,number_gems_guess,gem_categorization_difficulty,audio_task_difficulty,emphasis_what_task,trial_length_guess,performance_over_time,recommend) ~ day_night, my_qual_data24)

#post-hoc Games-Howell
pwc24 <- my_qual_data24 %>%
  gather(key = "variables", value = "value", enjoyment, comfortable, stable, convincing, walk_through_objects, ease_controller, audio_audibility, fatigue, disoriented_during, disoriented_after, sufficient_light, real_object_visibility, virtual_object_visibility, virtual_obj_brightness_absolute, virtual_obj_brightness_relative, likelihood_walk_into_physical, found_all_gems, number_gems_guess, gem_categorization_difficulty, audio_task_difficulty, emphasis_what_task, trial_length_guess, performance_over_time, recommend) %>%
  group_by(variables) %>%
  games_howell_test(value ~ day_night)
print(pwc24,n = Inf)

#load data with 48 subjects
my_qual_data48 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/all_data_48.csv")
ridx <- seq(1, 288, by = 6)
my_qual_data48 <- my_qual_data48[ridx,]
row.names(my_qual_data48) <- NULL

model48 <- lm(cbind(post_enjoyment,post_comfortable,post_stable,post_convincing,post_walk_through_objects,post_ease_controller,post_audio_audibility,post_fatigue,post_disoriented_during,post_disoriented_after,post_sufficient_light,post_real_object_visibility,post_virtual_object_visibility,post_virtual_object_brightness_overall,post_virtual_object_brightness_relative,post_likelihood_walk_into_physical,post_found_all_gems,post_gem_categorization_difficulty,post_audio_task_difficulty,post_emphasis_what_task,post_performance_over_time,post_recommend,post_number_gems_guess,post_trial_length_guess) ~ light, my_qual_data48)
Manova(model48, test.statistic = "Wilks")

#post-hoc Games-Howell
pwc48 <- my_qual_data48 %>%
  gather(key = "variables", value = "value", post_enjoyment, post_comfortable, post_stable, post_convincing, post_walk_through_objects, post_ease_controller, post_audio_audibility, post_fatigue, post_disoriented_during, post_disoriented_after, post_sufficient_light, post_real_object_visibility, post_virtual_object_visibility, post_virtual_object_brightness_overall, post_virtual_object_brightness_relative, post_likelihood_walk_into_physical, post_found_all_gems, post_number_gems_guess, post_gem_categorization_difficulty, post_audio_task_difficulty, post_emphasis_what_task, post_trial_length_guess, post_performance_over_time, post_recommend) %>%
  group_by(variables) %>%
  games_howell_test(value ~ light)
print(pwc48,n = Inf)

#pre-questionnaire analysis between night and day
my_qual_data48 <- read.csv("~/Documents/UCSB/Research/AR Study/Analysis/all_data_48.csv")
ridx <- seq(1, 288, by = 6)
my_qual_data48 <- my_qual_data48[ridx,]
row.names(my_qual_data48) <- NULL
pwc48 <- my_qual_data48 %>%
  gather(key = "variables", value = "value", pre_age, pre_dont_enjoy_giving_directions, pre_not_important_know_where_i_am, pre_familiarity_vr, pre_times_vr) %>%
  group_by(variables) %>%
  games_howell_test(value ~ light)
print(pwc48,n = Inf)

#t-test for light on times used VR
time_used_vr_day <- my_qual_data48[my_qual_data48$light == "day",62] 
time_used_vr_night <- my_qual_data48[my_qual_data48$light == "night",62] 
t.test(time_used_vr_day, time_used_vr_night, var.equal = TRUE)
