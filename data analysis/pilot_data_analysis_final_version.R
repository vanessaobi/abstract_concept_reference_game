## Title: Analyses pilot data
## Description: Analyzing pilot data abstract concept-level reference game
## Author: Vanessa Obi
## Last Edit: 2025-06-08

# This script analyses the pilot data for the abstract concept-level reference game

# loading libraries-----------------------------------------------------
# package for convenience functions (e.g. ggplot2, dplyr, etc.)
library(tidyverse)
#install.packages("ggpattern")
library(ggpattern)
# package for color universal design (optimized color palette)
library(oneclust)

# general settings------------------------------------------------------
# global color scheme / non-optimized
project_colors = cud(1:7)
project_colors[4] = "gray"

# setting theme colors globally
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values = project_colors)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values = project_colors)
}

set.seed(213)

## getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# Reading in files
# ----------------------------------------------------------------------
# Reading in the labvanced files
files = file.path("data_pilot_complete", 
                  list.files(path="data_pilot_complete", 
                             pattern="Session_[0-9]*"), 
                  "trials_and_sessions.csv")

data <- read_csv(files, 
                 col_select = c(Multi_User_Group_Id,
                                Crowdsourcing_SubjId,
                                Subject_Nr,
                                Role_Id,
                                Task_Nr,
                                Trial_Nr,                                   
                                Trial_Id,
                                Input,
                                process_time_listener,
                                rt_speaker,
                                condition,
                                category,
                                correct_selection,
                                choice_precise1,
                                choice_precise2,
                                choice_precise3,
                                difficulty_answer_listener,
                                difficulty_answer_speaker,
                                easiness_answer_listener,
                                easiness_answer_speaker,
                                need_for_discussion_listener,
                                need_for_discussion_speaker,
                                Completed,
                                SelectedAge,
                                SelectedGender,
                                SelectedLanguage,
                                SelectedLocation,
                                Start_Time,
                                End_Time))

df <- data %>%
  dplyr::rename(Group_Id = Multi_User_Group_Id,
                Prolific_Id = Crowdsourcing_SubjId,
                Subject_Id = Subject_Nr,
                rt_listener = process_time_listener,
                selection1 = choice_precise1,
                selection2 = choice_precise2,
                selection3 = choice_precise3,
                success = correct_selection,
                Age = SelectedAge,
                Gender = SelectedGender,
                Language = SelectedLanguage,
                Location = SelectedLocation)  %>% 
  filter(Task_Nr %in% c(3, 4)) # filter out introduction screen, test trials, and debriefing data


# ----------------------------------------------------------------------
# Data cleaning
# ----------------------------------------------------------------------
# because some participants did parts of the experiment before their completed session 

df_ordered <- df[order(df$Prolific_Id),]
table(df$Prolific_Id) #count number of trials to see who needs to be excluded 
#write.csv(df_ordered, "test.csv") #look at which sessions are affected

## exclude participants  67acd767e2a4a71268cc4e44, 673d00ce7364481560d1f3b6, 65577234800b1fea9ae3cf4d (completed only 13 trials)

df_cleaned <-  df %>% 
  filter(!Prolific_Id %in% c( "67acd767e2a4a71268cc4e44", "673d00ce7364481560d1f3b6", "65577234800b1fea9ae3cf4d"))

# because of inconsistency in naming 
df_cleaned$category <- recode(df_cleaned$category, 
                              "Sports" = "Sport", 
                              "Buildings" = "Building", 
                              "Fruits" = "Fruit", 
                              "Plants" = "Plant", 
                              "Tools" = "Tool")

write.csv(df_cleaned, "data_cleaned.csv") 
# ----------------------------------------------------------------------
# 2) Gender
# How many participants were female, how many were male, how many other?

gender_nr <- df_cleaned %>% 
  filter(Task_Nr == 3, Trial_Nr == 1) %>% 
  count(Gender) 
gender_nr
# 5 female, 3 male, 0 diverse

# ----------------------------------------------------------------------
# 3) Age 
# What was the age of the participants?

age_agr <- df_cleaned %>%
  filter(Task_Nr == 3, Trial_Nr == 1) %>% 
  summarize(min=min(Age), max=max(Age),
            median=median(Age), mean=mean(Age), sd=sd(Age))
age_agr
# --> min: 18, max: 30
# --> mean: 25.1, sd:3.70, median: 25.5

# ----------------------------------------------------------------------


# merge speaker and listener trials 
df_speaker <- df_cleaned %>% 
  filter(Role_Id == 1, Task_Nr == 3) %>% 
  select(Group_Id, Subject_Id, Trial_Nr, Trial_Id, condition, category, 
         success, rt_speaker, Input)

df_listener <- df_cleaned %>% 
  filter(Role_Id == 2, Task_Nr == 3) %>% 
  select(Group_Id, Subject_Id, Trial_Nr, Trial_Id, condition, category, 
         success, rt_listener, Input)

df_merged <- merge(df_speaker, df_listener, 
                   by=intersect(names(df_speaker), names(df_listener)), all.y=TRUE)

#write_csv(df_merged, "df_merged.csv")

# encode clue type

# load encoded data
df_encoded <- read_csv("df_encoded.csv")

df_encoded <- df_encoded %>% 
  mutate(clue_type = recode(clue_type,
                            '1' = 'Superordinate',
                            '2' = 'Modified Superordinate',
                            '3' = 'Other Superordinate',
                            '4' = 'Modified Other Superordinate',
                            '5' = 'Set Operational',
                            '6' = 'Property',
                            '7' = 'Thing-Property',
                            '8' = 'Context Association',
                            '9' = 'Additional Exemplar',
                            '0' = 'invalid'
  )) %>% 
  filter(!is.na(clue_type))%>% 
  mutate(int_superordinate = ifelse(clue_type %in% c("Superordinate"), TRUE, FALSE))

# because of inconsistency in naming 
df_encoded$category <- recode(df_encoded$category, 
                              "Sports" = "Sport", 
                              "Buildings" = "Building", 
                              "Fruits" = "Fruit", 
                              "Plants" = "Plant", 
                              "Tools" = "Tool")

write_csv(df_encoded, "df_encoded.csv")

#-----------------------------------------------------
# listener communicative success analysis
#-----------------------------------------------------

### mean listener accuracy

# mean success across trials
df_success_mean <- df_merged %>%
  group_by(condition) %>%
  summarize(success_mean = mean(success),  
            se = sd(success) / sqrt(n()))

df_success_mean

#condition success_mean     se
# Abstract          0.82 0.0549
# Concrete          0.76 0.0610


ggplot(df_success_mean, aes(x = condition, y = success_mean, fill = condition)) +
  geom_point(size = 7, aes(color = condition))+
  geom_errorbar(aes(ymin = success_mean - se, ymax = success_mean + se), width = 0.2) + 
  scale_y_continuous(labels = scales::percent) +
  labs( y = "Listener Accuracy", x = NULL ) +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13))

ggsave("mean_success.jpg", width = 7.00, height = 4.67, units = "in")

#-----------------------------------------------------
# clue type/listener communicative success analysis
#-----------------------------------------------------

### intended Superordinate vs other (Success and Proportion) -> Figure 3

df_clue_type_accuracy <- df_encoded %>%
  #filter(!is.na(rt_speaker)) %>% -> no filtering of speaker data because success depends on listener
  group_by(condition, int_superordinate) %>% 
  summarize(mean_accuracy = mean(success), se_accuracy = sd(success) / sqrt(n()))%>%
  mutate(int_superordinate = factor(int_superordinate,
                                    levels = c(TRUE, FALSE),
                                    labels = c("Intended\nSuperordinate", "Other")))

df_clue_type_accuracy

#condition int_superordinate mean_accuracy se_accuracy
# Abstract  Other                     0.538      0.144 
# Abstract  Intended Superordinate    0.919      0.0455
# Concrete  Other                     0.6        0.131 
# Concrete  Intended Superordinate    0.829      0.0646

ggplot(df_clue_type_accuracy, aes(x = int_superordinate, y = mean_accuracy, color = condition)) +
  geom_point(size = 7, alpha = 0.8, position = position_dodge(width = 0.1)) +  
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy, ymax = mean_accuracy + se_accuracy),
                width = 0.1, position = position_dodge(width = 0.1), alpha = 0.5) +
  geom_line(data = df_clue_type_accuracy,
            aes(group = condition, linetype = condition), linewidth = 0.4, position = position_dodge(width = 0.1)) +
  scale_y_continuous(limits = c(0.38, 1), breaks = c(0.4, 0.6, 0.8, 1), labels = scales::percent) +
  labs(x = "Clue Type", y = "Listener Accuracy", fill = "Condition") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18, margin = margin(b =10)),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"))

ggsave("clue_type_accuracy.pdf")
ggsave("clue_type_accuracy.jpg", width = 7.00, height = 4.67, units = "in")

df_clue_type_prop <- df_encoded %>%
  filter(!is.na(rt_speaker)) %>% # filtering of speaker data because proportions depend on speaker
  group_by(condition) %>%
  count(int_superordinate) %>%  
  mutate(proportion = n / sum(n)) %>%
  mutate(int_superordinate = factor(int_superordinate,
                                    levels = c(TRUE, FALSE),
                                    labels = c("Intended\nSuperordinate", "Other")))

df_clue_type_prop

# condition int_superordinate         n  proportion
# Abstract  Other                     8      0.267
# Abstract  Intended Superordinate   22      0.733
# Concrete  Other                     9      0.3  
# Concrete  Intended Superordinate   21      0.7  

ggplot(df_clue_type_prop, aes(x = int_superordinate, y = proportion, fill = condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  labs(x = "Clue Type", y = "Proportions", fill = "Condition") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18, margin = margin(b =10)),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"))
  
ggsave("clue_type_proportion.pdf", width = 7.00, height = 4.67, units = "in")

#-----------------------------------------------------
# Response time analysis
#-----------------------------------------------------

### Speaker Onset time 

df_speaker <- df_encoded %>% 
  filter(!is.na(rt_speaker)) 

#################### 
#using a cut-off of 2.5 stds above mean

agr_speaker <- df_speaker %>% 
  summarize(mean_RT = mean(rt_speaker), 
            sd_RT = sd(rt_speaker),
            min = min(rt_speaker),
            max = max(rt_speaker))
agr_speaker
#    mean_RT    sd_RT  min    max
#   15883. 25922.  1599 150683

# exclude RTs which are too long (2,5 stds above mean)
without_exc <- nrow(df_speaker)
cut_off <- agr_speaker$mean_RT + agr_speaker$sd_RT * 2.5 

cut_off
# cut-off: 80687.2

df_speaker_filtered <- df_speaker %>% 
  filter(rt_speaker < cut_off) 

with_exc <- nrow(df_speaker_filtered)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded 3.3% of speaker data 

rt_mean_speaker <- df_speaker_filtered %>% 
  group_by(condition) %>% 
  summarize(mean_RT = mean(rt_speaker, na.rm = TRUE), 
            sd_RT = sd(rt_speaker, na.rm = TRUE),
            min = min(rt_speaker, na.rm = TRUE),
            max = max(rt_speaker, na.rm = TRUE)) 

rt_mean_speaker
# condition mean_RT  sd_RT   min   max
#  Abstract   11748. 15925.  1599 62041
#  Concrete   12439. 15015.  1960 71550

##########################  

#using a cut-off of 60s for better visualisation

df_speaker_filtered <- df_speaker %>% 
  filter(rt_speaker < 60000) 

with_exc <- nrow(df_speaker_filtered)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded 6.67% of speaker data 

rt_mean_speaker <- df_speaker_filtered %>% 
  group_by(condition) %>% 
  summarize(mean_RT = mean(rt_speaker, na.rm = TRUE), 
            sd_RT = sd(rt_speaker, na.rm = TRUE),
            min = min(rt_speaker, na.rm = TRUE),
            max = max(rt_speaker, na.rm = TRUE),
            se = sd(rt_speaker) / sqrt(n())) 

rt_mean_speaker
# condition mean_RT  sd_RT   min   max   se
#  Abstract   9952. 12883.  1599 55048 2435.
#  Concrete   10328.  9988.  1960 35958 1888.


### boxplot
ggplot(df_speaker_filtered, aes(x=condition, y = rt_speaker, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color="black", size=0.5, alpha=0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", stroke = 1) +  
  labs( y = "Onset Time (ms)",x = NULL) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13))

ggsave("speaker_onset_time_boxplot.pdf", width = 7.00, height = 4.67, units = "in")

### Speaker Onset time per category 

rt_mean_speaker_category <- df_speaker %>% 
  group_by(condition, category) %>% 
  summarize(mean_RT = mean(rt_speaker), 
            sd_RT = sd(rt_speaker),
            min = min(rt_speaker),
            max = max(rt_speaker),
            se = sd(rt_speaker) / sqrt(n())) 
rt_mean_speaker_category

ggplot(rt_mean_speaker_category, aes(x = reorder(category, mean_RT), y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity", alpha = 0.8) + 
  geom_errorbar(aes(ymin = mean_RT - se, ymax = mean_RT + se), width = 0.2) + 
  labs(x = NULL, y = "Onset time (ms)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 15)) 

ggsave("speaker_onset_time_per_category.pdf", width = 7.50, height = 4.67, units = "in")

### Listener Response time 

df_listener <- df_encoded

agr_listener <- df_listener %>% 
  summarize(mean_RT = mean(rt_listener), 
            sd_RT = sd(rt_listener),
            min = min(rt_listener),
            max = max(rt_listener))
agr_listener
#   mean_RT    sd_RT  min    max
#   10424.     7362.  3247 45893

# exclude RTs which are too long (2,5 stds above mean)
without_exc <- nrow(df_listener)
cut_off <- agr_listener$mean_RT + agr_listener$sd_RT * 2.5 

cut_off
# cut-off: 28828.21

df_listener_filtered <- df_listener %>% 
  filter(rt_listener < cut_off) 

with_exc <- nrow(df_listener_filtered)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded 4% of speaker data 

rt_mean_listener <- df_listener_filtered %>% 
  group_by(condition) %>% 
  summarize(mean_RT = mean(rt_listener, na.rm = TRUE), 
            sd_RT = sd(rt_listener, na.rm = TRUE),
            min = min(rt_listener, na.rm = TRUE),
            max = max(rt_listener, na.rm = TRUE),
            se = sd(rt_listener) / sqrt(n())) 

rt_mean_listener
# condition mean_RT  sd_RT   min   max  se
#  Abstract   8721. 4480.  3247 23045  647.
#  Concrete   9885. 5181.  3611 25972  748.

### boxplot
ggplot(df_listener_filtered, aes(x=condition, y = rt_listener, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color="black", size=0.5, alpha=0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", stroke = 1) +  
  labs( y = "Response Time (ms)",x = NULL) + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 13))

ggsave("listener_response_time_boxplot.pdf", width = 7.00, height = 4.67, units = "in")

### Listener Onset time per category 

rt_mean_listener_category <- df_listener %>% 
  group_by(condition, category) %>% 
  summarize(mean_RT = mean(rt_listener), 
            sd_RT = sd(rt_listener),
            min = min(rt_listener),
            max = max(rt_listener),
            se = sd(rt_listener) / sqrt(n())) 
rt_mean_listener_category

ggplot(rt_mean_listener_category, aes(x = reorder(category, mean_RT), y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity", alpha = 0.8) + 
  geom_errorbar(aes(ymin = mean_RT - se, ymax = mean_RT + se), width = 0.2) 
  labs(x = NULL, y = "Response time (ms)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 15)) 

ggsave("listener_response_time_per_category.pdf", width = 7.50, height = 4.67, units = "in")

### Listener Onset time per category with filtering 

rt_mean_listener_category <- df_listener_filtered %>% 
  group_by(condition, category) %>% 
  summarize(mean_RT = mean(rt_listener), 
            sd_RT = sd(rt_listener),
            min = min(rt_listener),
            max = max(rt_listener),
            se = sd(rt_listener) / sqrt(n())) 
rt_mean_listener_category

ggplot(rt_mean_listener_category, aes(x = reorder(category, mean_RT), y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity", alpha = 0.8) + 
  geom_errorbar(aes(ymin = mean_RT - se, ymax = mean_RT + se), width = 0.2) + 
  labs(x = NULL, y = "Response time (ms)") +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 15)) 

ggsave("listener_response_time_per_category_filtered.pdf", width = 7.50, height = 4.67, units = "in")


#-----------------------------------------------------
# selection types per item
#-----------------------------------------------------

df_long <- df_cleaned %>%
  filter(Role_Id == 2, Task_Nr == 3) %>% 
  select(Subject_Id, Trial_Nr, Trial_Id, condition, category, selection1, selection2, selection3) %>% 
  pivot_longer(cols = starts_with("selection"), names_to = "selection_pos", values_to = "specific_selection")

df_selections_long <- df_long %>%
  mutate(selection_type = case_when(
    grepl("target", specific_selection) ~ "target",
    grepl("distractor", specific_selection) ~ "distractor",
    grepl("unrelated", specific_selection) ~ "unrelated"))

df_selection_counts_category <- df_selections_long %>% 
  group_by(category, selection_type, condition) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = selection_type, values_from = count, values_fill = 0)

df_selection_counts_category <- df_selection_counts_category %>% 
  pivot_longer(cols = c(target, distractor, unrelated),
               names_to = "selection_type",
               values_to = "count") %>%
  mutate(category = factor(category, levels = df_selection_counts_category %>%
                             arrange(target) %>%  # Sort by target count 
                             pull(category)))%>%
  mutate(selection_type = factor(selection_type, levels = c("unrelated", "distractor", "target")))

ggplot(df_selection_counts_category, aes(x = category, y = count, fill = interaction( selection_type, condition)))+
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(~ condition, scales = "free_x") +
  labs(title = NULL, x = NULL, y = "Count") +
  scale_fill_manual(values = c(
    "target.Abstract" = "#F6B33F",
    "distractor.Abstract" = "#FFD970", 
    "unrelated.Abstract" = "#E9E5DA",  
    "target.Concrete" = "#52BCEA",  
    "distractor.Concrete" = "#ADDEFB",  
    "unrelated.Concrete" = "#D2D8DD"),   
    name = "Selection Type",
    labels = c(
      "target.Abstract" = "Target (Abstract)",
      "target.Concrete" = "Target (Concrete)",
      "distractor.Abstract" = "Distractor (Abstract)",
      "distractor.Concrete" = "Distractor (Concrete)",
      "unrelated.Abstract" = "Unrelated (Abstract)",
      "unrelated.Concrete" = "Unrelated (Concrete)")) +  
  theme_minimal() +
  theme(plot.margin = margin(20, 10, 10, 5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17),
        panel.spacing = unit(0.8, "lines"),
        scale_x_discrete(expand = expansion(mult = c(1, 1))))

ggsave("selection_item.pdf", width = 10.00, height = 6.50, units = "in")

#-----------------------------------------------------
# debriefing questions
#-----------------------------------------------------

df_speaker_debr <- df_cleaned %>% 
  filter(Role_Id == 1, Task_Nr == 4) %>% 
  select(Group_Id, difficulty_answer_speaker, easiness_answer_speaker, need_for_discussion_speaker)

df_listener_debr <- df_cleaned %>% 
  filter(Role_Id == 2, Task_Nr == 4) %>% 
  select(Group_Id, difficulty_answer_listener, easiness_answer_listener, need_for_discussion_listener)

df_debr_merged <- merge(df_speaker_debr, df_listener_debr, 
                        by=intersect(names(df_speaker_debr), names(df_listener_debr)), all.y=TRUE)
df_debr_merged

#write_csv(df_debr_merged, "df_debriefing.csv")

#-----------------------------------------------------
# control variables analysis 
#-----------------------------------------------------

#prepare df

df_contr_var_all <-  read_csv("df_contr_var_all.csv")

contr_var_sup <- read_csv("contr_var_superordinates.csv")

sup_concr_mean <- contr_var_sup %>% 
  rename(category = Word)%>%
  mutate(category = str_to_title(category)) %>%
  group_by(Domain) %>% 
  summarize(mean_concr = mean(Concreteness))
  
sup_concr_mean

#Domain     mean_concr
# Abstract       2.88
# Concrete       4.69


means_contr_var_all <- df_contr_var_all %>%
  group_by(Domain) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -Domain,names_to = "Variable", values_to = "Mean") %>%
  pivot_wider(names_from = Domain,values_from = Mean)

write_csv(means_contr_var_all, "df_contr_var_all_mean.csv")

means_contr_var_all

sd_contr_var_all <- df_contr_var_all %>%
  group_by(Domain) %>%
  summarize(across(where(is.numeric), sd, na.rm = TRUE)) %>%
  pivot_longer(cols = -Domain,names_to = "Variable", values_to = "SD") %>%
  pivot_wider(names_from = Domain,values_from = SD)

sd_contr_var_all

write_csv(sd_contr_var_all, "df_contr_var_all_sd.csv")




###################################################### not in thesis

### similarity ratings distractors to targets

# word2vec similarity -> not used anymore

similarity_df <- read_csv("mean_similarity_word2vec.csv")

similarity_df <- similarity_df %>%
  rename(category = `Target concept`) %>%
  rename(similarity = Mean_Distractor_Similarity) %>%
  select(category, similarity)

# listener

combined_df_listener <- rt_mean_listener_category %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

similarity_mean <- combined_df_listener %>% 
  group_by(condition) %>% 
  summarize(mean_sim = mean(similarity),
            sd_sim = sd(similarity),
            min_sim = min(similarity),
            max_sim = max(similarity))

similarity_mean

#  condition mean_sim sd_sim min_sim max_sim
#  Abstract     0.170 0.0775  0.0948   0.305
#  Concrete     0.217 0.101   0.0957   0.381

# speaker

combined_df_speaker <- rt_mean_speaker_category %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)


df_combined <- bind_rows(
  combined_df_speaker %>% mutate(role = "Speaker"),
  combined_df_listener %>% mutate(role = "Listener"))

df_combined <- df_combined %>%
  mutate(role = factor(role, levels = c("Speaker", "Listener")))

ggplot(df_combined, aes(y = mean_RT, x = similarity)) +
  geom_point(size = 2, alpha = 0.6, aes(color = condition, shape = role)) +
  geom_smooth(method = "lm", se = TRUE, aes(linetype = role), color = "black", alpha = 0.3, fill = "grey", linewidth = 0.6) +
  labs(title = NULL, x = "Target-Distractor Similarity", y = "Response Time (ms)", color = "Condition", shape = "Role", linetype = "Role") +
  scale_shape_manual(name = "Role", values = c("Speaker" = 15, "Listener" = 16)) + 
  guides( shape = guide_legend(order = 1), linetype = guide_legend(order = 2, override.aes = list(shape = NA)), color = guide_legend(order = 0)) +
  theme_minimal()

ggsave("similarity_rt_speaker_listener.pdf")

#-----------------------------------------------------
#analysis Specificity
#-----------------------------------------------------
contr_var_targets
contr_spec <- contr_var_targets %>% 
  group_by(category) %>% 
  summarize(spec_mean = mean(Specificity, na.rm = TRUE), 
            sd = sd(Specificity, na.rm = TRUE),
            min = min(Specificity, na.rm = TRUE),
            max = max(Specificity, na.rm = TRUE))

contr_spec

### Listener

combined_df_listener <- rt_mean_listener_category %>%
  left_join(contr_spec, by = c("category")) %>%
  select(condition, category, mean_RT, spec_mean)

### Speaker

combined_df_speaker <- rt_mean_speaker_category %>%
  left_join(contr_spec, by = c("category")) %>%
  select(condition, category, mean_RT, spec_mean)  

### Figure 6

df_combined <- bind_rows(
  combined_df_speaker %>% mutate(role = "Speaker"),
  combined_df_listener %>% mutate(role = "Listener"))

df_combined <- df_combined %>%
  mutate(role = factor(role, levels = c("Speaker", "Listener")))


ggplot(df_combined, aes(x = spec_mean, y = mean_RT, color = condition)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey", alpha = 0.3, linewidth = 0.6) +
  facet_wrap(~ role, scales = "free_y") +  
  labs(
    title = NULL,
    x = "Specificity",
    y = "Response Time (ms)",
    color = "Condition"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  )

df_combined <- bind_rows(
  combined_df_speaker %>% mutate(role = "Speaker"),
  combined_df_listener %>% mutate(role = "Listener"))

df_combined <- df_combined %>%
  mutate(role = factor(role, levels = c("Speaker", "Listener")))

ggplot(df_combined, aes(y = mean_RT, x = spec_mean)) +
  geom_point(size = 2, alpha = 0.6, aes(color = condition, shape = role)) +
  geom_smooth(method = "lm", se = TRUE, aes(linetype = role), color = "black", alpha = 0.3, fill = "grey", linewidth = 0.6) +
  labs(title = NULL, x = "Specificty", y = "Response Time (ms)", color = "Condition", shape = "Role", linetype = "Role") +
  scale_shape_manual(name = "Role", values = c("Speaker" = 15, "Listener" = 16)) + 
  guides( shape = guide_legend(order = 1), linetype = guide_legend(order = 2, override.aes = list(shape = NA)), color = guide_legend(order = 0)) +
  theme_minimal()

ggsave("contr_specificity.pdf")
