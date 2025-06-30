## Title: Analyses pilot data
## Description: Analyzing pilot data abstract concept-level reference game
## Author: Vanessa Obi
## Last Edit: 2025-03-20

# purpose of this script------------------------------------------------

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
                Location = SelectedLocation) %>% 
  filter(Task_Nr %in% c(3, 4)) # filter out test trials, introduction screen and debriefing data

df$category <- recode(df$category, 
                      "Sports" = "Sport", 
                      "Buildings" = "Building", 
                      "Fruits" = "Fruit", 
                      "Plants" = "Plant", 
                      "Tools" = "Tool")

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

write.csv(df_cleaned, "data_cleaned.csv") 

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

write_csv(df_merged, "df_merged.csv")

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
  filter(!is.na(clue_type))

#-----------------------------------------------------
# listener communicative success analysis
#-----------------------------------------------------


### mean listener accuracy -> Figure 1

# mean success per subject
df_subject_means <- df_merged %>%
  group_by(Subject_Id, condition) %>%
  summarize(success_mean = mean(success), .groups = "drop")

# mean across all subjects 
df_success <- df_subject_means %>%
  group_by(condition) %>%
  summarize(success = mean(success_mean),  
            se = sd(success_mean) / sqrt(n()), 
            .groups = "drop")

ggplot(df_success, aes(x = condition, y = success, fill = condition)) +
  geom_point(size = 7, aes(color = condition))+
  geom_errorbar(aes(ymin = success - se, ymax = success + se), width = 0.2) + 
  labs( y = "Listener Accuracy", x = NULL ) +
  theme_minimal() + 
  theme(legend.position = "none",
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 11))

ggsave("mean_success.jpg")

### type of selection -> Table 1
### calculate hit rate, distractor false alarm, unrelated false alarm

df_long <- df_cleaned %>%
  filter(Role_Id == 2, Task_Nr == 3) %>% 
  select(Subject_Id, Trial_Nr, Trial_Id, condition, category, selection1, selection2, selection3) %>% 
  pivot_longer(cols = starts_with("selection"), names_to = "selection_pos", values_to = "specific_selection")

df_selections_long <- df_long %>%
  mutate(selection_type = case_when(
      grepl("target", specific_selection) ~ "target",
      grepl("distractor", specific_selection) ~ "distractor",
      grepl("unrelated", specific_selection) ~ "unrelated"))

### calculate hit rate for every trial 

df_selection_counts_trials <- df_selections_long %>% 
  group_by(Subject_Id, Trial_Nr, selection_type, condition ) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = selection_type, values_from = count, values_fill = 0)

df_rates <- df_selection_counts_trials %>%
  mutate(
    hit_rate = target / 3,             
    distractor_FA = distractor / 2,     
    unrelated_FA = unrelated / 4 
  )

# mean rates per subject
df_subject_means <- df_rates %>%
  group_by(Subject_Id, condition) %>%
  summarize(
    mean_hit_rate = mean(hit_rate), 
    mean_distractor_FA = mean(distractor_FA),
    mean_unrelated_FA = mean(unrelated_FA),
    .groups = "drop"
  )

df_subject_means

# mean across all subjects
df_overall_means <- df_subject_means %>%
  group_by(condition) %>% 
  summarize(
    hit_rate = mean(mean_hit_rate),
    distractor_false_alarm = mean(mean_distractor_FA),
    unrelated_false_alarm = mean(mean_unrelated_FA)
  )

df_overall_means


### success per category 

df_success_category <- df_merged %>%
  group_by(category, condition) %>%
  summarize(com_success = mean(success), .groups = "drop")

ggplot(df_success_category, aes(x = reorder(category, com_success), y = com_success, fill = condition)) +
  geom_bar(stat = "identity", position = "stack") +  
  facet_wrap(~ condition, scales = "free_x") + 
  labs(title = "Communicative success per Category",
       x = "Category",
       y = "Proportion of successful trials") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#ggsave("success_per_category.jpg")

### proportions of selections per item -> # Figure 2

df_selection_counts_subj <- df_selections_long %>% 
  group_by(Subject_Id, selection_type, condition) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = selection_type, values_from = count, values_fill = 0)

df_selection_counts_category <- df_selections_long %>% 
  group_by(category, selection_type, condition) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = selection_type, values_from = count, values_fill = 0)

df_selection_counts_category <- df_selection_counts_category %>% 
  pivot_longer(cols = c(target, distractor, unrelated),
               names_to = "selection_type",
               values_to = "count") %>%
  mutate(category = factor(category, levels = df_selection_counts_category %>%
                             arrange(target) %>%  # Sort by target count (ascending)
                             pull(category)))%>%
  mutate(selection_type = factor(selection_type, levels = c("unrelated", "distractor", "target")))

ggplot(df_selection_counts_category, aes(x = category, y = count, fill = interaction( selection_type, condition)))+
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(~ condition, scales = "free_x") +
  labs(title = NULL,
       x = NULL,
       y = "Count") +
  scale_fill_manual(values = c(
    "target.Abstract" = "#F6B33F",
    "distractor.Abstract" = "#FFD970", 
    "unrelated.Abstract" = "#E9E5DA",  
    "target.Concrete" = "#52BCEA",  
    "distractor.Concrete" = "#ADDEFB",  
    "unrelated.Concrete" = "#D2D8DD"
    ),   
    name = "Selection Type",
    labels = c(
    "target.Abstract" = "Target (Abstract)",
    "target.Concrete" = "Target (Concrete)",
    "distractor.Abstract" = "Distractor (Abstract)",
    "distractor.Concrete" = "Distractor (Concrete)",
    "unrelated.Abstract" = "Unrelated (Abstract)",
    "unrelated.Concrete" = "Unrelated (Concrete)"
  )
  ) +  
  theme_minimal() +
  theme(plot.margin = margin(20, 10, 10, 5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 9),
        panel.spacing = unit(0.8, "lines"),
        scale_x_discrete(expand = expansion(mult = c(1, 1)))
        ) 

ggsave("selection_count.jpg")

##### to do: clean up legends, make it wider, make proportions out of it or leave it as count?


#-----------------------------------------------------
# Response time analysis
#-----------------------------------------------------

### Speaker Onset time 

df_speaker_successful <- df_speaker %>% 
  filter(success == TRUE) %>% 
  select(Group_Id, Subject_Id, condition, category, 
         success, rt_speaker) %>%
  mutate(condition = as.factor(condition)) 

without_exc <- nrow(df_speaker)
with_exc <- nrow(df_speaker_successful)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded  21% of data

agr_speaker <- df_speaker_successful %>% 
  summarise(mean_RT = mean(rt_speaker), 
            sd_RT = sd(rt_speaker),
            min = min(rt_speaker),
            max = max(rt_speaker))

agr_speaker

#    mean_RT    sd_RT  min    max
#   10635.56 17738.98 1599 100857

# exclude RTs which are too long (2,5 stds above mean)
without_exc <- nrow(df_speaker_successful)
cut_off <- agr_speaker$mean_RT + agr_speaker$sd_RT * 2.5 # cut-off: 54983
cut_off

df_speaker_successful <- df_speaker_successful %>% 
  filter(rt_speaker < cut_off) 
with_exc <- nrow(df_speaker_successful)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded 4.44% of speaker data 

agr_speaker <- df_speaker_successful %>% 
  group_by(condition) %>% 
  summarise(mean_RT = mean(rt_speaker), 
            sd_RT = sd(rt_speaker),
            min = min(rt_speaker),
            max = max(rt_speaker)) 
agr_speaker

#condition  mean_RT  sd_RT   min   max
# Abstract     7965 10005.  1599 34542
# Concrete     6975  7606.  1960 35958

ggplot(agr_speaker, aes(x = condition, y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), width = 0.1) + 
  labs(x = "Condition", y = "Onset Time in ms", 
       title = "Onset Speaker") +
  theme_minimal()

ggsave("speaker_process_time.jpg")

### boxplot

ggplot(df_speaker_successful, aes(x=condition, y = rt_speaker, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color="black", size=0.5, alpha=0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", stroke = 1) +  
  labs(x = "Condition", y = "Onset Time in ms", 
       title = "Onset time Speaker") + 
  theme_minimal()
  
ggsave("speaker_process_time_boxplot.jpg")


### Speaker Response time per category 

agr_speaker <- df_speaker_successful %>% 
  group_by(condition, category) %>% 
  summarise(mean_RT = mean(rt_speaker), 
            sd_RT = sd(rt_speaker),
            min = min(rt_speaker),
            max = max(rt_speaker)) 
agr_speaker


ggplot(agr_speaker, aes(x = reorder(category, mean_RT), y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), width = 0.2) +  # Error bars
  labs(x = "Category", y = "Onset time in ms", 
       title = "Onset time Speaker per category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("speaker_process_time_per_category.jpg")

### Listener Response time 

df_listener_successful <- df_listener %>% 
  filter(success == TRUE) %>% 
  select(Group_Id, Subject_Id, condition, category, 
         success, rt_listener) %>%
  mutate(condition = as.factor(condition)) 

without_exc <- nrow(df_listener)
with_exc <- nrow(df_listener_successful)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded  21% of data

glimpse(df_listener_successful)

agr_listener <- df_listener_successful %>% 
  summarise(mean_RT = mean(rt_listener), 
            sd_RT = sd(rt_listener),
            min = min(rt_listener),
            max = max(rt_listener))

agr_listener

#    mean_RT    sd_RT  min    max
#    8963.     6361.  3247  45893

# exclude RTs which are too long (2,5 stds above mean)
without_exc <- nrow(df_listener_successful)
cut_off <- agr_listener$mean_RT + agr_listener$sd_RT * 2.5 # cut-off: 24866.55
cut_off

df_listener_successful <- df_listener_successful %>% 
  filter(rt_listener < cut_off) 
with_exc <- nrow(df_listener_successful)
round((without_exc - with_exc) / without_exc*100, 2) # -> excluded 2.53 % of data

agr_listener <- df_listener_successful %>% 
  group_by(condition) %>% 
  summarise(mean_RT = mean(rt_listener), 
            sd_RT = sd(rt_listener),
            min = min(rt_listener),
            max = max(rt_listener)) 
agr_listener

# condition  mean_RT  sd_RT   min   max
# Abstract    7346.   3253.  3247  16966
# Concrete    9059.   4596.  3611  19526


ggplot(agr_listener, aes(x = condition, y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), width = 0.1) + 
  labs(x = "Condition", y = "Process Time in ms", 
       title = "Response time Listener") +
  theme_minimal()

ggsave("listener_process_time.jpg")

### boxplot

ggplot(df_listener_successful, aes(x=condition, y = rt_listener, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color="black", size=0.5, alpha=0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", stroke = 1) +
  labs(x = "Condition", y = "Process Time in ms", 
       title = "Response time Listener") + 
  theme_minimal()

ggsave("listener_process_time_boxplot.jpg")


### Onset Time/Response Time -> Figure 3

# mean for each role
mean_listener <- df_listener_successful %>%
  summarize(mean_rt = mean(rt_listener, na.rm = TRUE)) %>%
  pull(mean_rt)

mean_speaker <- df_speaker_successful %>%
  summarize(mean_rt = mean(rt_speaker, na.rm = TRUE)) %>%
  pull(mean_rt)

df_speaker_successful <- df_speaker_successful %>%
  rename(rt = rt_speaker) %>% 
  mutate(role = "Speaker")

df_listener_successful <- df_listener_successful %>% 
  rename(rt = rt_listener) %>% 
  mutate(role = "Listener")

df_combined <- bind_rows(
  df_speaker_successful %>% mutate(role = "Speaker"),
  df_listener_successful %>% mutate(role = "Listener")
)

df_combined$role <- factor(df_combined$role, levels = c( "Speaker", "Listener"))

df_means <- df_combined %>%
  group_by(condition, role) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop")

df_means

ggplot(df_combined, aes(x = condition, y = rt, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(color = "black", size = 0.5, alpha = 0.5, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black", stroke = 1, show.legend = FALSE) +
  labs(x = NULL, y = "Response Time (ms)", 
       title = NULL) +
  theme_minimal() +
  facet_wrap(~role,  strip.position = "bottom") +
  geom_line(data = df_means, aes(x = condition, y = mean_rt, group = role), 
            color = "darkred", size = 1, linetype = "dashed") +
  scale_fill_discrete(name = "Condition") +
  theme(
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 9),
        panel.spacing = unit(0.8, "lines"),
        scale_x_discrete(expand = expansion(mult = c(1, 1)))
  ) 

ggsave("onset_response_time.jpg")

### Listener Response time per category 

agr_listener <- df_listener_successful %>% 
  group_by(condition, category) %>% 
  summarise(mean_RT = mean(rt), 
            sd_RT = sd(rt),
            min = min(rt),
            max = max(rt)) 

ggplot(agr_listener, aes(x = reorder(category, mean_RT), y = mean_RT, fill = condition)) +
  geom_bar(stat = "identity") +  
  geom_errorbar(aes(ymin = mean_RT - sd_RT, ymax = mean_RT + sd_RT), width = 0.2) +  # Error bars
  labs(x = "Category", y = "Process time in ms", 
       title = "Process time Listener per category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("listener_process_time_per_category.jpg")


#### RT difference between Speaker and Listener per Item -> Figure 4


df_rt_diff <- rbind(df_speaker_successful, df_listener_successful) %>% 
  group_by(category, role, condition) %>%  
  rename("Condition" = "condition") %>% 
  summarize(
    mean_rt = mean(rt),        
    se_rt = sd(rt) / sqrt(n()), 
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = role, values_from = c(mean_rt, se_rt)) %>%
  mutate(diff_rt =  mean_rt_listener - mean_rt_speaker) %>% 
  arrange(desc(diff_rt)) %>% 
  mutate(category = factor(category, levels = unique(category)))

df_mean_rt_diff <- df_rt_diff %>% pivot_longer(cols = starts_with("mean_rt"), names_to = "role", values_to = "mean_rt") %>%
  mutate(role = ifelse(role == "mean_rt_speaker", "Speaker", "Listener")) %>% 
  subset(select = -c(se_rt_listener, se_rt_speaker, diff_rt))

df_se_rt_diff <- df_rt_diff %>% pivot_longer(cols = starts_with("se_rt"), names_to = "role", values_to = "se_rt") %>%
  mutate(role = ifelse(role == "se_rt_speaker", "Speaker", "Listener")) %>% 
  subset(select = -c(mean_rt_listener, mean_rt_speaker, diff_rt))

df_rt_diff <- df_mean_rt_diff %>% left_join(df_se_rt_diff,
                        by = c("category", "Condition", "role")) 

ggplot(df_rt_diff, aes(x = category, y = mean_rt, color = Condition, shape = role)) +
  geom_point(size = 4, alpha = 0.6) + 
  geom_errorbar(aes(ymin = mean_rt - se_rt, ymax = mean_rt + se_rt), width = 0.2, color = "grey") + 
  scale_shape_manual(name = "Role", values = c("Speaker" = 15, "Listener" = 16)) + 
  labs(title = NULL,
       x = NULL,
       y = "Response Time (ms)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(r = 6)))





#-----------------------------------------------------
# clue type analysis
#-----------------------------------------------------

### clue type per condition 

df_clue_type_condition <- df_encoded %>%
  group_by(condition, clue_type) %>%
  summarize(count = n(), .groups= "drop")%>%  # Count occurrences of types
  arrange(condition, desc(count))

ggplot(df_clue_type_condition, aes(x = fct_reorder(clue_type, count), y = count, fill = clue_type)) +
  geom_bar(stat = "identity", position = "dodge") +  
  facet_wrap(~ condition) + 
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Clue Type Distibution per Condition",
       x = "Clue Type", y = "Number of trials") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(0.8, 0.8, 0.8, 1.8, "cm"))

ggsave("clue_type_per_condition.jpg")

### clue type per category

df_clue_type_category <- df_encoded %>%
  group_by(condition, category, clue_type) %>%
  summarize(count = n(), .groups= "drop")%>% 
  arrange(condition, category, desc(count))


ggplot(df_clue_type_category, aes(x = fct_reorder(category, count), y = count, fill = clue_type)) +
  geom_bar(stat = "identity", position = "stack") +  
  facet_wrap(~ condition, , scales = "free_x") +
  scale_fill_brewer(palette = "Set3") +  
  labs(title = "Clue Type Distibution per Category",
       x = "Category", y = "Number of trials", fill = "Clue Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("clue_type_per_category.jpg")

### success per clue type

df_success <- df_encoded %>%
  group_by(clue_type) %>%
  summarise(success_rate = mean(success))


ggplot(df_success, aes(x = reorder(clue_type, success_rate), y = success_rate, fill = clue_type)) +
  geom_bar(stat = "identity") +  
  scale_fill_brewer(palette = "Set3") + 
  labs(title = "Success Rate per Clue Type",
       x = "Clue Type", y = "Success Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(0.8, 0.8, 0.8, 1.8, "cm"))  

ggsave("success_per_clue_type.jpg")

### clue type (modified) intended superordinate vs other -> Figure 6

df_clue_type_success <- df_encoded %>% 
  mutate(int_superordinate = ifelse(
    clue_type %in% c("Superordinate", "Modified Superordinate"), TRUE, FALSE)) %>%
  group_by(condition, int_superordinate) %>%
  summarise(success_rate = mean(success), se_success = sd(success) / sqrt(n()),) 

df_clue_type_success

df_clue_type_prop <- df_encoded %>% 
  mutate(int_superordinate = clue_type %in% c("Superordinate", "Modified Superordinate")) %>%
  group_by(condition) %>%
  count(int_superordinate) %>%  
  mutate(proportion = n / sum(n))  


df_clue_type <- df_clue_type_success %>%
  left_join(df_clue_type_prop)

df_clue_type_prop
df_clue_type

ggplot(df_clue_type, aes(x = proportion, y = success_rate, color = condition, shape = int_superordinate)) +
  geom_point(size = 5, alpha = 0.8) +  # Dots for success rate
  geom_errorbar(aes(ymin = success_rate - se_success, ymax = success_rate + se_success), 
                width = 0.02, linewidth = 0.8, alpha = 0.7, color = "grey") + 
  scale_shape_manual(values = c(16, 17),
                     labels = c("FALSE" = "Other", "TRUE" = "Superordinate")) +
  scale_x_continuous(limits = c(0.2, 0.8), breaks = c(0.2, 0.4, 0.6, 0.8)) +
  scale_y_continuous(limits = c(0.3, 1), breaks = c( 0.4, 0.6, 0.8, 1)) +
  annotate("text", x = 0.25, y = 0.35, label = "Other Clue Types", hjust = 0, vjust = -1, size = 3.5, color = "#818181") +
  annotate("text", x = 0.8, y = 0.35, label = "Intended Superordinate", hjust = 1, vjust = -11, size = 3.5,  color = "#818181") +
  labs(title = NULL,
       x = "Proportion of Clue Types",
       y = "Listener Accuracy",
       color = "Condition",
       shape = "Clue Type") +
  theme_minimal()# +
  #guides(shape = "none") 



### success per clue type and condition

df_success_condition <- df_encoded %>%
  group_by(clue_type, condition) %>%
  summarise(success_rate = mean(success))
df_success_condition

ggplot(df_success_condition, aes(x = reorder(clue_type, success_rate), y = success_rate, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_discrete(drop = FALSE) +
  labs(title = "Success Rate per Clue Type and Condition",
       x = "Clue Type", y = "Success Rate", fill = "Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.margin = margin(0.3,0.3 ,0.3 , 0.8, "cm"))

ggsave("success_per_clue_type_per_condition.jpg")

#-----------------------------------------------------
# control variables analysis 
# -> only successful trials
# Create scatter plots comparing mean_RT and mean control variables values across categories
#-----------------------------------------------------

#prepare df

contr_var_all <-  read_csv("contr_var_all_words.csv")
contr_var_all <- contr_var_all %>% 
  rename(category = `Target concept`)

contr_var_all$category <- recode(contr_var_all$category, 
                      "Sports" = "Sport", 
                      "Buildings" = "Building", 
                      "Fruits" = "Fruit", 
                      "Plants" = "Plant", 
                      "Tools" = "Tool")

contr_var_targets <- read_csv("contr_var_targets.csv")
contr_var_targets <- contr_var_targets %>% 
  rename(banks_cat = category)  %>% 
  rename(category = `Target concept`)

contr_var_targets$category <- recode(contr_var_targets$category, 
                                 "Sports" = "Sport", 
                                 "Buildings" = "Building", 
                                 "Fruits" = "Fruit", 
                                 "Plants" = "Plant", 
                                 "Tools" = "Tool")

contr_var_sup <- read_csv("contr_var_superordinates.csv")
contr_var_sup <- contr_var_sup %>% 
  rename(category = Word)%>%
  mutate(category = str_to_title(category)) 

#-----------------------------------------------------
#analysis concreteness 
#-----------------------------------------------------


contr_var_concreteness <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(Concreteness, na.rm = TRUE), 
            sd = sd(Concreteness, na.rm = TRUE),
            min = min(Concreteness, na.rm = TRUE),
            max = max(Concreteness, na.rm = TRUE))

### listener

agr_listener

combined_df <- agr_listener %>%
  left_join(contr_var_concreteness, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

glimpse(agr_listener)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "RT in ms", y = "Concreteness", title = "Mean RT Listener vs Mean Concreteness of grid") +
  theme_minimal() 

ggsave("contr_concr_listener.jpg")

### speaker

agr_speaker
contr_var_concreteness
combined_df <- agr_speaker %>%
  left_join(contr_var_concreteness, by = c("category")) %>%
  select(condition, category, mean_RT, mean)  # Keep only relevant columns

combined_df

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Onset time in ms", y = "Concreteness", title = "Mean RT Speaker vs Mean Concreteness of grid") +
  theme_minimal()

#ggsave("contr_concr_speaker.jpg")

### superordinate speaker
combined_df <- agr_speaker %>%
  left_join(contr_var_sup, by = c("category")) %>%
  select(condition, category, mean_RT, Concreteness) 

ggplot(combined_df, aes(y = Concreteness, x = mean_RT, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Onset time in ms", y = "Concreteness", title = "Mean Onset time Speaker vs Concreteness of Superordinates") +
  theme_minimal()

#ggsave("contr_concr_speaker_sup.jpg")

### superordinate listener 

combined_df <- agr_listener %>%
  left_join(contr_var_sup, by = c("category")) %>%
  select(condition, category, mean_RT, Concreteness) 

ggplot(combined_df, aes(y = Concreteness, x = mean_RT, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "RT in ms", y = "Concreteness", title = "Mean RT Listener vs Concreteness of Superordinates") +
  theme_minimal() 

#ggsave("contr_concr_listener_sup.jpg")

#-----------------------------------------------------
#analysis prod.freq.percent
#-----------------------------------------------------

contr_var_prod_freq <- contr_var_targets %>% 
  group_by(category) %>% 
  summarize(mean = mean(prod.freq.percent, na.rm = TRUE), 
            sd = sd(prod.freq.percent, na.rm = TRUE),
            min = min(prod.freq.percent, na.rm = TRUE),
            max = max(prod.freq.percent, na.rm = TRUE))

### listener

combined_df <- agr_listener %>%
  left_join(contr_var_prod_freq, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean prod.freq.percent", title = "Mean RT Listener vs Mean category production frequency of targets") +
  theme_minimal()

#ggsave("contr_prod_freq_listener.jpg")

###speaker

combined_df <- agr_speaker %>%
  left_join(contr_var_prod_freq, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean prod.freq.percent", title = "Mean RT Speaker vs Mean category production frequency of targets") +
  theme_minimal()

#ggsave("contr_prod_freq_speaker.jpg")

#-----------------------------------------------------
#analysis Specificity
#-----------------------------------------------------

contr_spec <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(Specificity, na.rm = TRUE), 
            sd = sd(Specificity, na.rm = TRUE),
            min = min(Specificity, na.rm = TRUE),
            max = max(Specificity, na.rm = TRUE))

### Listener

combined_df <- agr_listener %>%
  left_join(contr_spec, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "RT in ms", y = "Specificty", title = "Mean RT Listener vs Mean Specificity of grid") +
  theme_minimal() 

#ggsave("contr_spec_listener.jpg")

### Speaker

combined_df <- agr_speaker %>%
  left_join(contr_spec, by = c("category")) %>%
  select(condition, category, mean_RT, mean)  

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Onset time in ms", y = "Specificty", title = "Mean Onset time Speaker vs Mean Specificity of grid") +
  theme_minimal()

#ggsave("contr_spec_speaker.jpg")


#-----------------------------------------------------
# analysis context diversity
#-----------------------------------------------------

contr_cd <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(Lg10CD, na.rm = TRUE), 
            sd = sd(Lg10CD, na.rm = TRUE),
            min = min(Lg10CD, na.rm = TRUE),
            max = max(Lg10CD, na.rm = TRUE))

### listener

combined_df <- agr_listener %>%
  left_join(contr_cd, by = c("category")) %>%
  select(condition, category, mean_RT, mean) 

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Lg10CD", title = "Mean RT Listener vs Mean Context diversity of grid") +
  theme_minimal()

ggsave("contr_cd_listener.jpg")

### speaker

combined_df <- agr_speaker %>%
  left_join(contr_cd, by = c("category")) %>%
  select(condition, category, mean_RT, mean)  # Keep only relevant columns

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Lg10CD", title = "Mean RT Speaker vs Mean Context diversity of grid") +
  theme_minimal() 

ggsave("contr_cd_speaker.jpg")

#-----------------------------------------------------
# analysis valence
#-----------------------------------------------------

contr_valence <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(Valence, na.rm = TRUE), 
            sd = sd(Valence, na.rm = TRUE),
            min = min(Valence, na.rm = TRUE),
            max = max(Valence, na.rm = TRUE))

### listener 

combined_df <- agr_listener %>%
  left_join(contr_valence, by = c("category")) %>%
  select(condition, category, mean_RT, mean)  

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Valence", title = "Mean RT Listener vs Mean Valence of grid") +
  theme_minimal() 

ggsave("contr_valence_listener.jpg")

### speaker 

combined_df <- agr_speaker %>%
  left_join(contr_valence, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Valence", title = "Mean RT Speaker vs Mean Valence of grid") +
  theme_minimal()

ggsave("contr_valence_speaker.jpg")

#-----------------------------------------------------
# analysis arousal
#-----------------------------------------------------

contr_arousal <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(Arousal, na.rm = TRUE), 
            sd = sd(Arousal, na.rm = TRUE),
            min = min(Arousal, na.rm = TRUE),
            max = max(Arousal, na.rm = TRUE))

### listener

combined_df <- agr_listener %>%
  left_join(contr_arousal, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Arousal", title = "Mean RT Listener vs Mean Arousal of grid") +
  theme_minimal() 

ggsave("contr_arousal_listener.jpg")

### speaker

combined_df <- agr_speaker %>%
  left_join(contr_arousal, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Arousal", title = "Mean RT Speaker vs Mean Arousal of grid") +
  theme_minimal() 

ggsave("contr_arousal_speaker.jpg")

#-----------------------------------------------------
#analysis dominance
#-----------------------------------------------------

contr_dom <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(Dominance, na.rm = TRUE), 
            sd = sd(Dominance, na.rm = TRUE),
            min = min(Dominance, na.rm = TRUE),
            max = max(Dominance, na.rm = TRUE))

### listener

combined_df <- agr_listener %>%
  left_join(contr_dom, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT in ms", y = "Mean Dominance", title = "Mean RT Listener vs Mean Dominance of grid") +
  theme_minimal() 

ggsave("contr_dom_listener.jpg")

### speaker

combined_df <- agr_speaker %>%
  left_join(contr_dom, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean Onset time in ms", y = "Mean Dominance", title = "Mean RT Speaker vs Mean Dominance of grid") +
  theme_minimal() 

ggsave("contr_dom_speaker.jpg")

#-----------------------------------------------------
#analysis length
#-----------------------------------------------------

contr_len <- contr_var_all %>% 
  group_by(category) %>% 
  summarize(mean = mean(length, na.rm = TRUE), 
            sd = sd(length, na.rm = TRUE),
            min = min(length, na.rm = TRUE),
            max = max(length, na.rm = TRUE))

### listener

combined_df <- agr_listener %>%
  left_join(contr_len, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT in ms", y = "Mean Length", title = "Mean RT Listener vs Mean Length of grid") +
  theme_minimal() 

ggsave("contr_len_listener.jpg")

### speaker

combined_df <- agr_speaker %>%
  left_join(contr_len, by = c("category")) %>%
  select(condition, category, mean_RT, mean)

ggplot(combined_df, aes(x = mean_RT, y = mean, color = condition)) +
  geom_point( size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean Onset time in ms", y = "Mean Length", title = "Mean Onset time Speaker vs Mean Length of grid") +
  theme_minimal() 

ggsave("contr_len_speaker.jpg")

#-----------------------------------------------------
# similarity ratings distractors to targets
#-----------------------------------------------------

# glove similarity

similarity_df <- read_csv("mean_similarity_glove.csv")

similarity_df <- similarity_df %>%
  rename(category = `Target concept`) %>%
  rename(similarity = Mean_Distractor_Similarity) %>%
  select(category, similarity)

### listener

combined_df <- agr_listener %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT in ms", y = "Mean Similarity", title = "Mean RT Listener vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_listener_glove.jpg")

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = condition)) + 
  labs(x = "Mean RT in ms", y = "Mean Similarity", title = "Mean RT Listener vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_listener_glove_condition.jpg")

### speaker

combined_df <- agr_speaker %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean Onset time in ms", y = "Mean Similarity", title = "Mean Onset time Speaker vs  Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_speaker_glove.jpg")

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", se = TRUE,aes(color = condition)) + 
  labs(x = "Mean Onset time in ms", y = "Mean Similarity", title = "Mean Onset time Speaker vs  Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_speaker_glove_condition.jpg")


### listener success

combined_df <- df_success_category %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, com_success, similarity)

ggplot(combined_df, aes(x = com_success, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Proportion of successful trials", y = "Mean Similarity", title = "Listener Communicative Success vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_success_wn.jpg")

ggplot(combined_df, aes(x = com_success, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = condition)) + 
  labs(x = "Proportion of successful trials", y = "Mean Similarity", title = "Listener Communicative Success vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_success_glove_condition.jpg")

### wordnet wup similarity


similarity_df <- read_csv("similarity_results_wup.csv")

similarity_df <- similarity_df %>%
  rename(category = `Target concept`) %>%
  rename(similarity = Mean_Distractor_Similarity) %>%
  select(category, similarity)

### listener

combined_df <- agr_listener %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean RT", y = "Mean Similarity", title = "Mean RT Listener vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_listener_wup.jpg")

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = condition)) + 
  labs(x = "Mean RT", y = "Mean Similarity", title = "Mean RT Listener vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_listener_wup_condition.jpg")

### speaker

combined_df <- agr_speaker %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Mean Onset time in ms", y = "Mean Similarity", title = "Mean Onset time Speaker vs  Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_speaker_wup.jpg")

ggplot(combined_df, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", se = TRUE,aes(color = condition)) + 
  labs(x = "Mean Onset time in ms", y = "Mean Similarity", title = "Mean Onset time Speaker vs  Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_speaker_wup_condition.jpg")

### listener success

combined_df <- df_success_category %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, com_success, similarity)

ggplot(combined_df, aes(x = com_success, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Proportion of successful trials", y = "Mean Similarity", title = "Listener Communicative Success vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_success_wup.jpg")

ggplot(combined_df, aes(x = com_success, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = condition)) + 
  labs(x = "Proportion of successful trials", y = "Mean Similarity", title = "Listener Communicative Success vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_success_wup_condition.jpg")

##### word2vec similarity

similarity_df <- read_csv("mean_similarity_word2vec.csv")

similarity_df <- similarity_df %>%
  rename(category = `Target concept`) %>%
  rename(similarity = Mean_Distractor_Similarity) %>%
  select(category, similarity)

### listener

combined_df_list <- agr_listener %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

ggplot(combined_df_list, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "RT in ms", y = "Similarity", title = "Mean RT Listener vs Mean Similarity between distractors and targets") +
  theme_minimal() 

#ggsave("similarity_listener_word2vec.jpg")

ggplot(combined_df_list, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = condition)) + 
  labs(x = "Mean RT in ms", y = "Mean Similarity", title = "Mean RT Listener vs Mean Similarity between distractors and targets") +
  theme_minimal() 

#ggsave("similarity_listener_word2vec_condition.jpg")


### speaker

combined_df_speak <- agr_speaker %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, mean_RT, similarity)

ggplot(combined_df_speak, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Onset time in ms", y = "Similarity", title = "Mean Onset time Speaker vs  Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_speaker_word2vec.jpg")

ggplot(combined_df_speak, aes(x = mean_RT, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", se = TRUE,aes(color = condition)) + 
  labs(x = "Mean Onset time in ms", y = "Mean Similarity", title = "Mean Onset time Speaker vs  Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_speaker_word2vec_condition.jpg")

### Figure 5

df_combined <- bind_rows(
  combined_df_speak %>% mutate(role = "Speaker"),
  combined_df_list %>% mutate(role = "Listener")
)

# Ensure role is a factor with the correct order
df_combined <- df_combined %>%
  mutate(role = factor(role, levels = c("Speaker", "Listener")))

# Plot
ggplot(df_combined, aes(y = mean_RT, x = similarity)) +
  geom_point(size = 2, alpha = 0.6, aes(color = condition, shape = role)) +
  geom_smooth(method = "lm", se = TRUE, aes(linetype = role), color = "black", alpha = 0.3, fill = "grey", linewidth = 0.6) +
  labs(title = NULL, x = "Target-Distractor Similarity", y = "Response Time (ms)", color = "Condition", shape = "Role", linetype = "Role") +
  guides( shape = guide_legend(order = 1), linetype = guide_legend(order = 2, override.aes = list(shape = NA)), color = guide_legend(order = 0)) +
  theme_minimal()

### listener success

combined_df <- df_success_category %>%
  left_join(similarity_df, by = c("category")) %>%
  select(condition, category, com_success, similarity)

ggplot(combined_df, aes(x = com_success, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + 
  labs(x = "Proportion of successful trials", y = "Mean Similarity", title = "Listener Communicative Success vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_success_word2vec.jpg")

ggplot(combined_df, aes(x = com_success, y = similarity)) +
  geom_point( size = 3, aes(color = condition)) + 
  geom_smooth(method = "lm", se = TRUE, aes(color = condition)) + 
  labs(x = "Proportion of successful trials", y = "Mean Similarity", title = "Listener Communicative Success vs Mean Similarity between distractors and targets") +
  theme_minimal() 

ggsave("similarity_success_word2vec_condition.jpg")


