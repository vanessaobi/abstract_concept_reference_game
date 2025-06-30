# abstract_concept_reference_game
This repository contains the experiment data and code used for stimulus creation and data analysis in my bachelor's thesis. The thesis focuses on a psycholinguistic experiment where participants engage in a two-player abstract-concept reference game. In this game, they communicate about abstract and concrete categories. The study aims to explore the underlying mechanisms and the concreteness effect in category production and comprehension within an interactive game-like setting.

## Content of this repository:

+ experiment_implementation -> how the experiment was implemented

### data analysis includes:

+ R script of data analysis
+ data_cleaned.csv -> includes all the data needed for analysis, excluding the excluded participants
+ df_encoded -> encoded with the original encoding by Rissman et al.(2023) (changed in R script)
+ df_contr_var_all.csv -> control variables for every word of every item
+ df_contr_var_all_mean.csv  -> mean for every condition (included in thesis)
+ df_contr_var_all_sd.csv -> sd for every condition (included in thesis)
+ contr_var_superordinates.csv -> control variables the superordinate terms
+ df_debriefing -> responses participants gave

### data_pilot_complete includes:

+ partcipants data

### figures includes:

+ figures used in the thesis
+ stimuli_grid-csv -> all item grids

### stimuli creation includes:

+ taxonomy_func.py -> functions to work with the WordNet taxonomy, i.e., finding targets and distractors
+ create_complete_stimuli_list_with_ratings.py -> creates a list of ratings (control variables) for every word in the list of stimuli
+ create_superordinate_list_with_ratings.py -> creates a list of ratings (controle variables) for the superordinates
+ plot_ratings_superordinates.py -> plots concreteness ratings of the superordinates
+ create_general_word_list_ratings.py -> creates a general word lists with ratings of the members from the categories used in Banks and Connell (2022)
+ ratings_func.py -> functions of for getting control variables of words manually (used in the early phase of stimuli creation)
+ get_unrelated_words.py -> code to get proposals for the unrelated words
+ general_word_list_without_category.csv -> used in get_unrelated_words.py

### test_data_pilot includes:

+ test data before experiment was run

### not_used_in_thesis includes:

+ prior versions of data analysis
+ simillarity_target_distractors_glove.py -> creates list of similarity between targets and distractors (using Glove)
+ csv of mean similarity (glove) between targets and distractors

