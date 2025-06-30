#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 15 13:15:33 2025

@author: vanessaobi
"""

import pandas as pd
import numpy as np


##loading data
df_grid = pd.read_csv('data/grid_items.csv', sep = ',', header= 0)
df_vad = pd.read_csv('data/VAD_ratings_lexicon.csv', sep='\t', header = None, names = ["Word","Valence","Arousal","Dominance"])
df_concr = pd.read_csv('data/concreteness_ratings_lexicon.csv', sep = ';', header= 1, usecols=['Word','Conc.M'],decimal=',')
df_spec = pd.read_csv('data/Scores_Conc_and_Spec.csv', sep = ',', header = 0, names = ["Word","Concreteness","Spec_1","Spec_2", "Spec_3"], usecols=['Word','Spec_3'])
df_freq = pd.read_csv('data/frequency_list.csv', sep = ',', header = 0, usecols=['Word','Lg10CD'])



# Melt the DataFrame/changing form of grid df
df_final = pd.melt(df_grid, id_vars=['Target concept','Domain'], var_name="Type", value_name='Word')
df_final["Word"] = df_final["Word"].str.lower()


##adding concreteness values
concreteness = []
for word in df_final["Word"]:
    try:
        value = df_concr.loc[df_concr['Word'] == word].reset_index().at[0,'Conc.M']
        concreteness.append(value)
        
    except:
        concreteness.append(np.nan)
        
df_final['Concreteness']= concreteness


##adding specificity values
specificity = []
for word in df_final["Word"]:
    try:
        value = df_spec.loc[df_spec['Word'] == word].reset_index().at[0,'Spec_3']
        specificity.append(value)
        
    except:
        specificity.append(np.nan)
        
df_final['Specificity']= specificity


##adding freq values
freq = []
for word in df_final["Word"]:
    try:
        value = df_freq.loc[df_freq['Word'] == word].reset_index().at[0,'Lg10CD']
        freq.append(value)
        
    except:
        freq.append(np.nan)
        
df_final['Lg10CD']= freq

##adding vad values
valence = []
arousal = []
dominance = []

for word in df_final["Word"]:
    try:
        value_val = df_vad.loc[df_vad['Word'] == word].reset_index().at[0,'Valence']
        value_arou = df_vad.loc[df_vad['Word'] == word].reset_index().at[0,'Arousal']
        value_dom = df_vad.loc[df_vad['Word'] == word].reset_index().at[0,'Dominance']

        valence.append(value_val)
        arousal.append(value_arou)
        dominance.append(value_dom)

    except:
        valence.append(np.nan)
        arousal.append(np.nan)
        dominance.append(np.nan)
        
df_final['Valence']= valence
df_final['Arousal']= arousal
df_final['Dominance']= dominance

##adding length
df_final['length']  = df_final['Word'].str.len()


df_final = df_final.sort_values(by=['Domain','Target concept']).reset_index(drop=True)

df_final.to_csv('stimuli_all_words_ratings.csv')