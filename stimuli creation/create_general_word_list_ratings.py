#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Feb  5 15:37:36 2025

@author: vanessaobi

creates a general word lists with ratings of the members from the categories used in Banks and Connell (2022)
"""

import pandas as pd

vad_df = pd.read_csv('data/VAD_ratings_lexicon.csv', sep='\t', header = None, names = ["Word","Valence","Arousal","Dominance"])
concr_spec_df = pd.read_csv('data/Scores_Conc_and_Spec.csv', sep = ',', header = 0, names = ["Word","Concreteness","Spec_1","Spec_2", "Spec_3"])
freq_df = pd.read_csv('data/frequency_list.csv', sep = ',', header = 0)

#print(freq_df.columns)

freq_df = freq_df[['Word', 'Lg10CD']]
concr_spec_df = concr_spec_df.drop(['Spec_1','Spec_2' ], axis=1)

df = pd.merge(freq_df, concr_spec_df, how="inner", on="Word")
#df = pd.merge(vad_df, freq_df, how="inner", on="Word")

df['length'] = df['Word'].str.len()




#create category prodcution norms dataframe
prod_norm_df = pd.read_csv('data/production_norm_data.csv', sep = ',', header = 0)
prod_norm_df = prod_norm_df[['category','category.member','domain','prod.freq.percent']]

prod_norm_df = prod_norm_df[prod_norm_df['category'].isin(['religion','animal','building','clothing','emotion','fruit','furniture','profession','social relationship','sport','unit of time','geometric shape', 'vehicle','tool'])]
prod_norm_df = prod_norm_df.rename(columns={'category.member':'Word'})

df = pd.merge(df, prod_norm_df, how="inner", on="Word")


print(prod_norm_df.head())

df = df.set_index('Word')

print(df.shape) #-> (17053,4)
print(df.columns)

df = df[['domain','category','prod.freq.percent','Concreteness','Spec_3','Lg10CD','length']]
df = df.sort_values(by=['category'])
df.to_csv('general_word_list_without_vad.csv')
