#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Dec  9 16:55:53 2024

@author: vanessaobi

early phase of stimuli creation: functions to get ratings of a word for concreteness, valence, arousal and dominance
"""
import pandas as pd

vad_df = pd.read_csv('data/VAD_ratings_lexicon.csv', sep='\t', header = None, names = ["Word","Valence","Arousal","Dominance"])
concr_df = pd.read_csv('data/concreteness_ratings_lexicon.csv', sep = ';', header= 1)


ratings_df = pd.merge(vad_df, concr_df, how="inner", on="Word")
ratings_df = ratings_df.drop(['Bigram', 'Conc.SD', 'Unknown', 'Total', 'Percent_known', 'SUBTLEX'], axis=1)
ratings_df = ratings_df.set_index('Word')
#print(ratings_df.shape) #-> (17053,4)

ratings_df['Conc.M'] = ratings_df['Conc.M'].str.replace(',', '.').astype(float)

def get_ratings(word_list):
    df = pd.DataFrame()
    for word in word_list.split(','):
        word_rating = ratings_df.loc[[word]]
        df = pd.concat([df,word_rating ]) 
    return df
        

def get_ratings_sample(n):
   sample = ratings_df.sample(n=n)
   return sample
        