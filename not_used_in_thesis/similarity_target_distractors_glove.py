#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 20 15:16:19 2025

@author: vanessaobi
"""
import gensim.downloader as api
import pandas as pd
from nltk.corpus import wordnet as wn
import numpy as np

#  Google's Word2Vec model 
#word2vec = api.load("word2vec-google-news-300")

# Glove wikipedia gigaword model
glove = api.load("glove-wiki-gigaword-100")

# Load the dataset
df = pd.read_csv("data/grid_items.csv")  # Replace with your actual file path



def compute_similarity(word1, word2):
    if word1 in glove:
        if word2 in glove:        
            similarity = glove.similarity(word1, word2)
        else: 
            print(word2, "is not in vocabulary!")
            similarity = None
    else: 
        print(word1, "is not in vocabulary!")
        similarity = None
    return similarity
    

# List to store computed means
mean_similarities = []

df_all_similarity_pairs = pd.DataFrame(columns = ['Distractor', 'Target', 'Similarity'])

# Iterate over each row
for _, row in df.iterrows():
    targets = [row["Target 1"], row["Target 2"], row["Target 3"]]
    distractors = [row["Distractor 1"], row["Distractor 2"]]

    similarities = []  # Store similarities for this row
    
    #for distractor in distractors:
    for distractor in distractors:  
        for target in targets:
            sim = compute_similarity(distractor, target)
            similarities.append(sim)
            new_dist_targ_sim = pd.DataFrame({'Distractor':[distractor], 'Target':[target], 'Similarity':[sim]})
            df_all_similarity_pairs = pd.concat([df_all_similarity_pairs,new_dist_targ_sim], ignore_index=True)
        
    # Compute mean similarity for this row (ignore empty cases)
    mean_sim = np.mean(similarities) 
    mean_similarities.append(mean_sim)

# Add results to dataframe
df["Mean_Distractor_Similarity"] = mean_similarities

# Save to CSV
#df.to_csv("mean_similarity_word2vec.csv", index=False)
#df_all_similarity_pairs.to_csv("similarity_all_targets_distractors_word2vec", index=False)

# Save to CSV
df.to_csv("mean_similarity_targets_distractors_glove.csv", index=False)
df_all_similarity_pairs.to_csv("similarity_all_targets_distractors_glove", index=False)

