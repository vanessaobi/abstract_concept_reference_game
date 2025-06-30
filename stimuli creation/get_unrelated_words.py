#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 12:19:34 2025

find unrelated words

"""

from nltk.corpus import wordnet as wn

def is_unrelated(word, superordinate):
    word_synsets = wn.synsets(word)
    superordinate_synsets = wn.synsets(superordinate)
    
    if not word_synsets or not superordinate_synsets:
        return True  # If no synset is found, assume unrelated

    max_sim = max((w1.wup_similarity(w2) or 0) for w1 in word_synsets for w2 in superordinate_synsets)
    
    return max_sim  

# Example check
#print(is_unrelated("lavender", "animal"))  # Expected: True
#print(is_unrelated("dog", "animal"))    # Expected: False

import pandas as pd
import random

df = pd.read_csv('outputs/general_word_list_without_category.csv')

def get_unrelated_words_from_df(superordinate, df, concreteness_col="Concreteness", word_col="Word", num_words=20, threshold=0.3):

    super_synsets = wn.synsets(superordinate, pos=wn.NOUN)
    
    if not super_synsets:
        raise ValueError(f"No synsets found for '{superordinate}'.")

    super_synset = super_synsets[0]  # Take the first synset (most common meaning)

    unrelated_words = []

    # Iterate over words in the DataFrame
    for _, row in df.iterrows():
        word = row[word_col]
        concreteness = row[concreteness_col]

        word_synsets = wn.synsets(word, pos=wn.NOUN)
        if not word_synsets:
            continue  # Skip words not in WordNet

        similarity = max((super_synset.wup_similarity(syn) or 0) for syn in word_synsets)  # Max similarity

        if similarity < threshold:
            unrelated_words.append((word, similarity, concreteness))

    unrelated_df = pd.DataFrame(unrelated_words, columns=["word", "similarity", "concreteness"])

    concrete_words = unrelated_df[unrelated_df["concreteness"] > 3.5]
    abstract_words = unrelated_df[unrelated_df["concreteness"] <= 3.5]


    selected_concrete = concrete_words.sample(min(50, len(concrete_words)), random_state=36)
    selected_abstract = abstract_words.sample(min(0, len(abstract_words)), random_state=30)

    final_selection = pd.concat([selected_concrete, selected_abstract]).reset_index(drop=True)
    
    return final_selection

# Example Usage
#df = pd.read_csv("word_data.csv")
result_df = get_unrelated_words_from_df("emotion", df)
print(result_df)