# -*- coding: utf-8 -*-
"""
functions to get hyponym relations 

"""

#import nltk
#nltk.download('wordnet')
from nltk.corpus import wordnet as wn
from itertools import islice
from pprint import pprint


print()

def print_hypernyms(word_list):
    for word in word_list.split(','):
        print("HYPERNYMS for -> ",word,"\n")
        for synset in islice(wn.synsets(word, pos=wn.NOUN), None): # for all synsets of word
            print("______" + synset.name() + ": ")
            for hypernym in islice(synset.hypernyms(), None):
                print(hypernym.name()+ ",")
        print("\n")

def print_hyponyms(word_list):
    for word in word_list.split(','):
        print("HYPONYMS for -> ",word,"\n")
        for synset in islice(wn.synsets(word, pos=wn.NOUN), None): # for all synsets of word
            print("______" + synset.name() + ": ")
            for hyponym in islice(synset.hyponyms(), None):
                print(hyponym.name()+ ",")
        print("\n")

def print_taxonomy(word_list):
    for word in word_list.split(','):
        print("##########################\n#####Taxonomy for:" , word,  "\n##########################\n")
        print_hypernyms(word)
        print_hyponyms(word)

        print("TAXONOMY TREE for -> ",word,"\n")
        #getting the tree
        synset = wn.synsets(word, pos=wn.NOUN)[0]
        hyp = lambda s:sorted(s.hypernyms())
        pprint(synset.tree(hyp))
        print()
        
        
def is_hyponym(word_a, word_b):
    """
    Check if word_a is a hyponym of word_b
    
    :param word_a: The potential hyponym (string).
    :param word_b: The potential hypernym (string).
    :return: True if word_a is a hyponym of word_b, False otherwise.
    """
    # Get all synsets for the words
    synsets_a = wn.synsets(word_a)
    synsets_b = wn.synsets(word_b)

    # Iterate through all combinations of synsets
    for syn_a in synsets_a:
        for syn_b in synsets_b:
            
            # Traverse the hypernym hierarchy of syn_a
            hypernyms = syn_a.hypernym_paths()
            for path in hypernyms:
                
                if syn_b in path:

                    print(word_a,"is a hyponym of",word_b)
                    return True
                    
    print(word_a,"is not! a hyponym of",word_b)        
    return False


def lowest_common_hypernyms(word_a, word_b):

    # Get all synsets for the words
    synsets_a = wn.synsets(word_a)
    synsets_b = wn.synsets(word_b)

    # Iterate through all combinations of synsets
    print("lowest common hypernym(s):")
    for syn_a in synsets_a:
        for syn_b in synsets_b:

            print(syn_a.lowest_common_hypernyms(syn_b))

def find_distractors():
  print('\nFinding distractors: \n')
  list_common_parents_str = input("Please enter the hypernyms of the superodinate to find possible common parents (use \",\" and no space between words): ")
  list_common_parents = list_common_parents_str.split(',')
  print()

  for common_parent in list_common_parents:
    print('for common parent:', common_parent +'\n')
    print_hyponyms(common_parent)

  parents_hypomyms_str = input("Please enter the hyponyms of the common parent(s) you just entered to find distractors: ")
  parents_hyponyms = parents_hypomyms_str.split(',')
  print()

  for hyponym in parents_hyponyms:
    print('for distractor parent:', hyponym +'\n')
    print_hyponyms(hyponym)

def analyse_taxonomy(superordinate):
  print("the superordinte analyzed is:", superordinate, "\n")
  print_taxonomy(superordinate)
  find_distractors()
  
def get_hypernym_depth(word, pos='n'):  # 'n' for noun, 'v' for verb, etc.
    synsets = wn.synsets(word, pos=pos)  # Get synsets for the word
    if synsets:
        return synsets[0].min_depth()  # Get the depth of the first synset
    return None  # Return None if no synset is found
  
def get_avg_hypernym_depth(word, pos='n'):
    synsets = wn.synsets(word, pos=pos)
    if synsets:
        depths = [synset.min_depth() for synset in synsets]
        return sum(depths) / len(depths)  # Compute the average depth
    return None  