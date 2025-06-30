#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec 20 15:14:58 2024

@author: vanessaobi

plots concreteness ratings of the superordinates

"""

import ratings_func as rt
import matplotlib.pyplot as plt
import pandas as pd
from scipy import stats

abstr_df = rt.get_ratings('craft,quantity,emotion,religion,relationship,sport,profession,time,communication,shape')
concr_df = rt.get_ratings('animal,vehicle,clothing,furniture,food,plant,building,tool,beverage,drink,fruit')

df = pd.concat([abstr_df, concr_df], axis=0)

for column in df.columns:
    
    x = df['Conc.M']
    y = df[column]
    
    x1 = abstr_df['Conc.M']
    y1 = abstr_df[column]
    
    x2 = concr_df['Conc.M']
    y2 = concr_df[column]  
    
    slope, intercept, r, p, std_err = stats.linregress(x, y)
    
    print(column+":")
    
    print("Slope:", slope)
    print("Intercept:", intercept)
    print("p:",p)
    
    def myfunc(x):
      return slope * x + intercept
    
    mymodel = list(map(myfunc, x))
    
    plt.scatter(x1, y1, c ='blue')
    plt.scatter(x2, y2, c ='red')
     
    plt.plot(x, mymodel)
    plt.xlabel(x.name)
    plt.ylabel(y.name)
    plt.title(y.name)
    plt.show()