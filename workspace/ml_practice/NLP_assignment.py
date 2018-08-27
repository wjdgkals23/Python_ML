# -*- coding: utf-8 -*-
"""
Created on Fri Aug 17 21:11:13 2018

@author: wjdgk
"""

# Tokenize
from konlpy.tag import *
import pandas as pd
import numpy as np
import re

twitter = Okt()
kkma = Kkma()

def clean_text(text):
    cleaned_text = re.sub('[a-zA-Z]', '', str(text))
    cleaned_text = re.sub('[\{\}\[\]\/?.,;:|\)*~`!^\-_+<>@\#$%&\\\=\(\'\"]',
                          '', cleaned_text)
    return cleaned_text

data = pd.read_csv('data/tw_Cr.csv')

data.iloc[:, 1:2] = data.iloc[:, 1:2].apply(clean_text, axis=0) 