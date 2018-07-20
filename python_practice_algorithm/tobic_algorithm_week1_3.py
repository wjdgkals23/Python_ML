# -*- coding: utf-8 -*-
"""
Created on Fri Jul 20 00:19:54 2018

@author: ewon
"""
import numpy as np
from copy import deepcopy
#input example list
num = [10,20,40,25,30,15,50]


#making check list
ch = np.zeros(len(num), int)
ma = 0
# 각 원소별로 포함한 상태와 안한 상태를 누적하며
# 확인한다.
def size_cnt(ch):
    global ma
    start_index = np.where(ch>0)
    if(len(start_index[0]) == 0 or len(start_index[0]) < ma):
        ## 현재 나온 길이보다 짧으면 탐색 과정 x
        return 0
    else:
        cnt = 1
        pre = num[start_index[0][0]]
        for i in start_index[0]:
            if(num[i] > pre):
                cnt+=1
                pre = num[i]
        if(ma < cnt):
            ma = cnt

def find_up_line_size(ch, i):
    ch_ = deepcopy(ch)
    i_ = i
    i_+=1
    if(i_==len(num)-1):
        size_cnt(ch_)
        ch_[i_] = 1
        size_cnt(ch_)
    else:
        find_up_line_size(ch_, i_)
        ch_[i_] = 1
        find_up_line_size(ch_, i_)

find_up_line_size(ch, 0)
ch[0] = 1
find_up_line_size(ch, 0)

print(ma)
    