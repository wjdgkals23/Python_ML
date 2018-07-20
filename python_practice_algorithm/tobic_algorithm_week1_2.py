# -*- coding: utf-8 -*-
"""
Created on Fri Jul 20 22:48:09 2018

@author: ewon
"""

#### 가장 큰 정사각형 찾기!
import numpy as np
a = np.array([[1,0,1,1,1], [0,0,0,1,1], [0,1,1,1,1], [0,1,1,1,1], [0,1,1,1,1]])
b = np.array([[1,0,1,1,1], [1,1,1,1,1], [0,1,1,1,1], [0,1,1,1,1], [0,1,1,1,1]])

def find_square(np_arr):
    width = len(np_arr)
    height = len(np_arr[0])
    
    for i in range(1,width):
        for j in range(1,height):
            if(np_arr[i-1][j-1]!=0 and np_arr[i-1][j]!=0 and np_arr[i][j-1]!=0):
                np_arr[i][j] = min(np_arr[i-1][j-1],np_arr[i-1][j],np_arr[i][j-1])+1
    return np.amax(np_arr)*np.amax(np_arr)

square_a = find_square(a)
square_b = find_square(b)

print(square_a)
print(square_b)