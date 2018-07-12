# -*- coding: utf-8 -*-
"""
Created on Thu Jul 12 23:57:38 2018

@author: ewon
"""
string = input("ex) 5 5 $")
numbers = string.split(" ")
one = int(numbers[0])
two = int(numbers[1])

num = 0
row = 0
col = -1
switch = 1
round = one

matrix = [[0]*one for i in range(two)]

while round!=0:
    for i in range(round):
        col+=switch
        num+=1
        matrix[row][col] = num
    round-=1
    for i in range(round):
        row+=switch
        num+=1
        matrix[row][col] = num
    switch*=(-1)
    
print(matrix)