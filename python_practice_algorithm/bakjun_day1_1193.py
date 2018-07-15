# -*- coding: utf-8 -*-
"""
Created on Mon Jul 16 00:18:36 2018

@author: ewon
"""
import sys

num = int(sys.stdin.readline().rstrip())
count = 0
parent = 1
ch = 1

flag = 1
nujuk = 0
sum_ = 0

while (flag):
    nujuk += 1
    sum_ += nujuk
    if(sum_>num):
        flag = 0
        
for i in range(1,nujuk+1):
    for j in range(0,i):
        count+=1
        answer = str(i+1-parent)+"/"+str(parent)
        parent+=ch
        if count==num:
            print(answer)
        else:
            continue
    if(i%2==0):
        parent = 1
        ch*=-1
    else:
        parent = i+1
        ch*=-1