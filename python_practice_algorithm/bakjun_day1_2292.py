# -*- coding: utf-8 -*-
"""
Created on Sun Jul 15 23:45:01 2018

@author: ewon
"""

#2292번 벌집 문
import sys
num1 = sys.stdin.readline()
num1 = int(num1.rstrip())
#ex) 13 // 1<13 1+6<13 1+6+12>13
flag = 1
count = 1
nujuk = 1

while (flag):
    if nujuk < num1:
        nujuk = nujuk + count*6
        count+=1
    else:
        flag=0
print(count)