# -*- coding: utf-8 -*-
"""
Created on Fri Jul 13 10:34:39 2018

@author: ewon
"""
#15552번 빠른 A+B
import sys

num = sys.stdin.readline()
num = int(num.rstrip())
print(num)

num = 5
while num > 0:
    numline = sys.stdin.readline()
    line = str(numline).rstrip()
    numarr = line.split(" ")
    print(int(numarr[0])+int(numarr[1]))
    num-=1

