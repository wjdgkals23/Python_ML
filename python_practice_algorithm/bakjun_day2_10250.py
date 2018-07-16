# -*- coding: utf-8 -*-
"""
Created on Mon Jul 16 23:56:57 2018

@author: ewon
"""

#10250번 문제 ACM 호텔

import sys

def output_func(h,w):
    if(w<10):
        print(str(h)+"0"+str(w))
    else:
        print(str(h)+str(w))

loop_cnt = sys.stdin.readline()
loop_cnt = int(loop_cnt.rstrip())
        
while loop_cnt > 0:
    task = sys.stdin.readline()
    task = task.rstrip().split(" ")
    height = int(task[0])
    width = int(task[1])
    num = int(task[2])
    cnt = 0
    for i in range(0,width):
        for j in range(0,height):
            cnt+=1
            if(cnt==num):
                output_func(j+1,i+1)
            else:
                continue
    loop_cnt-=1