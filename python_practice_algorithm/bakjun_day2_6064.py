# -*- coding: utf-8 -*-
"""
Created on Tue Jul 17 00:22:41 2018

@author: ewon
"""
#6064번 시간초과 상태 내일 수정

import sys

loop_cnt = sys.stdin.readline()
loop_cnt = int(loop_cnt.rstrip())
        
while loop_cnt > 0:
    task = sys.stdin.readline()
    task = task.rstrip().split(" ")
    x = int(task[0])
    y = int(task[1])
    x_ = int(task[2])
    y_ = int(task[3])
    flag = 1
    x__ = 0
    y__ = 0
    cnt = 0
    while (flag):
        x__+=1
        y__+=1
        cnt+=1
        if(x__ > x):
            x__ = 1
        if(y__ > y):
            y__ = 1
        if(x__ == x_ and y__ == y_):
            print(cnt)
            flag = 0
        if(x__ == x and y__ == y):
            print(-1)
            flag = 0
    loop_cnt-=1