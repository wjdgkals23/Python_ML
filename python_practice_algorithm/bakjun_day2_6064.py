# -*- coding: utf-8 -*-
"""
Created on Tue Jul 17 00:22:41 2018

@author: ewon
"""
#6064번

#각 x,y에 대해 M과N이 있다.
#M과N의 최소공배수를 넘지 않는 수 중
#M과N으로 나눈 나머지가 x와 y가 되는 것을 찾는 문제 

import sys

loop_cnt = sys.stdin.readline()
loop_cnt = int(loop_cnt.rstrip())

def get_gcd(a,b):
    if(b==0):
        return a
    else:
        return get_gcd(b, a%b)
            
while loop_cnt > 0:
    task = sys.stdin.readline()
    task = task.rstrip().split(" ")
    x = int(task[0])
    y = int(task[1])
    x_ = int(task[2])
    y_ = int(task[3])
    gcd = get_gcd(x,y) #최대공약수 찾기
    lcm = (x*y/gcd) #최소공배수 계산
    flag = 1
    while (x_!=y_ and x_<lcm and y_<lcm):
        if(x_ > y_):
            y_+=y
        else:
            x_+=x
    if(x_ == y_):
        print(x_)
    else:
        print(-1)
    loop_cnt-=1