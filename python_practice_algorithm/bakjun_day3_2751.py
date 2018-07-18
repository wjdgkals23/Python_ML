# -*- coding: utf-8 -*-
"""
Created on Wed Jul 18 00:33:43 2018

@author: ewon
"""

#2751번 정렬(Merge Sort)

import sys

def make_arr():
    arr_cnt = sys.stdin.readline()
    arr_cnt = int(arr_cnt.rstrip())
    
    list_ = []
    for i in range(0,arr_cnt):
        num = sys.stdin.readline()
        num = int(num.rstrip())
        list_.append(num)
    return list_

def show_list(list_):
    for i in range(0,int(len(list_))):
        print(list_[i])

class Merge:
    def setdata(self, list_):
        self.list_ = list_
    def slice_num(self,s_list):
        return len(s_list)/2
    
    def merge_list(self,left,right):
        arr = left.copy()
        arr.extend(right)
        arr.sort()
        return arr
    
    def merge_sort(self,m_list):
        if(len(m_list) > 1):
            mid = int(self.slice_num(m_list))
            left = m_list[:mid] 
            right = m_list[mid:]
            self.merge_sort(left)
            self.merge_sort(right)
            temp = self.merge_list(left,right)
            return temp
        else:
            return m_list

r_list = make_arr()
mer = Merge()
mer.setdata(r_list)
show_list(mer.merge_sort(mer.list_))

