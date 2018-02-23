# -*- coding: utf-8 -*-
"""
Created on Thu Feb 22 13:37:51 2018

@author: runan.yao
"""
from decimal import Decimal
import scipy


def calcVolume(d,r):
    #return Decimal( scipy.special.gamma(float(d)/2) )
    return (2*scipy.power(r,d)*scipy.power(scipy.pi, (d/2))/(d*scipy.special.gamma(d/2)))


r = Decimal(100)
e = Decimal(0.01)
d = range(500)

ratio = []

for di in d:
    if di == 0:
        continue
    big = calcVolume(Decimal(di), r)
    small = calcVolume(Decimal(di), r-e)
    ratio = 1 - scipy.divide(small, big)
    

