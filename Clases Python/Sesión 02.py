# -*- coding: utf-8 -*-
"""
Created on Sat Dec 17 16:01:51 2022

@author: lfigueroa
"""

##### INTRODUCCIÓN A PYTHON ###############################################
##### SESIÓN 02 ###########################################################
    
# In[]
##### STRINGS #############################################################

str = "Introducción a Python"
print(str)

#posiciones
print(str[0])
print(str[3])
print(str[-1])
print(str[-6])
print(str[1:5])

# In[]
##### BOOLEANO ############################################################
print(10<9)
print(10==10)
print(10>9)
True == 1
False == 0
print(type(True))

# In[]
##### TUPLAS ##############################################################

tupla = (1,2,3,4,5)
print(tupla)
print(type(tupla))

tupla1 = (1,2,3,4,5,6)
print(tupla1)
print(tupla1[1])
print(max(tupla1))
print(min(tupla1))

tupla2 = (1,3,3,5,5,55,10,11)
print(tupla2)
print(tupla2.count(5))
print(tupla2.index(10))

tupla3 = ("leonel",24,True)
print(tupla3)
print(len(tupla3[0]))


# In[]:
##### LISTA ###############################################################

Lista = [1,2,3]
print(type(Lista))

list1 = [1,2,3,"alex"]
print(type(list[3]))
print(list1[-2])

list2 = [1,2,3,5,8,76,13,57,9]
print(list2)
print(len(list2))
print(max(list2))
print(min(list2))

listcopy = list2.copy()
print(listcopy)

notas = [1,23,13,129,20,7]
print(notas)
notas.sort()
print(notas)

list3 = [1,4,45,78,12,444,32,55,121,6,90]
print(list3)
list3.append(53)
print(list3)

list4 = [14,51,754]
list5 = [12,25,37,48]
print(list4)
print(list5)
list4.extend(list5)
print(list4)
print(list4.index(754))

# In[]:
##### NÚMERO ##############################################################
    
a = 50
b = 30.47
print(a)
print(b)
print(type(a))
print(type(b))
