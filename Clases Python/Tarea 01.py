# -*- coding: utf-8 -*-
"""
Created on Tue Jan  3 14:52:17 2023

@author: lfigueroa
"""

# In[]:
##### TAREA 01 ##############################################################

##### 1.2. LISTAS ######

# In[]:
#1. Show the indices of the np.nan values in the f_list list. 
#We want to see this output: The indices 0, 1, 4, 7 have np.nan values. 
#Hint: Use print function and f-strings to insert the indices values.

f_list = [ "np.nan", "np.nan", "Austria", "Germany", "np.nan", "Pakistan", "np.nan", "np.nan"]
print("Lista Original : " + str(f_list))

#Método Básico
f_list_2 = []
print(f_list_2)
for i in range (0, len(f_list)) :
    if f_list[i] == "np.nan" :
        f_list_2.append(i)
print(f_list_2)
print("Los índices " + str(f_list_2) + " tienen por valor 'np.nan'")      

#Método con Comprehención de Listas
f_list_3 = [i for i in range(0, len(f_list)) if f_list[i] == "np.nan"]
print(f_list_3)
print("Los índices " + str(f_list_3) + " tienen por valor 'np.nan'")

#Método con F-string
index_npnan = f_list.index("np.nan" , 0, len(f_list))
print(index_npnan)

index_npnan = F"los índices {f_list_2} tienen por valor 'np.nan'"
index_npnan = F"los índices {f_list_3} tienen por valor 'np.nan'"
print(index_npnan)
print(index_npnan)

# In[]:
#2. Replicate 4 times the values of the list p2_list. 
#We expect an ouput like this: [ 2 , 3, 4, 5, 2 , 3, 4, 5, 2 , 3, 4, 5, 2 , 3, 
#4, 5]. 
#Hint: Use multiplication function in listsand see the output.



# In[]:
#3. Print the length of f_list. 
#Hint: Length function

f_list = [np.nan , np.nan, "Austria", "Germany", np.nan, "Pakistan", "np.nan", np.nan ]
print(len(f_list))

# In[]:
#4. Print My teacher assistant is so boring. using text1 list. 
#Hint: Use the join function

text1 = ['My', 'teacher', 'assistant', 'is', 'so', 'boring.']
text1_join = " ".join(text1)
print(text1_join)

# In[]:
#5. Print My TA is so boring, but is very funny. using text1 list.
#Hint: Use the join function, and extend method.

#Primer Método

text1 = ['My', 'teacher', 'assistant', 'is', 'so', 'boring.']
print(text1)
text2 = ['but', 'is', 'very', 'funny']
print(text2)

text1.extend(text2)
print(text1)
text3_join1 = " ".join(text1)
print(text3_join1)
text4_replace = text3_join1.replace("teacher", "T", 1).replace("assistant", "A", 1)
print(text4_replace)

#Segundo Método

text1 = ['My', 'teacher', 'assistant', 'is', 'so', 'boring.']
print(text1)
text2 = ['but', 'is', 'very', 'funny']
print(text2)

del text1[2]
text1[1] = "TA"
text1[4] = "boring, "
text1.extend(text2)
text3_join2 = " ".join(text1)
print(text3_join2)

# In[]:
#6. Print "The max value of values1 is 86 and is located in the 0 index."
#and "The min value of values1 is 0 and is located in the 7 index."
#Hint: Use the f-string, min, and max functions.

values1 = [ 86, 86, 85, 85, 85, 83, 23, 0, 84, 1 ]
max_values1 = max(values1)
min_values1 = min(values1)
max_index = values1.index(max_values1)
min_index = values1.index(min_values1)
print(max_values1)
print(max_index)
print(min_values1)
print(min_index)

max_value_index = F"The max value of 'values1' is {max_values1} and is located in the {max_index}"
min_value_index = F"The min value of 'values1' is {min_values1} and is located in the {min_index}"
print(max_value_index)
print(min_value_index)

# In[]:
#7. Get two lists: `names` and `last_names` using `last_and_name` list. 
#Hint: Use `map` and `split`.

last_and_name = [ "CORNEJO SANCHEZ, CHRISTIAN SANTOS", "ORELLANA QUISPE, CRISTIAN NASSER", "MORALES CHOQUEHUANCA, ANGELICA KARINA", "GUIMARAY RIBEYRO, JOSE ROBERTO", "CAMACHO GAVIDIA, ABEL FERNANDO", "TINTAYA ORIHUELA, MEIR ALVARO", "CHAVEZ MARTINEZ, JOSELIN ALEXANDRA", "FIGUEROA MURO, LEONEL ARTURO", "GOMEZ CRIBILLERO, JOSE FELIPE", "PALOMINO SEGUÍN, AFRANIA", "LUZON CUEVA, BIANCA MARIETTE", "SUAÑA ZEGARRA, ADRIAN ANDRE", "SOTO POMACHAGUA, DORKAS YOMIRA JHERMY", "FIORENTINO MARTINEZ, LADY ALY", "LAMA MAVILA, HECTOR ANDRE", "MEZA HINOJO, GUSTAVO", "LOZADA MURILLO, PERSEO MARCELO", "ZAMBRANO JIMENEZ, MIGUEL ALONZO", "JACOBS LUQUE, NICOLAS", "VIDAL VIDAL, ROCIO GABRIELA", "TORRES ANICAMA, JANE CAMILA", "LOPEZ ESTRADA, MARIA ELISA", "BOYCO ORAMS, ALEJANDRO", "DIAZ BERROSPI, KARLINE ROSMELI", "RIEGA ESCALANTE, STEPHY ROSARIO", "LEVANO TORRES, VALERIA CECILIA", "ESQUIVES BRAVO, SEBASTIAN RENATO", "PEREZ GONZALES, JUAN CARLOS", "OTERO MAGUIÑA, MARIANA", "CLAVO CAMPOS, ANDREA BRIZETH", "AGUILAR GARCIA, ERICK JOSUE", "CALDAS VELASQUEZ, JOSUE DANIEL", "SALAS NUÑEZ BORJA, FABIO MANUEL", "PIZARRO VILLANES, FERNANDA NICOLLE", "QUILLATUPA MORALES, ANGELA ADELINA", "HUANCAYA IDONE, CESAR DANTE", "CALVO PORTOCARRERO, GABRIELA ISABEL", "IBAÑEZ ABANTO, ANGEL MAURICIO", "MELÉNDEZ APONTE, JUAN DIEGO", "CRISTIAN SERRANO, ARONE", "HINOJOSA CAHUANA, PERCY ALBERTH", "ANGLAS GARCÍA, KEVIN ARTURO", "ALDAVE ACOSTA, CESAR ERNESTO", "NÚÑEZ HUAMÁN, CÉSAR AGUSTO", "OBREGON HUAMAN, DIANA EDITH", "SOTO PACHERRES, RODRIGO FRANCO", "INGARUCA RIVERA, GRETTEL ALEXANDRA", "ROJAS HUAMAN, ROSA ANGELA", "NEYRA SALAS, DANTE OMAR", "HUERTA ESPINOZA, YAJAIRA ALEXANDRA", "HUANCA MARTINEZ, JORGE ALBERTO", "FLORES CADILLO, ALEXIS" ]
print(last_and_name)

last_and_name2 = list(map(lambda n: n.split(", "), last_and_name))
last_names = list(map(lambda n: n[0], last_and_name2))
print(last_names)
names = list(map(lambda n: n[1], last_and_name2))
print(names)

# In[]:
#8. Give only the last names of students who do not have email. 
#Use the `emails` and `last_names` list.
#Hint: Use `map` and `split`

emails = ["cscornejo@pucp.edu.pe", "orellana.cn@pucp.edu.pe", "karina.morales@pucp.edu.pe", "a20083223@pucp.pe", "abel.camacho@pucp.pe", "mtintaya@pucp.edu.pe", "joselin.chavez@pucp.edu.pe", "a20105737@pucp.pe", "jfgomezc@pucp.pe", "afrania.palomino@pucp.pe", "luzon.bianca@pucp.pe", "adrian.suanaz@pucp.pe", "soto.y@pucp.edu.pe", "a20132766@pucp.pe", "andre.lama@pucp.edu.pe", "gustavo.meza@pucp.edu.pe", "pmlozada@pucp.edu.pe", "m.zambranoj@pucp.edu.pe", "nicolas.jacobs@pucp.edu.pe", "gvidal@pucp.edu.pe", "jane.torres@pucp.edu.pe", "m.lopez@pucp.edu.pe", "alejandro.boyco@pucp.edu.pe", "a20167070@pucp.edu.pe", "riega.stephy@pucp.edu.pe", "vlevanot@pucp.edu.pe", "sesquives@pucp.edu.pe", "perez.juanc@pucp.edu.pe", "mariana.otero@pucp.edu.pe", "aclavo@pucp.edu.pe", "a20182474@pucp.edu.pe", "josue.caldas@pucp.edu.pe", "fabio.salas@pucp.edu.pe", "fernanda.pizarro@pucp.edu.pe", "aquillatupa@pucp.pe", "", "", "", "", "", "f0873079@pucp.edu.pe", "", "", "", "", "", "", "", "", "", "", "flores.alexis@pucp.edu.pe", ]

import numpy as np
emails = np.array(emails)
result = np.where(emails == "")
index_emails = list(result[0])
print(index_emails)

last_names_emails = list(map(lambda n: last_names[n], index_emails))
print(last_names_emails)