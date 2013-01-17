import sqlite3

names = """Lupita Alday
Elizbeth Bosco
Leilani Cooper
Ward Galland
Berna Foston
Bruna Axford
Roxana Booth
Heather Bolivar
Scarlett Morrisey
Ryann Chenoweth
Tami Bautch
Lashawna Berrier
Abbie Quail
Edna Waynick
Gertie Forbus
Lemuel Everette
Claris Lentz
Carie Squire
Kimbery Clewis
Deloras Profitt
Carolee Woodford
Mickey Aguayo
Alissa Hausman
Alanna Detrick
Lasonya Hammers
Isaac Heineman
Mariana Stansell
Emmett Newcomer
Grant Lacey
Joanie Roberge
Odell Shunk
Yasuko Kerbs
Mozella Stalker
Tommy Peaden
Rigoberto Asaro
Ciara Ducan
Adrian Greeson
Sheri Sickels
Sharita Custard
Margurite Mok
Deann Sacks
Farrah Diggs
Ashlee Downer
Tuyet Wagaman
Leonor Asuncion
Herbert Hennis
Joleen Starkey
Hyman Swanigan
Denese Cleveland
Devona Espinosa"""
import random

s = sqlite3.Connection("example.sqlite3")
for name in names.split("\n"):
    firstName, lastName = name.split()
    age = random.randint(1,90)
    s.execute('INSERT INTO "User" VALUES (NULL, ?, ?, ?, ?, NULL);',
            (firstName.lower() + str(age) , firstName, lastName, age
            ))

s.commit()


