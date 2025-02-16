# Compilateur de C
Compilateur de C réalisé dans le cadre d'un travail de groupe à 3 en 1A (INF108).

Des librairies OCaml sont utilisées pour lexer et parser le code à compiler (expr2json/expr2json.ml) et créer un fichier json.

Le fichier python compilateur.py interprète ensuite ce fichier json pour écrire un fichier assembleur qui a le comportement voulu par le code initial.

Les pointeurs, les variables locales et globales, les opérations arithmétiques, les tableaux, les fonctions, les printf/scanf, les if/else, les boucles while et les malloc sont supportés.
