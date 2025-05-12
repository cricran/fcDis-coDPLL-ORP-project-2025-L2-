**Fiche 3 - Logique propositionnelle : la réfutation (Formes clausales disjonctives)**

---

# 2. Forme clausale disjonctive

La forme clausale disjonctive associée à une formule \( L \) se calcule en transformant la formule en un ensemble de clauses, où une clause est un ensemble de littéraux, et un littéral est un atome positif \( a \) ou négatif \( \neg a \).  
Une clause est évaluée comme **la conjonction** des littéraux qu’elle contient, et la forme clausale comme **la disjonction** des clauses qu’elle contient.

Par exemple :  
- La forme clausale \( \{\{a, b\}, \{\neg a, \neg c\}, \{\neg c, \neg b\}, \{c\}\} \) est évaluée comme :

\[(a \land b) \lor (\neg a \land \neg c) \lor (\neg c \land \neg b) \lor (c)\]

- La forme clausale vide est interprétée comme \( \bot \).
- La clause vide est interprétée comme \( \top \).

La transformation en forme clausale disjonctive se fait en trois étapes :
1. Transformation des opérateurs.
2. Descente de la négation.
3. Mise sous forme ensembliste.

---

# 5. La mise sous forme ensembliste

La mise en forme ensembliste est calculée ainsi :

- \( \neg a \) devient \( \{\{\neg a\}\} \)
- \( a \) devient \( \{\{a\}\} \)
- \( \bot \) devient \( \emptyset \)
- \( \top \) devient \( \{\emptyset\} \)

Ensuite :
- **Disjonction** de deux formes : union simple.

\[\{C_1, \dots, C_m\} \lor \{C'_1, \dots, C'_n\} = \{C_1, \dots, C_m, C'_1, \dots, C'_n\}\]

- **Conjonction** de deux formes : combinaison cartésienne.

\[\{C_i \cup C'_j \mid 1 \leq i \leq m, 1 \leq j \leq n\}\]

**Exemple de conjonction** :

\[(a \lor b) \land (c \lor d)\]

donne les clauses :

\[(a \land c) \lor (a \land d) \lor (b \land c) \lor (b \land d)\]

c'est-à-dire :

\[\{\{a, c\}, \{a, d\}, \{b, c\}, \{b, d\}\}\]

---

# 6. La règle de Robinson

La règle de Robinson est basée sur la tautologie :

\[(\neg G \land H) \lor (G \land I) \to H \lor I\]

Si deux clauses sont :

- \( C_1 = \{\neg a\} \cup \{l_1, \ldots, l_m\}\)
- \( C_2 = \{a\} \cup \{l'_1, \ldots, l'_n\}\)

alors leur résolvant est :

\[C_3 = \{l_1, \ldots, l_m, l'_1, \ldots, l'_n\}\]

Si \( C_1 = \{a\} \) et \( C_2 = \{\neg a\} \), alors \( C_3 = \emptyset \) et la forme clausale est une tautologie.

---

# 7. L'algorithme de réfutation

Pour montrer qu'une formule est une tautologie :

- Chercher deux clauses contenant \( a \) et \( \neg a \).
- Former leur résolvant en supprimant \( a \) et \( \neg a \) et en unionnant.
- Continuer jusqu'à trouver la clause vide \( \emptyset \) ou épuisement.

Si \( \emptyset \) est trouvée, alors la formule est une tautologie.

---

# 8. Exemple complet

Forme clausale :

\[\{\{a, b\}, \{\neg a, \neg c\}, \{\neg c, \neg b\}, \{c\}\}\]

Clauses :
- \( C_1 = \{a, b\} \)
- \( C_2 = \{\neg a, \neg c\} \)
- \( C_3 = \{\neg c, \neg b\} \)
- \( C_4 = \{c\} \)

Etapes :
1. \( C_1 \) et \( C_2 \) par \( a \) : \( C_5 = \{b, \neg c\} \)
2. \( C_4 \) et \( C_5 \) par \( c \) : \( C_6 = \{b\} \)
3. \( C_3 \) et \( C_4 \) par \( c \) : \( C_7 = \{\neg b\} \)
4. \( C_6 \) et \( C_7 \) par \( b \) : \( \emptyset \)

Donc la formule est une tautologie.

---

