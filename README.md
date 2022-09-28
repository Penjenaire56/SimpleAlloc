SimpleAlloc
===

SimpleAlloc est un allocateur de registre pour le langage Usuba0. 
Il peut être utilisé pour générer du Jasmin ou du C.
Il contient aussi des fonctions pour optimiser les algorithmes de chiffrements symétriques selon la méthode de l'optimisation FACE.


## Pour les développeurs 

Utilisez la fonction *mainGetDatas" pour récupérer l'ensemble des calculs de l'optimisation FACE ainsi que les tests et les fonctions de chiffrements optimisées.
Utilisez la fonction *buildUA0" pour compiler Usuba0 vers C ou Jasmin en choisissant au choix la fonction *slpToC* ou *slpToJasmin*.

Trouvez plus d'information dans le ficier *Rapport.pdf*.

Les tests peuvent être lancés sous Python3 avec le module Boolector.
