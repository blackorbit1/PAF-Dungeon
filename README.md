# PAF-Dungeon

<img src="https://i.gyazo.com/bd6546a0026d1a4f38b9eb16190c617e.png" alt="drawing" width="400"/>
<img src="https://i.gyazo.com/04c4539da391618e529ced715281f602.png" alt="drawing" width="300"/>

## Projet de PAF (2019 - 2020) - Dungeon Crawling


### <u>**Manuel d'utilisation du projet**</u>


#### Execution

Version du compilateur ghc en lts-12.26.\
Se placer dans la racine du projet et taper : `stack run`

#### Contrôles

Z : haut\
Q : gauche\
S : bas\
D : droite

#### Tests

Se placer dans la racine du projet et taper `stack test` pour executer les invariants.
Les tests basés sur les propriétés (property-based testing) seront executés sur un set de données prédéfini.


### <u>**Spécifications**</u>

#### Carte

L'invariant du type carte est constitué de plusieurs invariants :
- que toutes les coordonnées soient dans les limites de la carte 
- que toutes les coordonnées définies dans les limites correspondent bien à une case dans la carte.
- que la carte comporte une unique entrée et une unique sortie
- que la carte soit entierement entourée de murs
- qu'il y ait un mur de part et d'autre de chaque porte de la carte

_On remarque que pour avoir une entrée, une sortie et etre entierement entouré par des murs, la carte a des dimensions minimales de 3x4 ou 4x3._

Chaque fonctions, à part celles où cela ne ferait pas de sens (fonctions utilitaires ou post-condition de getter), possèdent une "pré" et "post" condition.



#### Environnement

L'invariant du type Envi est constitué de plusieurs invariants :
- que les coordonnées soient valides
- qu'il n'y ait qu'un seule mob infranchissable par case de la carte
- que les stats des mobs soient valides
- que tous les mobs soient identifiées de maniere unique



Chaque fonctions, à part celles où cela ne ferait pas de sens (fonctions utilitaires ou post-condition de getter), possèdent une "pré" et "post" condition.

#### Modele

L'invariant du modele vérifie les invariant de sa carte et de son environnement.
