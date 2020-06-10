# PAF-Dungeon

<img src="https://i.gyazo.com/a126600fe8fd39794108a4970566a47d.png" alt="drawing" width="400"/><img src="https://i.gyazo.com/04c4539da391618e529ced715281f602.png" alt="drawing" width="300"/>

## Projet de PAF (2019 - 2020) - Dungeon Crawling


### <u>**Manuel d'utilisation du projet**</u>


#### Execution

Version du compilateur ghc en lts-12.26.\
Se placer dans la racine du projet et taper : `stack run`

#### Contrôles

Le déplacement se fait via les touches suivantes :
- Z : haut
- Q : gauche
- S : bas
- D : droite
- SPACE : attaque (8-adjacent)
- F : utiliser les portes (4-adjacent)
- ESC : quitter le jeu

**Attention** : pour que l’input soit correctement pris en compte, n'appuyez pas sur les touches trop brièvement.

Pour récupérer un coffre, il faut passer dessus. Il vous donnera une clé permettant d’ouvrir les portes fermées, ce qui est indispensable pour sortir.

Si vous restez éloigné des monstres, ils vaquent à leur occupations en se déplaçant aléatoirement.
Si vous vous approchez trop d’eux, il ont beaucoup plus de chance de tenter de vous attaquer.
Neutralisez-les rapidement avant qu’ils ne vous tuent (attention ils sont assez puissants).

### <u>**Tests**</u>
Se placer dans la racine du projet et taper `stack test` pour executer les invariants.\
Les tests basés sur les propriétés (property-based testing) seront executés sur un set de données prédéfini.