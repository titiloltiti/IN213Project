# IN213 - Quentin Biharé
## Language de modélisation de scène 3D pour algorithme RTX IN204

### Compiler le compilateur

Pour compiler le compilateur, exécutez la commande `` make `` dans le repertoire Projet/.

### Décrire sa scène et l'afficher 

Pour décrire votre scène, il vous faut éditer le fichier Projet/input.txt. Il vous faut nécessairement UNE camera et UNE source qui doit s'appeler mainsource.
Une fois votre scène décrite, vous pouvez l'afficher en éxécutant la commande `` make `` dans le repertoire Projet/IN204-ObjectOrientedRayTracing.

Remarque : L'axe des x est vertical vers le haut, L'axe des y horizontal vers la droite, l'axe des z pointe vers l'avant. Le fond est noir par défaut et situé à z=10000.

Quelques éléments de syntaxe (Un code d'exemple est disponible dans input.txt): 

``` let ident = Object in add ident; // Ajoute l'objet ident à la scène```
``` remove ident; // Enlève l'objet nommé ident de la scène```
``` point3D : (float,float,float) ```
``` couleur : (int,int,int) ```
``` Source(origin : point3D, direction: point3D (vecteur),color : couleur) // source de lumière```
``` Sphere(centre : point3D, radius : float,color : couleur, coef_reflexion : float (entre 0. et 1.)) // sphere```
``` Plane(origin : point3D, normale: point3D (vecteur),color : couleur,coef_reflexion : float) // plan```
``` Eye(origin : point3D, originScene : point3D ) in add ey; // camera, la distance entre les deux points correspondra alors à la distance entre la caméra et le début de la scène``` 

Evitez d'utiliser les mots sphere, plan, eye, source, background, mainsource, origin comme identificateur de vos objets.
