> # Créer un package Conda
>
> Ceci est un guide rapide pour créer une "recette" conda. Vous aurez besoin d'avoir installé conda sur votre machine. 
>
> ## 1. Pypi
>
> Vérifier si votre package est sur Pypi.
>
> ## 2. Créer la recette avec Pypi
>
> - Ouvrir le terminal
> - Installer grayskull ``conda install -c conda-forge grayskull``
> - Taper ``grayskull pypi NOM_PACKAGE``
>
> Votre recette est ainsi générée vous pouvez la trouver dans vos dossiers. 
> > <details-title>Vérifications</details-title>
> > Il faut vérifier (à la fin) que le `recipe maintainers` contient bien votre identifiant github et la license renseigner est bien la bonne.
> {: .details}
## 2. Créer la recette sans Pypi (à la main)

Récupérer le template meta.yaml et le placer dans un nouveau dossier (nommer ce dossier avec le nom du package).

Avec le template :
- Remplir la 1ère ligne avec le nom de package entre les quotes
- Remplir la 2ème ligne avec la version du package entre les quotes
- Remplir la 9ème ligne :
	- Aller sur le github/gitlab de développement du package.
	- Aller dans tag ou release copier le tar.gz et coller le dans url: et remplacer dans cette url le numéro de verion par `{{ version }}`
- Remplir la 10ème ligne :
	- Ouvrir le terminal 
	- Installer openssl : `conda install openssl -c conda-forge`
	- Taper `curl sL tar.gz | openssl sha256`
- Remplir la 15ème ligne : si votre code est en python uniquement (avec la même indentation que la ligne 14) écrire `noarch: python`
- Remplir les requirements :
	- Si votre package est un noarch python vous pouvez écrire la version python voulue (sinon il faut enlever les versions)
	- Vérifier que les dépendances du package sont elles-même sur Conda
	- Ajouter toutes les dépendances de développement dans host et de run dans run.
	Exemple :
		- python >=3.8
		- package-dependance >=0.0.0,<1.0.0 (ne pas écrire avec des majuscules et généralment ce sont des "-" et pas "_")
- Remplir le test : quand le package n'est pas sur Pypi généralement le plus simple est d'utiliser pytest (dans requires)
	- source_files : mettre le nom du dossier de tests souvent "tests"
	- imports : mettre le nom du dossier où se trouve les main scripts (souvent le nom du package) 
- Remplir le about : 
	- home : mettre l'url du git de développement
	- license : mettre le SPDX correspondant de la license (liste des spdx <https://spdx.org/licenses/>)
	- license_file : mettre le nom (ou le chemin) du fichier de license présent dans le git de développement (souvent LICENSE ou LICENSE.txt)
	- summary : description de ce que fait le package
- Remplir recipe-maintainers : mettre votre identifiant github 

## 3. Mettre la recette sur conda

- Aller sur <https://github.com/conda-forge/staged-recipes>
- Fork le repository
- Créer une nouvelle branche avec le nom de votre package
- Dans le dossier recipe (attention de ne pas être dans example) cliquer sur upload files
- Ajouter le DOSSIER (nom-package/meta.yaml)
- Commit et Pull request mettre un titre compréhensible "Add my_package recipe"
- Vérifier que les checks se passent bien. Erreurs fréquentes : 
	- sha256 pas bon, copier coller le bon du message d'erreur 
	- Vérifier que les noms de dépendences sont bien ecrits et les versions disponibles sur conda (il faut parfois écrire si disponible sur conda : python-nomdépendence)
	- Si vous avez besoin d'aide vous pouvez poser vos question sur <https://gitter.im/conda-forge/conda-forge.github.io> (à privilégier) ou pinger @conda-forge/help-python et poser vos questions.
	- Quand la recette laisser un commentaire : "@conda-forge/help-python my recipe is ready !" et attendre la review, d'éventuels commentaires et/ou que la recette soient merger.

## 4. Faire une mise à jour de package

- Aller sur le feedstock conda (la ou se trouve la recette actuelle) de la recette à mettre à jour. Vous pouvez aller sur <https://conda-forge.org/feedstock-outputs/>, rechercher votre package et cliquer sur nompackage-feedstock.
- Fork le repository
- Créer une nouvelle branche
- Faites vos modifications sur le meta.yaml
- Si c'est une nouvelle version mettre number: 0 sinon ajouter 1 au number déjà inscrit
- Commit et pull request 
- Quand votre mise à jour a passé tous les checks laisser en commentaire "@conda-forge-admin, please rerender"

## 5. Liens utiles
- <https://conda-forge.org/#contribute>
- <https://conda-forge.org/docs/maintainer/adding_pkgs.html#the-recipe-meta-yaml>

