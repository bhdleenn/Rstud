# Marché automobile

Tableau de bord Shiny pour l’épreuve d’IA générative et d’automatisation en R. Il explore la base fournie de 600 entreprises et classe les partenaires potentiels. Le thème **Financial Times / Executive** associe une typographie éditoriale à une palette bleu nuit et gris perle.

**Réalisé par Lentier au carré**

## Lancer le tableau de bord

1. Décompressez l’archive et ouvrez **`Rstud.Rproj`** dans RStudio.
2. Installez les dépendances une fois depuis la console RStudio.

   ```r
   install.packages(c("shiny", "jsonlite", "readxl"))
   ```

3. Ouvrez **`app.R`** et cliquez sur **Run App**. Vous pouvez aussi lancer `shiny::runApp(".")` depuis le projet.

Une connexion Internet est nécessaire pour charger Plotly et les fonds de carte. Dans RStudio, vous pouvez choisir **Open in Browser** pour ouvrir le tableau de bord dans votre navigateur.

## Les quatre missions

| Mission | Dans le tableau de bord |
| --- | --- |
| 1. Visualisations | Globe interactif, diagramme en violon, radar, bulles en 3D, heatmap des motorisations et densités animées sur cinq secondes. |
| 2. Tableau de bord | L’onglet **Visualisations** regroupe les graphiques, le filtre régional et l’export PNG. Le bouton **Importer une base** accepte les formats RDS, Excel (`.xlsx`) et CSV avec les mêmes colonnes que la base fournie. |
| 3. Partenaires | Jusqu’à dix entreprises en Asie et cinq alternatives en Amérique du Nord, toutes admissibles selon le critère de conduite autonome. Cliquez sur une ligne pour afficher le contact et le site. |
| 4. Route animée | Les partenaires asiatiques rejoignent leur position en cinq secondes. Les meilleurs scores sont les plus à droite et le bouton **Rejouer** relance l’animation. |

Sur le globe et les fonds détaillés, cliquez sur un point pour consulter les entreprises de la ville. Les fonds détaillés sont fournis par [OpenFreeMap et OpenStreetMap](https://openfreemap.org/) et [Esri World Imagery](https://www.arcgis.com/home/item.html?id=10df2279f9684e4a9f6a7f08febac2a9).

### Calcul du score partenaire

Après filtrage sur la région et la conduite autonome, les cinq critères sont normalisés entre 0 et 100 sur l’ensemble des entreprises admissibles des deux régions. Le score est la somme pondérée des notes suivantes.

| Critère | Poids |
| --- | ---: |
| Budget de recherche et développement | 30 % |
| Sécurité Euro NCAP | 25 % |
| Indice de durabilité | 20 % |
| Nombre de modèles électriques | 15 % |
| Réseau de concessionnaires | 10 % |

## Organisation de l’archive

| Emplacement | Rôle |
| --- | --- |
| **`app.R`** | Point d’entrée Shiny et interface du tableau de bord |
| `Rstud.Rproj` | Projet RStudio |
| `scripts/` | Quatre scripts pour l’import, la carte, les graphiques et le classement |
| `data/` | Base RDS fournie pour l’épreuve |
| `www/` | Images utilisées dans l’application |

L’archive contient **huit fichiers hors images**, dont cinq scripts R. Conservez l’arborescence en joignant l’archive à l’e-mail de remise.
