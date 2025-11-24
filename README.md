# NormalityCheck - Application Shiny pour la Vérification de Normalité

<div align="center">

![Shiny](https://img.shields.io/badge/Shiny-1.8.0-blue.svg)
![R](https://img.shields.io/badge/R-4.3.0+-green.svg)
![License](https://img.shields.io/badge/License-MIT-yellow.svg)
![DOI](https://zenodo.org/badge/1103268852.svg)

**Application complète d'analyse de normalité statistique pour les sciences du sport**

</div>

## 📊 Description

NormalityCheck est une application Shiny complète permettant l'analyse de normalité statistique par des méthodes graphiques et statistiques avancées. Spécialement conçue pour la recherche en sciences du sport (STAPS), elle intègre :

- ✅ **Tests statistiques multiples** : Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling, Lilliefors
- 📈 **Analyses graphiques avancées** : QQ-plots, histogrammes, densités comparées, PP-plots
- 🔄 **Transformations automatiques** : Box-Cox, logarithmique, racine carrée
- 🎯 **Validation Monte Carlo** complète des performances
- 📤 **Export professionnel** : rapports PDF, Excel, format APA

## 🚀 Installation et Utilisation

### Prérequis
- R version 4.3.0 ou supérieure
- Packages listés dans `requirements.txt`

### Installation rapide
```r
# Méthode 1 : Installation directe
install.packages(c("shiny", "shinyjs", "tidyverse", "nortest", "moments", "car", "MASS", "rmarkdown"))

# Lancer l'application
shiny::runApp("app.R")
