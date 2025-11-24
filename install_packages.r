#!/usr/bin/env Rscript
# Script d'installation automatique pour NormalityCheck

cat("=== INSTALLATION DE NORMALITYCHECK ===\n")

# Liste des packages requis
required_packages <- c(
  "shiny", "shinyjs", "DT", "tidyverse", "ggplot2", "ggpubr",
  "psych", "nortest", "moments", "car", "ggforce", "gridExtra",
  "openxlsx", "MASS", "rmarkdown", "pROC", "microbenchmark"
)

# Vérification et installation
cat("Vérification des packages...\n")
for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installation de:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat("✓", pkg, "est déjà installé\n")
  }
}

# Vérification finale
cat("\n=== VÉRIFICATION FINALE ===\n")
missing_packages <- c()
for(pkg in required_packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    missing_packages <- c(missing_packages, pkg)
  }
}

if(length(missing_packages) == 0) {
  cat("✅ Tous les packages sont installés avec succès!\n")
  cat("Vous pouvez maintenant lancer l'application avec:\n")
  cat('shiny::runApp("app.R")\n')
} else {
  cat("❌ Packages manquants:", paste(missing_packages, collapse = ", "), "\n")
  cat("Veuillez les installer manuellement.\n")
}