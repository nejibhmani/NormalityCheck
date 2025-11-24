# app_verification_normalite_COMPLET.R
library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(psych)
library(nortest)
library(moments)
library(car)
library(ggforce)
library(gridExtra)
library(openxlsx)
library(MASS)
library(rmarkdown)

# Vérification des packages
required_packages <- c("shiny", "shinyjs", "DT", "tidyverse", "ggplot2", "ggpubr", 
                       "psych", "nortest", "moments", "car", "ggforce", "gridExtra", 
                       "openxlsx", "MASS", "rmarkdown")

lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
})

# Configuration
options(shiny.maxRequestSize = 30*1024^2) # Limite à 30MB

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css"),
    tags$style(HTML("
      .main-header {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
        padding: 2rem 0;
        margin-bottom: 2rem;
        border-radius: 0 0 20px 20px;
      }
      .card {
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        border: none;
      }
      .result-card {
        background: linear-gradient(135deg, #e3f2fd, #bbdefb);
        border-left: 5px solid #1976d2;
      }
      .warning-card {
        background: linear-gradient(135deg, #ffecb3, #ffd54f);
        border-left: 5px solid #ffa000;
      }
      .success-card {
        background: linear-gradient(135deg, #e8f5e8, #c8e6c9);
        border-left: 5px solid #27ae60;
      }
      .btn-analyse {
        background: linear-gradient(135deg, #3498db, #2980b9);
        color: white;
        border: none;
        padding: 12px 30px;
        font-weight: 600;
        border-radius: 8px;
      }
      .normalite-info {
        background: linear-gradient(135deg, #3498db, #2980b9);
        color: white;
        padding: 15px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .test-card {
        background: linear-gradient(135deg, #f3e5f5, #e1bee7);
        border-left: 5px solid #8e24aa;
      }
      .distribution-card {
        background: linear-gradient(135deg, #fff3e0, #ffe0b2);
        border-left: 5px solid #f57c00;
      }
      .statistics-card {
        background: linear-gradient(135deg, #e8f5e9, #c8e6c9);
        border-left: 5px solid #4caf50;
      }
      .transformation-card {
        background: linear-gradient(135deg, #e8f4fd, #d4e6f1);
        border-left: 5px solid #3498db;
      }
    "))
  ),
  
  # Header principal
  div(class = "main-header text-center",
      h1("📊 Vérification de la Normalité", style = "font-weight: 700;"),
      h4("Tests graphiques et statistiques pour évaluer la distribution normale - STAPS", style = "opacity: 0.9;"),
      p("📧 Contact : nejib.hmani@issepsf.u-sfax.tn", 
        style = "margin-top: 10px; font-size: 14px;")
  ),
  
  # Contenu principal
  div(class = "container-fluid",
      conditionalPanel(
        condition = "!output.fileUploaded",
        div(class = "alert alert-info",
            h4("📁 Instructions d'importation"),
            p("Veuillez importer un fichier Excel (.xlsx, .xls) ou CSV pour commencer l'analyse."),
            tags$ul(
              tags$li("Assurez-vous que la première ligne contient les noms des variables"),
              tags$li("Les données doivent être numériques pour l'analyse de normalité"),
              tags$li("Supprimez les lignes vides éventuelles")
            )
        )
      ),
      
      div(class = "normalite-info",
          h5("ℹ️ À propos de la vérification de normalité"),
          p("La normalité est une hypothèse fondamentale pour de nombreux tests statistiques paramétriques. Cette application permet d'évaluer graphiquement et statistiquement la distribution de vos données."),
          tags$ul(
            tags$li(strong("Tests statistiques:"), "Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling"),
            tags$li(strong("Graphiques:"), "QQ-Plot, Histogramme, Densité, Boxplot, PP-Plot"),
            tags$li(strong("Transformations:"), "Box-Cox, Logarithmique, Racine carrée")
          )
      ),
      
      sidebarLayout(
        # Sidebar
        sidebarPanel(width = 3,
                     div(class = "card p-3",
                         h4("📁 Import des données", style = "color: #3498db;"),
                         
                         fileInput("file", "Fichier Excel ou CSV",
                                   accept = c(".xlsx", ".xls", ".csv"),
                                   buttonLabel = "Parcourir..."),
                         
                         conditionalPanel(
                           condition = "output.fileUploaded",
                           hr(),
                           h5("Sélection des variables:"),
                           uiOutput("variable_selectors")
                         )
                     ),
                     
                     conditionalPanel(
                       condition = "output.fileUploaded",
                       div(class = "card p-3",
                           h4("⚙️ Paramètres d'analyse", style = "color: #3498db;"),
                           
                           numericInput("alpha_level", "Niveau alpha:", 
                                        value = 0.05, min = 0.01, max = 0.10, step = 0.01),
                           
                           checkboxGroupInput("normality_tests", "Tests statistiques:",
                                              choices = c("Shapiro-Wilk" = "shapiro",
                                                          "Kolmogorov-Smirnov" = "ks",
                                                          "Anderson-Darling" = "ad",
                                                          "Lilliefors" = "lillie"),
                                              selected = c("shapiro", "ad")),
                           
                           checkboxGroupInput("graphs", "Graphiques:",
                                              choices = c("QQ-Plot" = "qqplot",
                                                          "Histogramme + Densité" = "histogram",
                                                          "Boxplot" = "boxplot",
                                                          "Densité comparée" = "density",
                                                          "PP-Plot" = "ppplot"),
                                              selected = c("qqplot", "histogram", "boxplot", "density", "ppplot")),
                           
                           checkboxInput("outlier_detection", "Détection des valeurs extrêmes", TRUE),
                           checkboxInput("transformations", "Analyse des transformations", TRUE),
                           checkboxInput("descriptives", "Statistiques descriptives", TRUE),
                           
                           actionButton("run_analysis", "🚀 Lancer l'analyse", 
                                        class = "btn-analyse w-100")
                       )
                     )
        ),
        
        # Main Panel
        mainPanel(width = 9,
                  tabsetPanel(
                    # Onglet Vue d'ensemble
                    tabPanel("📋 Vue d'ensemble",
                             conditionalPanel(
                               condition = "output.fileUploaded",
                               div(class = "card",
                                   h4("Aperçu des données", style = "color: #3498db;"),
                                   DTOutput("data_preview"),
                                   br(),
                                   fluidRow(
                                     column(6, 
                                            div(class = "card",
                                                verbatimTextOutput("data_info")
                                            )
                                     ),
                                     column(6,
                                            div(class = "card",
                                                plotOutput("variables_overview", height = "250px")
                                            )
                                     )
                                   )
                               )
                             )
                    ),
                    
                    # Onglet Tests de Normalité
                    tabPanel("📊 Tests Statistiques",
                             conditionalPanel(
                               condition = "output.analysisDone",
                               h3("Résultats des Tests de Normalité", 
                                  style = "color: #2c3e50; margin-bottom: 25px;"),
                               
                               # Résultats des tests
                               div(class = "result-card card p-4",
                                   h4("📈 Tests de normalité", style = "color: #1976d2;"),
                                   DTOutput("normality_tests_results")
                               ),
                               
                               # Interprétation globale
                               div(class = "card p-4",
                                   h4("🎯 Synthèse et interprétation", style = "color: #3498db;"),
                                   uiOutput("global_interpretation")
                               ),
                               
                               # Statistiques descriptives
                               conditionalPanel(
                                 condition = "input.descriptives",
                                 div(class = "statistics-card card p-4",
                                     h4("📋 Statistiques descriptives", style = "color: #4caf50;"),
                                     DTOutput("descriptive_stats")
                                 )
                               ),
                               
                               # Indices de forme
                               div(class = "test-card card p-4",
                                   h4("📊 Indices de forme de distribution", style = "color: #8e24aa;"),
                                   DTOutput("shape_stats")
                               )
                             )
                    ),
                    
                    # Onglet Analyse Graphique
                    tabPanel("📈 Analyse Graphique",
                             conditionalPanel(
                               condition = "output.analysisDone",
                               h3("Analyse Graphique de la Normalité", 
                                  style = "color: #2c3e50; margin-bottom: 25px;"),
                               
                               conditionalPanel(
                                 condition = "input.graphs.includes('qqplot')",
                                 div(class = "card",
                                     h4("QQ-Plot (Quantile-Quantile)", style = "color: #3498db;"),
                                     plotOutput("qq_plot", height = "500px")
                                 )
                               ),
                               
                               conditionalPanel(
                                 condition = "input.graphs.includes('histogram')",
                                 div(class = "card",
                                     h4("Histogramme avec densité", style = "color: #e74c3c;"),
                                     plotOutput("histogram_plot", height = "500px")
                                 )
                               ),
                               
                               conditionalPanel(
                                 condition = "input.graphs.includes('boxplot')",
                                 div(class = "card",
                                     h4("Boxplot et valeurs extrêmes", style = "color: #27ae60;"),
                                     plotOutput("boxplot", height = "400px")
                                 )
                               ),
                               
                               conditionalPanel(
                                 condition = "input.graphs.includes('density')",
                                 div(class = "card",
                                     h4("Comparaison avec distribution normale", style = "color: #9b59b6;"),
                                     plotOutput("density_comparison_plot", height = "400px")
                                 )
                               ),
                               
                               conditionalPanel(
                                 condition = "input.graphs.includes('ppplot')",
                                 div(class = "card",
                                     h4("PP-Plot (Probability-Probability)", style = "color: #f39c12;"),
                                     plotOutput("pp_plot", height = "400px")
                                 )
                               )
                             )
                    ),
                    
                    # Onglet Transformations
                    tabPanel("🔄 Transformations",
                             conditionalPanel(
                               condition = "output.analysisDone && input.transformations",
                               h3("Transformations pour Améliorer la Normalité", 
                                  style = "color: #2c3e50; margin-bottom: 25px;"),
                               
                               div(class = "transformation-card card p-4",
                                   h4("💡 Résultats des transformations Box-Cox", style = "color: #3498db;"),
                                   DTOutput("boxcox_results")
                               ),
                               
                               fluidRow(
                                 column(6,
                                        div(class = "card",
                                            h4("📈 Avant/Après transformation", style = "color: #e74c3c;"),
                                            plotOutput("transformation_comparison", height = "400px")
                                        )
                                 ),
                                 column(6,
                                        div(class = "card",
                                            h4("📊 QQ-Plot après transformation", style = "color: #27ae60;"),
                                            plotOutput("transformed_qqplot", height = "400px")
                                        )
                                 )
                               ),
                               
                               div(class = "warning-card card p-4",
                                   h4("🎯 Recommandations de transformations", style = "color: #ffa000;"),
                                   uiOutput("transformation_recommendations")
                               )
                             )
                    ),
                    
                    # Onglet Export
                    tabPanel("📤 Export",
                             conditionalPanel(
                               condition = "output.analysisDone",
                               div(class = "card p-4",
                                   h4("📄 Export des résultats", style = "color: #3498db;"),
                                   p("Exportez les résultats complets de votre analyse de normalité."),
                                   
                                   fluidRow(
                                     column(6,
                                            downloadButton("export_results_excel", "💾 Résultats Excel", 
                                                           class = "btn-success w-100 mb-2"),
                                            downloadButton("export_results_csv", "📊 Données analysées CSV", 
                                                           class = "btn-info w-100 mb-2")
                                     ),
                                     column(6,
                                            downloadButton("export_report_pdf", "📋 Rapport PDF", 
                                                           class = "btn-warning w-100 mb-2"),
                                            downloadButton("export_apa_txt", "🎓 Format APA", 
                                                           class = "btn-primary w-100 mb-2")
                                     )
                                   )
                               ),
                               
                               div(class = "card p-4",
                                   h4("🖼️ Export des graphiques", style = "color: #27ae60;"),
                                   p("Exportez les graphiques en haute résolution pour publication."),
                                   
                                   fluidRow(
                                     column(4,
                                            downloadButton("export_qqplot_png", "📈 QQ-Plot PNG", 
                                                           class = "btn-outline-primary w-100 mb-2"),
                                            downloadButton("export_histogram_png", "📊 Histogramme PNG", 
                                                           class = "btn-outline-success w-100 mb-2")
                                     ),
                                     column(4,
                                            downloadButton("export_boxplot_png", "📦 Boxplot PNG", 
                                                           class = "btn-outline-info w-100 mb-2"),
                                            downloadButton("export_density_png", "📈 Densité PNG", 
                                                           class = "btn-outline-warning w-100 mb-2")
                                     ),
                                     column(4,
                                            downloadButton("export_ppplot_png", "📊 PP-Plot PNG", 
                                                           class = "btn-outline-danger w-100 mb-2"),
                                            downloadButton("export_all_plots_zip", "🎨 Tous les graphiques ZIP", 
                                                           class = "btn-danger w-100 mb-2")
                                     )
                                   )
                               ),
                               
                               div(class = "card p-4",
                                   h4("📋 Résumé des résultats", style = "color: #e74c3c;"),
                                   verbatimTextOutput("export_summary"),
                                   downloadButton("export_summary_txt", "💾 Sauvegarder le résumé", 
                                                  class = "btn-secondary w-100 mt-3")
                               )
                             )
                    )
                  )
        )
      )
  )
)

# Serveur - Première Partie
server <- function(input, output, session) {
  
  # Réactive values
  rv <- reactiveValues(
    data = NULL,
    analysis_done = FALSE,
    normality_results = NULL,
    selected_vars = NULL,
    descriptive_stats = NULL,
    boxcox_results = NULL
  )
  
  # Lecture des données
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      ext <- tools::file_ext(input$file$name)
      
      # Validation du format
      if (!ext %in% c("csv", "xlsx", "xls")) {
        showNotification("❌ Format de fichier non supporté. Utilisez CSV ou Excel.", 
                         type = "error", duration = 5)
        return()
      }
      
      # Vérification de la taille
      if (input$file$size > 10*1024^2) {
        showNotification("⚠️ Fichier volumineux (>10MB). L'analyse peut être lente.", 
                         type = "warning")
      }
      
      # Installation de readxl si nécessaire
      if (ext %in% c("xlsx", "xls") && !requireNamespace("readxl", quietly = TRUE)) {
        install.packages("readxl")
      }
      
      df <- if (ext == "csv") {
        read.csv(input$file$datapath, fileEncoding = "UTF-8")
      } else {
        readxl::read_excel(input$file$datapath)
      }
      
      rv$data <- as.data.frame(df)
      showNotification("✅ Données importées avec succès!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'importation:", e$message), 
                       type = "error", duration = 10)
    })
  })
  
  # Sélecteurs de variables
  output$variable_selectors <- renderUI({
    req(rv$data)
    
    numeric_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
    
    if (length(numeric_vars) == 0) {
      return(div(class = "alert alert-warning", 
                 "Aucune variable numérique détectée dans le jeu de données"))
    }
    
    tagList(
      checkboxGroupInput("selected_variables", "Sélectionnez les variables à analyser:",
                         choices = numeric_vars,
                         selected = if(length(numeric_vars) >= 3) numeric_vars[1:3] else numeric_vars),
      helpText("Sélectionnez une ou plusieurs variables numériques")
    )
  })
  
  # Output: fichier uploadé
  output$fileUploaded <- reactive({
    return(!is.null(rv$data))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Aperçu des données
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(head(rv$data, 10),
              options = list(dom = 't', scrollX = TRUE),
              class = 'table-striped table-hover')
  })
  
  output$data_info <- renderText({
    req(rv$data)
    numeric_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
    selected_count <- if(!is.null(input$selected_variables)) length(input$selected_variables) else 0
    
    paste(
      "Dimensions:", nrow(rv$data), "observations ×", ncol(rv$data), "variables\n",
      "Variables numériques:", length(numeric_vars), "\n",
      "Variables sélectionnées:", selected_count, "\n",
      "Valeurs manquantes:", sum(is.na(rv$data))
    )
  })
  
  # Vue d'ensemble des variables
  output$variables_overview <- renderPlot({
    req(rv$data, input$selected_variables)
    
    if (length(input$selected_variables) < 1) return()
    
    plot_vars <- input$selected_variables[1:min(4, length(input$selected_variables))]
    
    tryCatch({
      # Utilisation de base R pour la sélection
      df_selected <- rv$data[, plot_vars, drop = FALSE]
      df_long <- df_selected %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valeur")
      
      ggplot(df_long, aes(x = Valeur, fill = Variable)) +
        geom_histogram(alpha = 0.6, bins = 15) +
        facet_wrap(~ Variable, scales = "free") +
        labs(title = "Distribution des variables sélectionnées",
             x = "Valeur", y = "Fréquence") +
        theme_minimal() +
        theme(legend.position = "none")
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Erreur dans la génération du graphique", 
                 size = 6) + 
        theme_void()
    })
  })
  # Fonction pour les tests de normalité
  perform_normality_tests <- function(data, tests, alpha) {
    results <- list()
    
    if ("shapiro" %in% tests && length(data) <= 5000) {
      results$Shapiro_Wilk <- shapiro.test(data)
    } else if ("shapiro" %in% tests && length(data) > 5000) {
      results$Shapiro_Wilk <- list(
        statistic = NA, 
        p.value = NA, 
        note = "Non disponible pour n > 5000"
      )
    }
    
    if ("ks" %in% tests) {
      results$Kolmogorov_Smirnov <- tryCatch({
        ks.test(data, "pnorm", mean = mean(data), sd = sd(data))
      }, error = function(e) {
        list(statistic = NA, p.value = NA, note = "Erreur dans le test KS")
      })
    }
    
    if ("ad" %in% tests) {
      results$Anderson_Darling <- tryCatch({
        nortest::ad.test(data)
      }, error = function(e) {
        list(statistic = NA, p.value = NA, note = "Erreur dans le test AD")
      })
    }
    
    if ("lillie" %in% tests) {
      results$Lilliefors <- tryCatch({
        nortest::lillie.test(data)
      }, error = function(e) {
        list(statistic = NA, p.value = NA, note = "Erreur dans le test Lilliefors")
      })
    }
    
    return(results)
  }
  
  # Analyse principale
  observeEvent(input$run_analysis, {
    req(rv$data, input$selected_variables)
    
    if (length(input$selected_variables) == 0) {
      showNotification("❌ Sélectionnez au moins une variable à analyser", type = "error")
      return()
    }
    
    showNotification("🚀 Lancement de l'analyse de normalité...", type = "message")
    
    tryCatch({
      results_list <- list()
      desc_stats_list <- list()
      boxcox_list <- list()
      
      for (var in input$selected_variables) {
        if (!var %in% names(rv$data)) {
          showNotification(paste("❌ Variable", var, "non trouvée"), type = "error")
          next
        }
        
        var_data <- na.omit(rv$data[[var]])
        
        if (length(var_data) < 3) {
          showNotification(paste("❌ Variable", var, "ignorée : trop peu de données (n =", length(var_data), ")"), 
                           type = "warning")
          next
        }
        
        if (length(var_data) > 10000) {
          showNotification(paste("⚠️ Variable", var, ": échantillon très large (n =", length(var_data), ")"), 
                           type = "warning")
        }
        
        # Tests de normalité
        test_results <- perform_normality_tests(var_data, input$normality_tests, input$alpha_level)
        
        # Transformations Box-Cox
        optimal_lambda <- NA
        if (input$transformations && all(var_data > 0)) {
          bc <- suppressWarnings(
            try(MASS::boxcox(var_data ~ 1, lambda = seq(-2, 2, 0.1), plotit = FALSE), silent = TRUE)
          )
          if (!inherits(bc, "try-error")) {
            optimal_lambda <- bc$x[which.max(bc$y)]
            boxcox_list[[var]] <- round(optimal_lambda, 3)
            
            # Appliquer la transformation
            if (abs(optimal_lambda) < 0.001) {
              transformed_data <- log(var_data)
            } else {
              transformed_data <- (var_data^optimal_lambda - 1) / optimal_lambda
            }
            
            # Test de normalité sur les données transformées
            if (length(transformed_data) <= 5000) {
              test_results$Shapiro_Wilk_Transformed <- shapiro.test(transformed_data)
            }
          }
        }
        
        # Statistiques descriptives
        skewness_val <- suppressWarnings(moments::skewness(var_data))
        kurtosis_val <- suppressWarnings(moments::kurtosis(var_data))
        
        desc_stats <- data.frame(
          Variable = var,
          n = length(var_data),
          Mean = mean(var_data),
          Median = median(var_data),
          SD = sd(var_data),
          Skewness = skewness_val,
          Kurtosis = kurtosis_val,
          Min = min(var_data),
          Max = max(var_data),
          Q1 = quantile(var_data, 0.25),
          Q3 = quantile(var_data, 0.75),
          stringsAsFactors = FALSE
        )
        
        results_list[[var]] <- list(
          tests = test_results,
          data = var_data,
          stats = desc_stats,
          boxcox_lambda = optimal_lambda
        )
        
        desc_stats_list[[var]] <- desc_stats
      }
      
      if (length(results_list) == 0) {
        showNotification("❌ Aucune variable valide à analyser", type = "error")
        return()
      }
      
      rv$normality_results <- results_list
      rv$descriptive_stats <- desc_stats_list
      rv$boxcox_results <- boxcox_list
      rv$analysis_done <- TRUE
      rv$selected_vars <- input$selected_variables
      
      showNotification("✅ Analyse de normalité terminée avec succès!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Erreur lors de l'analyse:", e$message), type = "error", duration = 10)
    })
  })
  
  # Output: analyse done
  output$analysisDone <- reactive({
    return(rv$analysis_done)
  })
  outputOptions(output, "analysisDone", suspendWhenHidden = FALSE)
  
  # Résultats des tests de normalité
  output$normality_tests_results <- renderDT({
    req(rv$normality_results)
    
    tryCatch({
      tests_list <- list()
      
      for (var in names(rv$normality_results)) {
        tests <- rv$normality_results[[var]]$tests
        
        for (test_name in names(tests)) {
          test <- tests[[test_name]]
          p_value <- if(!is.null(test$p.value)) test$p.value else NA
          statistic <- if(!is.null(test$statistic)) test$statistic else NA
          
          # Gérer les tests avec notes
          if (!is.null(test$note)) {
            normalite <- test$note
          } else {
            normalite <- ifelse(is.na(p_value), "❌ Erreur",
                                ifelse(p_value > input$alpha_level, "✅ Normale", "❌ Non normale"))
          }
          
          test_row <- data.frame(
            Variable = var,
            Test = test_name,
            Statistique = ifelse(is.na(statistic), "N/A", round(statistic, 4)),
            `Valeur p` = ifelse(is.na(p_value), "N/A", 
                                ifelse(p_value < 0.001, "< 0.001", round(p_value, 4))),
            Normalité = normalite,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          tests_list[[length(tests_list) + 1]] <- test_row
        }
      }
      
      results_df <- do.call(rbind, tests_list)
      
      datatable(results_df,
                options = list(dom = 't', pageLength = 10, scrollX = TRUE),
                class = 'table-striped table-hover',
                caption = "Résultats des tests de normalité")
    }, error = function(e) {
      datatable(data.frame(Erreur = "Impossible d'afficher les résultats"))
    })
  })
  
  # Résultats Box-Cox
  output$boxcox_results <- renderDT({
    req(rv$normality_results, input$transformations)
    
    tryCatch({
      boxcox_list <- list()
      
      for (var in names(rv$normality_results)) {
        lambda <- rv$normality_results[[var]]$boxcox_lambda
        tests <- rv$normality_results[[var]]$tests
        
        if (!is.na(lambda)) {
          p_value_original <- if("Shapiro_Wilk" %in% names(tests)) {
            if (!is.null(tests$Shapiro_Wilk$p.value)) tests$Shapiro_Wilk$p.value else NA
          } else NA
          
          p_value_transformed <- if("Shapiro_Wilk_Transformed" %in% names(tests)) {
            tests$Shapiro_Wilk_Transformed$p.value 
          } else NA
          
          boxcox_list[[var]] <- data.frame(
            Variable = var,
            Lambda_optimal = as.character(round(lambda, 3)),
            p_value_original = ifelse(is.na(p_value_original), "N/A", 
                                      ifelse(p_value_original < 0.001, "< 0.001", 
                                             as.character(round(p_value_original, 4)))),
            p_value_transformé = ifelse(is.na(p_value_transformed), "N/A",
                                        ifelse(p_value_transformed < 0.001, "< 0.001", 
                                               as.character(round(p_value_transformed, 4)))),
            Amélioration = ifelse(!is.na(p_value_original) && !is.na(p_value_transformed),
                                  ifelse(p_value_transformed > p_value_original, "✅ Amélioré", "➡️ Stable"),
                                  "N/A"),
            stringsAsFactors = FALSE
          )
        } else {
          boxcox_list[[var]] <- data.frame(
            Variable = var,
            Lambda_optimal = "N/A",
            p_value_original = "N/A",
            p_value_transformé = "N/A",
            Amélioration = "❌ Données négatives ou nulles",
            stringsAsFactors = FALSE
          )
        }
      }
      
      boxcox_df <- do.call(rbind, boxcox_list)
      names(boxcox_df) <- c("Variable", "Lambda optimal", "p-value original", "p-value transformé", "Amélioration")
      
      datatable(boxcox_df,
                options = list(dom = 't', scrollX = TRUE),
                class = 'table-striped table-hover',
                caption = "Résultats des transformations Box-Cox")
    }, error = function(e) {
      datatable(data.frame(Erreur = "Impossible d'afficher les résultats Box-Cox"))
    })
  })
  
  # Graphiques multi-variables
  output$qq_plot <- renderPlot({
    req(rv$normality_results, input$selected_variables)
    
    tryCatch({
      plot_list <- list()
      vars_to_show <- head(input$selected_variables, 6) # Limiter à 6 variables
      
      for (var in vars_to_show) {
        if (var %in% names(rv$normality_results)) {
          var_data <- rv$normality_results[[var]]$data
          
          p <- ggplot(data.frame(x = var_data), aes(sample = x)) +
            stat_qq(color = "#3498db", alpha = 0.7) +
            stat_qq_line(color = "#e74c3c", linewidth = 1) +
            labs(title = var,
                 x = "Quantiles théoriques",
                 y = "Quantiles observés") +
            theme_minimal() +
            theme(plot.title = element_text(size = 10))
          
          plot_list[[var]] <- p
        }
      }
      
      if (length(plot_list) == 0) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune donnée disponible pour les graphiques", 
                   size = 6) + 
          theme_void()
      } else if (length(plot_list) == 1) {
        print(plot_list[[1]])
      } else {
        grid.arrange(grobs = plot_list, ncol = 2, 
                     top = "QQ-Plots des variables")
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les QQ-Plots:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  output$histogram_plot <- renderPlot({
    req(rv$normality_results, input$selected_variables)
    
    tryCatch({
      plot_list <- list()
      vars_to_show <- head(input$selected_variables, 6)
      
      for (var in vars_to_show) {
        if (var %in% names(rv$normality_results)) {
          var_data <- rv$normality_results[[var]]$data
          
          p <- ggplot(data.frame(x = var_data), aes(x = x)) +
            geom_histogram(aes(y = after_stat(density)), fill = "#3498db", alpha = 0.6, bins = 20) +
            geom_density(color = "#2c3e50", linewidth = 1) +
            stat_function(fun = dnorm, 
                          args = list(mean = mean(var_data), sd = sd(var_data)),
                          color = "#e74c3c", linewidth = 1, linetype = "dashed") +
            labs(title = var,
                 x = "Valeur", y = "Densité") +
            theme_minimal() +
            theme(plot.title = element_text(size = 10))
          
          plot_list[[var]] <- p
        }
      }
      
      if (length(plot_list) == 0) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune donnée disponible pour les graphiques", 
                   size = 6) + 
          theme_void()
      } else if (length(plot_list) == 1) {
        print(plot_list[[1]])
      } else {
        grid.arrange(grobs = plot_list, ncol = 2,
                     top = "Histogrammes avec densité normale")
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les histogrammes:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  # PP-Plot
  output$pp_plot <- renderPlot({
    req(rv$normality_results, input$selected_variables)
    
    tryCatch({
      plot_list <- list()
      vars_to_show <- head(input$selected_variables, 6)
      
      for (var in vars_to_show) {
        if (var %in% names(rv$normality_results)) {
          var_data <- rv$normality_results[[var]]$data
          n <- length(var_data)
          theoretical_probs <- pnorm(sort(var_data), mean = mean(var_data), sd = sd(var_data))
          empirical_probs <- (1:n) / n
          
          pp_data <- data.frame(
            Theoretical = theoretical_probs,
            Empirical = empirical_probs
          )
          
          p <- ggplot(pp_data, aes(x = Theoretical, y = Empirical)) +
            geom_point(color = "#3498db", alpha = 0.7) +
            geom_abline(slope = 1, intercept = 0, color = "#e74c3c", linewidth = 1) +
            labs(title = var,
                 x = "Probabilités théoriques",
                 y = "Probabilités empiriques") +
            theme_minimal() +
            theme(plot.title = element_text(size = 10))
          
          plot_list[[var]] <- p
        }
      }
      
      if (length(plot_list) == 0) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune donnée disponible pour les graphiques", 
                   size = 6) + 
          theme_void()
      } else if (length(plot_list) == 1) {
        print(plot_list[[1]])
      } else {
        grid.arrange(grobs = plot_list, ncol = 2,
                     top = "PP-Plots des variables")
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les PP-Plots:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  # Densité comparée
  output$density_comparison_plot <- renderPlot({
    req(rv$normality_results, input$selected_variables)
    
    tryCatch({
      plot_list <- list()
      vars_to_show <- head(input$selected_variables, 4) # Limiter pour performance
      
      for (var in vars_to_show) {
        if (var %in% names(rv$normality_results)) {
          var_data <- rv$normality_results[[var]]$data
          
          # Générer des données normales avec mêmes paramètres
          normal_data <- rnorm(length(var_data), mean = mean(var_data), sd = sd(var_data))
          
          comp_data <- data.frame(
            Type = rep(c("Observée", "Normale"), each = length(var_data)),
            Valeur = c(var_data, normal_data)
          )
          
          p <- ggplot(comp_data, aes(x = Valeur, fill = Type)) +
            geom_density(alpha = 0.5) +
            labs(title = var,
                 x = "Valeur", y = "Densité") +
            theme_minimal() +
            scale_fill_manual(values = c("Observée" = "#3498db", "Normale" = "#e74c3c")) +
            theme(plot.title = element_text(size = 10),
                  legend.position = "bottom")
          
          plot_list[[var]] <- p
        }
      }
      
      if (length(plot_list) == 0) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune donnée disponible pour les graphiques", 
                   size = 6) + 
          theme_void()
      } else if (length(plot_list) == 1) {
        print(plot_list[[1]])
      } else {
        grid.arrange(grobs = plot_list, ncol = 2,
                     top = "Comparaison avec distribution normale")
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les graphiques de densité:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  # Boxplot avec valeurs extrêmes
  output$boxplot <- renderPlot({
    req(rv$normality_results, input$selected_variables)
    
    tryCatch({
      plot_data <- list()
      
      for (var in names(rv$normality_results)) {
        plot_data[[var]] <- data.frame(
          Variable = var,
          Valeur = rv$normality_results[[var]]$data
        )
      }
      
      plot_df <- do.call(rbind, plot_data)
      
      ggplot(plot_df, aes(x = Variable, y = Valeur, fill = Variable)) +
        geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c", outlier.size = 2) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 2, fill = "white") +
        labs(title = "Boxplots des variables avec valeurs extrêmes",
             x = "Variables", y = "Valeurs") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les boxplots:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  # Graphiques pour les transformations
  output$transformation_comparison <- renderPlot({
    req(rv$normality_results, input$transformations)
    
    tryCatch({
      plot_list <- list()
      
      for (var in names(rv$normality_results)) {
        var_data <- rv$normality_results[[var]]$data
        lambda <- rv$normality_results[[var]]$boxcox_lambda
        
        if (!is.na(lambda) && all(var_data > 0)) {
          # Appliquer transformation Box-Cox
          if (abs(lambda) < 0.001) {
            transformed_data <- log(var_data)
            transformation_name <- "log(x)"
          } else {
            transformed_data <- (var_data^lambda - 1) / lambda
            transformation_name <- paste0("(x^", round(lambda, 2), " - 1)/", round(lambda, 2))
          }
          
          comp_data <- data.frame(
            Type = rep(c("Original", "Transformé"), each = length(var_data)),
            Valeur = c(var_data, transformed_data)
          )
          
          p <- ggplot(comp_data, aes(x = Valeur, fill = Type)) +
            geom_density(alpha = 0.6) +
            labs(title = paste(var, "-", transformation_name),
                 x = "Valeur", y = "Densité") +
            theme_minimal() +
            scale_fill_manual(values = c("Original" = "#e74c3c", "Transformé" = "#27ae60")) +
            theme(plot.title = element_text(size = 10),
                  legend.position = "bottom")
          
          plot_list[[var]] <- p
        }
      }
      
      if (length(plot_list) > 0) {
        if (length(plot_list) == 1) {
          print(plot_list[[1]])
        } else {
          grid.arrange(grobs = plot_list, ncol = 2,
                       top = "Comparaison avant/après transformation Box-Cox")
        }
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune transformation Box-Cox applicable\n(Données avec valeurs négatives ou nulles)", 
                   size = 5) + 
          theme_void()
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les graphiques de transformation:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  # QQ-Plot après transformation
  output$transformed_qqplot <- renderPlot({
    req(rv$normality_results, input$transformations)
    
    tryCatch({
      plot_list <- list()
      
      for (var in names(rv$normality_results)) {
        var_data <- rv$normality_results[[var]]$data
        lambda <- rv$normality_results[[var]]$boxcox_lambda
        
        if (!is.na(lambda) && all(var_data > 0)) {
          # Appliquer transformation Box-Cox
          if (abs(lambda) < 0.001) {
            transformed_data <- log(var_data)
            transformation_name <- "log(x)"
          } else {
            transformed_data <- (var_data^lambda - 1) / lambda
            transformation_name <- paste0("(x^", round(lambda, 2), " - 1)/", round(lambda, 2))
          }
          
          p <- ggplot(data.frame(x = transformed_data), aes(sample = x)) +
            stat_qq(color = "#27ae60", alpha = 0.7) +
            stat_qq_line(color = "#e74c3c", linewidth = 1) +
            labs(title = paste(var, "-", transformation_name),
                 x = "Quantiles théoriques",
                 y = "Quantiles observés") +
            theme_minimal() +
            theme(plot.title = element_text(size = 10))
          
          plot_list[[var]] <- p
        }
      }
      
      if (length(plot_list) > 0) {
        if (length(plot_list) == 1) {
          print(plot_list[[1]])
        } else {
          grid.arrange(grobs = plot_list, ncol = 2,
                       top = "QQ-Plots après transformation Box-Cox")
        }
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "Aucune transformation Box-Cox applicable\n(Données avec valeurs négatives ou nulles)", 
                   size = 5) + 
          theme_void()
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Erreur dans les QQ-Plots transformés:", e$message), 
                 size = 4) + 
        theme_void()
    })
  })
  
  # Statistiques descriptives
  output$descriptive_stats <- renderDT({
    req(rv$descriptive_stats)
    
    tryCatch({
      desc_df <- do.call(rbind, rv$descriptive_stats)
      desc_df$Variable <- NULL  # Supprimer la colonne Variable en double
      
      desc_df <- desc_df %>%
        mutate(across(where(is.numeric), ~ round(., 3)))
      
      datatable(desc_df,
                options = list(dom = 't', scrollX = TRUE),
                class = 'table-striped table-hover',
                caption = "Statistiques descriptives des variables")
    }, error = function(e) {
      datatable(data.frame(Erreur = "Impossible d'afficher les statistiques descriptives"))
    })
  })
  
  # Indices de forme
  output$shape_stats <- renderDT({
    req(rv$descriptive_stats)
    
    tryCatch({
      shape_df <- do.call(rbind, rv$descriptive_stats)
      
      # Sélection des colonnes avec base R
      shape_df <- shape_df[, c("Variable", "Skewness", "Kurtosis"), drop = FALSE]
      
      shape_df <- shape_df %>%
        mutate(
          `Interprétation Asymétrie` = case_when(
            abs(Skewness) < 0.5 ~ "Symétrique",
            Skewness >= 0.5 & Skewness < 1 ~ "Asymétrie positive modérée",
            Skewness >= 1 ~ "Asymétrie positive forte",
            Skewness <= -0.5 & Skewness > -1 ~ "Asymétrie négative modérée",
            Skewness <= -1 ~ "Asymétrie négative forte",
            TRUE ~ "Indéterminé"
          ),
          `Interprétation Aplatissement` = case_when(
            Kurtosis < 2.5 ~ "Platykurtique (aplatie)",
            Kurtosis >= 2.5 & Kurtosis <= 3.5 ~ "Mésokurtique (normale)",
            Kurtosis > 3.5 ~ "Leptokurtique (pointue)",
            TRUE ~ "Indéterminé"
          )
        ) %>%
        mutate(across(where(is.numeric), ~ round(., 3)))
      
      datatable(shape_df,
                options = list(dom = 't', scrollX = TRUE),
                class = 'table-striped table-hover',
                caption = "Indices de forme de distribution")
    }, error = function(e) {
      datatable(data.frame(Erreur = "Impossible d'afficher les indices de forme"))
    })
  })
  
  # Interprétation globale
  output$global_interpretation <- renderUI({
    req(rv$normality_results)
    
    tryCatch({
      normal_count <- 0
      total_count <- 0
      interpretation_list <- list()
      
      for (var in names(rv$normality_results)) {
        total_count <- total_count + 1
        tests <- rv$normality_results[[var]]$tests
        
        # Vérifier la normalité (au moins un test significatif)
        is_normal <- FALSE
        test_count <- 0
        normal_tests <- 0
        
        for(test_name in names(tests)) {
          test <- tests[[test_name]]
          if(!is.null(test$p.value) && !is.na(test$p.value)) {
            test_count <- test_count + 1
            if(test$p.value > input$alpha_level) {
              normal_tests <- normal_tests + 1
            }
          }
        }
        
        # Considérer comme normale si au moins la moitié des tests sont normaux
        if (test_count > 0 && normal_tests >= ceiling(test_count / 2)) {
          is_normal <- TRUE
          normal_count <- normal_count + 1
        }
        
        interpretation_list[[var]] <- tags$li(
          strong(var), ": ", 
          if(is_normal) tags$span("✅ Distribution normale", style = "color: #27ae60;") 
          else tags$span("❌ Distribution non normale", style = "color: #e74c3c;")
        )
      }
      
      tagList(
        h5("Résumé global:"),
        p(paste("Sur", total_count, "variable(s) analysée(s),", normal_count, 
                "présente(nt) une distribution normale (alpha =", input$alpha_level, ")")),
        tags$ul(interpretation_list),
        br(),
        h5("Recommandations:"),
        if(normal_count == total_count) {
          tags$div(class = "alert alert-success",
                   strong("✓ Conditions optimales:"),
                   "Toutes les variables suivent une distribution normale. Vous pouvez utiliser des tests paramétriques classiques.")
        } else if(normal_count >= total_count * 0.5) {
          tags$div(class = "alert alert-warning",
                   strong("⚠️ Conditions acceptables:"),
                   "La majorité des variables sont normales. Envisagez des transformations pour les variables non normales ou utilisez des tests non paramétriques.")
        } else {
          tags$div(class = "alert alert-danger",
                   strong("✗ Conditions problématiques:"),
                   "Peu de variables suivent une distribution normale. Utilisez des tests non paramétriques ou appliquez des transformations.")
        }
      )
    }, error = function(e) {
      tags$div(class = "alert alert-danger",
               "Erreur dans l'interprétation globale: ", e$message)
    })
  })
  
  # Recommandations de transformations
  output$transformation_recommendations <- renderUI({
    req(rv$normality_results)
    
    tryCatch({
      recommendations <- list()
      
      for (var in names(rv$normality_results)) {
        var_data <- rv$normality_results[[var]]$data
        skewness_val <- moments::skewness(var_data)
        lambda <- rv$normality_results[[var]]$boxcox_lambda
        
        rec_text <- if(skewness_val > 1) {
          "Asymétrie forte positive → Transformation logarithmique ou racine carrée recommandée"
        } else if(skewness_val > 0.5) {
          "Asymétrie modérée positive → Transformation racine carrée ou Box-Cox recommandée"
        } else if(skewness_val < -1) {
          "Asymétrie forte négative → Transformation carrée recommandée"
        } else if(skewness_val < -0.5) {
          "Asymétrie modérée négative → Transformation carrée ou Box-Cox recommandée"
        } else {
          "Distribution relativement symétrique → Aucune transformation nécessaire"
        }
        
        if (!is.na(lambda)) {
          rec_text <- paste(rec_text, paste0("\nLambda Box-Cox optimal: ", round(lambda, 3)))
        }
        
        recommendations[[var]] <- tags$li(
          strong(var), ": ", rec_text
        )
      }
      
      tagList(
        h5("Recommandations par variable:"),
        tags$ul(recommendations),
        br(),
        h5("Guide des transformations:"),
        tags$ul(
          tags$li(strong("Logarithmique (log(x)):"), "Pour les données avec asymétrie positive forte"),
          tags$li(strong("Racine carrée (√x):"), "Pour les données avec asymétrie positive modérée"),
          tags$li(strong("Carrée (x²):"), "Pour les données avec asymétrie négative"),
          tags$li(strong("Box-Cox:"), "Transformation optimale automatique")
        )
      )
    }, error = function(e) {
      tags$div(class = "alert alert-danger",
               "Erreur dans les recommandations: ", e$message)
    })
  })
  
  # Résumé pour export
  output$export_summary <- renderText({
    req(rv$normality_results)
    
    tryCatch({
      summary_text <- paste0(
        "RAPPORT D'ANALYSE DE NORMALITÉ\n",
        "==============================\n\n",
        "Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        "Niveau alpha: ", input$alpha_level, "\n",
        "Variables analysées: ", length(names(rv$normality_results)), "\n\n"
      )
      
      for (var in names(rv$normality_results)) {
        tests <- rv$normality_results[[var]]$tests
        stats <- rv$normality_results[[var]]$stats
        
        summary_text <- paste0(summary_text,
                               "VARIABLE: ", var, "\n",
                               "----------\n",
                               "Effectif: ", stats$n, "\n",
                               "Moyenne: ", round(stats$Mean, 3), "\n",
                               "Écart-type: ", round(stats$SD, 3), "\n",
                               "Asymétrie: ", round(stats$Skewness, 3), "\n",
                               "Aplatissement: ", round(stats$Kurtosis, 3), "\n\n",
                               "TESTS DE NORMALITÉ:\n")
        
        for (test_name in names(tests)) {
          test <- tests[[test_name]]
          if (!is.null(test$p.value)) {
            summary_text <- paste0(summary_text,
                                   "  ", test_name, ": p = ", 
                                   ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 4)),
                                   ifelse(test$p.value > input$alpha_level, " (Normal)", " (Non normal)"), "\n")
          } else if (!is.null(test$note)) {
            summary_text <- paste0(summary_text,
                                   "  ", test_name, ": ", test$note, "\n")
          }
        }
        summary_text <- paste0(summary_text, "\n")
      }
      
      return(summary_text)
    }, error = function(e) {
      return(paste("Erreur dans la génération du résumé:", e$message))
    })
  })
  
  # Export des résultats
  output$export_summary_txt <- downloadHandler(
    filename = function() {
      paste0("resume_analyse_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
    },
    content = function(file) {
      writeLines(output$export_summary(), file, useBytes = TRUE)
    }
  )
  
  # Export Excel
  output$export_results_excel <- downloadHandler(
    filename = function() {
      paste0("analyse_normalite_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      req(rv$normality_results, rv$descriptive_stats)
      
      tryCatch({
        # Créer un classeur Excel
        wb <- createWorkbook()
        
        # Feuille 1: Tests de normalité
        addWorksheet(wb, "Tests_Normalite")
        tests_data <- prepare_tests_data()
        writeData(wb, "Tests_Normalite", tests_data)
        
        # Feuille 2: Statistiques descriptives
        addWorksheet(wb, "Statistiques_Descriptives")
        desc_data <- do.call(rbind, rv$descriptive_stats)
        writeData(wb, "Statistiques_Descriptives", desc_data)
        
        # Feuille 3: Transformations Box-Cox
        if (input$transformations) {
          addWorksheet(wb, "Transformations_BoxCox")
          boxcox_data <- prepare_boxcox_data()
          writeData(wb, "Transformations_BoxCox", boxcox_data)
        }
        
        # Feuille 4: Données brutes
        addWorksheet(wb, "Donnees_Brutes")
        writeData(wb, "Donnees_Brutes", rv$data)
        
        saveWorkbook(wb, file)
        
        showNotification("✅ Fichier Excel exporté avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur lors de l'export Excel:", e$message), type = "error")
      })
    }
  )
  # Export des graphiques individuels PNG
  output$export_qqplot_png <- downloadHandler(
    filename = function() {
      paste0("qqplot_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        # Recréer le QQ-plot
        plot_list <- list()
        vars_to_show <- head(names(rv$normality_results), 6)
        
        for (var in vars_to_show) {
          var_data <- rv$normality_results[[var]]$data
          
          p <- ggplot(data.frame(x = var_data), aes(sample = x)) +
            stat_qq(color = "#3498db", alpha = 0.7) +
            stat_qq_line(color = "#e74c3c", linewidth = 1) +
            labs(title = paste("QQ-Plot -", var),
                 x = "Quantiles théoriques",
                 y = "Quantiles observés") +
            theme_minimal() +
            theme(plot.title = element_text(size = 12, face = "bold"))
          
          plot_list[[var]] <- p
        }
        
        # Sauvegarder le premier graphique ou un composite
        if (length(plot_list) == 1) {
          ggsave(file, plot_list[[1]], width = 10, height = 8, dpi = 300)
        } else {
          # Créer un grid pour l'export
          composite_plot <- grid.arrange(grobs = plot_list, ncol = 2)
          ggsave(file, composite_plot, width = 12, height = 10, dpi = 300)
        }
        
        showNotification("✅ QQ-Plot exporté avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export QQ-Plot:", e$message), type = "error")
      })
    }
  )
  
  output$export_histogram_png <- downloadHandler(
    filename = function() {
      paste0("histogrammes_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        plot_list <- list()
        vars_to_show <- head(names(rv$normality_results), 6)
        
        for (var in vars_to_show) {
          var_data <- rv$normality_results[[var]]$data
          
          p <- ggplot(data.frame(x = var_data), aes(x = x)) +
            geom_histogram(aes(y = after_stat(density)), fill = "#3498db", alpha = 0.6, bins = 20) +
            geom_density(color = "#2c3e50", linewidth = 1) +
            stat_function(fun = dnorm, 
                          args = list(mean = mean(var_data), sd = sd(var_data)),
                          color = "#e74c3c", linewidth = 1, linetype = "dashed") +
            labs(title = paste("Histogramme -", var),
                 x = "Valeur", y = "Densité") +
            theme_minimal() +
            theme(plot.title = element_text(size = 12, face = "bold"))
          
          plot_list[[var]] <- p
        }
        
        if (length(plot_list) == 1) {
          ggsave(file, plot_list[[1]], width = 10, height = 8, dpi = 300)
        } else {
          composite_plot <- grid.arrange(grobs = plot_list, ncol = 2)
          ggsave(file, composite_plot, width = 12, height = 10, dpi = 300)
        }
        
        showNotification("✅ Histogrammes exportés avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export histogrammes:", e$message), type = "error")
      })
    }
  )
  
  output$export_boxplot_png <- downloadHandler(
    filename = function() {
      paste0("boxplots_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        plot_data <- list()
        
        for (var in names(rv$normality_results)) {
          plot_data[[var]] <- data.frame(
            Variable = var,
            Valeur = rv$normality_results[[var]]$data
          )
        }
        
        plot_df <- do.call(rbind, plot_data)
        
        p <- ggplot(plot_df, aes(x = Variable, y = Valeur, fill = Variable)) +
          geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c", outlier.size = 2) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
          labs(title = "Boxplots des variables",
               x = "Variables", y = "Valeurs") +
          theme_minimal() +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 14, face = "bold"))
        
        ggsave(file, p, width = 10, height = 8, dpi = 300)
        showNotification("✅ Boxplots exportés avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export boxplots:", e$message), type = "error")
      })
    }
  )
  
  output$export_density_png <- downloadHandler(
    filename = function() {
      paste0("densites_comparaison_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        plot_list <- list()
        vars_to_show <- head(names(rv$normality_results), 4)
        
        for (var in vars_to_show) {
          var_data <- rv$normality_results[[var]]$data
          normal_data <- rnorm(length(var_data), mean = mean(var_data), sd = sd(var_data))
          
          comp_data <- data.frame(
            Type = rep(c("Observée", "Normale"), each = length(var_data)),
            Valeur = c(var_data, normal_data)
          )
          
          p <- ggplot(comp_data, aes(x = Valeur, fill = Type)) +
            geom_density(alpha = 0.5) +
            labs(title = paste("Densité -", var),
                 x = "Valeur", y = "Densité") +
            theme_minimal() +
            scale_fill_manual(values = c("Observée" = "#3498db", "Normale" = "#e74c3c")) +
            theme(plot.title = element_text(size = 12, face = "bold"),
                  legend.position = "bottom")
          
          plot_list[[var]] <- p
        }
        
        if (length(plot_list) == 1) {
          ggsave(file, plot_list[[1]], width = 10, height = 8, dpi = 300)
        } else {
          composite_plot <- grid.arrange(grobs = plot_list, ncol = 2)
          ggsave(file, composite_plot, width = 12, height = 10, dpi = 300)
        }
        
        showNotification("✅ Graphiques de densité exportés avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export densités:", e$message), type = "error")
      })
    }
  )
  
  output$export_ppplot_png <- downloadHandler(
    filename = function() {
      paste0("ppplots_", format(Sys.time(), "%Y%m%d_%H%M"), ".png")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        plot_list <- list()
        vars_to_show <- head(names(rv$normality_results), 6)
        
        for (var in vars_to_show) {
          var_data <- rv$normality_results[[var]]$data
          n <- length(var_data)
          theoretical_probs <- pnorm(sort(var_data), mean = mean(var_data), sd = sd(var_data))
          empirical_probs <- (1:n) / n
          
          pp_data <- data.frame(
            Theoretical = theoretical_probs,
            Empirical = empirical_probs
          )
          
          p <- ggplot(pp_data, aes(x = Theoretical, y = Empirical)) +
            geom_point(color = "#3498db", alpha = 0.7) +
            geom_abline(slope = 1, intercept = 0, color = "#e74c3c", linewidth = 1) +
            labs(title = paste("PP-Plot -", var),
                 x = "Probabilités théoriques",
                 y = "Probabilités empiriques") +
            theme_minimal() +
            theme(plot.title = element_text(size = 12, face = "bold"))
          
          plot_list[[var]] <- p
        }
        
        if (length(plot_list) == 1) {
          ggsave(file, plot_list[[1]], width = 10, height = 8, dpi = 300)
        } else {
          composite_plot <- grid.arrange(grobs = plot_list, ncol = 2)
          ggsave(file, composite_plot, width = 12, height = 10, dpi = 300)
        }
        
        showNotification("✅ PP-Plots exportés avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export PP-Plots:", e$message), type = "error")
      })
    }
  )
  
  # Export ZIP de tous les graphiques
  output$export_all_plots_zip <- downloadHandler(
    filename = function() {
      paste0("tous_graphiques_", format(Sys.time(), "%Y%m%d_%H%M"), ".zip")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        # Créer un dossier temporaire
        temp_dir <- tempdir()
        plot_files <- c()
        
        # 1. QQ-Plots
        qq_file <- file.path(temp_dir, "qqplots.png")
        plot_list <- list()
        for (var in names(rv$normality_results)) {
          var_data <- rv$normality_results[[var]]$data
          p <- ggplot(data.frame(x = var_data), aes(sample = x)) +
            stat_qq(color = "#3498db") + stat_qq_line(color = "#e74c3c") +
            labs(title = var) + theme_minimal()
          plot_list[[var]] <- p
        }
        if (length(plot_list) > 0) {
          composite <- grid.arrange(grobs = plot_list, ncol = 2)
          ggsave(qq_file, composite, width = 12, height = 10, dpi = 300)
          plot_files <- c(plot_files, qq_file)
        }
        
        # 2. Histogrammes
        hist_file <- file.path(temp_dir, "histogrammes.png")
        plot_list <- list()
        for (var in names(rv$normality_results)) {
          var_data <- rv$normality_results[[var]]$data
          p <- ggplot(data.frame(x = var_data), aes(x = x)) +
            geom_histogram(aes(y = after_stat(density)), fill = "#3498db", alpha = 0.6, bins = 20) +
            geom_density(color = "#2c3e50") +
            stat_function(fun = dnorm, args = list(mean = mean(var_data), sd = sd(var_data)),
                          color = "#e74c3c", linetype = "dashed") +
            labs(title = var) + theme_minimal()
          plot_list[[var]] <- p
        }
        if (length(plot_list) > 0) {
          composite <- grid.arrange(grobs = plot_list, ncol = 2)
          ggsave(hist_file, composite, width = 12, height = 10, dpi = 300)
          plot_files <- c(plot_files, hist_file)
        }
        
        # 3. Boxplot
        box_file <- file.path(temp_dir, "boxplots.png")
        plot_data <- do.call(rbind, lapply(names(rv$normality_results), function(var) {
          data.frame(Variable = var, Valeur = rv$normality_results[[var]]$data)
        }))
        p <- ggplot(plot_data, aes(x = Variable, y = Valeur, fill = Variable)) +
          geom_boxplot() + theme_minimal() + theme(legend.position = "none")
        ggsave(box_file, p, width = 10, height = 8, dpi = 300)
        plot_files <- c(plot_files, box_file)
        
        # Créer le ZIP
        zip::zipr(file, plot_files)
        
        showNotification("✅ Tous les graphiques exportés en ZIP!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export ZIP:", e$message), type = "error")
      })
    }
  )
  
  # Export CSV des données analysées
  output$export_results_csv <- downloadHandler(
    filename = function() {
      paste0("donnees_analysees_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      req(rv$data, input$selected_variables)
      
      tryCatch({
        # Exporter les données originales avec les variables sélectionnées
        data_export <- rv$data
        
        # Ajouter une colonne avec les statistiques résumées si souhaité
        write.csv(data_export, file, row.names = FALSE, fileEncoding = "UTF-8")
        showNotification("✅ Données CSV exportées avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export CSV:", e$message), type = "error")
      })
    }
  )
  
  # Export format APA
  output$export_apa_txt <- downloadHandler(
    filename = function() {
      paste0("resultats_apa_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
    },
    content = function(file) {
      req(rv$normality_results)
      
      tryCatch({
        apa_text <- "RÉSULTATS STATISTIQUES - FORMAT APA\n"
        apa_text <- paste0(apa_text, "===================================\n\n")
        apa_text <- paste0(apa_text, "Date: ", format(Sys.time(), "%d %B %Y"), "\n")
        apa_text <- paste0(apa_text, "Niveau alpha: ", input$alpha_level, "\n\n")
        
        for(var in names(rv$normality_results)) {
          tests <- rv$normality_results[[var]]$tests
          stats <- rv$normality_results[[var]]$stats
          
          apa_text <- paste0(apa_text, "Variable: ", var, "\n")
          apa_text <- paste0(apa_text, "Statistiques descriptives: M = ", round(stats$Mean, 2),
                             ", SD = ", round(stats$SD, 2),
                             ", Mdn = ", round(stats$Median, 2),
                             ", asymétrie = ", round(stats$Skewness, 2),
                             ", aplatissement = ", round(stats$Kurtosis, 2), "\n")
          
          if("Shapiro_Wilk" %in% names(tests)) {
            test <- tests$Shapiro_Wilk
            if(!is.null(test$statistic) && !is.null(test$p.value)) {
              apa_text <- paste0(apa_text,
                                 "Test de Shapiro-Wilk: W = ", round(test$statistic, 3),
                                 ", p = ", ifelse(test$p.value < 0.001, "< .001", 
                                                  sprintf("%.3f", test$p.value)), "\n")
            }
          }
          apa_text <- paste0(apa_text, "---\n\n")
        }
        
        writeLines(apa_text, file, useBytes = TRUE)
        showNotification("✅ Format APA exporté avec succès!", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur export APA:", e$message), type = "error")
      })
    }
  )
  
  # Fonctions utilitaires pour l'export
  prepare_tests_data <- function() {
    tests_list <- list()
    
    for (var in names(rv$normality_results)) {
      tests <- rv$normality_results[[var]]$tests
      
      for (test_name in names(tests)) {
        test <- tests[[test_name]]
        
        test_row <- data.frame(
          Variable = var,
          Test = test_name,
          Statistique = ifelse(!is.null(test$statistic), round(test$statistic, 4), NA),
          p_value = ifelse(!is.null(test$p.value), 
                           ifelse(test$p.value < 0.001, "< 0.001", 
                                  as.character(round(test$p.value, 4))), 
                           NA),
          Interpretation = ifelse(!is.null(test$p.value),
                                  ifelse(test$p.value > input$alpha_level, "Normal", "Non normal"),
                                  ifelse(!is.null(test$note), test$note, "N/A")),
          stringsAsFactors = FALSE
        )
        tests_list[[length(tests_list) + 1]] <- test_row
      }
    }
    
    do.call(rbind, tests_list)
  }
  
  prepare_boxcox_data <- function() {
    boxcox_list <- list()
    
    for (var in names(rv$normality_results)) {
      lambda <- rv$normality_results[[var]]$boxcox_lambda
      
      boxcox_row <- data.frame(
        Variable = var,
        Lambda_optimal = ifelse(!is.na(lambda), round(lambda, 3), "N/A"),
        Transformation_recommandee = ifelse(!is.na(lambda),
                                            ifelse(abs(lambda) < 0.001, "log(x)",
                                                   paste0("(x^", round(lambda, 2), " - 1)/", round(lambda, 2))),
                                            "Non applicable"),
        stringsAsFactors = FALSE
      )
      boxcox_list[[length(boxcox_list) + 1]] <- boxcox_row
    }
    
    do.call(rbind, boxcox_list)
  }
  
  # Nettoyage de la mémoire
  session$onSessionEnded(function() {
    rm(list = ls())
    gc()
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)