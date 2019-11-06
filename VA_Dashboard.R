# treemap
# d3treeR

#devtools::install_github("mattflor/chorddiag")
#install.packages("ggraph")
#install.packages("randomcoloR")
#install.packages("visNetwork")
#devtools::install_github("d3treeR/d3treeR")
#install.packages("gridSVG")

library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(DT)
library(plotly)
library(RColorBrewer)
library(scanstatistics)
library(visNetwork)
library(chorddiag)
library(ggraph)
library(randomcoloR)
library(treemap)
library(d3treeR)
library(rsconnect)
library(igraph)
library(BBmisc)
library(dplyr)
library(DescTools)
library(pals)

ui = bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = FALSE,
  title = "VA Project SS 19",
  navbar = bs4DashNavbar(skin = "light",
                         status = "white",
                         border = TRUE,
                         fixed = FALSE,
                         rightUi = tagList(
                           bs4DropdownMenu(
                             show = FALSE,
                             labelText = "!",
                             status = "primary",
                             bs4DropdownMenuItem(
                               text = HTML("You can collapse the sidebars <br/> by clicking on the icons")
                             )
                           ))),
  
  
  sidebar = bs4DashSidebar(skin = "light",
                           status = "primary",
                           title = "ML-ModelExplorer",
                           brandColor = "primary",
                           bs4SidebarMenu(
                             bs4SidebarHeader("Dashboards"),
                             bs4SidebarMenuItem("Model Overview", tabName = "dashboard1", icon = "sliders"),
                             bs4SidebarMenuItem("Model Details", tabName = "dashboard2", icon = "calculator"),
                             bs4SidebarMenuItem("Model Comparison", tabName = "modelcomparison", icon = "handshake"),
                             bs4SidebarMenuItem("Data Properties", tabName = "dataproperties", icon = "th-large"),
                             bs4SidebarMenuItem("debug", tabName = "debug"))),
  
  controlbar = bs4DashControlbar(skin = "light",
                                 title = "Controlbar",
                                 h5(helpText("Upload a Confusion Matrix")),
                                 h6(helpText("Choose the seperator")),
                                 pickerInput(inputId = "sep", label = NULL, choices = c(Comma=",", Semicolon=";", Tab="\t", Space=" "), selected = ",", multiple = FALSE),
                                 h6(helpText("Choose the file")),
                                 fileInput("file", accept = c(".csv"), label = NULL, buttonLabel = "Search"),
                                 h5(helpText("Upload model descriptions")),
                                 h6(helpText("Choose the seperator")),
                                 pickerInput(inputId = "sep2", label = NULL, choices = c(Newline = "\n", Comma=",", Semicolon=";", Tab="\t", Space=" "), selected = "\n", multiple = FALSE),
                                 h6(helpText("Choose the file")),
                                 fileInput("modelnames", accept = c(".txt"), label = NULL, buttonLabel = "Search"),
                                 h5(helpText("Select the Models")),
                                 pickerInput(inputId = "models",
                                             label = NULL,
                                             choices = "",
                                             options = pickerOptions(actionsBox = TRUE,
                                                                     showTick = TRUE,
                                                                     size = 10,
                                                                     selectedTextFormat = "count > 3"),
                                             multiple = TRUE),
                                 h5(helpText("Select the Classes which are not relevant")),
                                 uiOutput("classlimit"),
                                 h5(helpText("Display absolute or percentage values?")),
                                 switchInput("valueswitch", label = NULL, value = TRUE, onLabel = "Percentages",
                                             offLabel = "Absolute", onStatus = "primary", offStatus = NULL,
                                             size = "large")),
  footer = bs4DashFooter(),
  body = bs4DashBody(bs4TabItems(bs4TabItem(tabName = "dashboard1",
                                            h2("Comparison of different classification models"),
                                            fluidRow(bs4InfoBoxOutput("acc_box_all", width = 2),
                                                     bs4InfoBoxOutput("baseline_box_all", width = 2),
                                                     bs4InfoBoxOutput("precision_box_all", width = 2),
                                                     bs4InfoBoxOutput("recall_box_all", width = 2),
                                                     bs4InfoBoxOutput("f1_box_all", width = 2),
                                                     bs4InfoBoxOutput("gini_all", width = 2)),

                                            fluidRow(bs4TabCard(id = "Distribution_Error_Tab", title = "Per-model Metrics Plot", width = 8, closable = FALSE, status = "primary", maximizable = TRUE,
                                                                bs4TabPanel(tabName = "Boxplot", plotlyOutput("boxplot", width = "100%")),
                                                                bs4TabPanel(tabName = "Line Plot", plotlyOutput("errorline")),
                                                                bs4TabPanel(tabName = "Metric Info", HTML("<ul> <li>F1: Harmonic mean of precision and recall  <li>Precision: Positive predictive rate  <li>Recall: True positive rate  
                                                                                                   <li>Accuracy: Accuracy of the model  <li>Baseline: Accuracy of always predicting the most frequent class  <li>Random: Accuracy of a completly random prediction"))),
                                                     bs4Card(title = "Model Similarity Plot", plotlyOutput("acc_std_plot"), width = 4, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can hover over the points"),
                                                               dropdownItem(name = "to show detailed information")
                                                             ))),
                                            fluidRow(bs4Card(title = "Per-class Error Query View", plotlyOutput("parcoord"), width = 8, collapsible = TRUE, status = "primary", collapsed = FALSE, closable = FALSE, maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can highlight aspects by"),
                                                               dropdownItem(name = "using a brush stroke on the axis")
                                                             )),
                                                     bs4Card(title = "Class Error Radar Chart", plotlyOutput("radarchart"), width = 4, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can hover over the rectangles"),
                                                               dropdownItem(name = "to show detailed information"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can resize the radar by"),
                                                               dropdownItem(name = "clicking and dragging with the mouse"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can select/deselect models"),
                                                               dropdownItem(name = "by clicking on them")
                                                             ))),
                                            fluidRow(bs4Card(title = "Error Hierarchy Plot", plotlyOutput("sunburst_plot", width = "100%"), width = 6, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can drill down/up by"),
                                                               dropdownItem(name = "clicking on the panels"),
                                                               dropdownItem(name = HTML("<br>")),
                                                               dropdownItem(name = "You can hover over the panel"),
                                                               dropdownItem(name = "to show detailed information")
                                                             )),
                                                     bs4Card(title = "Network Graph", plotlyOutput("network_plot"), width = 6, closable = FALSE, status = "primary", maximizable = TRUE))),

                                 bs4TabItem(tabName = "modelcomparison",
                                            fluidRow(bs4Card(title = "Select Reference Model", width = 2, status = "primary", collapsible = TRUE, collapsed = FALSE, closable = FALSE,
                                                             pickerInput(inputId = "defaultmodel",
                                                                         label = NULL,
                                                                         choices = "",
                                                                         multiple = FALSE)),
                                                     bs4Card(title = "Select Comparing Model", width = 2, status = "primary", collapsible = TRUE, collapsed = FALSE, closable = FALSE,
                                                             pickerInput(inputId = "comparingmodel",
                                                                         label = NULL,
                                                                         choices = "",
                                                                         multiple = FALSE)),
                                                     bs4Card(title = "Model Information", DT::dataTableOutput("model_comparison_table"), width = 8, status = "primary", collapsible = TRUE, closable = FALSE, collapsed = FALSE)),
                                            fluidRow(bs4Card(title = "Delta Confusion Matrix", plotlyOutput("heatmap_comparison"), width = 6, collapsible = TRUE, collapsed = FALSE, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "Red displays better Performance of Reference Model"),
                                                               dropdownItem(name = "Green displays better Performance of Comparing Model"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "Color intensity displays the degree of difference")
                                                             )),
                                                     bs4Card(title = "Delta Error Radar Chart", plotlyOutput("radarchartdeltaplot"), width = 6, collapsible = TRUE, collapsed = FALSE, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can hover over the rectangles"),
                                                               dropdownItem(name = "to show detailed information"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can resize the radar by"),
                                                               dropdownItem(name = "clicking and dragging with the mouse"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can select/deselect models"),
                                                               dropdownItem(name = "by clicking on them")
                                                             )))),
                                 bs4TabItem(tabName = "dashboard2",
                                            h2("Detailed Information on a singular classification model"),
                                            fluidRow(bs4InfoBoxOutput("singleacc_box", width = 2),
                                                     bs4InfoBoxOutput("singlebaseacc_box", width = 2),
                                                     bs4InfoBoxOutput("precision_box", width = 2),
                                                     bs4InfoBoxOutput("recall_box", width = 2),
                                                     bs4InfoBoxOutput("f1_box", width = 2),
                                                     bs4InfoBoxOutput("kappa_box", width = 2)),
                                            fluidRow(bs4Card(title = "Distribution Plot", width = 12, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE)),

                                            fluidRow(bs4Card(title = "Confusion Circle", chorddiagOutput("chorddiagramm", height = 500), width = 6, closable = FALSE, status = "primary", maximizable = TRUE, 
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can hover over the connections"),
                                                               dropdownDivider(),
                                                               dropdownItem(name = "to show detailed information")
                                                             )),
                                                     bs4Card(title = "Confusion Matrix", plotlyOutput("heatmap", height = 500), width = 6, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "Every Class Confusion is transformed into a Confusion Score"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "The absolute value of a Confusion Score displays:"),
                                                               dropdownItem(name = "Absolute value near 1 = very good"),
                                                               dropdownItem(name = "Absolute value near 0 = very poor")
                                                             ))),
                                            fluidRow(bs4Card(title = "Bilateral Confusion Plot",plotlyOutput("sankey"), width = 6, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can hover over the connections"),
                                                               dropdownItem(name = "to show detailed information"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can hover over the classes"),
                                                               dropdownItem(name = "to show summarized information")
                                                             )),
                                                     bs4Card(title = "Confusion Treemap", d3tree2Output("treemap"),width = 6, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can click on a tile"),
                                                               dropdownItem(name = "to show detailed information")
                                                             )))),

                                 bs4TabItem(tabName = "dataproperties",
                                            h2("Data Properties"),
                                            fluidRow(bs4InfoBoxOutput("nomodels_box", width = 2),
                                                     bs4InfoBoxOutput("samples_box", width = 2)),
                                            fluidRow(bs4Card(title = "Lorenz Curve", plotlyOutput("lorenzcurve"), width = 6, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE),
                                                     bs4Card(title = "Class Histogramm", plotlyOutput("histogram"), width = 6, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE)),
                                            fluidRow(bs4Card(title = "Tabular Plot", DT::dataTableOutput(outputId = "table"), width = 12, closable = FALSE, collapsible = TRUE))),
                                 bs4TabItem(tabName = "debug",
                                            h2("debug"),
                                            fluidRow(bs4Card(title = "debug", tableOutput("test")))))))


server = function(input, output, session) {
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file=file1$datapath, sep = input$sep, header = TRUE, stringsAsFactors = FALSE)
  })
  
  modelnames <- reactive({
    modelnames <- input$modelnames
    if(is.null(modelnames) && is.null(data())){return()}
    if(is.null(modelnames)){
      models <- paste("Model", 1:(nrow(data())/ncol(data())))
      return(models)
    }
    models <- read.delim(file=modelnames$datapath, sep = input$sep2, header = FALSE, stringsAsFactors = FALSE)
    models <- unlist(models)
    models <- unname(models)
    models <- c(models)
  })
  
  
  
  output$classlimit <- renderUI({
    pickerInput(inputId = "classes",
                choices = "",
                options = pickerOptions(actionsBox = FALSE,
                                        showTick = TRUE,
                                        size = 10,
                                        selectedTextFormat = "count > 3",
                                        maxOptions = length(classnames()) -3),
                multiple = TRUE)   
  })
  
  selected_models <- reactive({
    if(is.null(modelnames)){return()}
    options <- modelnames()
    rows <- match(input$models,options)
    rows <- as.numeric(rows) * ncol(data())
    rows <- c(rep(rows, each = ncol(data())) - 0:(ncol(data()) - 1))
    rows <- sort(rows)
    data <- data()[rows,]
    if(!is.null(input$classes)){
      delseq <- rep(classdelete(), each = length(input$models)) + seq(0,nrow(data)-1,ncol(data))
      data[-delseq, -classdelete()]
      
    }
    else{
      data
    }
  })
  
  selected_models_percentage <- reactive({
    if(is.null(modelnames)){return()}
    cm <- selected_models()
    csum <- cm %>%
      group_by(indx = gl(ceiling(nrow(cm)/ncol(cm)), ncol(cm), nrow(cm))) %>%
      summarise_each(list(sum))
    csum <- csum[,-1]
    csumdivide <- csum[rep(seq_len(nrow(csum)), each = ncol(cm)), ]
    csumdivide[csumdivide == 0] <- 1
    selected_models_percentage <- cm/csumdivide
    selected_models_percentage
  })
  
  selected_models_missclassified <- reactive({
    if(is.null(modelnames)){return()}
    options <- modelnames()
    rows <- match(input$models,options)
    rows <- as.numeric(rows) * ncol(data())
    rows <- c(rep(rows, each = ncol(data())) - 0:(ncol(data()) - 1))
    rows <- sort(rows)
    sel_models <- data()[rows,]
    a <- c(1:nrow(sel_models))
    b <- rep(1:ncol(sel_models), nrow(sel_models) / ncol(sel_models))
    d <- cbind(a,b)
    sel_models[d] <- 0
    if(!is.null(input$classes)){
      delseq <- rep(classdelete(), each = length(input$models)) + seq(0,nrow(sel_models)-1,ncol(sel_models))
      sel_models[-delseq, -classdelete()]
    }
    else{
      sel_models
    }
  })
  
  # Nur die wirklichen Prozente der Fehlklassifizierten
  selected_models_missclassified_percentage <- reactive({
    if(is.null(modelnames)){return()}
    cm <- selected_models_percentage()
    a <- c(1:nrow(cm))
    b <- rep(1:ncol(cm), nrow(cm) / ncol(cm))
    d <- cbind(a,b)
    cm[d] <- 0.0
    cm
  })
  
  # Percentage der einzelnen Fehlklassifikationen summiert sich zu 1
  selected_models_missclassified_percentage_per_class <- reactive({
    if(is.null(modelnames)){return()}
    cm <- selected_models_missclassified()
    csum <- cm %>%
      group_by(indx = gl(ceiling(nrow(cm)/ncol(cm)), ncol(cm), nrow(cm))) %>%
      summarise_each(list(sum))
    csum <- csum[,-1]
    csumdivide <- csum[rep(seq_len(nrow(csum)), each = ncol(cm)), ]
    csumdivide[csumdivide == 0] <- 1
    selected_models_missclassified_percentage_per_class <- cm/csumdivide
    selected_models_missclassified_percentage_per_class
  })
#  classnames <- reactive({
#    classnames <- input$classnames
#    if(input$classnamesheader == FALSE){return()}
#    if(input$classnamesheader == TRUE){
#      classnames <- colnames(data())
#      return(classnames)
#    }
#  })
  
  comparisondata <- reactive({
  if(is.null(data())){return ()}
  options <- modelnames()
  rows_d <- match(input$defaultmodel, options) # rows_d = rows_default
  start <- (rows_d*ncol(selected_models())) - (ncol(selected_models())) + 1
  end <- rows_d*ncol(selected_models())
  rows_d <- seq(start, end)
  rows_c <- match(input$comparingmodel, options) # rows_c = rows_compare
  start <- (rows_c*ncol(selected_models())) - (ncol(selected_models())) + 1
  end <- rows_c*ncol(selected_models())
  rows_c <- seq(start, end)
  data_d <- selected_models()[rows_d,]
  data_c <- selected_models()[rows_c,]
  data <- rbind(data_d, data_c)
  data
  })
  
  comparisondata_percentage <- reactive({
    if(is.null(data())){return ()}
    options <- modelnames()
    rows_d <- match(input$defaultmodel, options) # rows_d = rows_default
    start <- (rows_d*ncol(selected_models_percentage())) - (ncol(selected_models_percentage())) + 1
    end <- rows_d*ncol(selected_models_percentage())
    rows_d <- seq(start, end)
    rows_c <- match(input$comparingmodel, options) # rows_c = rows_compare
    start <- (rows_c*ncol(selected_models_percentage())) - (ncol(selected_models_percentage())) + 1
    end <- rows_c*ncol(selected_models_percentage())
    rows_c <- seq(start, end)
    data_d <- selected_models_percentage()[rows_d,]
    data_c <- selected_models_percentage()[rows_c,]
    data <- rbind(data_d, data_c)
    data
  })
  
  
  classnames <- reactive({
    classnames <- colnames(data())
  })
  
  selected_classes <- reactive({
    selected_classes <- colnames(selected_models())
    selected_classes
  })
  
  classdelete <- reactive({
    options <- classnames()
    colChoice <- match(input$classes,options)
  })
  
  
  output$test <- renderTable({
    selected_models_missclassified_percentage_per_class()
  })
  
  samples <- reactive({
    if(is.null(input$models)){return(0)}
    samples <- as.integer(sum(selected_models()) / length(input$models))
    samples
  })
  
  classnames <- reactive({
    classnames <- colnames(data())
    })
  
  selected_classes <- reactive({
    selected_classes <- colnames(selected_models())
    selected_classes
  })
  
  classdelete <- reactive({
    options <- classnames()
    colChoice <- match(input$classes,options)
  })
  
  
  output$test <- renderTable({
    selected_models_missclassified_percentage_per_class()
  })
  
  samples <- reactive({
    if(is.null(input$models)){return(0)}
    samples <- as.integer(sum(selected_models()) / length(input$models))
    samples
  })
  
  sunburst_data <- reactive({
    if(is.null(data())){return()}
    
    if (input$valueswitch == TRUE) {
      # Prozent
      sunburst_modelle <- selected_models_missclassified_percentage()
    } else {
      # Absolut
      sunburst_modelle <- selected_models_missclassified()
    }
    
    # Labels festlegen
    labels <- "All"
    labels <- c(labels, input$models)
    classes <- paste(rep(input$models, each=ncol(sunburst_modelle)), selected_classes())
    labels <- c(labels, classes)

    # Eltern festlegen
    parents <- " "
    parents <- c(parents, rep("All", length(input$models))) #nrow(sunburst_modelle) / ncol(sunburst_modelle)))
    parents <- c(parents, rep(input$models, each = ncol(sunburst_modelle)))

    # Hilfsvariable um über Klassennamen zu verfügen
    vec_classes <- selected_classes()

    for (i in seq(1, length(input$models))) {
      for (j in seq(1, ncol(sunburst_modelle))) {
        for (k in seq(1, ncol(sunburst_modelle))) {
          if (sunburst_modelle[((i*ncol(sunburst_modelle))-(ncol(sunburst_modelle)-1)+(k-1)),j] != 0) {
            labels <- c(labels, paste(input$models[i], vec_classes[j], vec_classes[k]))
            parents <- c(parents, paste(input$models[i], vec_classes[j]))
          }
        }
      }
    }

    # Schleife für Anzahl Fehlklassifizierungen über alle Modelle
    values <- sum(sunburst_modelle)
    # Schleife für Anzahl Fehlklassifizierungen je Modell
    for (i in seq(1, length(input$models))) {
      values <- c(values, sum(colSums(sunburst_modelle[((i*ncol(sunburst_modelle))-(ncol(sunburst_modelle)-1)):(i*ncol(sunburst_modelle)), ])))
    }

    # Schleife für Anzahl Fehlklassifizierungen je Modell und Klasse
    for (i in seq(1, length(input$models))) {
      for (j in seq(1, ncol(sunburst_modelle))) {
        values <- c(values, sum(sunburst_modelle[((i*ncol(sunburst_modelle))-(ncol(sunburst_modelle)-1)):(i*ncol(sunburst_modelle)), j]))
      }
    }

    # Schleife für Anzahl Fehlklassifizierungen je Classconfusion
    for (i in seq(1, length(input$models))) {
      for (j in seq(1, ncol(sunburst_modelle))) {
        for (k in seq(1, ncol(sunburst_modelle))) {
          if (sunburst_modelle[((i*ncol(sunburst_modelle))-(ncol(sunburst_modelle)-1)+(k-1)),j] != 0) {
            values <- c(values, sunburst_modelle[((i*ncol(sunburst_modelle))-(ncol(sunburst_modelle)-1)+(k-1)),j])
          }
        }
      }
    }
    
    sunburst_data <- as.data.frame(cbind(parents, labels, values))
    sunburst_data
  })
  

  
  output$samples_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Number of Samples",
      samples(),
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$nomodels_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Number of Models",
      length(input$models),
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$acc_box_all <- renderbs4InfoBox({
    if(is.null(input$models)){return(bs4InfoBox(
      title = "Average Accuracy",
      0,
      icon = "credit-card",
      status = "primary"
    ))}
    bs4InfoBox(
      title = "Average Accuracy",
      round((sum(selected_models()) - sum(selected_models_missclassified())) / sum(selected_models()), 4),
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$baseline_box_all <- renderbs4InfoBox({
    if(is.null(input$models)){return(bs4InfoBox(
      title = "Baseline Accuracy",
      0,
      icon = "credit-card",
      status = "primary"
    ))}
    bs4InfoBox(
      title = "Baseline Accuracy",
      round(max(colSums(selected_models())) / sum(selected_models()),4),
      icon = "credit-card",
      status = "primary"
    )
  })
  
  kpi_precision <- reactive({
    results <- boxplot_calculation()
    cm <- selected_models()
    results <- round(mean(results[1:nrow(cm),1]),4)
    results
  })
  
  kpi_recall <- reactive({
    results <- boxplot_calculation()
    cm <- selected_models()
    results <- round(mean(results[(1+nrow(cm)):(2*nrow(cm)),1]),4)
    results
  })
  
  kpi_f1 <- reactive({
    results <- boxplot_calculation()
    cm <- selected_models()
    results <- round(mean(results[(1+2*nrow(cm)):(3*nrow(cm)),1]),4)
    results
  })
  
  output$precision_box_all <- renderbs4InfoBox({
    if(is.null(input$models)){return(bs4InfoBox(
      title = "Average Precision",
      0,
      icon = "credit-card",
      status = "primary"
    ))}
    bs4InfoBox(
      title = "Average Precision",
      kpi_precision(),
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$recall_box_all <- renderbs4InfoBox({
    if(is.null(input$models)){return(bs4InfoBox(
      title = "Average Recall",
      0,
      icon = "credit-card",
      status = "primary"
    ))}
    bs4InfoBox(
      title = "Average Recall",
      kpi_recall(),
      icon = "credit-card",
      status = "primary"
    )  
  })
  
  output$f1_box_all <- renderbs4InfoBox({
    if(is.null(input$models)){return(bs4InfoBox(
      title = "Average F1",
      0,
      icon = "credit-card",
      status = "primary"
    ))}
    bs4InfoBox(
      title = "Average F1",
      kpi_f1(),
      icon = "credit-card",
      status = "primary"
    )  
  })
  
  output$gini_all <- renderbs4InfoBox({
    if(is.null(input$models)){return(bs4InfoBox(
      title = "Gini-Index",
      0,
      icon = "credit-card",
      status = "primary"
    ))}
    bs4InfoBox(
      title = "Gini-Index",
      calculate_gini(),
      icon = "credit-card",
      status = "primary"
    )  
  })

  output$singleacc_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Accuracy",
      if (length(input$models) != 1) {
        0
      } else {
        round((sum(selected_models()) - sum(selected_models_missclassified())) / sum(selected_models()),4)},
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$singlebaseacc_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Baseline Accuracy",
      if (length(input$models) != 1) {
        0
      } else {
        round(max(colSums(selected_models())) / sum(selected_models()),4)
      },
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$precision_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Precision",
      if (length(input$models) != 1) {
        0
      } else {
      round(mean(diag(as.matrix(selected_models())) / rowSums(selected_models())), 4)},
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$recall_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Recall",
      if (length(input$models) != 1) {
        0
      } else {
      round(mean(diag(as.matrix(selected_models())) / colSums(selected_models())), 4)},
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$f1_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "F1-Score",
      if (length(input$models) != 1) {
        0
      } else {
      round((2 * (mean(diag(as.matrix(selected_models())) / colSums(selected_models())) * (mean(diag(as.matrix(selected_models())) / rowSums(selected_models()))) / (mean(diag(as.matrix(selected_models())) / colSums(selected_models())) + (mean(diag(as.matrix(selected_models())) / rowSums(selected_models())))))),4)},
      icon = "credit-card",
      status = "primary"
    )
  })
  
  output$kappa_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Kappa-Score",
      if (length(input$models) != 1) {
        0
      } else {
      round((((sum(selected_models()) - sum(selected_models_missclassified())) / sum(selected_models())) - (sum((rowSums(selected_models()) / sum(selected_models())) * ((colSums(selected_models()) / sum(selected_models())))))) / (1 - (sum((rowSums(selected_models()) / sum(selected_models())) * ((colSums(selected_models()) / sum(selected_models())))))), 4)},
      icon = "credit-card",
      status = "primary"
    )
  })
  
  
  output$sunburst_plot <- renderPlotly({
    if(is.null(input$models)){return()}
    data <- sunburst_data()
    p <- plot_ly(data, labels = ~labels, parents = ~parents, values = ~values, type="sunburst", maxdepth=4, marker = list(colors = c("#e0e0e0", unname(alphabet()[1:max(length(input$models), length(colnames(selected_models())))]))), hovertemplate = paste('<b>%{label}</b><br>', 'Avg. Miss: %{value:.3f}', '<extra></extra>')) #color = ~parents, colors = ~parents)
      #add_trace(labels = ~labels2, parents = ~parents, values = ~values, type="sunburst", maxdepth=3, color = ~parents) %>%
      #layout(colorway = c('#f3cec9', '#e7a4b6', '#cd7eaf', '#a262a9', '#6f4d96', '#3d3b72', '#182844'))
    
    p
    
    #p <- plot_ly(sunburst_data(), labels = ~labels2, parents = ~parents, values = ~values, type = "sunburst", maxdepth = 3, sunburstcolors = c("#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3","#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"))
  })
  
  output$sunburst_plot_single <- renderPlotly({
    if(is.null(input$models)){return()}
    p <- plot_ly(sunburst_data(), labels = ~labels2, parents = ~parents, values = ~values, type = "sunburst", maxdepth = 3)
  })
  

  parcoordplot <- reactive({
    if(is.null(input$models)){return()}
    if(input$valueswitch == TRUE){
      parcoord_data <- selected_models_missclassified_percentage()
      axisformat <- '.3f'}
    else{
      parcoord_data <- selected_models_missclassified()
      axisformat <- 'f'}
    #models <- input$models
    models <- match(input$models,modelnames())
    classes <- selected_classes()
    #sums <- colSums(matrix(t(parcoord_data), nrow = ncol(parcoord_data)))
    cm2=data.frame(matrix(ncol=0,nrow=ncol(parcoord_data)))
    for(i in seq(1, nrow(parcoord_data), ncol(parcoord_data))){
      cm2 <- cbind(cm2, parcoord_data[i:(i+ncol(parcoord_data)-1), ])
    }
    sums <- t(matrix(colSums(cm2), nrow = nrow(cm2)))
    max_missclassified <- max(sums)
    parcoord_data <- as.data.frame(cbind(models, sums))
    colnames(parcoord_data) <- c("Models", classes)
    colr <- unname(alphabet())
    
    #start_statement = "list("
    start_statement = "list(list(range = c(1, max(models)),tickvals = models, label = 'Model', values = ~Models, ticktext = input$models),"
    loop_liste = c(start_statement)
    for(i in seq(1:ncol(selected_models_missclassified()))){
      text = sprintf("list(range = c(0,max_missclassified),tickformat = '%s', label = '%s', values = ~`%s`),",axisformat, classes[i], classes[i]) # Range entfernen um Balken zu skalieren
      loop_liste = c(loop_liste, text)
      if(i == ncol(selected_models_missclassified())){
        loop_liste[i+1] = substring(loop_liste[i+1], first = 0, last = nchar(loop_liste[i+1])-1)
        loop_liste[i+1] = paste(loop_liste[i+1], ")")
      }
    }
    loop_liste = paste(loop_liste, collapse = " ")

    
    sep_values <- seq(0, 1, 1/(length(input$models)-1))
    sep_colr <- unname(colr[1:length(input$models)])
    
    loop_color = "list("
    for (i in seq(1:length(input$models))) {
      if (i != length(input$models)) {
        text = sprintf("c(%s,'%s'),", sep_values[i], sep_colr[i])
      } else {
        text = sprintf("c(%s,'%s'))", sep_values[i], sep_colr[i])
      }
      loop_color = c(loop_color, text)
    }
    
    loop_color = paste(loop_color, collapse = " ")
    

    p <- parcoord_data %>%
      plot_ly(type = 'parcoords',
              line = list(color = ~Models,
                          colorscale = eval(parse(text = loop_color))),
              dimensions = eval(parse(text = loop_liste ))
      )
    p
  })
  output$parcoord <- renderPlotly({parcoordplot()})
  
  
  
  radarchartplot <- reactive({
    if(is.null(input$models)){return()}
    if(input$valueswitch == TRUE){
      cm <- round(selected_models_missclassified_percentage(),4)}
    else{
      cm <- selected_models_missclassified()}
    models <- input$models
    classes <- selected_classes()
    cm2=data.frame(matrix(ncol=0,nrow=ncol(cm)))
    for(i in seq(1, nrow(cm), ncol(cm))){
      cm2 <- cbind(cm2, cm[i:(i+ncol(cm)-1), ])
    }
    sums <- colSums(cm2)
    p <- plot_ly(type = 'scatterpolar', mode = "lines")
    i = 0
    for(j in seq(ncol(cm),nrow(cm),ncol(cm))){
      i = i+1
      k = j+1
      p<-add_trace(p,r = sums[(j-ncol(cm)+1):j], mode = "markers", theta = classes, fill = 'toself', fillcolor = adjustcolor(unname(alphabet()[i]), alpha.f = 0.5), name = input$models[i], marker = list(symbol = "square", size = 8, color = unname(alphabet()[i])), hovertemplate = paste('<i>Count </i>: %{r} <br> <i>Miss. as </i>: %{theta}'))
    }
    #mittel <- colMeans(matrix(sums, ncol = ncol(cm), byrow = TRUE))
    #p <- add_trace(p, r = mittel, mode = "markers", theta = classes, name = "AVG", marker = list(symbol = "square", size = 8))
    p
  })
  
  output$radarchart <- renderPlotly({radarchartplot()})
  
  radarchartdeltaplot <- reactive({
    if(is.null(input$models)){return()}
    if(input$valueswitch == TRUE){
      cm <- comparisondata_percentage()
      mittel <- selected_models_missclassified_percentage()
    }
    else{
      cm <- comparisondata()
      mittel <- selected_models_missclassified()
    }
    diag1 <- 1:(2*ncol(cm))
    diag2<- rep(1:ncol(cm),2)
    diagonal <- cbind(diag1, diag2)
    cm[diagonal] <- 0
    models <- c(input$defaultmodel, input$comparingmodel)
    classes <- selected_classes()
    cm2=data.frame(matrix(ncol=0,nrow=ncol(cm)))
    for(i in seq(1, nrow(cm), ncol(cm))){
      cm2 <- cbind(cm2, cm[i:(i+ncol(cm)-1), ])
    }
    sums <- round(colSums(cm2),4)
    p <- plot_ly(type = 'scatterpolar', mode = "lines")
    i = 0
    for(j in seq(ncol(cm),nrow(cm),ncol(cm))){
      i = i+1
      k = j+1
      p<-add_trace(p,r = sums[(j-ncol(cm)+1):j], mode = "markers", theta = classes, fill = 'toself', fillcolor = adjustcolor(c("Red", "Green")[i], alpha.f = 0.5), name = models[i], marker = list(symbol = "square", size = 8, color = c("Red", "Green")[i]), hovertemplate = paste('<i>Count </i>: %{r} <br> <i>Miss. as </i>: %{theta}'))
    }
    #mittel <- colMeans(matrix(sums, ncol = ncol(cm), byrow = TRUE))
    if(input$valueswitch == FALSE){
      mittel <- round(colSums(mittel)/length(input$models),0)
    }
    else{
      mittel <- round(colSums(mittel)/length(input$models),4)
    }
    p <- add_trace(p, r = c(mittel, mittel[1]), mode = "lines", line = list(color = "Blue"), theta = c(classes, classes[1]), name = "Average", hovertemplate = paste('<i>Average Count </i>: %{r} <br> <i>Miss. as </i>: %{theta}'))
    p
  })
  
  output$radarchartdeltaplot <- renderPlotly({radarchartdeltaplot()})
  
  heatmapplot <- reactive({
    if(length(input$models) != 1){return()}
    if(input$valueswitch == TRUE){
      data <- round(selected_models_percentage(),3)}
    else{
      data <- selected_models()}
    classes <- selected_classes()
    rownames(data) <- classes
    colnames(data) <- rev(classes)
    new_data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    new_data <- as.matrix(new_data)
    norm_data <- as.matrix(data)
    max_diag <- max(diag(norm_data)) # Finde Max-Wert auf Diagonalen
    min_diag <- min(diag(norm_data)) # Finde Min-Wert auf Diagonalen
    max_not_diag <- max(c(data[upper.tri(norm_data)],data[lower.tri(norm_data)])) # Finde Max-Wert außerhalb der Diagonalen
    min_not_diag <- min(c(data[upper.tri(norm_data)],data[lower.tri(norm_data)])) # Finde Min-Wert außerhalb der Diagonalen
    if ((0.8*min_diag) > max_not_diag) {
      diag(norm_data) <- ((diag(norm_data)-min_diag)/(max_diag - min_diag))
      norm_data[upper.tri(norm_data)] <- (norm_data[upper.tri(norm_data)] - min_not_diag) / (max_not_diag - min_not_diag) - 1
      norm_data[lower.tri(norm_data)] <- (norm_data[lower.tri(norm_data)] - min_not_diag) / (max_not_diag - min_not_diag) - 1 
    } else {
      if (max_diag >= max_not_diag) {
        norm_data <- ((norm_data-min_diag)/(max_diag-min_diag))
      } else {
        if (min_diag <= min_not_diag) {
          norm_data <- ((norm_data-min_diag)/(max_not_diag-min_diag))
        } else {
          norm_data <- ((norm_data-min_not_diag)/(max_not_diag-min_not_diag))        
        }
      }
    }
    anno_x <- NULL
    anno_y <- NULL
    for (i in 1:(ncol(data))) {
      for (j in 1:(ncol(data))) {
        anno_x <- append(anno_x, classes[i])
      }
    }
    
    for (i in 1:(ncol(data))) {
      for (j in 1:(ncol(data))) {
        anno_y <- append(anno_y, classes[j])
      }
    }
    
    # Farbskala
    col <- brewer.pal(n = 9, name = 'Blues')
    
    p <- plot_ly(x = rownames(norm_data), y=colnames(norm_data), z=apply(norm_data, 2, rev), type="heatmap", colors=col, hovertemplate = paste('<i>True Value </i>: %{x}<br><i>Pred. Value </i>: %{y},<br><i>Confusion Score </i>: %{z:.3f}<extra></extra>')) %>%
      add_annotations(x=anno_x, y=anno_y, text = new_data, showarrow = FALSE, font=list(color='black'))
    p
  })
  
  output$heatmap <- renderPlotly({heatmapplot()})
 
  # heatmapplot_comparison <- reactive({
  #   if(is.null(input$models)) {return()}
  #   
  #   if(input$valueswitch == TRUE) {
  #     data <- round(comparisondata_percentage(),3)
  #   } else {
  #     data <- comparisondata()
  #   }
  #   
  #   classes <- selected_classes()
  #   rownames(data) <- c(classes, paste(classes, classes))
  #   colnames(data) <- rev(classes)
  #   #new_data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  #   #new_data <- as.matrix(new_data)
  #   norm_data <- as.matrix(data)
  #   data_def <- as.matrix(norm_data[1:ncol(data),])
  #   data_comp <- as.matrix(norm_data[(ncol(data)+1):(2*ncol(data)),])
  #   norm_data <- data_def - data_comp
  #   norm_data <- as.data.frame(norm_data)
  #   rownames(norm_data) <- classes
  #   colnames(norm_data) <- rev(classes)
  #   norm_data <- as.matrix(norm_data)
  #   norm_data <- round(norm_data, 3)
  #   abs_norm_data <- data.frame(lapply(norm_data, as.character), stringsAsFactors=FALSE)
  #   abs_norm_data <- as.matrix(abs_norm_data)
  #   
  #   max_element <- max(norm_data)
  #   min_element <- min(norm_data)
  #   if (min(norm_data) < 0) {
  #     if (abs(max_element) >= abs(min_element)) {
  #       min_element = max_element * (-1)
  #     } else {
  #       max_element = min_element * (-1)
  #     }
  #     norm_data[norm_data >= 0] <- ((norm_data[norm_data >= 0])/max_element)
  #     norm_data[norm_data < 0] <- ((norm_data[norm_data < 0])/min_element)*(-1)      
  #   } else {
  #     norm_data <- (norm_data - min_element)/(max_element - min_element)
  #   }
  #   
  #   anno_x <- NULL
  #   anno_y <- NULL
  #   for (i in 1:(ncol(data))) {
  #     for (j in 1:(ncol(data))) {
  #       anno_x <- append(anno_x, classes[i])
  #     }
  #   }
  #   
  #   for (i in 1:(ncol(data))) {
  #     for (j in 1:(ncol(data))) {
  #       anno_y <- append(anno_y, classes[j])
  #     }
  #   }
  #   
  #   # Farbskala
  #   
  #   col_red <- rev(brewer.pal(n = 9, name = 'Reds'))
  #   col_red <- col_red[-(7:9)]
  #   col_white <- "#FFFFFF"
  #   col_green <- brewer.pal(n = 9, name = 'Greens')
  #   col_green <- col_green[-(1:3)]
  #   col <- c(col_red, col_white, col_green)
  #   print(col)
  #   
  #   p <- plot_ly(x = rownames(norm_data), y=colnames(norm_data), z=apply(norm_data, 2, rev), type="heatmap", 
  #   colors = col, zauto = F, zmin = -1, zmax = 1) %>%
  #     add_annotations(x=anno_x, y=anno_y, text = abs_norm_data, showarrow = FALSE, font=list(color='black'))
  #   p    
  # })
  
  heatmapplot_comparison <- reactive({
    if(is.null(input$models)) {return()}
    
    if(input$valueswitch == TRUE) {
      data <- round(comparisondata_percentage(),3)
    } else {
      data <- comparisondata()
    }
    
    classes <- selected_classes()
    rownames(data) <- c(classes, paste(classes, classes))
    colnames(data) <- rev(classes)
    #new_data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    #new_data <- as.matrix(new_data)
    norm_data <- as.matrix(data)
    data_def <- as.matrix(norm_data[1:ncol(data),])
    data_comp <- as.matrix(norm_data[(ncol(data)+1):(2*ncol(data)),])
    norm_data <- data_def - data_comp
    norm_data <- as.data.frame(norm_data)
    rownames(norm_data) <- classes
    colnames(norm_data) <- rev(classes)
    norm_data <- as.matrix(norm_data)
    norm_data <- round(norm_data, 3)
    abs_norm_data <- data.frame(lapply(norm_data, as.character), stringsAsFactors=FALSE)
    abs_norm_data <- as.matrix(abs_norm_data)
    
    max_element <- max(norm_data)
    min_element <- min(norm_data)
    if (min(norm_data) < 0) {
      if (abs(max_element) >= abs(min_element)) {
        min_element = max_element * (-1)
      } else {
        max_element = min_element * (-1)
      }
      norm_data[norm_data >= 0] <- ((norm_data[norm_data >= 0])/max_element)
      norm_data[norm_data < 0] <- ((norm_data[norm_data < 0])/min_element)*(-1)      
    } else {
      norm_data <- (norm_data - min_element)/(max_element - min_element)
    }
    
    diag(norm_data) <- diag(norm_data) * (-1)
    
    anno_x <- NULL
    anno_y <- NULL
    for (i in 1:(ncol(data))) {
      for (j in 1:(ncol(data))) {
        anno_x <- append(anno_x, classes[i])
      }
    }
    
    for (i in 1:(ncol(data))) {
      for (j in 1:(ncol(data))) {
        anno_y <- append(anno_y, classes[j])
      }
    }
    
    # Farbskala
    
    col_red <- rev(brewer.pal(n = 9, name = 'Reds'))
    col_red <- col_red[-(7:9)]
    col_white <- "#FFFFFF"
    col_green <- brewer.pal(n = 9, name = 'Greens')
    col_green <- col_green[-(1:3)]
    col <- c(col_red, col_white, col_green)
    
    p <- plot_ly(x = rownames(norm_data), y=colnames(norm_data), z=apply(norm_data, 2, rev), type="heatmap", 
                 colors = col, zauto = F, zmin = -1, zmax = 1, hovertemplate = paste('<i>True Value </i>: %{x}', '<br><i>Pred. Value </i>: %{y}', '<br><i>Comp. Index </i>: %{z:.3f}<extra></extra>')) %>%
      add_annotations(x=anno_x, y=anno_y, text = abs_norm_data, showarrow = FALSE, font=list(color='black'))
    p    
  })
  
  
  
  output$heatmap_comparison <- renderPlotly({heatmapplot_comparison()})
  
  chorddiagrammplot <- reactive({
    if(length(input$models) != 1){return()}
    if(input$valueswitch == TRUE){
      cm <- selected_models_missclassified_percentage()}
    else{
      cm <- selected_models_missclassified()}
    cm <- t(as.matrix(cm))
    rownames(cm) <- selected_classes()
    colnames(cm) <- rownames(cm)
    chorddiag(cm, type = "directional", showTicks = T, groupnameFontsize = 14, groupnamePadding = 30, margin = 90, groupColors = unname(alphabet()))
  })
  output$chorddiagramm <- renderChorddiag({chorddiagrammplot()})
  
  
  
  sankeyplot <- reactive({
    if(length(input$models) != 1){return()}
    cm <- selected_models()
    
    # Anzahl Klassen aus Konfusionsmatrix ermitteln
    no_classes <- ncol(cm)
    # Anzahl Modelle aus Konfusionsmatrix ermitteln
    no_models <- length(input$models)
    
    cm_used_model <- selected_models() # 1 ist abhängig vom ausgewÃ¤hlten Modell
    
    id <- c(1:(2*no_classes))
    labels <- selected_classes()
    labels <- append(labels, selected_classes())
    cm_model_totals <- colSums(cm_used_model) # Gesamtanzahl der jeweiligen Klassen ermitteln
    cm_model_totals <- append(cm_model_totals, cm_model_totals)
    
    # Farben
    #col <- distinctColorPalette(no_classes, altCol=T)
    col <- unname(alphabet()[1:no_classes])
    col <- append(col, col)
    
    # Gewichtete Knoten erstellen
    nodes <- as.data.frame(cbind(id, labels, cm_model_totals, col), stringsAsFactors = FALSE)
    colnames(nodes) <- c("id","label","weight", "color")
    rownames(nodes) <- c(1:(2*no_classes))
    nodes$id <- as.numeric(nodes$id)
    nodes$weight <- as.numeric(nodes$weight)
    
    # EintrÃ¤ge auf der Diagonalen der Konfusionsmatrix werden auf 0 gesetzt
    diag(cm_used_model) <- 0
    colnames(cm_used_model) <- c(1:no_classes)
    rownames(cm_used_model) <- c(1:no_classes)
    cm_used_model <- data.matrix(cm_used_model)
    
    # Gewichtete Kanten erstellen
    g <- graph.adjacency(cm_used_model, weighted=TRUE)
    edges <- get.data.frame(g)
    colnames(edges) <- c("to","from","weight")
    edges$to <- as.numeric(edges$to) + (ncol(selected_models_missclassified()) -1)
    edges$from <- as.numeric(edges$from) - 1
    edges <- transform(edges, color = col[from +1])
    edges <- edges[c("from", "to", "weight", "color")]
    edges <- transform(edges, label = paste("Class", to - (ncol(selected_models_missclassified()) -1)))
    #edges <- edges[order(edges$from),]   # Sortierung bringt nichts in Darstellung
    
    p <- plot_ly(
      type = "sankey",
      domain = list(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ".0f",
      valuesuffix = " missclassified",
      
      node = list(
        label = nodes$label, 
        color = nodes$color, 
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = edges$from, 
        target = edges$to, 
        value =  edges$weight 
      )
    ) %>% 
      layout(
        title = "Sankey Diagram for Missclassification per Class",
        font = list(
          size = 10
        ),
        xaxis = list(showgrid = F, zeroline = F),
        yaxis = list(showgrid = F, zeroline = F)
      )
    
    p
  })
  output$sankey <- renderPlotly({sankeyplot()})
  
  networkplot <- reactive({
    return()
    # cm <- selected_models_missclassified()
    # 
    # # Anzahl Klassen aus Konfusionsmatrix ermitteln
    # no_classes <- ncol(cm)
    # # Anzahl Modelle aus Konfusionsmatrix ermitteln
    # no_models <- length(input$models)
    # 
    # # Nodes
    # class_labels <- selected_classes()
    # class_totals_missclassified <- colSums(cm)
    # nodes <- as.data.frame(cbind(class_labels, cm_totals_missclassified), stringsAsFactors = FALSE)
    # colnames(nodes) <- c("label","weight")
    # rownames(nodes) <- c(1:no_classes)
    # nodes$weight <- as.numeric(nodes$weight)
    # 
    # # Farben
    # col <- distinctColorPalette(no_classes, altCol=T)
    # col <- append(col, col)
    # 
    # # EintrÃ¤ge auf der Diagonalen der Konfusionsmatrix werden auf 0 gesetzt
    # colnames(cm) <- c(1:no_classes)
    # rownames(cm_used_model) <- c(1:no_classes)
    # cm_used_model <- data.matrix(cm_used_model)
    # 
    # # Gewichtete Kanten erstellen
    # g <- graph.adjacency(cm_used_model, weighted=TRUE)
    # edges <- get.data.frame(g)
    # colnames(edges) <- c("to","from","weight")
    # edges$to <- as.numeric(edges$to) + (ncol(selected_models_missclassified()) -1)
    # edges$from <- as.numeric(edges$from) - 1
    # edges <- transform(edges, color = col[from +1])
    # edges <- edges[c("from", "to", "weight", "color")]
    # edges <- transform(edges, label = paste("Class", to - (ncol(selected_models_missclassified()) -1)))
    # #edges <- edges[order(edges$from),]   # Sortierung bringt nichts in Darstellung
  })
  
  output$network_plot <- renderPlotly({networkplot()})
  
  output$sunburst_data2 <- renderTable(sunburst_data())
  
  output$selected_models <- renderText(selected_models())
  
  #output$models <- renderText(input$models)
  
  lorenzplot <- reactive({
    if(is.null(data())){return ()}
    data <- selected_models()
    counts <- colSums(data)
    classes <- selected_classes()
    data <- as.data.frame(counts)
    data$names <- selected_classes()
    colnames(data) <- c("Count", "Class")
    newdata <- data[order(data$Count),]
    Sum <- cumsum(as.vector(newdata$Count))
    newdata$Sum <- Sum / length(input$models)
    xform <- list(categoryorder = "array",
                  categoryarray = newdata$Sum)
    b <- list(
      range = c(newdata$Class[1], newdata$Class[length(classes)])
    )
    p <- plot_ly(newdata, x = ~Class, y = ~Sum, type = 'scatter', mode = "lines", name = "Value") %>% layout(xaxis = xform) %>%
      add_trace(y = seq((sum(selected_models())/ncol(selected_models()))/length(input$models),sum(selected_models())/length(input$models), (sum(selected_models())/ncol(selected_models()))/length(input$models)), name = 'Uniform Distribution',mode = 'lines') %>%
      layout(xaxis = b)
    p
  })
  output$lorenzcurve <- renderPlotly({lorenzplot()})
  
  calculate_gini <- reactive({
    data <- selected_models()
    counts <- colSums(data)
    classes <- selected_classes()
    data <- as.data.frame(counts)
    data$names <- selected_classes()
    colnames(data) <- c("Count", "Class")
    newdata <- data[order(data$Count),]
    gini_score <- round(Gini(newdata$Count), 4)
    gini_score
  })
  
  
  
  histogramplot <- reactive({
    data <- selected_models()
    counts <- colSums(data) / length(input$models)
    classes <- selected_classes()
    p <- plot_ly() %>%
      add_trace(y=counts, x=classes, type = "bar", name = "Value") %>%
      add_trace(x = classes, y = (sum(data)/ncol(data))/length(input$models), type = "scatter", mode = "lines", name = "Mean")
    p
  })
  output$histogram <- renderPlotly({histogramplot()})
  
  
  
  treemapplot <- reactive({
    if(length(input$models) != 1){return()}
    if(input$valueswitch == TRUE){
      data <- selected_models_missclassified_percentage()}
    else{
      data <- selected_models_missclassified()}
    classes <- selected_classes()
    rownames(data) <- classes
    colnames(data) <- classes
    
    gr <- NULL
    sub <- NULL
    val <- NULL
    
    for (i in 1:ncol(selected_models())) {
      for (j in 1:ncol(selected_models())) {
        if (data[j,i] != 0) {
          data[j,i]
          gr <- append(gr, classes[i])
          sub <- append(sub, sprintf("Missclassified As %s", classes[j]))
          val <- append(val, data[j,i])     
        }
      }
    }
    
    group=gr
    subgroup=sub
    value=val
    data=data.frame(group,subgroup,value)
    
    p=treemap(data,
              index=c("group","subgroup"),
              vSize="value",
              type="index",
              palette=alphabet()
    )            
    
    inter=d3tree2( p ,  rootname = "All" )
    inter
    
  })
  output$treemap <- renderD3tree2({treemapplot()})
  

  errorlineplot <- reactive({
    if(is.null(input$models)){return()}
    data <- selected_models()
    missclassified_data <- selected_models_missclassified()
    chunk <- ncol(data)
    n <- nrow(data)
    r  <- rep(1:length(input$models),each=chunk)[1:n]
    d <- split(data,r)
    d_miss <- split(missclassified_data, r)
    e <- c()
    f <- c()
    for(i in seq(1, length(d))){
      e <- c(e, sum(unlist(d[i])))
      f <- c(f, sum(unlist(d_miss[i])))
    }
    acc <- (e-f)/e
    models <- input$models
    p <- plot_ly(x = models, y = acc, type = 'scatter', mode = 'lines+markers', name = "Accuracy") %>%
      add_trace(x = models, y = max(colSums(data)) / sum(data), type = "scatter", mode = "lines", name = "Baseline") %>%
      add_trace(x = models, y = 1/ncol(data), type = "scatter", mode = "lines", name = "Random") %>%
      layout(xaxis = list(tickvals = models, tickmode = "array"))
  })
  output$errorline <- renderPlotly({errorlineplot()})
  
  boxplot_calculation <- reactive({
    if(is.null(input$models)){return()}
    cm <- selected_models()
    cm_col <- vector(mode="numeric")
    for(i in seq(1, length(input$models))) {
      cm_col <- append(cm_col, colSums(cm[((i*ncol(cm))-(ncol(cm)-1)):(((i*ncol(cm))-(ncol(cm)-1))+(ncol(cm)-1)),]))
    }
    cm_row <- rowSums(cm)
    precision <- cm/cm_row
    recall <- cm/cm_col
    f1 <- 2 * ((precision * recall) / (precision+recall))
    results <- data.frame(Score = numeric(), Model = numeric(), Metric = character(), stringsAsFactors = FALSE)
    for(i in seq(1,length(input$models))) {
      vector_score <- vector(mode="numeric")
      vector_model <- vector(mode="character")
      vector_metric <- vector(mode="character")
      for(j in seq(1,ncol(cm))) {
        vector_score <- append(vector_score, round(precision[((i*ncol(cm))-(ncol(cm)-1))+(j-1),j], digits=4))
        vector_model <- append(vector_model, as.character(input$models[i]))
        vector_metric <- append(vector_metric, "Precision")
      }
      vector_score[is.nan(vector_score)] <- 0
      df_model <- data.frame(Score = vector_score, Model = vector_model, Metric = vector_metric, stringsAsFactors = FALSE)
      results <- rbind(results, df_model)
    }
    for(i in seq(1,length(input$models))) {
      vector_score <- vector(mode="numeric")
      vector_model <- vector(mode="character")
      vector_metric <- vector(mode="character")
      for(j in seq(1,ncol(cm))) {
        vector_score <- append(vector_score, round(recall[((i*ncol(cm))-(ncol(cm)-1))+(j-1),j], digits=4))
        vector_model <- append(vector_model, as.character(input$models[i]))
        vector_metric <- append(vector_metric, "Recall")
      }
      vector_score[is.nan(vector_score)] <- 0
      df_model <- data.frame(Score = vector_score, Model = vector_model, Metric = vector_metric, stringsAsFactors = FALSE)
      results <- rbind(results, df_model)
    }
    for(i in seq(1,length(input$models))) {
      vector_score <- vector(mode="numeric")
      vector_model <- vector(mode="character")
      vector_metric <- vector(mode="character")
      for(j in seq(1,ncol(cm))) {
        vector_score <- append(vector_score, round(f1[((i*ncol(cm))-(ncol(cm)-1))+(j-1),j], digits=4))
        vector_model <- append(vector_model, as.character(input$models[i]))
        vector_metric <- append(vector_metric, "F1")
      }
      vector_score[is.nan(vector_score)] <- 0
      df_model <- data.frame(Score = vector_score, Model = vector_model, Metric = vector_metric, stringsAsFactors = FALSE)
      results <- rbind(results, df_model)
    }
    results
  })
  
  boxplotplot <- reactive({
    if(is.null(input$models)){return()}
    results <- boxplot_calculation()
    p <- plot_ly(results, y = ~Score, x = ~Model, color=~Metric, type = "box") %>%
      layout(boxmode = "group", yaxis = list(title = "Score over all classes"), xaxis = list(tickvals = input$models, tickmode = "array"))
    p
  })
  
  output$boxplot <- renderPlotly({boxplotplot()})
  
  acc_std_plot <- reactive({
    if(is.null(input$models)){return()}
    data <- selected_models()
    missclassified_data <- selected_models_missclassified()
    chunk <- ncol(data)
    n <- nrow(data)
    r  <- rep(1:length(input$models),each=chunk)[1:n]
    d <- split(data,r)
    d_miss <- split(missclassified_data, r)
    e <- c()
    f <- c()
    for(i in seq(1, length(d))){
      e <- c(e, sum(unlist(d[i])))
      f <- c(f, sum(unlist(d_miss[i])))
    }
    acc <- (e-f)/e
    
    cm <- selected_models()
    cm_row <- rowSums(cm)
    precision <- cm/cm_row
    results <- c()
    for(i in seq(1,length(input$models))) {
      vector_score <- vector(mode="numeric")
      for(j in seq(1,ncol(cm))) {
        vector_score <- append(vector_score, round(precision[((i*ncol(cm))-(ncol(cm)-1))+(j-1),j], digits=4))
      }
      vector_score <- sd(vector_score)
      results <- c(results, vector_score)
    }
    
    x <- list(
      title = "Standard deviation of recalls",
      titlefont = f
    )
    y <- list(
      title = "Overall Accuracy",
      titlefont = f
    )
    
    p <- plot_ly(x = results, y = acc, type = "scatter", mode = "markers", color = input$models, colors = unname(alphabet()[1:length(input$models)]), marker = list(size = 12), hovertemplate = paste('<i>Accuracy</i>: %{y}', '<br><i>1-Std</i>: %{x}'))%>%
      layout(xaxis = x, yaxis = y)
    p
  })
  
  output$acc_std_plot <- renderPlotly({acc_std_plot()})
  
  observeEvent(modelnames(), {
    available_models <- modelnames()
    updatePickerInput(session, "models", choices = available_models, selected = available_models)
  })
  
  observeEvent(modelnames(), {
    available_models <- modelnames()
    updatePickerInput(session, "defaultmodel", choices = available_models, selected = available_models[1])
  })
  
  observeEvent(modelnames(), {
    available_models <- modelnames()
    updatePickerInput(session, "comparingmodel", choices = available_models, selected = available_models[2])
  })

  observeEvent(data(), {
    available_classes <- classnames()
    updatePickerInput(session, "classes", choices = available_classes , selected = NULL)
  })
  

  
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
  })
  
  output$model_comparison_table <- DT::renderDataTable({
    if(is.null(data())){return ()}
    comparisondata()
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(data())){return ()}
    #data()[1:10,]
    selected_models()
  }, filter = "top")

}

shiny::shinyApp(ui, server)

