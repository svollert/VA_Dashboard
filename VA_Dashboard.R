# treemap
# d3treeR

#Warning in install.packages :
#package 'chorddiag' is not available (for R version 3.6.1)
#Warning in install.packages :
#package 'd3treeR' is not available (for R version 3.6.1)

# install.packages("devtools")
# devtools::install_github("mattflor/chorddiag")
# install.packages("ggraph")
# install.packages("randomcoloR")
# install.packages("visNetwork")
# devtools::install_github("d3treeR/d3treeR")
# install.packages("gridSVG")


# install.packages("bs4Dash")
# install.packages("shinyWidgets")
# install.packages("DT")
# install.packages("plotly")
# 
# install.packages("RColorBrewer")
# install.packages("scanstatistics")
# install.packages("visNetwork")
# install.packages("chorddiag")
# 
# install.packages("ggraph")
# install.packages("randomcoloR")
# install.packages("treemap")
# install.packages("d3treeR")
# install.packages("rsconnect")
# 
# install.packages("igraph")
# install.packages("BBmisc")
# install.packages("dplyr")
# install.packages("DescTools")
# install.packages("pals")


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
library(class)
library(matrixStats)
library(shinyjs)


ui = bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = FALSE,
  title = "ML-ModelExplorer",
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
                               HTML("You can collapse the sidebars <br/> by clicking on the icons")
                             )
                           ))),
  
  
  sidebar = bs4DashSidebar(skin = "light",
                           status = "primary",
                           title = "ML-ModelExplorer",
                           brandColor = "primary",
                           bs4SidebarMenu(
                             bs4SidebarHeader("Dashboards"),
                             bs4SidebarMenuItem("Model Overview", tabName = "dashboard1", icon = "chart-bar"),
                             bs4SidebarMenuItem("Model Details", tabName = "dashboard2", icon = "microscope"),
                             bs4SidebarMenuItem("Model Comparison", tabName = "modelcomparison", icon = "balance-scale"),
                             bs4SidebarMenuItem("Data Properties", tabName = "dataproperties", icon = "cogs"),
                             bs4SidebarMenuItem("Information", tabName = "information", icon = "info"))),
  
  controlbar = bs4DashControlbar(skin = "light",
                                 title = "Controlbar",
                                 width=400,
                                 h5(helpText(HTML("<br>Load Confusion Matrices"))),
                                 h6(helpText("Choose separator")),
                                 pickerInput(inputId = "sep", label = NULL, choices = c(Comma=",", Semicolon=";", Tab="\t", Space=" "), selected = ",", multiple = FALSE),
                                 h5(helpText("Select file")),
                                 h6(helpText(HTML("Labels in columns or rows?"))),
                                 switchInput("column_row_switch", label=NULL, value=TRUE, onLabel = "Columns", offLabel = "Rows", onStatus = "primary", offStatus = NULL),
                                 fileInput("file", accept = c(".csv"), label = NULL, buttonLabel = "Search"),
                                 h6(HTML("<hr>")),
                                 h5(helpText("Load model names")),
                                 h6(helpText("Choose separator")),
                                 pickerInput(inputId = "sep2", label = NULL, choices = c(Newline = "\n", Comma=",", Semicolon=";", Tab="\t", Space=" "), selected = "\n", multiple = FALSE),
                                 h6(helpText("Select file")),
                                 fileInput("modelnames", accept = c(".txt"), label = NULL, buttonLabel = "Search"),
                                 h6(HTML("<hr>")),
                                 h5(helpText("Select Models")),
                                 pickerInput(inputId = "models",
                                             label = NULL,
                                             choices = "",
                                             options = pickerOptions(actionsBox = TRUE,
                                                                     showTick = TRUE,
                                                                     size = 10,
                                                                     selectedTextFormat = "count > 3"),
                                             multiple = TRUE),
                                 h5(helpText("Exclude irrelevant Classes")),
                                 uiOutput("classlimit"),
                                 h6(HTML("<hr>")),
                                 h5(helpText("Absolute or percentage values?")),
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
                                            
                                            fluidRow(bs4TabCard(id = "Distribution_Error_Tab", title = "Per-model Metrics Plots", width = 8, closable = FALSE, status = "primary", tabStatus = "secondary", maximizable = TRUE,
                                                                bs4TabPanel(tabName = "Model Rank", plotlyOutput("model_rank_plot")),
                                                                bs4TabPanel(tabName = "Model Accuracies", plotlyOutput("errorline")),
                                                                bs4TabPanel(tabName = "Boxplot", plotlyOutput("boxplot", width = "100%")),
                                                                bs4TabPanel(tabName = "Metric Info", HTML("<ul><li>Model Rank: Score between 0 and 1, where Score near 0 corresponds to a weak model and Score near 1 corresponds to a good model.<br> The score is calculated by Macro-Avg. Recall Class Imbalances <li>Model Accuracy: Accuracy of the model. Amount of correctly classified samples in relation to all samples. 
                                                                                                          <li>Avg. Accuracy: Average Accuracy across all models. <li>Baseline Accuracy: Accuracy of always predicting the most frequent class. <li>Random Accuracy: Accuracy of a completly random prediction.
                                                                                                          <li>Precision: Fraction of positive instances that were actually correct. <li>Recall: Percentage of correctly classified instances of a class. <li>F1-Score: Harmonic mean of precision and recall."))),
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
                                                               dropdownItem(name = "using a brush stroke on the axis"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can deselect the aspects by"),
                                                               dropdownItem(name = "clicking on the the axis")
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
                                            fluidRow(bs4Card(title = "Error Hierarchy Plot", plotlyOutput("sunburst_plot", width = "100%"), width = 12, closable = FALSE, status = "primary", maximizable = TRUE,
                                                             dropdownIcon = "question",
                                                             dropdownMenu = dropdownItemList(
                                                               dropdownItem(name = "You can drill down/up by"),
                                                               dropdownItem(name = "clicking on the panels"),
                                                               dropdownItem(name = HTML("<br>")),
                                                               dropdownItem(name = "You can hover over the panel"),
                                                               dropdownItem(name = "to show detailed information")
                                                             )))),
                                 
                                 bs4TabItem(tabName = "modelcomparison",
                                            fluidRow(bs4Card(title = "Select Reference Model", width = 3, status = "primary", collapsible = TRUE, collapsed = FALSE, closable = FALSE,
                                                             pickerInput(inputId = "defaultmodel",
                                                                         label = NULL,
                                                                         choices = "",
                                                                         multiple = FALSE)),
                                                     bs4Card(title = "Select Comparing Model", width = 3, status = "primary", collapsible = TRUE, collapsed = FALSE, closable = FALSE,
                                                             pickerInput(inputId = "comparingmodel",
                                                                         label = NULL,
                                                                         choices = "",
                                                                         multiple = FALSE))),
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
                                            fluidRow(bs4Card(title = "Select Model for Detailed View", width = 12, status = "primary", collapsible = TRUE, collapsed = FALSE, closable = FALSE,
                                                             pickerInput(inputId = "detailedmodel",
                                                                         label = NULL,
                                                                         choices = "",
                                                                         multiple = FALSE))),
                                            
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
                                                               dropdownItem(name = "to show detailed information"),
                                                               dropdownItem(name = HTML("<hr>")),
                                                               dropdownItem(name = "You can click on the heading of the box"),
                                                               dropdownItem(name = "to return to the original view")
                                                             )))),
                                 
                                 bs4TabItem(tabName = "dataproperties",
                                            h2("Data Properties"),
                                            fluidRow(bs4InfoBoxOutput("nomodels_box", width = 2),
                                                     bs4InfoBoxOutput("samples_box", width = 2),
                                                     bs4InfoBoxOutput("gini_all_prop", width = 2)),
                                            fluidRow(bs4Card(title = "Lorenz Curve", plotlyOutput("lorenzcurve"), width = 6, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE),
                                                     bs4Card(title = "Class Histogramm", plotlyOutput("histogram"), width = 6, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE)),
                                            fluidRow(bs4Card(title = "Tabular Plot", DT::dataTableOutput(outputId = "table"), width = 12, closable = FALSE, collapsible = TRUE))),
                                 bs4TabItem(tabName = "information",
                                            h2("Dashboard Information"),
                                            fluidRow(bs4Card(title = "Dashboard Information", "A major challenge during the development of machine learning systems is the large number of models resulting from testing different model types, parameters, or feature subsets. The common approach of selecting the best model using one overall metric does not necessarily find the most suitable model for a given application, since it ignores the different effects of class confusions. Expert knowledge is key to evaluate, understand and compare model candidates and hence to control the training process. This dashboard addresses the research question of how we can support experts in the evaluation and selection of, and the reasoning about, machine learning models. ML-ModelExplorer is proposed -- an explorative, interactive, and model-agnostic approach utilising the confusion matrices. It enables machine learning and domain experts to conduct a thorough and efficient evaluation of multiple models by taking overall metrics, per-class errors, and individual class confusions into account. For detailed information about the components, please see the information provided below.", width = 12)),
                                            fluidRow(bs4Card(title = "Explanation Video", "To be uploaded soon.", width = 6),
                                                     bs4Card(title = "Conference Paper", "To be uploaded soon.", width = 6)),
                                            h2("Model overview information"),
                                            fluidRow(bs4Card(title = "Per-model metrics plot", "This set of plots gives an overview of generally good or weak models, hence serving as a starting point to detect potential model candidates for further refinement or for the exclusion from the training process. The per-model metrics can be viewed at different levels of granularity with the following subplots: a list of ranked and grouped models, a line plot with the model accuracies, and a box plot with the dispersions of recall, precision and F1-score.
                                                             In the model rank subplot, the models are ranked and grouped into strong, medium and weak models. For the ranking of the models, the underlying assumption is, that a good model has a high accuracy and a low classification imbalance, i.e. all classes have a similarly high detection rate. In the second subplot the models' accuracies are contrasted with each other in order to a allow a coarse comparison of the models. In addition the accuracies are shown in reference to a virtual random classifier, randomly classifying each instance with equal probability for each class, and to a baseline classifier, assigning all instances to the majority class.
                                                             In the third subplot, the models are contrasted using box plots showing the dispersions of the per-class metrics. Box plots positioned at the top indicate stronger models while the height indicates a model's variability over the classes.", width = 6, height = 350),
                                                     bs4Card(title = "Model similarity plot", "The (dis-)similarities between models are visible in this plot. For each model the overall accuracy and the standard deviation of the true prediction rates are extracted, allowing to present all models in a 2D-scatter plot. Similar models are thereby placed close to each other, where multiple similar models might reveal clusters. Weak, strong, or models with highly different results become obvious as outliers.", width = 6, height = 350)),
                                            fluidRow(bs4Card(title = "Per-class errors query view", "This view shows the per-class errors for all models and allows for refined queries for models or classes. The per-class errors are mapped to a parallel coordinates which in general show multi-dimensional relations line segments between parallel axes. A brushing operation allows for the highlighting of value ranges. In the per-class errors query view, the first axis shows the models and each class is mapped to one axis with low per-class errors at the bottom. For each model, line segments connect the per-class errors.", width = 6, height = 260),
                                                     bs4Card(title = "Class error radar chart", "The per-class errors can be interactively analysed in a radar chart, where the errors are mapped to axes. Transparent colour encoding allows to distinguish overlapping models. The models' results can be rapidly contrasted, larger areas showing generally high per-class errors, and the shape indicating high or low errors on specific classes. The analysis can be incrementally refined by removing models from the plot.", width = 6, height = 260)),
                                            fluidRow(bs4Card(title = "Error hierarchy plot", "This plot allows to navigate through all errors per model and class in one view. The hierarchy of the overall errors for each model (1 - Macro-avg Recall), the per-class errors (1 - Recall), and the class confusions are accessible in a sun burst diagram. The errors at each level are ordered clockwise allowing to see the ranking.", width = 6, height = 260)),
                                            h2("Model details informaion"),
                                            fluidRow(bs4Card(title = "Confusion circle", "The classes are depicted by circle segments in one surrounding circle. The class confusions are shown with chords connecting the circle segments. The chords' widths encode the error between the classes. Individual classes can be highlighted and the detailed errors are shown on demand.", width = 6, height = 260),
                                                     bs4Card(title = "Confusion matrix", "The model's confusion matrix is shown in the familiar tabular way, with a colour gradient encoding the class confusions.", width = 6, height = 260)),
                                            fluidRow(bs4Card(title = "Bilateral confusion plot", "Misclassifications can be studied in an interactive Sankey diagram with one bar per class label on the left and predictions on the right. By rearranging and highlighting, the focus can be put on individual classes.", width = 6, height = 260),
                                                     bs4Card(title = "Confusion tree map", "A model's per-class errors are ranked in a tree map and can be further investigated by viewing how one selected error is composed of the individual class confusions. If a class is selected, the ranked misclassifications to the other classes are shown on the next level.", width = 6, height = 260)),
                                            h2("Model comparison information"),
                                            fluidRow(bs4Card(title = "Delta confusion matrix", "The class confusions of a comparing model can be contrasted to a reference model showing where the comparing model is superior and where it needs optimization. The difference between the class confusions is visible per cell with shades of green encoding where the reference is superior to the comparing model and red where the comparing model is superior, respectively.", width = 6, height = 260),
                                                     bs4Card(title = "Delta error radar chart", "The differences in the per-class errors of a comparing model w.r.t. a reference model or the average of all models is illustrated in a radar chart. The area and shape in the radar chart allows to rapidly draw conclusions about weak or strong accuracies on certain classes.", width = 6, height = 260))))))


server = function(input, output, session) {
  values_data <- reactiveValues(df = data.frame())
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){
      data <- read.table(file="https://raw.githubusercontent.com/svollert/VA_Dashboard/master/CNN_simple_mnist_kernel_size_strides_20epochs.csv", sep = input$sep, header = TRUE, stringsAsFactors = FALSE)
    }
    else{
      data <- read.table(file=file1$datapath, sep = input$sep, header = TRUE, stringsAsFactors = FALSE)
    }
    
    if(input$column_row_switch == FALSE){ # Wenn Switch auf Zeile steht
        print("Zeile")
        rows = nrow(data)
        cols = ncol(data)
        models = rows/cols
        
        start = seq(1, rows, cols)
        end = seq(cols, rows, cols)
        print(start)
        print(end)
        
        for(i in seq(1,models)){ # F?r jedes Modell (Sequenz ist eine Modelllaenge) transponiere die Confusion Matrix und speichere sie wieder im Data Dataframe
          data[start[i]:end[i],] <- t(data[start[i]:end[i],])}
      }
    else{
    }
    
    classes <- ncol(data)
    check <- nrow(data) %% classes
    format_plausible <- ifelse(check == 0, TRUE, FALSE)
    
    if(format_plausible == FALSE){
      sendSweetAlert(
        session,
        title = "Error!",
        text = "Please check the dimensions of your confusion matrizes! Restoring previous data set!",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#3085d6",
        html = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL)
      data <- values_data$df
      reset("file")
      #data <- read.table(file="https://raw.githubusercontent.com/svollert/VA_Dashboard/master/CNN_simple_mnist_kernel_size_strides_20epochs.csv", sep = input$sep, header = TRUE, stringsAsFactors = FALSE)
    }
    
    models <- nrow(data)/classes
    sums <- NULL
    
    for(i in seq(1,classes)){
      conf_1class <- data[,i] # get complete column of all consec. conf matrices
      
      for(j in seq(1,length(conf_1class),classes)){
        s <- sum( conf_1class[j : (j+classes-1)] ) # one column of one conf. matrix
        sums <- c(sums,s)
      }
    }
    
    #check class confusions for each model
    errors <- NULL
    for(i in seq(1,length(sums), models)){
      sums_1class <- sums[i:(i+models-1)]
      errors <- c(errors, which(sums_1class != sums_1class[1]))
    }
    
    if(length(errors) != 0){
      sendSweetAlert(
        session,
        title = "Error!",
        text = "Inconsistency in the confusion matrizes! Please check the results! Restoring previous data set!",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#3085d6",
        html = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL)
      data <- values_data$df
      reset("file")
      #data <- read.table(file="https://raw.githubusercontent.com/svollert/VA_Dashboard/master/CNN_simple_mnist_kernel_size_strides_20epochs.csv", sep = input$sep, header = TRUE, stringsAsFactors = FALSE)
    }
    
    values_data$df <- data
    return(data)
  })
  
  plotcolors <- reactive({
    plotcolors <- c(alphabet(), alphabet2())
  })
  
  modelnames <- reactive({
    modelnames <- input$modelnames
    if(is.null(modelnames) && is.null(data())){return()}
    if(is.null(input$file) && is.null(modelnames)) {
      models <- read.delim(file="https://raw.githubusercontent.com/svollert/VA_Dashboard/master/CNN_simple_mnist__models.txt", sep = input$sep2, header = FALSE, stringsAsFactors = FALSE)
      models <- unlist(models)
      models <- unname(models)
      models <- c(models)
      return(models)
    }
    if(is.null(modelnames)){
      models <- paste("Model", 1:(nrow(data())/ncol(data())))
      return(models)
    }
    models <- read.delim(file=modelnames$datapath, sep = input$sep2, header = FALSE, stringsAsFactors = FALSE)
    models <- unlist(models)
    models <- unname(models)
    models <- c(models)
    models
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
  
  
# Die Confusionmatrix der ausgew?hlten Modelle und Klassen wird zurueckgegeben (Absolutzahlen)
# Hierzu wird die Anzahl der notwendigen Zeilen und Spalten berechnet
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
  
  # 
  #
  selected_models_percentage <- reactive({
    if(is.null(modelnames)){return()}
    data <- selected_models()
    csum <- data %>%
      group_by(indx = gl(ceiling(nrow(data)/ncol(data)), ncol(data), nrow(data))) %>%
      summarise_each(list(sum))
    csum <- csum[,-1]
    csumdivide <- csum[rep(seq_len(nrow(csum)), each = ncol(data)), ]
    csumdivide[csumdivide == 0] <- 1
    data <- data/csumdivide
    data
  })
  
  # Die Confusionmatrix f?r ein ausgew?hltes Modell wird zur?ckgegeben 
  # Wird sp?ter f?r die Detailansicht eines Modells verwendet
  selected_models_single <- reactive({
    if(is.null(modelnames)){return()}
    model <- match(input$detailedmodel ,input$models)
    data <- selected_models()
    data <- data[(model*ncol(data)-(ncol(data)-1)):(model*ncol(data)),]
    data
  })
  
  # Die Confusionmatrix f?r ausgew?hlte Modelle wird zur?ckgegeben, wobei die Diagonalelemente 0 gesetzt wurden
  selected_models_missclassified <- reactive({
    if(is.null(modelnames)){return()}
    options <- modelnames()
    rows <- match(input$models,options)
    rows <- as.numeric(rows) * ncol(data())
    rows <- c(rep(rows, each = ncol(data())) - 0:(ncol(data()) - 1))
    rows <- sort(rows)
    data <- data()[rows,]
    a <- c(1:nrow(data))
    b <- rep(1:ncol(data), nrow(data) / ncol(data))
    d <- cbind(a,b)
    data[d] <- 0
    if(!is.null(input$classes)){
      delseq <- rep(classdelete(), each = length(input$models)) + seq(0,nrow(data)-1,ncol(data))
      data[-delseq, -classdelete()]
    }
    else{
      data
    }
  })
  
  # Die Confusionmatrix f?r ein ausgew?hltes Modell wird zur?ckgegeben, wobei die Diagonalelemente 0 gesetzt wurden
  selected_models_missclassified_single <- reactive({
    if(is.null(modelnames)){return()}
    model <- match(input$detailedmodel ,input$models)
    data <- selected_models_missclassified()
    data <- data[(model*ncol(data)-(ncol(data)-1)):(model*ncol(data)),]
    data
  })
  
  # Nur die wirklichen Prozente der Fehlklassifizierten
  selected_models_missclassified_percentage <- reactive({
    if(is.null(modelnames)){return()}
    data <- selected_models_percentage()
    a <- c(1:nrow(data))
    b <- rep(1:ncol(data), nrow(data) / ncol(data))
    d <- cbind(a,b)
    data[d] <- 0.0
    data
  })
  
  # Die Confusionmatrix von ausgew?hlten Modellen wird zur?ckgegeben, wobei die summierte prozentuale Fehlklassifizierung pro Spalte und Modell immer 1 ergeben muss
  # Falls eine Spaltensumme 0 ergibt, wird k?nstlich eine 1 f?r csumdivide eingef?hrt, um eine Division durch 0 zu verhindern
  selected_models_missclassified_percentage_per_class <- reactive({
    if(is.null(modelnames)){return()}
    data <- selected_models_missclassified()
    csum <- data %>%
      group_by(indx = gl(ceiling(nrow(data)/ncol(data)), ncol(data), nrow(data))) %>%
      summarise_each(list(sum))
    csum <- csum[,-1]
    csumdivide <- csum[rep(seq_len(nrow(csum)), each = ncol(data)), ]
    csumdivide[csumdivide == 0] <- 1
    data <- data/csumdivide
    data
  })

#-------------------------------------------------------------------------------------------------------------------------------------------
  # Die Confusionmatrix von zwei Modellen wird zur?ckgegeben, um diese sp?ter vergleichen zu k?nnen (Absolutzahlen)
  # Hierzu werden die dazugeh?rigen Zeilensequenzen berechnet, sodass die Confusionmatrix auf diese beiden Modelle eingeschr?nkt wird
  comparisondata <- reactive({
    if(is.null(data())){return ()}
    options <- modelnames()
    rows_d <- match(input$defaultmodel, options) 
    start <- (rows_d*ncol(selected_models())) - (ncol(selected_models())) + 1
    end <- rows_d*ncol(selected_models())
    rows_d <- seq(start, end)
    rows_c <- match(input$comparingmodel, options) 
    start <- (rows_c*ncol(selected_models())) - (ncol(selected_models())) + 1
    end <- rows_c*ncol(selected_models())
    rows_c <- seq(start, end)
    data_d <- selected_models()[rows_d,]
    data_c <- selected_models()[rows_c,]
    data <- rbind(data_d, data_c)
    data
  })
  
  # Die Confusionmatrix von zwei Modellen wird zur?ckgegeben, um diese sp?ter vergleichen zu k?nnen (Prozentzahlen)
  # Hierzu werden die dazugeh?rigen Zeilensequenzen berechnet, sodass die Confusionmatrix auf diese beiden Modelle eingeschr?nkt wird  
  comparisondata_percentage <- reactive({
    if(is.null(data())){return ()}
    options <- modelnames()
    rows_d <- match(input$defaultmodel, options) 
    start <- (rows_d*ncol(selected_models_percentage())) - (ncol(selected_models_percentage())) + 1
    end <- rows_d*ncol(selected_models_percentage())
    rows_d <- seq(start, end)
    rows_c <- match(input$comparingmodel, options) 
    start <- (rows_c*ncol(selected_models_percentage())) - (ncol(selected_models_percentage())) + 1
    end <- rows_c*ncol(selected_models_percentage())
    rows_c <- seq(start, end)
    data_d <- selected_models_percentage()[rows_d,]
    data_c <- selected_models_percentage()[rows_c,]
    data <- rbind(data_d, data_c)
    data
  })
#------------------------------------------------------------------------------------------------------------------------------------------- 
  
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
  
#--------------------------Ab hier Metriken f?r Infoboxen---------------------------------------------------------------
  output$samples_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Number of Samples"),
      subtitle = HTML("<h5>", samples()),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$nomodels_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Number of Models"),
      subtitle = HTML("<h5>", length(input$models)),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$acc_box_all <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5> Avg. Accuracy"),
      subtitle= HTML("<h5>", 0),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Avg. Accuracy"),
      footer = "Over all models",
      subtitle = HTML("<h5>", round((sum(selected_models()) - sum(selected_models_missclassified())) / sum(selected_models()), 4)),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$baseline_box_all <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5>Baseline Accuracy"),
      subtitle = HTML("<h5>", 0),
      footer = "Based on most frequent class",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Baseline Accuracy"),
      subtitle = HTML("<h5>", round(max(colSums(selected_models())) / sum(selected_models()),4)),
      footer = "Based on most frequent class",
      icon = "fas fa-calculator",
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
  
  output$precision_box_all <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5>Macro-Avg. Precision"),
      subtitle = HTML("<h5>", 0),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Macro-Avg. Precision"),
      subtitle = HTML("<h5>", kpi_precision()),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$recall_box_all <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5>Macro-Avg. Recall"),
      subtitle = HTML("<h5>", 0),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Macro-Avg. Recall"),
      subtitle= HTML("<h5>", kpi_recall()),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    )  
  })
  
  output$f1_box_all <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5>Macro-Avg. F1-Score"),
      subtitle = HTML("<h5>", 0),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Macro-Avg. F1-Score"),
      subtitle = HTML("<h5>", kpi_f1()),
      footer = "Over all models",
      icon = "fas fa-calculator",
      status = "primary"
    )  
  })
  
  output$gini_all <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5>Gini-Index"),
      subtitle = HTML("<h5>", 0),
      footer = "Based on class distribution",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Gini-Index"),
      subtitle= HTML("<h5>", calculate_gini()),
      footer = "Based on class distribution",
      icon = "fas fa-calculator",
      status = "primary"
    )  
  })
  
  output$gini_all_prop <- renderbs4ValueBox({
    if(is.null(input$models)){return(bs4ValueBox(
      value = HTML("<h5>Gini-Index"),
      subtitle = HTML("<5>", 0),
      footer = "Based on class distribution",
      icon = "fas fa-calculator",
      status = "primary"
    ))}
    bs4ValueBox(
      value = HTML("<h5>Gini-Index"),
      subtitle = HTML("<h5>", calculate_gini()),
      footer = "Based on class distribution",
      icon = "fas fa-calculator",
      status = "primary"
    )  
  })
  
  output$singleacc_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Accuracy"),
      if (length(input$models) == 0) {
        subtitle = HTML("<h5>", 0)
      } else {
        subtitle = HTML("<h5>", round((sum(selected_models_single()) - sum(selected_models_missclassified_single())) / sum(selected_models_single()),4))},
      footer = paste0("Based on ", input$detailedmodel),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$singlebaseacc_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Baseline Accuracy"),
      if (length(input$models) == 1) {
        subtitle = HTML("<h5>", 0)
      } else {
        subtitle = HTML("<h5>", round(max(colSums(selected_models_single())) / sum(selected_models_single()),4))
      },
      footer = paste0("Based on class distribution"),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$precision_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Macro Precision"),
      if (length(input$models) == 0) {
        subtitlee = HTML("<h5>", 0)
      } else {
        subtitle = HTML("<h5>", round(mean(diag(as.matrix(selected_models_single())) / rowSums(selected_models_single())), 4))},
      footer = paste0("Based on ", input$detailedmodel),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$recall_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Macro Recall"),
      if (length(input$models) == 0) {
        subtitle = HTML("<h5>", 0)
      } else {
        subtitle = HTML("<h5>", round(mean(diag(as.matrix(selected_models_single())) / colSums(selected_models_single())), 4))},
      footer = paste0("Based on ", input$detailedmodel),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$f1_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Macro F1-Score"),
      if (length(input$models) == 0) {
        subtitle = HTML("<h5>", 0)
      } else {
        subtitle = HTML("<h5>", round((2 * (mean(diag(as.matrix(selected_models_single())) / colSums(selected_models_single())) * (mean(diag(as.matrix(selected_models_single())) / rowSums(selected_models_single()))) / (mean(diag(as.matrix(selected_models_single())) / colSums(selected_models_single())) + (mean(diag(as.matrix(selected_models_single())) / rowSums(selected_models_single())))))),4))},
      footer = paste0("Based on ", input$detailedmodel),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
  output$kappa_box <- renderbs4ValueBox({
    bs4ValueBox(
      value = HTML("<h5>Kappa-Score"),
      if (length(input$models) == 0) {
        subtitle = HTML("<h5>", 0)
      } else {
        subtitle = HTML("<h5>", round((((sum(selected_models_single()) - sum(selected_models_missclassified_single())) / sum(selected_models_single())) - (sum((rowSums(selected_models_single()) / sum(selected_models_single())) * ((colSums(selected_models_single()) / sum(selected_models_single())))))) / (1 - (sum((rowSums(selected_models_single()) / sum(selected_models_single())) * ((colSums(selected_models_single()) / sum(selected_models_single())))))), 4))},
      footer = paste0("Based on ", input$detailedmodel),
      icon = "fas fa-calculator",
      status = "primary"
    )
  })
  
#--------------------------Ab hier Plots---------------------------------------------------------------
  # Die Daten werden f?r den Sunburstplot vorbereitet (insgesamt gibt es vier Stufen/Ebenen im Sunburst, wobei jede Ebene feingranularer wird)
  # Ziel ist es ein Dataframe zu erstellen, indem Parents, Labels und Values hinterlegt sind
  sunburst_data <- reactive({
    if(is.null(data())){return()}
    
    if (input$valueswitch == TRUE) {
      data <- selected_models_missclassified_percentage()
    } else {
      data <- selected_models_missclassified()
    }
    
    # Labels und Parents f?r die ersten drei Stufen festlegen
    labels <- "All"
    parents <- " "
    labels <- c(labels, input$models)
    parents <- c(parents, rep("All", length(input$models)))
    classes <- paste(rep(input$models, each=ncol(data)), selected_classes())
    labels <- c(labels, classes)
    parents <- c(parents, rep(input$models, each = ncol(data)))
    
    if(input$valueswitch==TRUE){
      # Values f?r die ersten drei Stufen festlegen
      values <- sum(data)/ (ncol(data)*nrow(data)) # Anzahl Fehlklassifizierungen ?ber alle Modelle
      values <- values * ncol(data)
      
      print(values)
      for (i in seq(1, length(input$models))) {
        values <- c(values, mean(colSums(data[((i*ncol(data))-(ncol(data)-1)):(i*ncol(data)), ]))) # Anzahl Fehlklassifizierungen je Modell
      }
      
      for (i in seq(1, length(input$models))) {
        for (j in seq(1, ncol(data))) {
          values <- c(values, mean(data[((i*ncol(data))-(ncol(data)-1)):(i*ncol(data)), j])) # Anzahl Fehlklassifizierungen je Modell und Klasse
        }
      }
      
    }else{
    # Values f?r die ersten drei Stufen festlegen
    values <- sum(data) # Anzahl Fehlklassifizierungen ?ber alle Modelle

    for (i in seq(1, length(input$models))) {
      values <- c(values, sum(colSums(data[((i*ncol(data))-(ncol(data)-1)):(i*ncol(data)), ]))) # Anzahl Fehlklassifizierungen je Modell
    }
    
    for (i in seq(1, length(input$models))) {
      for (j in seq(1, ncol(data))) {
        values <- c(values, sum(data[((i*ncol(data))-(ncol(data)-1)):(i*ncol(data)), j])) # Anzahl Fehlklassifizierungen je Modell und Klasse
      }
    }
    }
    # Labels, Parents und Values f?r die vierte Stufe festlegen
    vec_classes <- selected_classes()
    for (i in seq(1, length(input$models))) {
      for (j in seq(1, ncol(data))) {
        for (k in seq(1, ncol(data))) {
          if (data[((i*ncol(data))-(ncol(data)-1)+(k-1)),j] != 0) {
            labels <- c(labels, paste(input$models[i], vec_classes[j], vec_classes[k]))
            parents <- c(parents, paste(input$models[i], vec_classes[j]))
            values <- c(values, data[((i*ncol(data))-(ncol(data)-1)+(k-1)),j]) # Anzahl Fehlklassifizierungen je Classconfusion
          }
        }
      }
    }
    
    data <- as.data.frame(cbind(parents, labels, values))
    data
  })  
  
  # Ausgabe Sunburstplot, wobei die zuvor erstellten Sunburstdaten (Labels, Parents, Values) verwendet werden
  output$sunburst_plot <- renderPlotly({
    if(is.null(input$models) || length(input$models) != (nrow(selected_models()) / ncol(selected_models()))){return()}
    data <- sunburst_data()
    if(input$valueswitch==TRUE){
      p <- plot_ly(data, labels = ~labels, parents = ~parents, values = ~values, type="sunburst", maxdepth=4, marker = list(colors = c("#e0e0e0", unname(plotcolors()[1:max(length(input$models), length(colnames(selected_models())))]))), hovertemplate = paste('<b>%{label}</b><br>', 'Avg. Miss: %{value:.3p}', '<extra></extra>'))
    }else{
      p <- plot_ly(data, labels = ~labels, parents = ~parents, values = ~values, type="sunburst", maxdepth=4, marker = list(colors = c("#e0e0e0", unname(plotcolors()[1:max(length(input$models), length(colnames(selected_models())))]))), hovertemplate = paste('<b>%{label}</b><br>', 'Total Miss: %{value:.i}', '<extra></extra>'))
    }
    p
  })
  
  # Die Daten werden f?r den Parallelcoordinateplot vorbereitet
  parcoordplot <- reactive({
    if(is.null(input$models)){return()}
    
    if(input$valueswitch == TRUE){
      data <- selected_models_missclassified_percentage()
      axisformat <- '.3f'}
    else{
      data <- selected_models_missclassified()
      axisformat <- 'f'}
    
    models <- match(input$models,modelnames())
    classes <- selected_classes()
    cm <- data.frame(matrix(ncol=0,nrow=ncol(data)))
    for(i in seq(1, nrow(data), ncol(data))){
      cm <- cbind(cm, data[i:(i+ncol(data)-1), ])
    }
    sums <- t(matrix(colSums(cm), nrow = nrow(cm)))
    max_missclassified <- max(sums)
    data <- as.data.frame(cbind(models, sums))
    colnames(data) <- c("Models", classes)
    colr <- unname(plotcolors())
    
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
    
    p <- data %>%
      plot_ly(type = 'parcoords',
              line = list(color = ~Models,
                          colorscale = eval(parse(text = loop_color))),
              dimensions = eval(parse(text = loop_liste ))
      )
    p
  })
  
  # Ausgabe Parallelcoordinates, wobei der zuvor erstellten Plotlyplot verwendet wird
  output$parcoord <- renderPlotly({parcoordplot()})
  
  # Die Daten werden f?r das Radarchart vorbereitet
  radarchartplot <- reactive({
    if(is.null(input$models)){return()}
    if(input$valueswitch == TRUE){
      cm <- round(selected_models_missclassified_percentage(),4)
      hover <- '<i>Percentage </i>: %{r:.4p} <br><i>Miss. as </i>: %{theta}'}
    else{
      cm <- selected_models_missclassified()
      hover <- '<i>Count </i>: %{r} <br><i>Miss. as </i>: %{theta}'}
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
      p<-add_trace(p,r = sums[(j-ncol(cm)+1):j], mode = "markers", theta = classes, fill = 'toself', fillcolor = adjustcolor(unname(plotcolors()[i]), alpha.f = 0.5), name = input$models[i], marker = list(symbol = "square", size = 8, color = unname(plotcolors()[i])), hovertemplate = paste(hover))
    }
    p
  })
  
  # Ausgabe Radarchart, wobei der zuvor erstellten Plotlyplot verwendet wird
  output$radarchart <- renderPlotly({radarchartplot()})
  
  # Die Daten werden f?r das Delta-Radarchart vorbereitet
  radarchartdeltaplot <- reactive({
    if(is.null(input$models)){return()}
    if(input$valueswitch == TRUE){
      cm <- comparisondata_percentage()
      mittel <- selected_models_missclassified_percentage()
      hover <- '<i>Percentage </i>: %{r:.4p} <br><i>Miss. as </i>: %{theta}'
      avghover <- '<i>Percentage</i>: %{r:.4p} <br><i>Miss. as </i>: %{theta}'
    }
    else{
      cm <- comparisondata()
      mittel <- selected_models_missclassified()
      hover <- '<i>Count </i>: %{r} <br><i>Miss. as </i>: %{theta}'
      avghover <- '<i>Count</i>: %{r} <br><i>Miss. as </i>: %{theta}'
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
      p<-add_trace(p,r = sums[(j-ncol(cm)+1):j], mode = "markers", theta = classes, fill = 'toself', fillcolor = c(rgb(180/255,0,0,0.5), rgb(0,200/255,0,0.5))[i], name = models[i], marker = list(symbol = "square", size = 8, color = c("Red", "Green")[i]), hovertemplate = paste(hover))
    }
    #mittel <- colMeans(matrix(sums, ncol = ncol(cm), byrow = TRUE))
    if(input$valueswitch == FALSE){
      mittel <- round(colSums(mittel)/length(input$models),0)
    }
    else{
      mittel <- round(colSums(mittel)/length(input$models),4)
    }
    p <- add_trace(p, r = c(mittel, mittel[1]), mode = "lines", line = list(color = "Black", dash = "dot"), theta = c(classes, classes[1]), name = "Average", hovertemplate = paste(avghover))
    p
  })
  
  # Ausgabe Delta-Radarchart, wobei der zuvor erstellten Plotlyplot verwendet wird
  output$radarchartdeltaplot <- renderPlotly({radarchartdeltaplot()})
  
  # Die Daten werden f?r die Heatmap vorbereitet
  heatmapplot <- reactive({
    if(length(input$models) == 0){return()}
    model <- match(input$detailedmodel,input$models)
    if(input$valueswitch == TRUE){
      data <- round(selected_models_percentage(),3)}
    else{
      data <- selected_models()}
    data <- data[(model*ncol(data)-(ncol(data)-1)):(model*ncol(data)),]
    classes <- selected_classes()
    rownames(data) <- classes
    colnames(data) <- rev(classes)
    new_data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    new_data <- as.matrix(new_data)
    norm_data <- as.matrix(data)
    max_diag <- max(diag(norm_data)) # Finde Max-Wert auf Diagonalen
    min_diag <- min(diag(norm_data)) # Finde Min-Wert auf Diagonalen
    max_not_diag <- max(c(data[upper.tri(norm_data)],data[lower.tri(norm_data)])) # Finde Max-Wert au?erhalb der Diagonalen
    min_not_diag <- min(c(data[upper.tri(norm_data)],data[lower.tri(norm_data)])) # Finde Min-Wert au?erhalb der Diagonalen
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
    
    # Farbskala mit Abstufungen von Blau
    col <- brewer.pal(n = 9, name = 'Blues')
    
    p <- plot_ly(x = rownames(norm_data), y=colnames(norm_data), z=apply(norm_data, 2, rev), type="heatmap", colors=col, hovertemplate = paste('<i>True Value </i>: %{x}<br><i>Pred. Value </i>: %{y}<br><i>Confusion Score </i>: %{z:.3f}<extra></extra>')) %>%
      add_annotations(x=anno_x, y=anno_y, text = new_data, showarrow = FALSE, font=list(color='black')) %>%
      layout(xaxis = list(title = "True Value"), yaxis = list(title = "Pred. Value"))
    p
  })
  
  # Ausgabe Heatmap, wobei der zuvor erstellten Plotlyplot verwendet wird
  output$heatmap <- renderPlotly({heatmapplot()})
  
  # Die Daten werden f?r das Delta-Heatmap vorbereitet
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
      if ((min_element != 0) && (max_element != 0)) {
        norm_data <- (norm_data - min_element)/(max_element - min_element)       
      }
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
    
    # Farbskala mit sechs Abstufungen von Rot und sechs Abstufungen von Gr?n
    col_red <- rev(brewer.pal(n = 9, name = 'Reds'))
    col_red <- col_red[-(7:9)]
    col_white <- "#FFFFFF"
    col_green <- brewer.pal(n = 9, name = 'Greens')
    col_green <- col_green[-(1:3)]
    col <- c(col_red, col_white, col_green)
    
    p <- plot_ly(x = rownames(norm_data), y=colnames(norm_data), z=apply(norm_data, 2, rev), type="heatmap", 
                 colors = col, zauto = F, zmin = -1, zmax = 1, hovertemplate = paste('<i>True Value </i>: %{x}', '<br><i>Pred. Value </i>: %{y}', '<br><i>Comp. Index </i>: %{z:.3f}<extra></extra>')) %>%
      add_annotations(x=anno_x, y=anno_y, text = abs_norm_data, showarrow = FALSE, font=list(color='black')) %>%
      layout(xaxis = list(title = "True Value"), yaxis = list(title = "Pred. Value"))
    p    
  })
  
  # Ausgabe Delta-Heatmap, wobei der zuvor erstellten Plotlyplot verwendet wird
  output$heatmap_comparison <- renderPlotly({heatmapplot_comparison()})
  
  chorddiagrammplot <- reactive({
    if(length(input$models) == 0){return()}
    model <- match(input$detailedmodel,input$models)
    print(model)
    if(input$valueswitch == TRUE){
      cm <- selected_models_missclassified_percentage()}
    else{
      cm <- selected_models_missclassified()}
    cm <- cm[(model*ncol(cm)-(ncol(cm)-1)):(model*ncol(cm)),]
    cm <- t(as.matrix(cm))
    ticks <- round(max(colSums(cm))/40, 2)
    if(input$valueswitch == FALSE){
      ticks <- trunc(ticks / 10) * 10
      if(ticks == 0){
        ticks <- 1
      }
    }
    if(ticks == 0.00 && input$valueswitch == TRUE){
      ticks <- 0.002
    }
    rownames(cm) <- selected_classes()
    colnames(cm) <- rownames(cm)
    if(input$valueswitch == TRUE){
      cm <- round(cm,4)
    }
    chorddiag(cm, type = "directional", showTicks = T, tickInterval = ticks, groupnameFontsize = 14, groupnamePadding = 30, groupPadding = 3, margin = 50, groupColors = unname(plotcolors()))
  })
  output$chorddiagramm <- renderChorddiag({chorddiagrammplot()})
  
  
  
  sankeyplot <- reactive({
    if(length(input$models) == 0){return()}
    model <- match(input$detailedmodel,input$models)
    cm <- selected_models()
    cm <- cm[(model*ncol(cm)-(ncol(cm)-1)):(model*ncol(cm)),]
    # Anzahl Klassen aus Konfusionsmatrix ermitteln
    no_classes <- ncol(cm)
    # Anzahl Modelle aus Konfusionsmatrix ermitteln
    no_models <- length(input$models)
    
    cm_used_model <- selected_models() # 1 ist abh?ngig vom ausgewÃ¤hlten Modell
    cm_used_model <- cm_used_model[(model*ncol(cm_used_model)-(ncol(cm_used_model)-1)):(model*ncol(cm_used_model)),]
    
    id <- c(1:(2*no_classes))
    labels <- selected_classes()
    labels <- append(labels, selected_classes())
    cm_model_totals <- colSums(cm_used_model) # Gesamtanzahl der jeweiligen Klassen ermitteln
    cm_model_totals <- append(cm_model_totals, cm_model_totals)
    
    # Farben
    #col <- distinctColorPalette(no_classes, altCol=T)
    col <- unname(plotcolors()[1:no_classes])
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
    if(length(input$models) == 0){return()}
    model <- match(input$detailedmodel,input$models)
    if(input$valueswitch == TRUE){
      data <- selected_models_missclassified_percentage()}
    else{
      data <- selected_models_missclassified()}
    data <- data[(model*ncol(data)-(ncol(data)-1)):(model*ncol(data)),]
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
              palette=plotcolors()
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
    average_acc <- mean(acc)
    models <- input$models
    
    recall <- boxplot_calculation()
    recall <- recall[(1+nrow(data)):(2*nrow(data)),1]
    macro_avg_recall <- colMeans(matrix(recall, ncol(data)))
    
    p <- plot_ly(x = models, y = acc, type = 'scatter', mode = 'lines+markers', name = "Model Accuracy", hovertemplate = paste('<i>Model: </i> %{x}<br><i>Model Accuracy</i>: %{y:.4p}<extra></extra>')) %>%
      add_trace(x = models, y = macro_avg_recall, type = "scatter", mode = "lines+markers", name = "Macro-Avg. Recall", hovertemplate = paste('<i>Model: </i> %{x}<br><i>Macro-Avg. Recall</i>: %{y:.4p}<extra></extra>')) %>%
      add_trace(x = models, y = average_acc, type = "scatter", mode = "lines", name = "Avg. Accuracy", hovertemplate = paste('<i>Avg. Accuracy</i>: %{y:.4p}<extra></extra>')) %>%
      add_trace(x = models, y = max(colSums(data)) / sum(data), type = "scatter", mode = "lines", name = "Baseline Accuracy", hovertemplate = paste('<i>Baseline Accuracy</i>: %{y:.4p}<extra></extra>')) %>%
      add_trace(x = models, y = 1/ncol(data), type = "scatter", mode = "lines", name = "Random Accuracy", hovertemplate = paste('<i>Random Accuracy</i>: %{y:.4p}<extra></extra>')) %>%
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
        vector_metric <- append(vector_metric, "F1-Score")
      }
      vector_score[is.nan(vector_score)] <- 0
      df_model <- data.frame(Score = vector_score, Model = vector_model, Metric = vector_metric, stringsAsFactors = FALSE)
      results <- rbind(results, df_model)
    }
    results
  })
  
  boxplotplot <- reactive({
    if(is.null(input$models)){return()}
    t <- list(
      size = 14)
    results <- boxplot_calculation()
    p <- plot_ly(results, y = ~Score, x = ~Model, color=~Metric, type = "box") %>%
      layout(boxmode = "group", yaxis = list(title = "Score over all classes"), xaxis = list(tickvals = input$models, tickmode = "array"), font=t)
    p
  })
  
  output$boxplot <- renderPlotly({boxplotplot()})
  
  acc_std_plot <- reactive({
    if(is.null(input$models) || length(input$models) != (nrow(selected_models()) / ncol(selected_models()))){return()}
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
    

    
    recall <- boxplot_calculation()
    
    recall <- recall[(1+nrow(data)):(2*nrow(data)),1]
    
    recallsd <- colSds(matrix(recall, ncol(data)))
    
    x <- list(
      title = "Standard deviation of recalls"
    )
    y <- list(
      title = "Model Accuracy"
    )
    modelnames <- factor(input$models, levels = input$models)
    p <- plot_ly(x = recallsd, y = acc, type = "scatter", mode = "markers", color = modelnames, colors = unname(plotcolors()[1:length(input$models)]), marker = list(size = 12), hovertemplate = paste('<i>Accuracy</i>: %{y:.4p}', '<br><i>Std</i>: %{x:.4p}'))%>%
      layout(xaxis = x, yaxis = y)
    p
  })
  
  output$acc_std_plot <- renderPlotly({acc_std_plot()})
  
  ## Plot für den Modellrank nach Formel im Paper
  output$model_rank_plot <- renderPlotly({
    if(is.null(input$models) || length(input$models) != (nrow(selected_models()) / ncol(selected_models()))){return()}
    data <- selected_models()
    
    recall <- boxplot_calculation()
    
    recall <- recall[(1+nrow(data)):(2*nrow(data)),1]
    macro_avg_recall_model <- colMeans(matrix(recall, ncol(data)))
    macro_avg_recall <- rep(macro_avg_recall_model, each=ncol(data))
    
    ci <- abs(recall-macro_avg_recall) 
    
    ci <- colSums(matrix(ci, nrow=ncol(data)))
 
    ci_avg <- ci/ncol(data)
    
    ci_avg <- normalize(ci_avg, range=c(0,1), method="range")
    accuracy_model <- normalize(macro_avg_recall_model, range=c(0,1), method="range")
    
    ci_prime <- 1 - ci_avg
 
    m_rank <- 0.5*abs(accuracy_model + ci_prime)
    
    knn_train <- c(max(m_rank), median(m_rank), min(m_rank))
    knn_label <- as.factor(c("Good", "Medium", "Weak"))
    
    data <- data.frame(input$models, m_rank)
    data <- data[order(-data$m_rank),]
    
    classifier_knn = knn(train = as.data.frame(knn_train), test=as.data.frame(data$m_rank) ,cl=knn_label, k=1)
    
    data$label <- classifier_knn
    
    
    y <- list(
      title = "Model Rank" #,
#      titlefont = f
    )
    
    plot_ly(x=data$input.models, y=data$m_rank, type="bar", color = data$label, colors = c("#006D2C", "Orange", "#A50F15"), hovertemplate = paste('<i>Model</i>: %{x}', '<br><i>Score</i>: %{y:.4f}')) %>%
      layout(yaxis = y, xaxis = list(tickvals = data$input.models, tickmode = "array"))
    
  })
  
  observeEvent(modelnames(), {
    available_models <- modelnames()
    updatePickerInput(session, "models", choices = available_models, selected = available_models)
  })
  
  observeEvent(input$models, {
    available_models <- input$models
    updatePickerInput(session, "defaultmodel", choices = available_models, selected = available_models[1])
  })

  observeEvent(input$models, {
    available_models <- input$models
    updatePickerInput(session, "comparingmodel", choices = available_models, selected = available_models[2])
  })
  
  observeEvent(modelnames(), {
    available_models <- modelnames()
    updatePickerInput(session, "detailedmodel", choices = available_models, selected = available_models[1])
  })
  
  observeEvent(data(), {
    available_classes <- classnames()
    updatePickerInput(session, "classes", choices = available_classes , selected = NULL)
  })
  
  observeEvent(data(), { # Nach Dataupload und bei Start -> Alle Modelle ausgew?hlt
    available_models <- modelnames()
    disabled_choices <- available_models %in% input$models
    updatePickerInput(session, "models",
                      choices = available_models,
                      selected = available_models)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$models, {
    if(input$detailedmodel %in% input$models){
      selected_detail_model <- input$detailedmodel
    }
    else{
      selected_detail_model <- input$models[1] 
    }
    updatePickerInput(session, "detailedmodel",
                      selected = selected_detail_model,
                      choices = input$models)
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

