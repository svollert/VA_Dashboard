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
                           title = "VA Project",
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
                                 uiOutput("classlimit")),
  footer = bs4DashFooter(),
  body = bs4DashBody(bs4TabItems(bs4TabItem(tabName = "dashboard1",
                                            h2("Comparison of different classification models"),
                                            fluidRow(bs4InfoBoxOutput("acc_box", width = 2),
                                                     bs4InfoBoxOutput("baseline_box", width = 2),
                                                     bs4InfoBox(title="F1/Precision", width = 2, status = "primary"),
                                                     bs4InfoBox(title = "Gini Index", width = 2, status = "primary")),
                                            fluidRow(bs4Card(title = "Distribution Plot", plotlyOutput("boxplot"), width = 12, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE)),
                                            fluidRow(bs4TabCard(id = "Test123", title = "Parallel Coordinates  Line Plot", width = 12, closable = FALSE, status = "primary", maximizable = TRUE, 
                                                                bs4TabPanel(tabName = "Parallel Coordinates", plotlyOutput("parcoord")),
                                                                bs4TabPanel(tabName = "Line Plot", plotlyOutput("errorline")))),
                                            fluidRow(bs4Card(title = "Radar Chart", plotlyOutput("radarchart"), width = 6, closable = FALSE, status = "primary", maximizable = TRUE),
                                                     bs4Card(title = "Sun Burst", plotlyOutput("sunburst_plot"), width = 6, closable = FALSE, status = "primary", maximizable = TRUE))),
                                 bs4TabItem(tabName = "modelcomparison",
                                            h2("Test")),
                                 bs4TabItem(tabName = "dashboard2",
                                            h2("Detailed Information on a singular classification model"),
                                            fluidRow(bs4InfoBoxOutput("singleacc_box", width = 2),
                                                     bs4InfoBoxOutput("singlebaseacc_box", width = 2),
                                                     bs4InfoBoxOutput("precision_box", width = 2),
                                                     bs4InfoBoxOutput("recall_box", width = 2),
                                                     bs4InfoBoxOutput("f1_box", width = 2),
                                                     bs4InfoBoxOutput("kappa_box", width = 2)),
                                            fluidRow(bs4Card(title = "Distribution Plot", width = 12, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE)),
                                            fluidRow(bs4Card(title = "Confusion Wheel", chorddiagOutput("chorddiagramm", height = 500), width = 6, closable = FALSE, status = "primary", maximizable = TRUE),
                                                     bs4Card(title = "Confusion Matrix", plotlyOutput("heatmap", height = 500), width = 6, closable = FALSE, status = "primary", maximizable = TRUE)),
                                            fluidRow(bs4Card(title = "Sankey Diagram",plotlyOutput("sankey"), width = 6, closable = FALSE, status = "primary", maximizable = TRUE),
                                                     bs4Card(title = "Treemap", d3tree2Output("treemap"),width = 6, closable = FALSE, status = "primary", maximizable = TRUE))),
                                 bs4TabItem(tabName = "dataproperties",
                                            h2("Data Properties"),
                                            fluidRow(bs4InfoBoxOutput("nomodels_box", width = 2),
                                                     bs4InfoBoxOutput("samples_box", width = 2)),
                                            fluidRow(bs4Card(title = "Lorenz Curve", plotlyOutput("lorenzcurve"), width = 6, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE),
                                                     bs4Card(title = "Class Histogramm", plotlyOutput("histogram"), width = 6, collapsible = TRUE, collapsed = TRUE, closable = FALSE, maximizable = TRUE)),
                                            fluidRow(bs4Card(title = "Tabular Plot", DT::dataTableOutput(outputId = "table"), width = 12, closable = FALSE, collapsible = TRUE))),
                                 bs4TabItem(tabName = "debug",
                                            h2("debug"),
                                            fluidRow(bs4Card(title = "debug", textOutput("test")))))))


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
    print(rows)
    rows <- as.numeric(rows) * ncol(data())
    rows <- c(rep(rows, each = ncol(data())) - 0:(ncol(data()) - 1))
    rows <- sort(rows)
    print(rows)
    data <- data()[rows,]
    if(!is.null(input$classes)){
      delseq <- rep(classdelete(), each = length(input$models)) + seq(0,nrow(data)-1,ncol(data))
      data[-delseq, -classdelete()]
    }
    else{
      data
    }
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
  
#  classnames <- reactive({
#    classnames <- input$classnames
#    if(input$classnamesheader == FALSE){return()}
#    if(input$classnamesheader == TRUE){
#      classnames <- colnames(data())
#      return(classnames)
#    }
#  })
  
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
  
  
  output$test <- renderText({
    print(selected_models())
  })
  
  samples <- reactive({
    if(is.null(input$models)){return(0)}
    samples <- as.integer(sum(selected_models()) / length(input$models))
    samples
  })
  

  
  sunburst_data <- reactive({
    if(is.null(data())){return()}
    sunburst_modelle <- selected_models_missclassified()
    parents <- " "
    labels1 <- "All"
    parents <- c(parents, rep("All", nrow(sunburst_modelle) / ncol(sunburst_modelle)))
    parents <- c(parents, rep(input$models, each = ncol(sunburst_modelle)))
    labels1 <- c(labels1, input$models)
    classes1 <- rep(input$models, each = ncol(sunburst_modelle))
    classes2 <- paste(classes1, selected_classes())
    labels2 <- c(labels1, classes2)
    cm2=data.frame(matrix(ncol=0,nrow=ncol(sunburst_modelle)))
    for(i in seq(1, nrow(sunburst_modelle), ncol(sunburst_modelle))){
      cm2 <- cbind(cm2, sunburst_modelle[i:(i+ncol(sunburst_modelle)-1), ])
    }
    values <- c(sum(cm2),colSums(matrix(colSums(cm2), nrow = ncol(sunburst_modelle))), colSums(cm2))
    sunburst_data <- as.data.frame(cbind(parents, labels2, values))
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
  
  output$acc_box <- renderbs4InfoBox({
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
  
  output$baseline_box <- renderbs4InfoBox({
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
  
  output$nomodels_box <- renderbs4InfoBox({
    bs4InfoBox(
      title = "Number of Models",
      length(input$models),
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
      round(diag(as.matrix(selected_models())) / colSums(as.matrix(selected_models())),4)},
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
      round(diag(as.matrix(selected_models())) / rowSums(selected_models()),4)},
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
      round((2 * (diag(as.matrix(selected_models())) / colSums(selected_models())) * (diag(as.matrix(selected_models())) / rowSums(selected_models())) / (diag(as.matrix(selected_models())) / colSums(selected_models()) + diag(as.matrix(selected_models())) / rowSums(selected_models()))),4)},
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
      round(((sum(selected_models()) - sum(selected_models_missclassified())) / sum(selected_models()) - (rowSums(selected_models()) / sum(selected_models())) * (colSums(selected_models()) / sum(selected_models()))) / (1- (rowSums(selected_models()) / sum(selected_models())) * (colSums(selected_models()) / sum(selected_models()))),4)},
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
  
  
  output$sunburst_plot <- renderPlotly({
    if(is.null(input$models)){return()}
    p <- plot_ly(sunburst_data(), labels = ~labels2, parents = ~parents, values = ~values, type="sunburst", maxdepth=4, marker = list(colors = c('#f3cec9', '#e7a4b6', '#cd7eaf', '#a262a9', '#6f4d96', '#3d3b72', '#182844', '#2e4215', '#7a2b14', '#2f162a'))) #color = ~parents, colors = ~parents)
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
    parcoord_data <- selected_models_missclassified()
    #models <- input$models
    #models <- as.integer(substring(models, 7))
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
    
    start_statement = "list(list(range = c(1, max(models)),tickvals = models, label = 'Model', values = ~Models, ticktext = input$models),"
    print(input$models)
    loop_liste = c(start_statement)
    for(i in seq(1:ncol(selected_models_missclassified()))){
      text = sprintf("list(range = c(0,max_missclassified),label = '%s', values = ~`%s`),", classes[i], classes[i]) # Range entfernen um Balken zu skalieren
      loop_liste = c(loop_liste, text)
      if(i == ncol(selected_models_missclassified())){
        loop_liste[i+1] = substring(loop_liste[i+1], first = 0, last = nchar(loop_liste[i+1])-1)
        loop_liste[i+1] = paste(loop_liste[i+1], ")")
      }
    }
    loop_liste = paste(loop_liste, collapse = " ")
    
    p <- parcoord_data %>%
      plot_ly(type = 'parcoords',
              line = list(color = ~Models,
                          colorscale = list(c(0,'yellow'),c(1,'violetred'),c(2,'turquoise'),c(3,'lightgreen'),c(4,'green'),c(5,'orangered'),c(6,'red'),c(7,'chocolate'),c(8,'blue'),c(9,'brown'))),
              dimensions = eval(parse(text = loop_liste ))
              
      )
    p
  })
  output$parcoord <- renderPlotly({parcoordplot()})
  
  
 #sums[(j-ncol(cm)+1):j] 
  
  radarchartplot <- reactive({
    if(is.null(input$models)){return()}
    cm <- selected_models_missclassified()
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
      p<-add_trace(p,r = sums[(j-ncol(cm)+1):j], mode = "markers", theta = classes, fill = 'toself', name = input$models[i], marker = list(symbol = "square", size = 8))
    }
    #mittel <- colMeans(matrix(sums, ncol = ncol(cm), byrow = TRUE))
    #p <- add_trace(p, r = mittel, mode = "markers", theta = classes, name = "AVG", marker = list(symbol = "square", size = 8))
    p
  })
  output$radarchart <- renderPlotly({radarchartplot()})
  
  heatmapplot <- reactive({
    if(length(input$models) != 1){return()}
    data <- selected_models()
    classes <- selected_classes()
    rownames(data) <- classes
    colnames(data) <- rev(classes)
    new_data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    new_data <- as.matrix(new_data)
    norm_data <- as.matrix(data)
    max_diag <- max(diag(norm_data))
    min_diag <- min(diag(norm_data))
    diag(norm_data) <- ((diag(norm_data)-min_diag)/(max_diag - min_diag)) + 1.25
    max_not_diag <- max(c(data[upper.tri(norm_data)],data[lower.tri(norm_data)]))
    min_not_diag <- min(c(data[upper.tri(norm_data)],data[lower.tri(norm_data)]))
    norm_data[upper.tri(norm_data)] <- (norm_data[upper.tri(norm_data)] - min_not_diag) / (max_not_diag - min_not_diag)
    norm_data[lower.tri(norm_data)] <- (norm_data[lower.tri(norm_data)] - min_not_diag) / (max_not_diag - min_not_diag)
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
    
    p <- plot_ly(x = rownames(norm_data), y=colnames(norm_data), z=apply(norm_data, 2, rev), type="heatmap", colors=col) %>%
      add_annotations(x=anno_x, y=anno_y, text = new_data, showarrow = FALSE, font=list(color='black'))
    p
  })
  
  output$heatmap <- renderPlotly({heatmapplot()})
 
  
  chorddiagrammplot <- reactive({
    if(length(input$models) != 1){return()}
    cm <- selected_models_missclassified()
    cm <- t(as.matrix(cm))
    rownames(cm) <- selected_classes()
    colnames(cm) <- rownames(cm)
    chorddiag(cm, type = "directional", showTicks = T, groupnameFontsize = 14, groupnamePadding = 30, margin = 90, palette = "Set3")
  })
  output$chorddiagramm <- renderChorddiag({chorddiagrammplot()})
  
  
  
  sankeyplot <- reactive({
    if(length(input$models) != 1){return()}
    cm <- selected_models()
    
    # Anzahl Klassen aus Konfusionsmatrix ermitteln
    no_classes <- ncol(cm)
    # Anzahl Modelle aus Konfusionsmatrix ermitteln
    no_models <- length(input$models)
    
    cm_used_model <- selected_models() # 1 ist abhängig vom ausgewählten Modell
    
    id <- c(1:(2*no_classes))
    labels <- selected_classes()
    labels <- append(labels, selected_classes())
    cm_model_totals <- colSums(cm_used_model) # Gesamtanzahl der jeweiligen Klassen ermitteln
    cm_model_totals <- append(cm_model_totals, cm_model_totals)
    
    # Farben
    col <- distinctColorPalette(no_classes, altCol=T)
    col <- append(col, col)
    
    # Gewichtete Knoten erstellen
    nodes <- as.data.frame(cbind(id, labels, cm_model_totals, col), stringsAsFactors = FALSE)
    colnames(nodes) <- c("id","label","weight", "color")
    rownames(nodes) <- c(1:(2*no_classes))
    nodes$id <- as.numeric(nodes$id)
    nodes$weight <- as.numeric(nodes$weight)
    
    # Einträge auf der Diagonalen der Konfusionsmatrix werden auf 0 gesetzt
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
    data <- selected_models_missclassified()
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
              palette="Set3"
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
      add_trace(x = models, y = 1/ncol(data), type = "scatter", mode = "lines", name = "Random")
  })
  output$errorline <- renderPlotly({errorlineplot()})
  
  boxplotplot <- reactive({
    cm <- selected_models()
    cm_col <- vector(mode="numeric")
    for(i in seq(1, length(input$models))) {
      cm_col <- append(cm_col, colSums(cm[((i*ncol(cm))-(ncol(cm)-1)):(((i*ncol(cm))-(ncol(cm)-1))+(ncol(cm)-1)),]))
    }
    cm_row <- rowSums(cm)
    precision <- cm/cm_col
    recall <- cm/cm_row
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
      df_model <- data.frame(Score = vector_score, Model = vector_model, Metric = vector_metric, stringsAsFactors = FALSE)
      results <- rbind(results, df_model)
    }

    p <- plot_ly(results, y = ~Score, x = ~Model, color=~Metric, type = "box") %>%
      layout(boxmode = "group")
    p
  })
  
  output$boxplot <- renderPlotly({boxplotplot()})
  
  observeEvent(modelnames(), {
    available_models <- modelnames()
    updatePickerInput(session, "models", choices = available_models, selected = available_models)
  })

  observeEvent(data(), {
    available_classes <- classnames()
    updatePickerInput(session, "classes", choices = available_classes , selected = NULL)
  })
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
  })
  
  output$table <- DT::renderDataTable({
    if(is.null(data())){return ()}
    #data()[1:10,]
    selected_models()
  }, filter = "top")

  output$tablemodels <- DT::renderDataTable({
    if(is.null(modelnames())){return ()}
    used_names <- paste("Model", 1:ncol(data()))
    models <- as.data.frame(cbind(used_names, modelnames()))
    colnames(models) <- c("Internal Name", "Actual Name")
    models
  }, filter = "top")

  


  

  
  x <- reactive({
    c(paste("Model ", 1:ncol(data())))
  })
  
  reactive({
    list2env(setNames(x,paste0("df",1:ncol(cm))),environment()) 
  })
  
  output$tb <- renderUI({
    if(is.null(data()))
      h5("Powered by")
    else
      tabsetPanel(tabPanel("About File", tableOutput("filedf")), tabPanel("Data", tableOutput("table")))
  })
}

shiny::shinyApp(ui, server)

