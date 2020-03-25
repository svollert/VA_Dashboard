# library
#install.packages("treemap")
#devtools::install_github("timelyportfolio/d3treeR")
library(treemap)
library(d3treeR)
library(RColorBrewer)

# cSV-Datei mit Konfusionsmatrizen einlesen 
cm <- read.csv("C:/Users/Patrick/Documents/MLD-Studium/Visual Analytics/Project/CNN_simple_mnist_labels_in_columns.csv", 
               header=TRUE, sep=",")

# Anzahl Klassen aus Konfusionsmatrix ermitteln
no_classes <- ncol(cm)
# Anzahl Modelle aus Konfusionsmatrix ermitteln
no_models <- nrow(cm)/no_classes

# Gesamte Konfusionsmatrix( aufsplitten (jeweils eine Konfusionsmatrix für ein Modell)
cm_models <- split(cm, rep(1:no_models, each=no_classes))
cm_used_model <- as.data.frame(cm_models[1]) # 1 ist abhängig vom ausgewählten Modell

colnames(cm_used_model) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7", "Class 8", "Class 9", "Class 10")
rownames(cm_used_model) <- c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7", "Class 8", "Class 9", "Class 10")

diag(cm_used_model) <- 0

labels <- c("All")
parents <- c("")
values <- sum(cm_used_model)

labels <- c(labels, colnames(cm_used_model))
parents <- c(parents, rep("All", ncol(cm_used_model)))
values <- c(values, colSums(cm_used_model))

labels <- c(labels, paste(rep(colnames(cm_used_model), each=ncol(cm_used_model)), "-->", colnames(cm_used_model)))
parents <- c(parents, rep(colnames(cm_used_model), each=ncol(cm_used_model)))
values <- c(values, unlist(cm_used_model))

id_null <- which(values == 0)
labels <- labels[-id_null] 
parents <- parents[-id_null]
values <- values[-id_null]

library(plotly)

fig <- plot_ly(
  type="treemap",
  labels=labels,
  parents = parents,
  values = values,
  branchvalues = "total",
  textinfo = "label+value+percent parent+percent"
)
fig