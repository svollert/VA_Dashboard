################  functions


# converts confusion matrix file with labels in rows to labels in columns
cmRowToCol <- function(fileName){
  
  cmr <- read.csv(fileName, header=TRUE)
  n_classes <- ncol(cmr)
  cmt <- t(cmr)  # transpose
  
  cmc <- data.frame() # 
  
  for(i in seq(1,nrow(cmr),n_classes)){
    cm1model <- cmt[, i: (i + n_classes-1) ]
    cmc <- rbind(cmc, cm1model)
  }
  
  #use header
  cmr <- as.data.frame(cmr)
  names(cmc) <- names(cmr)

  fileNameOut <- tools::file_path_sans_ext(fileName)
  fileNameOut <- paste0(fileNameOut, "_conv_columns.csv")
  
  write.table(cmc, file=fileNameOut, row.names = FALSE, sep=",", quote=FALSE)
  
  print(paste0("File written: ", fileNameOut))
  return(cmc)
}


# checks if number of rows and columns is plausible
isFormatOk <- function(cm){
  classes <- ncol(cm)
  check <- nrow(cm) %% classes
  
  ret <- ifelse(check == 0, TRUE, FALSE)
  return (ret)
}


# function checks the sums per classes for all models, assuming labels in rows
checkLabelsInRows <- function(cm){
  classes <- ncol(cm)
  
  sums <- apply(cm, MARGIN=1, FUN=sum) # sum up rows
  errors <- NULL
  
  for(i in seq(1,classes)){
    idx <- seq(i,length(sums), classes) # get confusions of one class
    sums_1class <- sums[idx]
    errors <- c(errors, which(sums_1class != sums_1class[1]))
  }

  return(errors)
}


# function checks the sums per classes for all models, assuming labels in columns 
checkLabelsInColumns <- function(cm){
  classes <- ncol(cm)
  models <- nrow(cm)/classes
  sums <- NULL
  
  for(i in seq(1,classes)){
    conf_1class <- cm[,i] # get complete column of all consec. conf matrices
    
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
  
  return (errors)
}


# calculates the per-class errors, assuming class labels to be in rows 
perClassErrors_Rows <- function(cm){
  n_classes <- ncol(cm)
  pcErrs <- NULL
  
  for(r in seq(1,nrow(cm))){

    c <- ifelse(r %% n_classes == 0, 3, r %% n_classes ) # get index of column
    
    err_pc <- 1 - (cm[r,c] / sum(cm[r,]))  # error in current row = per-class error
    pcErrs <- c(pcErrs, round(err_pc, 2))
  } 
  
  return (pcErrs)
}


# calculates the overall accuracy 
# check: (70+150+450) / (100+200+500) 
overallAccuracies <- function(cm){
  
  n_classes <- ncol(cm)
  tpPerModel <- NULL
  tp <- NULL
  
  for(r in seq(1,nrow(cm))){
    c <- ifelse(r %% n_classes == 0, 3, r %% n_classes ) # get index of column (diagonal trace in matrix)

    tp <- sum(tp, cm[r,c]) # sum up True Predictions of current model
    
    if(c == 3){  # all rows of model
      tpPerModel <- c(tpPerModel, tp)
      tp <- NULL
    }
  } 
  
  acc <- tpPerModel / sum(cm[1:n_classes,])
  return (acc)
}

# average accuracy 
# (70/100 + 150/200 + 450/500) / 3
# (85/100 + 190/200 + 480/500) / 3


########################### main #########################

path <- "C:/Users/anth/Dropbox/myresearch/2019_confusion_Matrices/ConfusionVis/data/"

################### check labels in rows

cm_row <- read.csv(paste0(path, "cm_artificial_3x3_labels_in_rows.csv"), header=TRUE)

# cm_row <- read.csv("C:/Users/anth/Dropbox/myresearch/2019_confusion_Matrices/ConfusionVis/data/whales/v2/validation_set_results_labels_in_rows.csv")

if(!isFormatOk(cm_row)){
  print("Format error: number of rows and/or columns do not match")
}

errsRowCheck <- checkLabelsInRows(cm_row)
if(length(errsRowCheck) == 0){
  print("Labels in rows: File is ok")
}else{
  print("Labels in rows: Check failed")
}

#overall accuracies
acc <- overallAccuracies(cm_row)
print("Overall acc:")
print(acc)


#per class errors
pcErrs <- perClassErrors_Rows(cm_row)
print("Per class errors:")
print(pcErrs)

################### check labels in columns

cm_col <- read.csv(paste0(path, "cm_artificial_3x3_labels_in_columns.csv"),header=TRUE)


if(!isFormatOk(cm_col)){
  print("Format error: number of rows and/or columns do not match")
}

errsColCheck <- checkLabelsInColumns(cm_col)

if(length(errsColCheck) == 0){
  print("Labels in columns: File is ok")
}else{
  print("Labels in columns: Check failed")
}


r <- cmRowToCol(paste0(path, "cm_artificial_3x3_labels_in_rows.csv"))

cmRowToCol("C:/Users/anth/Dropbox/myresearch/2019_confusion_Matrices/ConfusionVis/data/whales/v2/validation_set_results_labels_in_rows.csv")






