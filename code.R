#### BRAND PREFERENCE ####

#Last commit Raúl and Gerardo

library("readr")
library("caret")
library("corrplot")
library("rbokeh")
library("summarytools")

#### Functions ####
plotDistributions <- function(dataSet){
  
  # Plot a grid of histograms and bar charts
  # according to the data type of each column of a given dataframe
  
  plot_list <- vector(mode = 'list', length=0)
  for (column in names(dataSet)){
    x <- dataSet[,column]
    if(is.numeric(x)){
      hist_bins <- diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
      plot_list[[column]] <- figure(xlab = NULL) %>%
        ly_hist(x, freq=TRUE, breaks=hist_bins) %>%
        # ly_density(x) %>%
        x_axis(number_formatter = "numeral", format = "0.") %>%
        theme_axis("x", major_label_orientation = 45)
    } else if (is.factor(x)) {
      plot_list[[column]] <- figure(xlab = NULL, ylab = "Frequency") %>%
        ly_bar(x = x) %>%
        theme_axis("x", major_label_orientation = 45)
    }
  }
  total_columns = 2
  p <- grid_plot(plot_list,
    ncol = total_columns)
  return(p)
}


selectAttributes <- function(dataSet, excludedAttr){
  
  # Subset given dataset based on a list of attributes
  
  outputSet <- subset(
    dataSet,
    select = -c(
      BestSellersRank,
      x5StarReviews,
      x3StarReviews,
      x1StarReviews,
      ProductDepth,
      ProductWidth,
      ProductHeight,
      ShippingWeight,
      ProfitMargin,
      ProductNum,
      NegativeServiceReview,
      Recommendproduct,
      Price ))
  return(outputSet)
  
}


normalizeThis <- function(dataSet){
  
  # normalize numeric variables
  # of a given dataset
  # except for Volume
  
  numericIndices <- sapply(
    subset(dataSet,
      select = -Volume),
    is.numeric)
  dataSet[numericIndices] <- lapply(dataSet[numericIndices], scale)
  return(dataSet)
}


dummyfyThis <- function(dataSet){
  dumm <- dummyVars(" ~ .", data = dataSet)
  pred <- predict(dumm, newdata = dataSet)
  df <- data.frame(pred)
  return(df)
}

#### Load Data ####
# set working directory first!!!
existingProducts_orig <- read.csv("./data/existingproductattributes2017.csv")
newProducts <- read.csv("./data/newproductattributes2017.csv")


#### Preprocess ####
# replace NA in BestSellersRank with the mean of the existing values
existingProducts <- existingProducts_orig
existingProducts[is.na(existingProducts[,"BestSellersRank"]), "BestSellersRank"] <- mean(
    existingProducts[,"BestSellersRank"],
    na.rm = TRUE)

# select attributes
dataSet <- selectAttributes(existingProducts, excludedAttr)
newSet <- selectAttributes(newProducts, excludedAttr)

# eliminate outliers
dataSet <- subset(
  dataSet,
  Volume < 7000)

# normalize values
dataSet <- normalizeThis(dataSet)
newSet <- normalizeThis(newSet)

# build dummies
dataSet <- dummyfyThis(dataSet)
newSet <- dummyfyThis(newSet)

plotCorr <- corrplot(
  cor(dataSet),
  method="color",
  type="lower")

#### Data Split ####
set.seed(8888)
inTraining <- createDataPartition(
  dataSet$Volume,
  p = .75,
  list = FALSE)
trainingSet <- dataSet[inTraining,]
testingSet <- dataSet[-inTraining,]

#### Train Control ####
cvFoldNum <- 10
cvRepeatNum <- 5
cvFolds <- createMultiFolds(
  trainingSet$Volume,
  k = cvFoldNum,
  times = cvRepeatNum)

fitControl <- trainControl(
  method = "repeatedcv",
  index = cvFolds)

#### Modeling ####
formula <- as.formula("Volume ~ .")
tuneLength <- 5
models <- list(
  "rf",
  "svmLinear",
  "svmRadial",
  "svmPoly",
  "lm",
  "knn",
  "gbm")

trainedModels <- list()
outputSets <- list()
outputPlots <- list()

for(model in models){
  print(paste("Trying training with", model, "method."))
  modelFile <- paste("./models/", model, "_model.rba", sep="")
  if(!file.exists(modelFile)){
    print("Training new model")
    trainedModel <- train(
      formula,
      data = trainingSet,
      method = model,
      trControl = fitControl,
      tuneLength = tuneLength)#,
      #importance = T
    
    save(trainedModel, file = modelFile)
  } else {
    print("Loading from file")
    load(modelFile) # will create variable trainedModel
  }
  trainedModels[[model]] <- trainedModel
  label <- trainedModel$modelInfo$label
  
  trainingSet_output <- trainingSet
  # predict for training set
  trainingSet_output$Prediction <- round(
    predict(trainedModel,trainingSet_output),2)
  # calculate normalized error for training set
  trainingSet_output$Error <- round(
    scale(
      trainingSet_output$Prediction - trainingSet_output$Volume),2)
  trainingSet_output$Model <- model
  # set training set label
  trainingSet_output$Partition <- "training"
  
  testingSet_output <- testingSet
  # predict for testing set
  testingSet_output$Prediction <- round(
    predict(
      trainedModel,
      testingSet_output),2)
  # calculate normalized error for testing set
  testingSet_output$Error <- round(
    scale(
      testingSet_output$Prediction - testingSet_output$Volume),2)
  testingSet_output$Model <- model
  # set testing set label
  testingSet_output$Partition <- "testing"
  
  # union sets and save in list and file
  rebindSet <- rbind(trainingSet_output, testingSet_output)
  outputSets[[model]] <- rebindSet
  write.csv(rebindSet, file=paste("./output/", model, "output.csv"))
  
  # draw Error plot for this model and save in list
  outputPlots[[label]] <- figure(
    legend_location = NULL,
    title = label) %>%
    ly_points(
      Volume,
      Error,
      data = rebindSet,
      color = Partition,
      hover = c(Partition, Volume, Prediction, Error)) %>%
    ly_abline(a = 0,b = 0,color = "red")
}

# create whole output set with all models
outputSet <- Reduce(
  rbind,
  outputSets)
# write output set to file
write.csv(outputSet, file="./output/output.csv")

# create error comparing plot
p_full <- figure(
  legend_location = "bottom_left") %>%
  ly_points(Volume,Error,
    data = subset(outputSet, Model %in% c("gbm", "rf")),
    # glyph = Partition,
    color = Model)

# draw grid with error plot for each model
p_grid <- grid_plot(outputPlots,ncol = 2)

# test scatter?
p_scatter_type <- figure(
  legend_location = "top_left") %>%
  ly_points(
    data = existingProducts,
    color = ProductType,
    hover = c(ProductType, x4StarReviews, Volume),
    y = Volume,
    x = x4StarReviews) %>%
  theme_axis("x",major_label_orientation = 45)

# build metrics dataframe
metrics <- list(
  "Rsquared",
  "MAE",
  "RMSE")

metricsSet <- data.frame()
for (model in models){
  label <- trainedModels[[model]]$modelInfo$label
  testingResults <- as.list(
    postResample(
      predict(
        trainedModels[[model]],
        testingSet),
      testingSet$Volume))
  
  for(metric in metrics){
    metricsSet[label, paste(metric, "", sep="")] = round(
      tail(
        trainedModels[[model]]$results,
        1)[[metric]],2)
    metricsSet[label, paste(metric, "*", sep="")] = round(
      testingResults[[metric]],2)
  }
}


# calculate new product predictions
newSet$Volume <- round(
  predict(
    trainedModels[["rf"]],
    newSet),2)
newSet <- newSet[, -grep("ProductType.", colnames(newSet))]
newSet <- cbind(
  subset(
    newSet,
    select = Volume),
  subset(
    newProducts,
    select = c(
      ProfitMargin,
      Price,
      ProductNum,
      ProductType)))
newSet <- subset(
  newSet,
  newSet$ProductType %in% c(
    "PC",
    "Laptop",
    "Netbook",
    "Smartphone"))
newSet$TotalProfit <- round(newSet$Price * newSet$ProfitMargin * newSet$Volume, 2)
write.csv(newSet, file="./output/new.csv")
