
########################################################
## Cassandra Postma
## DS MRP - R Results on Table Data
## Hybrid Algorithms
########################################################
install.packages("caret")
install.packages("caretEnsemble")
library("caretEnsemble")
library("caret")


##Read and clean data, convert to bipartite undirected graph
fullTrain <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\FullTrain.csv", sep=",", header=TRUE)

##Convert Date to number of Days
startdate <- as.Date("01/01/2000","%d/%m/%Y")
fullTrain$Date2 <-  as.Date(fullTrain$Date,"%Y-%m-%d")
fullTrain$Days  <- difftime(fullTrain$Date2,startdate ,units="days")

##Subset Full Train on only vars needed for models
vars<-c("MovieID","Year","CustomerID","Rating","Days")
DataSet<- fullTrain[vars]

# Generate the training and test samples
set.seed(1)
ft_index <- createDataPartition(DataSet$Rating, p = .75, list = FALSE)
tr <- DataSet[ ft_index, ]
te <- DataSet[-ft_index, ]


##Set seed for algorithms
set.seed(42)

control <- trainControl(method = "cv", 
                        number = 5, 
                        savePredictions = "final",
                        allowParallel = TRUE,
                        verboseIter = TRUE)




#########
#H1
#########
Hybrid1 <- caretList(Rating ~ .,
                     tr,
                     trControl = control,
                     methodList = c("lm", "svmRadial", "rf", "xgbTree", "xgbLinear"),
                     tuneList = NULL,
                     continue_on_fail = FALSE, 
                     preProcess = c("center","scale"))

resamples1 <- resamples(Hybrid1)
dotplot(resamples1, metric = "RMSE")

ensemble_1 <- caretEnsemble(Hybrid1, metric = "MAE", trControl = control)
summary(ensemble_1)
plot(ensemble_1)

#########
#H2
#########
Hybrid2 <- caretList(Rating ~ .,
                     tr,
                     trControl = control,
                     methodList = c("lm", "knn", "xgbTree"),
                     tuneList = NULL,
                     continue_on_fail = FALSE, 
                     preProcess = c("center","scale"))

resamples2 <- resamples(Hybrid2)
dotplot(resamples2, metric = "RMSE")

ensemble_2 <- caretEnsemble(Hybrid2, metric = "RMSE", trControl = control)
summary(ensemble_2)
plot(ensemble_2)

#########
#H3
#########
Hybrid3 <- caretList(Rating ~ .,
                     tr,
                     trControl = control,
                     methodList = c("rf","xgbLinear","xgbTree"),
                     tuneList = NULL,
                     continue_on_fail = FALSE, 
                     preProcess = c("center","scale"))

resamples3 <- resamples(Hybrid3)
dotplot(resamples3, metric = "RMSE")

ensemble_3 <- caretEnsemble(Hybrid3, metric = "RMSE", trControl = control)
summary(ensemble_3)
plot(ensemble_3)



#########
#H4
#########
Hybrid4 <- caretList(Rating ~ .,
                     tr,
                     trControl = control,
                     methodList = c("bartMachine","lm", "svmRadial", "knn", "rf","xgbLinear","xgbTree", "ranger"),
                     tuneList = NULL,
                     continue_on_fail = FALSE, 
                     preProcess = c("center","scale"))

resamples4 <- resamples(Hybrid4)
dotplot(resamples4, metric = "RMSE")

ensemble_4 <- caretEnsemble(Hybrid4, metric = "RMSE", trControl = control)
summary(ensemble_4)
plot(ensemble_4)
