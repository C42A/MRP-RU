########################################################
## Cassandra Postma
## DS MRP - R Results on Table Data
## Running learning algorithms on 5 fold Cross Validation
## Using caret package to execute training
########################################################
install.packages("caret")
install.packages("caretEnsemble")
install.packages("mlbench")
library("caretEnsemble")
library("caret")
library("mlbench")

##Read and clean data, convert to bipartite undirected graph
graphData <- read.table("C:\\Users\\cassy\\Desktop\\GraphTrain.csv", sep=",", header=TRUE)
fullTrain <- read.table("C:\\Users\\cassy\\Desktop\\FullTrain.csv", sep=",", header=TRUE)

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

#########################################################
## Linear Regression Model
#########################################################
##Run Linear Regression on 5 by 2-fold Cross Validation
LinModel <- train(
  Rating ~ .,
  tr,
  method = "lm",
  trControl = control
)

##Run prediction on regression model
LinPrediction <- predict(LinModel, te)
LinModel
summary(LinModel)
hist(residuals(LinModel))
plot(fitted(LinModel),residuals(LinModel))
postResample(pred = LinPrediction, obs = te$Rating)



#########################################################
## xgb Tree
#########################################################
xgbTree_model <- train(Rating ~ .,
                       tr,
                       trControl = control,
                       method = "xgbTree",
                       metric = "RMSE",
                       preProcess = c("center","scale"),
                       importance = TRUE)

plot(xgbTree_model)
plot(varImp(xgbTree_model))
summary(xgbTree_model)
xgbTreePrediction <- predict(xgbTree_model, te)
postResample(pred = xgbTreePrediction, obs = te$Rating)



#########################################################
## Ranger
#########################################################
RangerModel <- train(
  Rating ~ .,
  fullTrain,
  method = "ranger",
  trControl = control,
  tuneGrid = expand.grid(
    mtry=  1724, min.node.size=5, splitrule="variance"
  )
)


RangerModel

##Build vectors of tuning values and the resulting RMSE
RMSE_Ranger<- c(1.059644,1.01549)
MTry<- c(2,58,1724)


#########################################################
## Random Forest
#########################################################
##Run Random Forest with tune length of 10 to find optimal number of components
RFModel <- train(
  Rating ~ .,
  fullTrain,
  method = "rf",
  trControl = control,
  tuneGrid = expand.grid(
    mtry=10
  )
)


plot(RFModel)
RFModel

##Build vectors of tuning values and the resulting RMSE
RMSE_RF<- c(1.059386,1.035828, 1.019909,1.015562,1.024636)
MTry<- c(2,5,10,50,100)

##Create plot for methodology
plot(MTry, RMSE_RF, col= "blue", xlab="MTry", ylab="RMSE (Repeated Cross-Validation)", tck=1)
lines(MTry, RMSE_RF, col="blue")
axis(1,tck=1,col.ticks="light gray")
axis(1, tck=-0.03, col.tcks="black")
axis(2,tck=1,col.ticks="light gray")
axis(2, tck=-0.03, col.tcks="black")

##Run prediction on Conditional Tree model
RFPrediction <- predict(RFModel, te)
RFModel

postResample(pred = RFPrediction, obs = te$Rating)


#########################################################
## SVM
#########################################################
##Run Random Forest with tune length of 10 to find optimal number of components
SVMModel <- train(
  Rating ~ .,
  fullTrain,
  method = "svmRadial",
  trControl = control
)


plot(SVMModel)


##Run Random Forest on 5 by 2-fold Cross Validation
SVMModel <- train(
  Rating ~ .,
  tr,
  method = "svmRadial",
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 5,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    mincriterion=0.01
  )
)

##Run prediction on Conditional Tree model
SVMPrediction <- predict(SVMModel, te)
SVMModel

postResample(pred = SVMPrediction, obs = te$Rating)





#########################################################
## GLM Net
#########################################################
##Run GLM Net with tune length of 10 to find optimal number of components
GLMModel <- train(
  Rating ~ .,
  fullTrain,
  method = "glmnet",
  tuneLength=10,
  trControl = control
)


plot(GLMModel)


##Run GLM Net on 5 by 2-fold Cross Validation
GModel <- train(
  Rating ~ .,
  tr,
  method = "glmnet",
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 5,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    alpha=1,
    lambda=0.0186
  )
)

##Run prediction on GLM model
GPrediction <- predict(GModel, te)
GModel

postResample(pred = GPrediction, obs = te$Rating)



#########################################################
## K-NN
#########################################################
##Run K-NN with on different tuning values to find optimal number of components

KNNModel <- train(
  Rating ~ .,
  tr,
  method = "knn",
  tuneGrid = expand.grid(
    k=75
  )
)


plot(KNNModel)

##Build vectors of tuning values and the resulting RMSE
RMSE_KNN<- c(1.439039,1.250727,1.132491,1.09722,1.078134,1.070057,1.067745, 1.064388, 1.063537, 1.062945)
Neighbours<- c(1,2,5,10,20,50,75,150,250,400)

##Create plot for methodology
plot(Neighbours, RMSE_KNN, col= "blue", xlab="#Neighbours", ylab="RMSE (Repeated Cross-Validation)", tck=1)
lines(Neighbours, RMSE_KNN, col="blue")
axis(1,tck=1,col.ticks="light gray")
axis(1, tck=-0.03, col.tcks="black")
axis(2,tck=1,col.ticks="light gray")
axis(2, tck=-0.03, col.tcks="black")


##Run KNN on 5 by 2-fold Cross Validation
KModel <- train(
  Rating ~ .,
  tr,
  method = "knn",
  trControl = control,
  tuneGrid = expand.grid(
    k=75
  )
)

KModel

##Run prediction on KNN model
KPrediction <- predict(KModel, te)
KModel

postResample(pred = KPrediction, obs = te$Rating)
