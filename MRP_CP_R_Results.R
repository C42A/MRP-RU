########################################################
## Cassandra Postma
## DS MRP - R Results on Table Data
## Running learning algorithms on 5x2 Cross Validation
## Using caret package to execute training
########################################################


install.packages("igraph")
install.packages("compare")
install.packages("caret")
install.packages("mlbench")
library("caret")
library("igraph")
library("compare")
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


#########################################################
## Linear Regression Model
#########################################################
##Run Linear Regression on 5 by 2-fold Cross Validation
LinModel <- train(
  Rating ~ .,
  tr,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 5,
    verboseIter = TRUE
    
  )
)

##Run prediction on regression model
LinPrediction <- predict(LinModel, te)
LinModel

postResample(pred = LinPrediction, obs = te$Rating)

#########################################################
## Neural Network Model
#########################################################
##Run Neural Network with tune length of 10 to find optimal hidden units
NNModel <- train(
  Rating ~ .,
  DataSet,
  method = "nnet",
  tuneLength=10
)

plot(NNModel)


##Run Neural Network on 5 by 2-fold Cross Validation
NNetModel <- train(
  Rating ~ .,
  tr,
  method = "nnet",
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 5,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    size=10,
    decay=0.001
  )
)

##Run prediction on Neural Network model
NNetPrediction <- predict(NNetModel, te)
NNetModel

postResample(pred = NNetPrediction, obs = te$Rating)


#########################################################
## Conditional Tree
#########################################################
##Run Conditional  Tree with tune length of 10 to find optimal number of components
CTModel <- train(
  Rating ~ .,
  fullTrain,
  method = "ctree",
  tuneLength=10,
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 1,
    verboseIter = TRUE
  )
)


plot(CTModel)


##Run Conditional Tree on 5 by 2-fold Cross Validation
CTreeModel <- train(
  Rating ~ .,
  tr,
  method = "ctree",
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
CTreePrediction <- predict(CTreeModel, te)
CTreeModel

postResample(pred = CTreePrediction, obs = te$Rating)



#########################################################
## GLM Net
#########################################################
##Run GLM Net with tune length of 10 to find optimal number of components
GLMModel <- train(
  Rating ~ .,
  fullTrain,
  method = "glmnet",
  tuneLength=10,
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 1,
    verboseIter = TRUE
  )
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
## Partial Least Squares Model
#########################################################
##Run Least Squares Model with tune length of 10 to find optimal number of components
LSModel <- train(
  Rating ~ .,
  fullTrain,
  method = "kernelpls",
  tuneLength=10
)

plot(LSModel)


##Run Least Squares on 5 by 2-fold Cross Validation
LeastSqModel <- train(
  Rating ~ .,
  tr,
  method = "kernelpls",
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 5,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    ncomp=8
  )
)

##Run prediction on Least Squares model
LeastSqPrediction <- predict(LeastSqModel, te)
LeastSqModel

postResample(pred = LeastSqPrediction, obs = te$Rating)




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
  trControl = trainControl(
    method = "repeatedcv",
    number = 2,
    repeats = 5,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    k=75
  )
)

KModel

##Run prediction on KNN model
KPrediction <- predict(KModel, te)
KModel

postResample(pred = KPrediction, obs = te$Rating)


