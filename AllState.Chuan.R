#System set
library(doMC)
library(parallel)
number_of_cores <- detectCores()
registerDoMC(cores = number_of_cores/2)

#Load data and summary
setwd("~/Desktop/AllState")
as.train <- read.csv("train.csv")
as.test <- read.csv("test.csv")
dim(as.train)

#Missingness
library(VIM)
aggr(as.train)

## Visualization corr
library(dplyr)
library(corrplot)
corrs <- cor(as.train %>% select(contains("cont")), method = "pearson")
corrplot.mixed(corrs, upper = "square", order="hclust")

library(VIM)
plot(as.train[117:132])

## Creating dummy variables
library(caret)
dm.train <- model.matrix(loss ~ ., data = as.train)
head(dm.train, n = 4)
log.loss <- log(as.train$loss+1) # create this vecter for model analysis

## Removing NZV
preProc <- preProcess(dm.train, method = "nzv")
preProc #Created from 188318 samples and 885 variables
        # Pre-processing:
        #   - ignored (0)
        #   - removed (885)

## Transformations
dm.train <- predict(preProc,dm.train)
dim(dm.train) #[1] 188318    154

## Train lm w/ caret
# (1) Data spliting
set.seed(0)
trainIdx <- createDataPartition(log.loss, 
                                p = .8,
                                list = FALSE,
                                times = 1)
subTrain <- dm.train[trainIdx,]
subTest <- dm.train[-trainIdx,]
lossTrain <- log.loss[trainIdx] 
lossTest <- log.loss[-trainIdx]

# (2) LM model
system.time(
  lmFit <- train(x = subTrain, 
                 y = lossTrain,
                 method = "lm")
)

summary(lmFit)
# Residual standard error: 0.5768 on 150503 degrees of freedom
# Multiple R-squared:  0.4955,	Adjusted R-squared:  0.495 
# F-statistic: 972.5 on 152 and 150503 DF,  p-value: < 2.2e-16

# (3) Variable importance
lmImp <- varImp(lmFit, scale = FALSE)
lmImp

plot(lmImp,top = 20)

# (4) Performance measures for regression
mean(lmFit$resample$RMSE) #[1] 0.5777136

predicted <- predict(lmFit, subTest)
RMSE(pred = predicted, obs = lossTest) #[1] 0.576594

plot(x = predicted, y = lossTest, pch=16)


## Train xgboost w/ caret
library(xgboost)
xgb.grid <- expand.grid(
  nrounds= 2400,
  lambda = 1,
  alpha =0
)

xgb.ctrl <- trainControl(
  method="cv",
  number = 5,
  verboseIter = TRUE,
  returnData=FALSE,
  returnResamp = "all",
  allowParallel = TRUE
)

system.time(
  xgbFit <- train(x = subTrain, 
                  y = lossTrain,
                  trControl = xgb.ctrl,
                  tuneGrid = xgb.grid,
                  method="xgbLinear")
)

summary(xgbFit)

xgbImp <- varImp(xgbFit, scale = FALSE)
xgbImp

mean(xgbFit$resample$RMSE) #[1] 0.5909002

predicted <- predict(xgbFit, subTest)
RMSE(pred = predicted, obs = lossTest) #[1]0.5899112

plot(x = predicted, y = lossTest, pch=16)


