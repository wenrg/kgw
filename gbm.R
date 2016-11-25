library(caret)

setwd("~/path")
allstate.train = read.csv('train.csv')
allstate.test = read.csv('test.csv')
sample_submission = read.csv('sample_submission.csv')

# continous/categorical vars indices
cont = 118:132
cat = 2:117

# transform loss
loss.log = log(allstate.train$loss) + 1


# dummy variables: cat vars over 53 levels
dm.astrain = model.matrix(loss ~., data = allstate.train[, -1])
head(dm.astrain)
dm.astest = model.matrix(~., data = allstate.test[, -1])

# drop near zero variables
prep = preProcess(dm.astrain, method = 'nzv')
dm.astrain = predict(prep, dm.astrain)
dim(dm.astrain) # [1] 188318   153
dm.astest = predict(prep, dm.astest)
dim(dm.astest)

# create train and test sets
set.seed(0)
train.index = createDataPartition(loss.log, times = 1, p = 0.8, list = F)

sub.dmtrain = dm.astrain[train.index, ]
sub.dmtest = dm.astrain[-train.index, ]
loss.train = loss.log[train.index]
loss.test = loss.log[-train.index]

# library(doMC)
# library(parallel)
# number_of_cores = detectCores()
# registerDoMC(cores = number_of_cores/2)

##### gradient boosting machines #####
# n.trees = 200
# interaction.depth = 6
# shrinkage = 0.5
tCtrl = trainControl(method = 'cv', number = 10, verboseIter = T, summaryFunction = defaultSummary)
gbmGrid = expand.grid(n.trees = seq(100, 300, 20), 
                      interaction.depth = 6, 
                      shrinkage = 0.1,
                      n.minobsinnode = 20)
gbmFit = train(x = sub.dmtrain, 
               y = loss.train,
               method = "gbm", 
               trControl = tCtrl,
               tuneGrid = gbmGrid,
               metric = 'RMSE',
               maximize = F)

plot(gbmFit)
plot(gbmFit, plotType = "level")
gbmImp = varImp(gbmFit, scale = F)
plot(gbmImp, top = 20)
mean(gbmFit$resample$RMSE)
predicted = predict(gbmFit, sub.dmtest)
postResample(pred = predicted, obs = loss.test)[1]
plot(x = loss.test, y = predict(gbmFit, sub.dmtest))


# modeling with all data
tCtrl = trainControl(method = 'cv', number = 10, verboseIter = T, summaryFunction = defaultSummary)
gbmGrid = expand.grid(n.trees = 140, 
                      interaction.depth = 6, 
                      shrinkage = 0.5,
                      n.minobsinnode = 20)
gbmFit = train(x = dm.astrain, 
               y = loss.log,
               method = "gbm", 
               trControl = tCtrl,
               tuneGrid = gbmGrid,
               metric = 'RMSE',
               maximize = F)


predicted.loss = predict(gbmFit, dm.astest)
predicted.eloss = exp(predicted.loss)
sample_submission$loss = predicted.eloss
write.csv(sample_submission, file = 'sample_submission.csv', row.names = F)


#########################################################################################################
##############################best tuning parameters#####################################################
#########################################################################################################

tCtrl = trainControl(method = 'cv', number = 5, verboseIter = T, summaryFunction = defaultSummary)
gbmGrid = expand.grid(n.trees = 1700,
                      interaction.depth = 6, # this is the best tree depth, no need to change
                      shrinkage = 0.1,
                      n.minobsinnode = 50)
gbmFit = train(x = sub.dmtrain, 
               y = loss.train,
               method = 'gbm', 
               trControl = tCtrl,
               tuneGrid = gbmGrid,
               metric = 'RMSE',
               maximize = F)
