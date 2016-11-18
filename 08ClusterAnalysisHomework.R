############################################################################ - @wen
plot(allstate.train[, 118], allstate.train[, 130]) # cont117 and cont129, aes(fill = cat_), will find out the key categorical var.

library(ggplot2)
library(vioplot)
library(car)

# setwd()
allstate.train = read.csv('train.csv')
allstate.test = read.csv('test.csv')

cont = 118:132
cat = 2:117
summary(allstate.train[, cont])
sapply(allstate.train[, cont], sd)

# correlation between cont vars
cors = cor(allstate.train[, cont])

pairs(allstate.train[, cont]) #### never run this line...


plot(allstate.train[, 128], allstate.train[, 129])
train_cont = paste(paste('allstate.train[,', 118:132), ']', collapse = ',')
vioplot(allstate.train[, 128], allstate.train[, 129])
vioplot(allstate.train[, 118 ],allstate.train[, 119 ],allstate.train[, 120 ],allstate.train[, 121 ])
vioplot(allstate.train[, 122 ],allstate.train[, 123 ],allstate.train[, 124 ],allstate.train[, 125 ])
vioplot(allstate.train[, 126 ],allstate.train[, 127 ],allstate.train[, 130 ],allstate.train[, 131 ])
vioplot(allstate.train[, 132 ])
 

# transform loss
plot(density(allstate.train[, 132]))
model.saturated = lm(loss ~., data = allstate.train)
ms.summary = summary(model.saturated)
ms.coef = ms.summary$coefficients
bc = boxCox(model.saturated) # lambda = 0, log(y)
loss.log = log(allstate.train$loss)


ggplot(allstate.train, aes(x = cat112, fill = cat112)) + geom_bar() + guides(fill = F)
levels(allstate.train$cat112) # states


plot(allstate.train[, 118], allstate.train[, 123]) # possible linear
plot(allstate.train[, 118], allstate.train[, 126]) # possible linear
plot(allstate.train[, 118], allstate.train[, 127]) # possible linear
plot(allstate.train[, 118], allstate.train[, 130]) # two lines ???

# to be continued ...
