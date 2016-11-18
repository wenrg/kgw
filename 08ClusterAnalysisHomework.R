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
model.saturated = lm(loss ~., data = allstate.train[, -1])
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

## differences of the levels in cat vars between train.csv and test.csv
lst = c()
for (i in 2:117){
  lst[i-1] = identical(levels(allstate.train[, i]), levels(allstate.test[, i]))
}
cat.different = which(lst == F) + 1

for (i in cat.different){
  cat("cat", i-1,"train set has", setdiff(allstate.train[, i], allstate.test[, i]), ';')
  cat("test set has", setdiff(allstate.test[, i], allstate.train[, i]),"\n")
}
# cat 89 train set has I ;test set has F 
# cat 90 train set has G ;test set has  
# cat 92 train set has F ;test set has G E 
# cat 96 train set has  ;test set has H 
# cat 99 train set has  ;test set has U 
# cat 101 train set has N U ;test set has  
# cat 102 train set has H J ;test set has  
# cat 103 train set has  ;test set has M 
# cat 105 train set has R S ;test set has  
# cat 106 train set has  ;test set has Q 
# cat 109 train set has BM CJ BV BY BT B BF BP J AG AK ;test set has AD 
# cat 110 train set has BK H BN DV EI BD BI AN AF CB EH ;test set has BH CA EN 
# cat 111 train set has D ;test set has L 
# cat 113 train set has BE T AC ;test set has AA R 
# cat 114 train set has X ;test set has  
# cat 116 train set has BI V BL X FS P GQ AY MF JD AH EV CC AB W AM IK AT JO AS JN BF DY IB EQ JT AP MB C IO DQ HO MT FO JI FN HU IX ;
#          test set has AQ EM FY AI N ET KO BJ IW DB LP MX BR BH JS ER A BN BE IS LS HS EX 

