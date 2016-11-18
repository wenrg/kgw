library(dplyr)
library(shinythemes)
library(plotly)


allstate.train = read.csv('train.csv', header = TRUE)

allstate.sub = allstate.train[1:10,]
