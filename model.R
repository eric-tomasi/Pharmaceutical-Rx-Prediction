#Load in required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggformula)
library(caret)
library(corrplot)
library(RColorBrewer)
library(reshape2)
library(randomForest)
library(tidyr)


# KNN, ANN, SVM, RF, LR

######################### Load dataset  #########################
pharma <- read.csv('pharma.csv')