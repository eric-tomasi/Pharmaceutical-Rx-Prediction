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



######################### Exploratory Data Analysis #########################
head(pharma)
summary(pharma)

# Data Preparation and Cleaning

#Removing records with "NATIONAL ASSIGNMENT"
pharma <- pharma %>% 
  filter(Last.Name != "NATIONAL ASSIGNMENT") %>% 
  #dropping TIMOLOL MAL/DORZ HCL/PF since there is no volume
  filter(Product != "TIMOLOL MAL/DORZ HCL/PF")

#Remove unnecessary columns
pharma <- pharma %>%
  select(-c(1,3,4,5,6,7,8,9,14:37,39, 41))

#Rename columns for ease of use
pharma <- pharma %>%
  rename(Calls = Number.of.Calls.on.Physician.in.Last.12.Months, 
         TRx = Total.TRx.Count.for.Last.12.Months)

#Combine Specialties other than Optometrist and Opthalmologist into one group
pharma <- pharma %>%
  mutate(Specialty = factor(case_when(!Specialty %in% c("OPHTHALMOLOGY", "OPTOMETRIST") ~ "OTHER", 
                                      TRUE ~ Specialty)))

#Fill in null values with defaults
pharma <- pharma %>%
  mutate(Region = factor(case_when(Region == "" ~ "Whitespace",
                                   Region == "UNASSIGNED" ~ "Whitespace",
                                   TRUE ~ Region)),
         Current.Target = factor(case_when(Current.Target == "" ~ "N",
                                           TRUE ~ Current.Target)))



#change datatypes of columns and create factor variables
pharma <- pharma %>%
  mutate(IQVIA.ID = as.character(IQVIA.ID), 
         Product = as.factor(Product))


#Sum TRx and max of calls
pharma <- pharma %>%
  group_by(IQVIA.ID, Region, Specialty, Current.Target, Product) %>%
  summarise(Calls = max(Calls),
            TRx = sum(TRx))

#Pivot products into columns
pharma <- pharma %>% 
  pivot_wider(names_from = "Product", values_from = "TRx") %>%
  as.data.frame(pharma)


#Fill NA with 0
pharma <- pharma %>%
  replace(is.na(.), 0)

#combine logical groups of products to reduce dimensionality
pharma <- pharma %>%
  mutate(generic_PGA = LATANOPROST + TRAVOPROST, 
         branded_PGA = LUMIGAN + XALATAN + `TRAVATAN Z` + XELPROS,
         generic_beta_blocker = `TIMOLOL MAL` + `TIMOLOL MAL/DORZ HCL` + `TIMOLOL MAL/DORZ HCL /AURO`,
         branded_beta_blocker = BETIMOL + ISTALOL + TIMOPTIC + `TIMOPTIC-XE`,
         rho_kinase = RHOPRESSA + ROCKLATAN,
         alpha_agonist = `BRIMONIDINE TART` + `ALPHAGAN P` + SIMBRINZA,
         combo = AZOPT + COSOPT + `COSOPT PF` + VYZULTA + COMBIGAN,
         non_glaucoma = BESIVANCE + AZASITE + GATIFLOXACIN + MOXEZA + `TOBRADEX ST` + VIGAMOX + ZYMAXID) %>%
  select(-c(6:28,30:37))

pharma <- pharma %>% select(-non_glaucoma)

#Filter out doctors with no glaucoma prescriptions at all
pharma <- pharma %>%
  filter(ZIOPTAN + generic_PGA + branded_PGA + generic_beta_blocker + branded_beta_blocker + rho_kinase + alpha_agonist + combo >= 1)

#define response variable (Zioptan) as binary
pharma <- pharma %>%
  mutate(response = factor(case_when(ZIOPTAN >= 1 ~ 1, 
                                     ZIOPTAN <  1 ~ 0)))

#create balanced dataset
pharma_no_zioptan <- pharma %>%
  filter(response == 0)

pharma_zioptan <- pharma %>%
  filter(response == 1)

set.seed(23)
pharma_no_zioptan <- pharma_no_zioptan[sample(nrow(pharma_no_zioptan), nrow(pharma_zioptan)), ]

pharma <- rbind(pharma_zioptan, pharma_no_zioptan)

#look at relationship between predictors and response
#isolate categorical variables
pharma_cat <- pharma %>%
  select(Specialty, Current.Target, Region, response)


#plot bar charts
ggplot(data=pharma_cat, aes(x=Specialty, fill=response)) + 
  geom_bar( position='dodge') +
  ggtitle("Specialty")

ggplot(data=pharma_cat, aes(x=Current.Target, fill=response)) + 
  geom_bar(position='dodge') +
  ggtitle("Current Target")

ggplot(data=pharma_cat, aes(x=Region, fill=response)) + 
  geom_bar(position='dodge') +
  ggtitle("Region")



#isolate quantitative variables
pharma_quant <- pharma %>%
  select(-Specialty, -Current.Target, -IQVIA.ID, -Region, -response)

#correlation matrix
correl <- cor(pharma_quant)

par(mfrow=c(1,1))
# visualizing correlogram
corrplot(correl, method="color", diag=FALSE, type='upper')

#Strongest to weakest correlation table
cor_mat <- as.data.frame.table(cor(pharma_quant)) %>%
  filter(Var1 == "ZIOPTAN") %>%
  rename(Product1 = Var1, Product2 = Var2, Correlation_Coefficient = Freq) %>%
  mutate(Correlation_Coefficient = round(Correlation_Coefficient,3)) %>%
  arrange(desc(Correlation_Coefficient))


#remove ZIOPTAN  for modeling
pharma <- pharma %>%
  select(-ZIOPTAN)

######################### Model selection via cross-validation #########################
#Data
data_used = pharma

#hyper-parameters
lambdalist = c(0:5)/10
sizelist = c(1:5)
clist = c(.001, .01, 1, 5, 10)
klist = c(1,3,5)
  
#cv definition
ctrl = trainControl(method = "cv", number = 5)

#Fit Random Forest
fit_RandomForest_init = train(response ~ . -IQVIA.ID,
                              data = data_used,
                              method = "rf",
                              trControl = ctrl)

#Fit ANN
fit_ANN_init = train(response ~ . -IQVIA.ID,
                     data = data_used,
                     method = "nnet",
                     tuneGrid = expand.grid(size = sizelist, 
                                            decay = lambdalist),
                     preProc = c("center", "scale"),
                     maxit = 2000,
                     trace = FALSE,
                     trControl = ctrl)

#Fit KNN
fit_KNN_init = train(response ~ . -IQVIA.ID,
                     data = data_used,
                     method = "knn",
                     #tuneGrid = expand.grid(k = klist),
                     tuneLength = 5,
                     preProc = c("center", "scale"),
                     trControl = ctrl)

# Fit SVM w linear kernel
fit_SVM_init = train(response ~ . -IQVIA.ID,
                    data = data_used,
                    method = "svmLinear",
                    tuneGrid = expand.grid(C = clist),
                    preProcess = c("center","scale"),
                    prob.model = TRUE,
                    trControl = ctrl)

# Fit LR
fit_LR_init = train( response ~ . -IQVIA.ID,
                     data = data_used,
                     method = "glm",
                     family="binomial",
                     preProcess = c("center","scale"),
                     trControl = ctrl)


#refit both models with new hyperparameters to maximize performance based on initial run
#New hyperparameters
lambdalist2 = c(0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175)
sizelist2 = c(1:5)
clist2 = c(8, 9, 10, 11, 12, 15)
klist2 = c(12,13,14,15,20)
mtrylist2 = c(2,3,4,5,6,7)

#Fit Random Forest
fit_RandomForest = train(response ~ . -IQVIA.ID,
                         data = data_used,
                         method = "rf",
                         tuneGrid = expand.grid(mtry = mtrylist2),
                         trControl = ctrl)

#Fit ANN
fit_ANN = train(response ~ . -IQVIA.ID,
                data = data_used,
                method = "nnet",
                tuneGrid = expand.grid(size = sizelist2, 
                                       decay = lambdalist2),
                preProc = c("center", "scale"),
                maxit = 2000,
                trace = FALSE,
                trControl = ctrl)

#Fit KNN
fit_KNN = train(response ~ . -IQVIA.ID,
                data = data_used,
                method = "knn",
                tuneGrid = expand.grid(k = klist2),
                preProc = c("center", "scale"),
                trControl = ctrl)

# Fit SVM w linear kernel
fit_SVM = train(response ~ . -IQVIA.ID,
                data = data_used,
                method = "svmLinear",
                tuneGrid = expand.grid(C = clist2),
                preProcess = c("center","scale"),
                prob.model = TRUE,
                trControl = ctrl)

# Fit LR
fit_LR = train( response ~ . -IQVIA.ID,
                data = data_used,
                method = "glm",
                family="binomial",
                preProcess = c("center","scale"),
                trControl = ctrl)

######################### Double cross-validation for model assessment #########################

##### model assessment OUTER shell #####
# prep data and produce loops for 5-fold cross-validation for model ASSESSMENT
raw_data <- pharma %>% select(-IQVIA.ID)
n = dim(raw_data)[1]
nfolds = 5
groups = rep(1:nfolds,length=n) 
set.seed(101)
cvgroups = sample(groups,n)  

# set up storage for predicted values from the double-cross-validation
allpredictedCV = rep(NA,n)

# set up storage to see what models are "best" on the inner loops
allbestTypes = rep(NA,nfolds)
allbestPars = vector("list",nfolds)

# loop through outer splits
for (ii in 1:nfolds)  {  
  groupii = (cvgroups == ii)
  train_set = raw_data[!groupii, ]
  test_set = raw_data[groupii, ]
  
  #specify data to be used
  data_used = train_set
  
  # set up training method
  set.seed(101)
  ctrl = trainControl(method = "cv", number = 5)
  
  #Fit Random Forest
  fit_RandomForest = train(response ~ . ,
                           data = data_used,
                           method = "rf",
                           tuneGrid = expand.grid(mtry = mtrylist2),
                           trControl = ctrl)
  
  #Fit ANN
  fit_ANN = train(response ~ . ,
                  data = data_used,
                  method = "nnet",
                  tuneGrid = expand.grid(size = sizelist2, 
                                         decay = lambdalist2),
                  preProc = c("center", "scale"),
                  maxit = 2000,
                  trace = FALSE,
                  trControl = ctrl)
  
  
  ############# identify best model  #############
  # all best models
  all_best_Types = c("RF", "ANN")
  all_best_Pars = list(fit_RandomForest$bestTune,fit_ANN$bestTune)
  all_best_Models = list(fit_RandomForest, fit_ANN)
  all_best_Accuracy = c(min(fit_RandomForest$results$Accuracy),
                        min(fit_ANN$results$Accuracy))
  
  
  
  one_best_Type = all_best_Types[which.max(all_best_Accuracy)]
  one_best_Pars = all_best_Pars[which.max(all_best_Accuracy)]
  one_best_Model = all_best_Models[[which.max(all_best_Accuracy)]]
  
  allbestTypes[ii] = one_best_Type
  allbestPars[[ii]] = one_best_Pars
  
  #Store prediction
  if (one_best_Type == "RF") {  
    allpredictedCV[groupii] = predict(fit_RandomForest, newdata = test_set)
  } else if (one_best_Type == "ANN") { 
    allpredictedCV[groupii]  = predict(fit_ANN, newdata = test_set)
  } 
  
}


# identify best model on each of the splits
allbestTypes
allbestPars
# print individually
for (j in 1:nfolds) {
  writemodel = paste("The best model at loop", j, 
                     "is of type", allbestTypes[j],
                     "with parameter(s)",allbestPars[[j]])
  print(writemodel, quote = FALSE)
}



#assessment
y = pharma$response
conf_mat = table(allpredictedCV, y); conf_mat
accuracy = sum(diag(conf_mat))/sum(conf_mat); accuracy




######################### Fit final model to entire dataset #########################
final_model <- randomForest(response~ . -IQVIA.ID, data=pharma, mtry=6, importance=TRUE)

predictions = predict(final_model)

#Accuracy
conf_mat = table(predictions, y); conf_mat
accuracy = sum(diag(conf_mat))/sum(conf_mat); accuracy





######################### Model Visualizations #########################

#Confusion Matrix
confusion_matrix <- as.data.frame(table(predictions, y))

ggplot(data = confusion_matrix,
       mapping = aes(x = predictions,
                     y = y)) +
  labs(x = "Predicted", 
       y = 'Actual', 
       title = 'Confusion Matrix for Random Forest') +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq))) +
  scale_fill_gradient(low = "light blue",
                      high = "light green")

#Variable importance
importance(final_model)
varImpPlot(final_model)


partialPlot(final_model, pred.data = pharma,
            x.var = Calls, which.class=1)
partialPlot(final_model, pred.data = pharma,
            x.var = Specialty, which.class=1)
partialPlot(final_model, pred.data = pharma,
            x.var = combo, which.class=1)
partialPlot(final_model, pred.data = pharma,
            x.var = generic_PGA, which.class=1)
