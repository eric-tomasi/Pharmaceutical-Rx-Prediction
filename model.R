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