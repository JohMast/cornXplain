#Needed Packages
#library(ncdf4) This not as I already calculated Par in .csv files
library(rgdal)
library(gtools)
library(raster)
library(reshape2)
library(terra)
library(dplyr)
library(sf)
library(ehaGoF)

# Its the main code but I am just explaining as an example 
# Reading .csv file (Yearly) # Yearly it takes very less time; however, it takes a lot of time if we run all years together

# For example: 2003
Corn_predictors_2003 <- read.csv("Supp_Data/merge_years_all_corn_yield.csv");

Corn_predictors_2003 <- 
  Corn_predictors_2003 |> 
  filter(year!=2019)

# read data (berry data)

# link PAR from csv

# selecting flowering period 11

# calculate aggregate metrics for each year & county

# MODELING

## 2003-2018 run crop model -> biomass and FPAR and APAR

## 2003-2018 calculate LAI -> LAI

## 2003-2018 random forest training&tuning

## 2019 run crop model -> biomass and FPAR and APAR

## 2019 calculate LAI -> LAI

## 2019 random forest prediction

# When app is running

## Scenario:

## 2019 run crop model -> biomass and FPAR and APAR

## 2019 calculate LAI -> LAI

## 2019 random forest prediction

 
 ######### START of LUE MODEl ########################
      ###### LUE MODEL Variables ######
 
 fun_fpar <- function(ndvi){ndvi*1.4371 - 0.4039}  # R2 0.851 for Corn by Yang et al 2012
 fun_apar <- function(par,fpar){par*fpar}
 fun_biomass <- function (ndvi) {1252 *ndvi - 83.6 }  # relationship between biomass and ndvi is derived by literature by Dhillon et al 2020, 2022a, 2022b
 # Calculating FPAR
 Corn_predictors_2003$FPAR <- fun_fpar(Corn_predictors_2003$NDVI)
 
 #Calculating APAR
 Corn_predictors_2003$APAR <- fun_apar(Corn_predictors_2003$PAR, Corn_predictors_2003$FPAR)
 
 #Calculating the Temperature Stress  
# tmin_values  <- df_corn_dates_sf$tmin
# tmax_values  <- df_corn_dates_sf$tmax
# tmean_values<-(tmin_values + tmax_values)/2
# tmin_sv<-c()
# for (j in 1:length(tmin_values)){
#   if (tmin_values[j]<= 13){
#     tmin_sv[j] <- 0
#   } else if (tmax_values[j] >= 30){
 #    tmin_sv[j] <- 1
 #  } else {
 #    tmin_sv[j] <- (tmax_values[j]* 0.067)-1
 #  }
 #}
 
# Corn_predictors_2003$Tstress <- tmin_sv

 ####### Calculating Biomass #####################
 Corn_predictors_2003$BIomass <- fun_biomass(Corn_predictors_2003$NDVI) # Literature by Dhillon et al 2020, 2022a, 2022b
 
 ####################### Calculating LAI ##########################
 fun_LAI <- function(ndvi){(ndvi*9.75 - 0.311)} # Extra Variable to improve the prediction accuracy Dhillon et al 2022b (in Review Process)
 
 Corn_predictors_2003$LAI <- fun_LAI(Corn_predictors_2003$NDVI)
 
 ######### End of LUE MODEl ########################
 
 ######### START of Random Forest MODEl ########################
 
 #Load required packages
 library(ggplot2)
 library(cowplot)
 library(randomForest)
 library(caret)
 library(tidyverse)
 library(DALEX)
 
 data1<- Corn_predictors_2003
 #Improve the data
 data1<-data1[, !(colnames(data1) %in% c("year","id","acq_within_year","date","ID","DOY"))]
 #head(data1)
 data1<-data1[complete.cases(data1), ]
 str(data1)
 #Splitting the data into training and validation data
 #Training set : Validation set = 70:30
 set.seed(100)
 train <- createDataPartition(data1$yield, p = .70, list = FALSE)
 TrainSet <- data1
 
 #Define Train Control 
 #Create grid search
 tunegrid <- expand.grid(.mtry = c(1:7)) # divided by three to the total number of predictors
 train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10, savePredictions = TRUE, search = "grid")
 train.control
 
 #Train the model
 rf_model <- train(yield ~., data = TrainSet, method = "rf", metric = "RMSE", tuneGrid = tunegrid, trControl = train.control)
 rf_model
 
 #predict on TrainSet
 predTrain <- predict(rf_model,TrainSet, type = "raw")
 predTrain
 postResample(predTrain, TrainSet$yield)
 
 #Plot eastimated vs. acutal values for TrainSet
 model_Train_Pred <- lm(predTrain ~ TrainSet$yield, data= TrainSet)
 model_Train_Pred
 summary(model_Train_Pred)
 #ggplot(TrainSet, aes(yield, predTrain)) +
   #geom_point(col = "blue") +
   #geom_smooth(method = "lm", se = FALSE, col ="red") +
   #labs(x='Observed Yield', y='Observed Yield', title='Predicted versus Observed Yield')
 
 #predict on Validset
 predValid <- predict(rf_model, ValidSet, type = "raw")
 predValid
 postResample(predValid, ValidSet$yield)
 
 #plot eastimated vs. actual values for ValidSet
 model_Valid_Pred <- lm(predValid ~ ValidSet$yield, data= ValidSet)
 model_Valid_Pred
 summary(model_Valid_Pred)
 
 
 
 #Variable Importance 
 rf_model_Imp <- varImp(rf_model, useModel = T, scale = T)
 
 imp <- as.data.frame(rf_model_Imp$importance)
 
 imp <- data.frame(overall = imp$Overall,
                   names   = rownames(imp))
 
 imp_sort<-imp[order(imp$overall,decreasing = T),]
 
 sel_20_var<-imp_sort[c(1:30),]
 
 names(sel_20_var)<-c("value","variable")
 
 p <- ggplot(data=sel_20_var, aes(x=value, y=reorder(variable, value))) +
   geom_bar(aes(fill=value),stat="identity", position=position_dodge(),width=0.4,color="black")+
   theme_minimal()+labs(x="Variable Importance", y = "Climate Variables", fill="") +
   theme(text = element_text(size=12), axis.text.x = element_text(angle=0, hjust=0.5,color = "black",size=12),axis.text.y = element_text(color = "black",size=12) )
 
 p
 
 p<-p +scale_fill_continuous(high = "#238b45",low = "#edf8e9")+guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
 
 p
 
 ######### END of THE Random Forest MODEl ########################
 
 
 
 
 
 