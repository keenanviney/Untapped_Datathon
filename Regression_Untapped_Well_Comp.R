###---------------------------------------------------
#  Well Production Regression - Untapped Oil 2019
###---------------------------------------------------

set.seed(3801)

## Load Packages

library(mlr)
library(tidyverse)
library(lubridate)
library(fastDummies)
library(xgboost)

## Read in data

Raw_Train <- read_csv("~/Header_train.csv", col_names = TRUE)

Raw_Validation <- read_csv("~/Header_validation.csv", col_names = TRUE)

Raw_Test <- read_csv("~/Header_test.csv", col_names = TRUE)

Test_IDs <- read_csv("~/Test_Rows.csv", col_names = TRUE) %>% select(EPAssetsId, UWI)

Viking_Train_Raw <- read_csv("~/Viking_train.csv", col_names = TRUE)

Viking_Validation_Raw <- read_csv("~/Viking_validation.csv", col_names = TRUE)

## Joining in Well Classes and binding Training and Validation (for custom splits)

Joined_Train <- right_join(Raw_Train, Viking_Train_Raw, by = "EPAssetsId")

Joined_Validation <- right_join(Raw_Validation, Viking_Validation_Raw, by = "EPAssetsId")

Joined_Test <- right_join(Raw_Test, Test_IDs, by = "EPAssetsId")

## Changing dates into days, imputing NAs, removing some columns, and creating a flag if the Licensee and Current Operator are different firms

is_na_val <- function(x) x %in% c("Not Available", "<NA>")
days_from_origin <- function(y) {as.numeric(interval(ymd("1883-01-01"), mdy_hms(y)), "days")}


Training_Data <- bind_rows(Joined_Train, Joined_Validation, .id = "PARENT_TABLE_NAME") %>%    # Union of Train and Validation
  mutate_all(funs(ifelse(is_na_val(.), NA, .))) %>% 
  mutate_at(c("LicenceDate", "ConfidentialReleaseDate", "SpudDate", "FinalDrillDate", 
              "RigReleaseDate", "StatusDate", "CompletionDate", "SurfAbandonDate"), days_from_origin) %>% 
  mutate(Operator_Transfer = case_when(Licensee == CurrentOperator ~ 1, Licensee != CurrentOperator ~ 0)) %>% 
  mutate(No_Current_Operator_Flag = case_when(CurrentOperator == "No Current Operator" ~ 1, CurrentOperator != "No Current Operator" ~ 0)) %>% 
  mutate(Well_Type_Unspecified = case_when(WellTypeStandardised == "Unspecified" ~ 1, WellTypeStandardised != "Unspecified" ~ 0)) %>% 
  mutate(Surface_to_Bottom_Euclidean = sqrt(((Surf_Longitude - BH_Longitude)^2) + (Surf_Latitude - BH_Latitude)^2)) %>% 
  select(-PARENT_TABLE_NAME, -EPAssetsId, -WellName, -WellNameAmended, -SurveySystem, -Surf_Location, -Surf_Township, -Surf_Meridian,
         -Surf_Range, -Surf_Section, -Surf_LSD, -Surf_TownshipRange, -Surf_QuarterUnit, -Surf_Unit,
         -Surf_Block, -Surf_NTSMapSheet, -Surf_Series, -Surf_Area, -Surf_Sheet, -Surf_QuarterSection, 
         -BH_Location, -BH_TownshipRange, -BH_QuarterUnit, -BH_Unit, -BH_Block, -BH_NTSMapSheet, 
         -BH_Series, -BH_Area, -BH_Sheet, -BH_QuarterSection, -BH_Township, -BH_Meridian, -BH_Range, 
         -BH_Section, -BH_LSD, -Country, -RegulatoryAgency, -PSACAreaName, -Municipality, 
         -LicenseeID, -CurrentOperatorID, -WellTypeStandardised, -UnitName, -UnitFlag, 
         -Completion_Events, -UWI.x, -UWI.y, -LicenceNumber, -StatusSource, -OSArea, 
         -OSDeposit, -WellSymbPt1) %>% 
  select(-Agent, -CurrentOperatorParent, -LicenseeParentCompany, -SurfaceOwner,
         -Licensee, -DrillingContractor, -CurrentOperator, -Pool)    #Interesting features removed to fit memory

Test_Data <- Joined_Test %>% mutate_all(funs(ifelse(is_na_val(.), NA, .))) %>% 
  mutate_at(c("LicenceDate", "ConfidentialReleaseDate", "SpudDate", "FinalDrillDate", 
              "RigReleaseDate", "StatusDate", "CompletionDate", "SurfAbandonDate"), days_from_origin) %>%
  mutate(Operator_Transfer = case_when(Licensee == CurrentOperator ~ 1, Licensee != CurrentOperator ~ 0)) %>% 
  mutate(No_Current_Operator_Flag = case_when(CurrentOperator == "No Current Operator" ~ 1, CurrentOperator != "No Current Operator" ~ 0)) %>%
  mutate(Well_Type_Unspecified = case_when(WellTypeStandardised == "Unspecified" ~ 1, WellTypeStandardised != "Unspecified" ~ 0)) %>%
  mutate(Surface_to_Bottom_Euclidean = sqrt(((Surf_Longitude - BH_Longitude)^2) + (Surf_Latitude - BH_Latitude)^2)) %>% 
  select(-EPAssetsId, -WellName, -WellNameAmended, -SurveySystem, -Surf_Location, -Surf_Township, -Surf_Meridian,
         -Surf_Range, -Surf_Section, -Surf_LSD, -Surf_TownshipRange, -Surf_QuarterUnit, -Surf_Unit,
         -Surf_Block, -Surf_NTSMapSheet, -Surf_Series, -Surf_Area, -Surf_Sheet, -Surf_QuarterSection, 
         -BH_Location, -BH_TownshipRange, -BH_QuarterUnit, -BH_Unit, -BH_Block, -BH_NTSMapSheet, 
         -BH_Series, -BH_Area, -BH_Sheet, -BH_QuarterSection, -BH_Township, -BH_Meridian, -BH_Range, 
         -BH_Section, -BH_LSD, -Country, -RegulatoryAgency, -PSACAreaName, -Municipality, 
         -LicenseeID, -CurrentOperatorID, -WellTypeStandardised, -UnitName, -UnitFlag, -Completion_Events,
         -UWI.x, -UWI.y, -LicenceNumber, -StatusSource, -OSArea, -OSDeposit, -WellSymbPt1) %>% 
  select(-Agent, -CurrentOperatorParent, -LicenseeParentCompany, -SurfaceOwner, 
         -Licensee, -DrillingContractor, -CurrentOperator, -Pool)

## Checking the number of NAs and unique occurances

Training_na_count <- Training_Data %>% map_df(~sum(is.na(.)))

Training_unique_count <- Training_Data %>% map_df(~length(unique(.)))

## Regressions to understand the target relationships

# Regression_Data <- Training_Data %>% select(Normalized_IP_BOE_d, Normalized_IP_Oil_Bbls, Normalized_IP_Gas_Boe_d, Normalized_IP_Water_Bbls)
# 
# summary(lm(Normalized_IP_BOE_d ~ Normalized_IP_Oil_Bbls, data = Regression_Data))
# summary(lm(Normalized_IP_BOE_d ~ Normalized_IP_Gas_Boe_d, data = Regression_Data))
# summary(lm(Normalized_IP_BOE_d ~ Normalized_IP_Water_Bbls, data = Regression_Data))
# 
# 
# summary(lm(Normalized_IP_Oil_Bbls ~ Normalized_IP_Gas_Boe_d, data = Regression_Data))
# summary(lm(Normalized_IP_Oil_Bbls ~ Normalized_IP_Water_Bbls, data = Regression_Data))
# summary(lm(Normalized_IP_Oil_Bbls ~ Normalized_IP_BOE_d, data = Regression_Data))
# 
# 
# summary(lm(Normalized_IP_Gas_Boe_d ~ Normalized_IP_Water_Bbls, data = Regression_Data))
# summary(lm(Normalized_IP_Gas_Boe_d ~ Normalized_IP_BOE_d, data = Regression_Data))
# summary(lm(Normalized_IP_Gas_Boe_d ~ Normalized_IP_Oil_Bbls, data = Regression_Data))
# 
# 
# summary(lm(Normalized_IP_Water_Bbls ~ Normalized_IP_BOE_d, data = Regression_Data))
# summary(lm(Normalized_IP_Water_Bbls ~ Normalized_IP_Oil_Bbls, data = Regression_Data))
# summary(lm(Normalized_IP_Water_Bbls ~ Normalized_IP_Gas_Boe_d, data = Regression_Data))


## Remove some dataframes from memory

rm(Raw_Test, Raw_Train, Raw_Validation, Joined_Train, Joined_Validation, Joined_Test,
   Training_na_count, Training_unique_count, Viking_Train_Raw, Viking_Validation_Raw)


## Create dummy columns for the categorical variables

Training_Data_ML <- dummy_columns(Training_Data, select_columns = c("WellType", "Formation", "LaheeClass", "Confidential", 
                                                                    "WellProfile", "PSACAreaCode", "UnitID", "Open_Hole", 
                                                                    "Province", "Field")) %>% 
  select(-WellType, -Formation, -LaheeClass, -Confidential, 
         -WellProfile, -PSACAreaCode, -UnitID, -Open_Hole, -Province, -Field)

Test_Data_ML <- dummy_columns(Test_Data, select_columns = c("WellType", "Formation", "LaheeClass", "Confidential", 
                                                            "WellProfile","PSACAreaCode", "UnitID", "Open_Hole", 
                                                            "Province", "Field")) %>% 
  select(-WellType, -Formation, -LaheeClass, -Confidential, 
         -WellProfile, -PSACAreaCode, -UnitID, -Open_Hole, -Province, -Field)

## Select only the intersecting vectors

Training_colnames <- colnames(Training_Data_ML)

Test_colnames <- colnames(Test_Data_ML)

intersecting_vectors <- dplyr::intersect(Training_colnames, Test_colnames)

Training_Data_ML <- Training_Data_ML %>% select(intersecting_vectors, Normalized_IP_BOE_d, Normalized_IP_Oil_Bbls, Normalized_IP_Gas_Boe_d, Normalized_IP_Water_Bbls)

Test_Data_ML <- Test_Data_ML %>% select(intersecting_vectors)


## Normalize the numeric vectors

#Numeric_Vector <- colnames(select_if(Training_Data_ML, is.numeric))

Training_Data_ML <- normalizeFeatures(Training_Data_ML, 
                                      method = "standardize", 
                                      cols = c("DaysDrilling","DrillMetresPerDay","TVD", "NumberofWells"))

Test_Data_ML <- normalizeFeatures(Test_Data_ML, 
                                  method = "standardize", 
                                  cols = c("DaysDrilling", "DrillMetresPerDay", "TVD", "NumberofWells"))

## Re-Sample the training and validation data

Training_Data_ML <- Training_Data_ML %>% mutate(ID = row_number()) 

Training_Resampled <- Training_Data_ML %>% sample_frac(0.7) 

Validation_Resampled <- anti_join(Training_Data_ML, Training_Resampled, by = 'ID')

Training_Data_ML <- Training_Data_ML %>% select(-ID)

# Training
# Training_Matrix_Water <- Training_Resampled %>% select(-ID, -Normalized_IP_Water_Bbls, -Normalized_IP_Oil_Bbls, -Normalized_IP_Gas_Boe_d)
# 
# Training_Label_Water <- Training_Resampled %>% select(Normalized_IP_Water_Bbls)
# 
# Training_Matrix_Gas <- Training_Resampled %>% select(-ID, -Normalized_IP_Oil_Bbls, -Normalized_IP_Gas_Boe_d)
# 
# Training_Label_Gas <- Training_Resampled %>% select(Normalized_IP_Gas_Boe_d)
# 
# Training_Matrix_Oil <- Training_Resampled %>% select(-ID, -Normalized_IP_Oil_Bbls)
# 
# Training_Label_Oil <- Training_Resampled %>% select(Normalized_IP_Oil_Bbls)
# 
# #Validation
# Validation_Matrix_Water <- Validation_Resampled %>% select(-ID, -Normalized_IP_Water_Bbls, -Normalized_IP_Oil_Bbls, -Normalized_IP_Gas_Boe_d)
# 
# Validation_Label_Water <- Validation_Resampled %>% select(Normalized_IP_Water_Bbls)
# 
# Validation_Matrix_Gas <- Validation_Resampled %>% select(-ID, -Normalized_IP_Oil_Bbls, -Normalized_IP_Gas_Boe_d)
# 
# Validation_Label_Gas <- Validation_Resampled %>% select(Normalized_IP_Gas_Boe_d)
# 
# Validation_Matrix_Oil <- Validation_Resampled %>% select(-ID, -Normalized_IP_Oil_Bbls)
# 
# Validation_Label_Oil <- Validation_Resampled %>% select(Normalized_IP_Oil_Bbls)

#Full Training
Full_Training_Matrix_Water <- Training_Data_ML %>% 
  drop_na(Normalized_IP_Water_Bbls) %>% 
  select(-Normalized_IP_BOE_d, -Normalized_IP_Water_Bbls, -Normalized_IP_Oil_Bbls, -Normalized_IP_Gas_Boe_d)

Full_Training_Label_Water <- Training_Data_ML %>%
  drop_na(Normalized_IP_Water_Bbls) %>% 
  select(Normalized_IP_Water_Bbls)

Full_Training_Matrix_Gas <- Training_Data_ML %>% 
  drop_na(Normalized_IP_Gas_Boe_d) %>% 
  select(-Normalized_IP_BOE_d, -Normalized_IP_Oil_Bbls, -Normalized_IP_Gas_Boe_d)

Full_Training_Label_Gas <- Training_Data_ML %>% 
  drop_na(Normalized_IP_Gas_Boe_d) %>%
  select(Normalized_IP_Gas_Boe_d)

Full_Training_Matrix_Oil <- Training_Data_ML %>% 
  drop_na(Normalized_IP_Oil_Bbls) %>%
  select(-Normalized_IP_BOE_d, -Normalized_IP_Oil_Bbls)

Full_Training_Label_Oil <- Training_Data_ML %>%
  drop_na(Normalized_IP_Oil_Bbls) %>%
  select(Normalized_IP_Oil_Bbls)

#

rm(Training_Data, Test_Data, Training_colnames, Test_colnames, Validation_Resampled, Training_Resampled)

## Convert to matrix

Test_Matrix <- as.matrix(Test_Data_ML)

Test_XGB <- xgb.DMatrix(data = Test_Matrix)

#Water
Full_Training_Matrix_Water <- as.matrix(Full_Training_Matrix_Water)
Full_Training_Label_Water <- as.matrix(Full_Training_Label_Water)

Full_Training_XGB_Water <- xgb.DMatrix(data = Full_Training_Matrix_Water, label = Full_Training_Label_Water)

#Gas
Full_Training_Matrix_Gas <- as.matrix(Full_Training_Matrix_Gas)
Full_Training_Label_Gas <- as.matrix(Full_Training_Label_Gas)

Full_Training_XGB_Gas <- xgb.DMatrix(data = Full_Training_Matrix_Gas, label = Full_Training_Label_Gas)

#Oil
Full_Training_Matrix_Oil <- as.matrix(Full_Training_Matrix_Oil)
Full_Training_Label_Oil <- as.matrix(Full_Training_Label_Oil)

Full_Training_XGB_Oil <- xgb.DMatrix(data = Full_Training_Matrix_Oil, label = Full_Training_Label_Oil)

#
rm(Full_Training_Label_Gas, Full_Training_Label_Oil, Full_Training_Label_Water,
   Full_Training_Matrix_Gas, Full_Training_Matrix_Oil, Full_Training_Matrix_Water)

## Building a Test Model

params <- list(
  "booster" = "gbtree",
  "objective" = "reg:squarederror",
  "eval_metric" = "mae",
  "eta" = 0.5
)

#watchlist_water <- list(train = Training_XGB_Water, evaluate = Validation_XGB_Water)

Regression_XGB_Test_Water <- xgb.train(
  params = params,
  data = Full_Training_XGB_Water,
  nrounds = 10
)

Test_Predictions_Water <- as.data.frame(predict(Regression_XGB_Test_Water, Test_XGB, reshape = TRUE))

Test_Matrix_Step1 <- bind_cols(Test_Data_ML, Test_Predictions_Water) %>% 
  rename(Normalized_IP_Water_Bbls = 123)

Test_Matrix_Step1 <- as.matrix.data.frame(Test_Matrix_Step1)

Test_XGB_Step1 <- xgb.DMatrix(data = Test_Matrix_Step1)

#Gas Model

Regression_XGB_Test_Gas <- xgb.train(
  params = params,
  data = Full_Training_XGB_Gas,
  nrounds = 10
)

Test_Predictions_Gas <- as.data.frame(predict(Regression_XGB_Test_Gas, Test_XGB_Step1, reshape = TRUE))

Test_Matrix_Step2 <- bind_cols(as.data.frame(Test_Matrix_Step1), Test_Predictions_Gas) %>% 
  rename(Normalized_IP_Gas_Boe_d = 124) %>% 
  select(-Normalized_IP_Water_Bbls, everything())

Test_Matrix_Step2 <- as.matrix(Test_Matrix_Step2)

Test_XGB_Step2 <- xgb.DMatrix(data = Test_Matrix_Step2)

#Oil Model

Regression_XGB_Test_Oil <- xgb.train(
  params = params,
  data = Full_Training_XGB_Oil,
  nrounds = 10
)

Test_Predictions_Oil <- as.data.frame(predict(Regression_XGB_Test_Oil, Test_XGB_Step2, reshape = TRUE))

Final_Test_Predictions <- bind_cols(as.data.frame(Test_Matrix_Step2), Test_Predictions_Oil) %>%
  rename(Normalized_IP_Oil_Bbls = 125)

Regression_Output_File <- bind_cols(Test_IDs, Final_Test_Predictions) %>% 
  select(EPAssetsId, UWI, Normalized_IP_Oil_Bbls, Normalized_IP_Gas_Boe_d, Normalized_IP_Water_Bbls) %>% 
  arrange(EPAssetsId)

write_csv(Regression_Output_File, "~/Regression_Output_File_10R.csv")


###=============================================================================================================
