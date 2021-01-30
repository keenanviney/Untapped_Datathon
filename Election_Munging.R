setwd("~/2011_Election/")
library(tidyverse)
library(raster)
library(fuzzyjoin)

read_append <- function(file_name) {read_csv(file_name, col_names = FALSE) %>% mutate(filename = file_name)}
tbl <- list.files(pattern = ".csv", full.names = TRUE) %>% map_df(~read_append(.))

tbl_renamed <- tbl %>% dplyr::select(X1, X2, X11, X13, X14, X16, X17) %>% 
  rename(ED_Name_11 = X2, Candidate_First_Name_11 = X11, Candidate_Last_Name_11= X13, Political_Party_11 = X14) %>%
  mutate(Incumbant_Indicator_11 = ifelse(X16 == "Y", 1, 0), Elected_Indicator_11 = ifelse(X17 == "Y", 1, 0)) %>%
  mutate(ED_Number_11 = as.numeric(X1))

tbl_renamed <- tbl_renamed[!is.na(as.numeric(as.character(tbl_renamed$ED_Number_11))),]

Federal_Districts_2011 <- tbl_renamed %>% filter(Elected_Indicator_11 == 1) %>% distinct(ED_Number_11, .keep_all = TRUE)

setwd("~/Desktop/Portfolio_Projects/Untapped_Datathon/Election_Results/")

df_2015 <- read_csv("~/2015_electoral_district_results.csv", col_names = TRUE)

Federal_Districts_2015 <- df_2015 %>% dplyr::select(`Electoral District Name/Nom de circonscription`, 
                         `Electoral District Number/Numéro de circonscription`, `Elected Candidate/Candidat élu`) %>% 
  mutate(Political_Party_15 = str_extract(`Elected Candidate/Candidat élu`, 
                                          "Liberal/Libéral|NDP-New Democratic Party/NPD-Nouveau Parti démocratique|Conservative/Conservateur|Bloc Québécois/Bloc Québécois|Green Party/Parti Vert"),
         Candidate_Name_15 = str_remove(`Elected Candidate/Candidat élu`, 
                                         " Liberal/Libéral| NDP-New Democratic Party/NPD-Nouveau Parti démocratique| Conservative/Conservateur|Bloc Québécois/Bloc Québécois| Green Party/Parti Vert")) %>% 
  dplyr::select(-`Elected Candidate/Candidat élu`) %>% 
  rename(ED_Name_15 = `Electoral District Name/Nom de circonscription`, 
         ED_Number_15 = `Electoral District Number/Numéro de circonscription`)

#Election_Results_Merge <- left_join(Federal_Districts_2015, Federal_Districts_2011, by = c("Electoral_District_Number_15" = "Electoral_District_Number_11")) %>% 
#  dplyr::select(-X16, -X17) %>% mutate(Candidate_Name_11 = paste0(Candidate_Last_Name_11,", ",Candidate_First_Name_11)) %>% 
#  dplyr::select(-Candidate_Last_Name_11, -Candidate_First_Name_11)


#Election_Results_Merge <- mutate_if(as_tibble(Election_Results_Merge), is.character, stringr::str_replace_all, pattern = "Liberal/Libéral", replacement = "Liberal") %>% 
#  mutate_if(is.character, stringr::str_replace_all, pattern = "NDP-New Democratic Party/NPD-Nouveau Parti démocratique", replacement = "NDP-New Democratic Party") %>% 
#  mutate_if(is.character, stringr::str_replace_all, pattern = "Conservative/Conservateur", replacement = "Conservative") %>% 
#  mutate_if(is.character, stringr::str_replace_all, pattern = "Bloc Qu\xe9b\xe9cois", replacement = "Bloc Québécois") %>% 
#  mutate_if(is.character, stringr::str_replace_all, pattern = "Bloc Québécois/Bloc Québécois", replacement = "Bloc Québécois") %>%
#  mutate_if(is.character, stringr::str_replace_all, pattern = "Green Party/Party Vert", replacement = "Green Party")

Election_Districts_Shape <- shapefile("~/Electoral_Pipeline_REV3.shp")

Election_Results_Fuzzy_Merge <- Federal_Districts_2015 %>% 
  stringdist_left_join(Federal_Districts_2011, 
                       by = c(ED_Name_15 = "ED_Name_11"), 
                       distance_col = NULL) %>% 
  rename(FED_NUM = ED_Number_15) %>% 
  distinct(FED_NUM, .keep_all = TRUE)

Election_Pipeline_REV4 <- merge(Election_Districts_Shape, Election_Results_Fuzzy_Merge, key = "FED_NUM")

shapefile(Election_Pipeline_REV4, "Election_Pipeline_REV4.shp", overwrite = TRUE)
