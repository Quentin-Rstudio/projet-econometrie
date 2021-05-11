library(tidyverse)
library(readxl)


setwd("C:/Users/quent/Desktop/ENS Paris-Saclay/1A/1S2/Econométrie/TDs/Bases de données/Documents/projet-econometrie")
# setwd("~/Documents/ENS/econometrie/projet_r")


EVS <- read_dta("ZA7500_v4-0-0.dta")
depenses <-read_xlsx("depenses-env.xlsx")

EVS_env <-EVS %>% 
  select(caseno, year, country, c_abrv, v7, v199, v200, v201, v202, v203, v204) %>%
  mutate(env1 = as.numeric(case_when(v199 %in% c("4","5") ~ "0", 
                                     v199 %in% c("1", "2", "3") ~ "1",
                                     v199 %in% c("8", "9") ~ "NA")), 
         env2 = as.numeric(case_when(v200 %in% c("1","2") ~ "0", 
                                     v200 %in% c("3", "4", "5") ~"1",
                                     v200 %in% c("8", "9") ~ "NA")), 
         env3 = as.numeric(case_when(v201 %in% c("1","2") ~ "0", 
                                     v201 %in% c("3", "4", "5") ~"1",
                                     v201 %in% c("8", "9") ~ "NA")), 
         env4 = as.numeric(case_when(v202 %in% c("1","2") ~ "0", 
                                     v202 %in% c("3", "4", "5") ~"1",
                                     v202 %in% c("8", "9") ~ "NA")), 
         env5 = as.numeric(case_when(v203 %in% c("1","2") ~ "0", 
                                     v203 %in% c("3", "4", "5") ~"1",
                                     v203 %in% c("8", "9") ~ "NA")), 
         env6 = as.numeric(case_when(v204 %in% c("1") ~ "1", 
                                     v204 %in% c("2") ~"0",
                                     v204 %in% c("8", "9") ~ "NA")))%>%
  group_by(country, c_abrv) %>%
  summarize(
    effectif = n_distinct(caseno),
    perc_env = 100*mean(mean(env1 == 1, na.rm = T), 
                        mean(env2 == 1, na.rm = T), 
                        mean(env3 == 1, na.rm = T), 
                        mean(env4 == 1, na.rm = T), 
                        mean(env5 == 1, na.rm = T)), 
    soutien_pol_env = 100*mean(env6 == 1, na.rm = T))

names(EVS_env)[names(EVS_env)=="c_abrv"] <- "Pays" 
names(EVS_env)[names(EVS_env)=="country"] <- "code"


EVS_et_depenses <- EVS_env %>%
  full_join(depenses, by = "Pays")%>%
  filter(Pays != "NA") %>%
  select(Pays, perc_env, soutien_pol_env, MOI_EUR_2016, PC_GDP_2016) 


eai_data <- read_excel("EAI-data.xlsx") %>%
  filter(EAI != "NA")

code_pays <- read_csv("all.csv")

env_et_depenses <- EVS_et_depenses %>%
  full_join(eai_data, by = "code") %>%
  full_join(code_pays, by = c("Pays" = "alpha-2")) %>%
  filter(EAI != "NA", perc_env != "NA", PC_GDP_2016 != "NA") %>%
  group_by(Pays) %>%
  summarize(Nom_pays = first(name), 
            code2 = first(code),
            code = first(`alpha-3`), 
            Pays = first(Pays), 
            sensi_env = (perc_env + soutien_pol_env + EAI)/3, 
            perc_env = first(perc_env), 
            soutien_pol_env = first(soutien_pol_env), 
            conn_env = first(EAI), 
            depenses_brutes_env = first(MOI_EUR_2016), 
            part_depenses_env = first(PC_GDP_2016))


gini <- read_excel("GINI_2017.xlsx") %>%
  filter(EDUC_SUP_2017 != 'NA')

epi2020results <- read.csv("epi2020results.csv") %>%
  select(code, EPI.new)

gini_et_plus <- env_et_depenses %>%
  left_join(gini, by = "code") %>%
  select(-Country)

base_complete <- gini_et_plus %>%
  left_join(epi2020results, by = c("code2" = "code")) %>%
  mutate(code2 = as.character(code2)) %>%
  select(Pays, code, code2, everything())

write_csv(base_complete, "base_complete.csv")
