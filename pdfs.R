library(tidyverse)
library(janitor)
library(gsheet)
library(googledrive)
library(googlesheets4)

library(googledrive)
gs4_deauth()


drive_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
1

# drive_auth_configure(api_key = "AIzaSyAJI5TfyRDt6YEcsIXLWzVw3Yr2DLZtB1M")
# gs4_auth()
# # options(gargle_oauth_cache = '.secrets')
# 
# options(gargle_oauth_email = TRUE)
# 
# list.files(".secrets/")
# 
# gs4_auth(
#   cache = ".secrets",
#   email = "aljasriin@gmail.com"
# )

# drive_auth(
#   email = gargle::gargle_oauth_email(),
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/drive",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL
# )



##Koroonaviiruse levik Eestis (graafik)----


viimased_14 <- read_csv("https://opendata.digilugu.ee/opendata_covid19_tests_total.csv") %>% 
  clean_names()

haiglaravil <- read_csv("https://opendata.digilugu.ee/opendata_covid19_hospitalization_timeline.csv") %>% 
  clean_names()

viimane <- viimased_14 %>% 
  filter(statistics_date == max(statistics_date)) %>% 
  select(uued_juhtumid=daily_cases, 
         nakatunute_suhtarv = per_population, 
         kuupaev = statistics_date) %>% 
  bind_cols(haiglaravil %>% 
              filter(statistics_date == max(statistics_date)) %>% 
              select(haiglaravil = active))


doose_paevas <- read_csv("https://opendata.digilugu.ee/covid19/vaccination/v3/opendata_covid19_vaccination_total.csv") %>%
  clean_names() %>% 
  filter(statistics_date == max(statistics_date)) %>% 
  #select(5, 7) %>% 
  group_by(measurement_type) %>% 
  summarise(doose_paevas_kokku = sum(daily_count)) %>% 
  filter(measurement_type == "DosesAdministered") %>% 
  select(-1)

vakts <- read_csv("https://opendata.digilugu.ee/covid19/vaccination/v3/opendata_covid19_vaccination_total.csv") %>%
  clean_names() %>% 
  filter(statistics_date == max(statistics_date)) %>% 
  select(1,4,5,8) %>% 
  filter(measurement_type != "DosesAdministered", 
         vaccination_series %in% c(1,2)) %>% 
  mutate(vakts_tuup = ifelse((vaccination_series == 1 & measurement_type == "FullyVaccinated"), 
                             "lopetatud_vaktsineerimine", 
                             "vahemalt_1_doos")) %>% 
  mutate(vakts_tuup = ifelse(vaccination_series == 2 , 
                             "tohustusdoosi_saanud", 
                             vakts_tuup)) %>% 
  select(-1, -2, -3) %>% 
  pivot_wider(names_from = 2, values_from = 1)


kokku <-viimane %>% 
  bind_cols(vakts) %>% 
  bind_cols(doose_paevas) %>% 
  select(kuupaev, everything()) %>% 
  #bind_rows(eelmine) %>% 
  unique() %>% 
  arrange(desc(kuupaev)) %>% 
  mutate(test = Sys.time())
gs4_auth()
1
mydataurl <-("https://docs.google.com/spreadsheets/d/1rlBv2-427pL7-KhVLPC5eQ1Ypm9QN79oVoXhU0jsVK4/edit#gid=0")      

sheet_write(kokku, mydataurl, sheet = 1)



