library(tidyverse)
library(magrittr)
library(readxl)
library(lubridate)

## load data
main_record <- read_xlsx("For Masanao Class - ROMS Full Data Set - March 19th, 2019 Upload.xlsx", sheet = "Master Data Set",col_names = T,na = c("NULL","0000-00-00","Null","null","0000-00-01","0000-00-02","unknown"))

payment_info <- read_xlsx("For Masanao Class - ROMS Full Data Set - March 19th, 2019 Upload.xlsx", sheet = "Claim and Payment Info", col_names = T)


## prep
#delete ROMS ID that contains NA Visit IDs
invalid_roms <- main_record %>% 
  dplyr::filter(is.na(`Visit ID`)) %>%
  dplyr::select(`ROMS ID`) %>% dplyr::pull() %>% unique()

raw <- main_record %>% dplyr::select(`ROMS ID`,
                              Payer,`Payer Category`,Outcome,
                              Age,`Sex (1=male, 2=female)`,
                              `Outcome Score Range`,
                              `Outcome - Is high score good or bad?`,
                              `Body Region`,
                              Surgical,Classification,`Admission Date`,
                              `Admission Outcome Score`,`Admission Pain`,
                              `Injury Date`,`Surgery Date`,Visits,`Length Of Stay (days)`,
                              `Chronic Pain (Yes/No)`,`Start Back Total`,
                              `Start Back Subscore`) %>% 
  dplyr::filter(!`ROMS ID` %in% invalid_roms) %>% 
  dplyr::distinct()

visitid_to_rom <- main_record %>% dplyr::select(`ROMS ID`,`Visit ID`,`Visit Date`) %>% 
  dplyr::filter(!is.na(`Visit ID`)) %>% dplyr::distinct()

payment_info <- dplyr::left_join(payment_info,visitid_to_rom,by = "Visit ID")
## body region needs cleaning
raw %<>% dplyr::mutate(`Body Region` = tolower(trimws(`Body Region`, which = "both")))

roms_payer <- payment_info %>% 
  dplyr::select(`ROMS ID`,PayerCategory) %>% 
  dplyr::filter(!is.na(`ROMS ID`) & !is.na(`PayerCategory`)) %>% 
  dplyr::distinct()
raw_1 <- dplyr::left_join(raw,roms_payer,by = "ROMS ID")
#There are typos
raw_1 %<>% dplyr::mutate(Outcome = trimws(tolower(Outcome)))
## there are negative ages
## impute with mean age
median_age <- median(raw_1$Age[raw_1$Age >0])
raw_1 %<>% dplyr::mutate(Age = dplyr::if_else(Age < 0,median_age, Age))

raw_2 <- raw_1 %>% dplyr::mutate( weeksdiff_injury_to_ads = 
                             as.numeric(
                               difftime(raw_1$`Admission Date` , raw_1$`Injury Date`, units = 'weeks')
                             ) ) %>%  # add a column of days from injury to admission
  dplyr::mutate( weeksdiff_surgery_to_ads = 
            as.numeric(
              difftime(raw_1$`Admission Date` , raw_1$`Surgery Date`, units = 'weeks')
            ) ) %>% # add a column of days from surgery to admission
  dplyr::mutate( `Sex (1=male, 2=female)` = if_else(`Sex (1=male, 2=female)`==1, 0, 1) ) %>%
  dplyr::rename_( 'Sex' = '`Sex (1=male, 2=female)`' ) %>% # 0 -- male , 1 -- female
  dplyr::mutate( `Chronic Pain (Yes/No)` = if_else(`Chronic Pain (Yes/No)`== 'Yes', 1 , 0) ) %>%
  dplyr::rename_('Chronic_Pain' = '`Chronic Pain (Yes/No)`') %>%  # 0 -- no chronic pain, 1 -- chronic pain
  dplyr::mutate(`Outcome - Is high score good or bad?` = dplyr::if_else(`Outcome - Is high score good or bad?`== 'Good', 1, 0) ) %>%
  dplyr::rename_('Outcome_judge' = '`Outcome - Is high score good or bad?`') # 0 -- high score is bad, 1 -- high score is good

raw_3 <- raw_2 %>% 
  tidyr::separate(col = `Outcome Score Range`,into = c("lower_score","upper_score"),sep = "-",convert = T) %>%
  dplyr::mutate(scale_temp = as.numeric(upper_score) - as.numeric(lower_score),
         admin_score = dplyr::if_else(Outcome_judge == 1, 
                               round((`Admission Outcome Score`-lower_score)/scale_temp,2),
                               round((upper_score - `Admission Outcome Score`)/scale_temp,2)),
         admin_pain = (10-`Admission Pain`)/10) %>%
  dplyr::select(-scale_temp,
         -lower_score,
         -upper_score, 
         -Outcome_judge,
         -`Admission Outcome Score`,
         -`Admission Pain`)

dates <- read_csv("semesterdates.csv") %>% pull()
raw_4 <- raw_3 %>% 
  dplyr::mutate(weeks_to_smsend = round(dplyr::if_else(`Admission Date` < dates[1],
                                                          as.numeric(difftime( dates[1], raw_3$`Admission Date`, units = 'weeks')),
                                                       dplyr::if_else(`Admission Date` < dates[2],
                                                                  as.numeric(difftime( dates[2], raw_3$`Admission Date`, units = 'weeks')),
                                                                  dplyr::if_else(`Admission Date` < dates[3],
                                                                          as.numeric(difftime( dates[3], raw_3$`Admission Date`, units = 'weeks')),
                                                                          dplyr::if_else(`Admission Date` < dates[4],
                                                                                  as.numeric(difftime( dates[4], raw_3$`Admission Date`, units = 'weeks')),
                                                                                  dplyr::if_else(`Admission Date` < dates[5],
                                                                                          as.numeric(difftime( dates[5], raw_3$`Admission Date`, units = 'weeks')),
                                                                                          dplyr::if_else(`Admission Date` < dates[6],
                                                                                                  as.numeric(difftime( dates[6], raw_3$`Admission Date`, units = 'weeks')),
                                                                                                  dplyr::if_else(`Admission Date` < dates[7],
                                                                                                          as.numeric(difftime( dates[7], raw_3$`Admission Date`, units = 'weeks')),
                                                                                                          as.numeric(difftime( dates[8], raw_3$`Admission Date`, units = 'weeks'))))))))),2)) %>%
  dplyr::select(-`Admission Date`,
         -`Injury Date`)


raw_5 <- raw_4 %>% dplyr::select(-Payer,
                          -`Payer Category`,
                          -Classification,
                          -`Surgery Date`,
                          -Visits,
                          -`Length Of Stay (days)`,
                          -`Start Back Total`,
                          -`Start Back Subscore`) %>%
  dplyr::distinct()

temp_1 <- raw_5 %>% 
  dplyr::select(-weeksdiff_injury_to_ads,-weeksdiff_surgery_to_ads,
                -weeks_to_smsend,-admin_score,-admin_pain,-Age,-Chronic_Pain) %>% 
  dplyr::distinct()

temp_3 <- temp_1 %>% 
  dplyr::group_by(`ROMS ID`,Sex,`Body Region`,Surgical,PayerCategory) %>% 
  dplyr::summarise(outcome_ct = n()) %>% 
  dplyr::ungroup()

temp_2 <- raw_5 %>% 
  dplyr::select(`ROMS ID`,Outcome,Surgical,`Body Region`,Surgical,PayerCategory,Chronic_Pain,Age,
         weeksdiff_injury_to_ads,weeksdiff_surgery_to_ads,weeks_to_smsend,admin_score,admin_pain) %>%
  dplyr::mutate(weeks_to_treat = dplyr::if_else(is.na(weeksdiff_injury_to_ads),
                                                weeksdiff_surgery_to_ads,
                                                weeksdiff_injury_to_ads)) %>%
  dplyr::group_by(`ROMS ID`,Surgical,`Body Region`,Surgical,PayerCategory) %>% 
  dplyr::summarise(weeks_to_treat = min(weeks_to_treat),
                                                                                  weeks_to_smsend = max(weeks_to_smsend),
                                                                                  admin_pain = min(admin_pain),
                                                                                  admin_score = min(admin_score),
                                                                                  Age = round(max(Age)),
                                                                                  Chronic_Pain = max(Chronic_Pain)) %>%
  dplyr::ungroup()

raw_cleaned <- dplyr::left_join(temp_3,temp_2) %>% dplyr::rename_('body_region' = "`Body Region`")

main_rec_payer <- main_record %>% 
  dplyr::select(`ROMS ID`,Payer) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(!is.na(Payer) & Payer %in% c("BU BCBS","Non BU BCBS"))
raw_cleaned_add_payer <- dplyr::left_join(raw_cleaned,main_rec_payer,by = "ROMS ID")
payercat <- c("Blue Cross Blue Shield","Aetna","Medicare")
bodyregion <- c("cervical","foot/ankle","hip","knee","lumbar","shoulder","thoracic")
raw_cleaned_1<- raw_cleaned_add_payer %>% 
  dplyr::mutate(PayerCategory = dplyr::if_else(!PayerCategory %in% payercat,
                                               "Other",
                                               dplyr::if_else(PayerCategory == "Blue Cross Blue Shield",
                                                              Payer,
                                                              PayerCategory)),
                PayerCategory = dplyr::if_else(is.na(PayerCategory),
                                               "Other",
                                               PayerCategory),
                body_region = dplyr::if_else(body_region %in% bodyregion,body_region,"other")) %>% 
  dplyr::select(-Payer)

mean_week <- mean(raw_cleaned_1$weeks_to_treat,na.rm = T)
raw_cleaned_1[is.na(raw_cleaned_1$weeks_to_treat), ] <- mean_week

payment_ROM <- payment_info %>% 
  dplyr::filter(!is.na(`ROMS ID`)) %>%
  dplyr::group_by(`ROMS ID`) %>% dplyr::summarise(total = sum(Amount),
                                    expected_tot = sum(Expected))

## clean weeks_to_treat
md_data <- dplyr::left_join(raw_cleaned_1,payment_ROM) %>% dplyr::filter(!is.na(total)) ## exclude ones that dont have a total

md_data_1 <- md_data %>% dplyr::filter(weeks_to_treat < 0 | weeks_to_treat > 2000)

md_data_2 <- md_data %>% dplyr::filter(weeks_to_treat >= 0 & weeks_to_treat <= 2000)

md_imp <- lm(weeks_to_treat ~ body_region, md_data_2)

md_data_1$weeks_to_treat <- predict(md_imp,md_data_1)

md_data_new <- dplyr::bind_rows(md_data_2,md_data_1)

md_data_new_1<- md_data_new %>% 
  dplyr::mutate(weeks_to_treat_cat = 
           dplyr::if_else(weeks_to_treat <= 2,
                                             "<2weeks",
                                             dplyr::if_else(weeks_to_treat <= 4,
                                                            "<month",
                                                            dplyr::if_else(weeks_to_treat <= 13,
                                                                           "<3months",
                                                                           dplyr::if_else(weeks_to_treat <= 26,
                                                                                          "<6months",
                                                                                          dplyr::if_else(weeks_to_treat <= 52,
                                                                                                         "<year",
                                                                                                         ">year")))))) %>% 
  dplyr::select(-weeks_to_treat)


md_data_new_1 %<>% 
  dplyr::mutate(weeks_to_treat_cat = factor(weeks_to_treat_cat,
                                            levels = c("<2weeks","<month","<3months",
                                                       "<6months","<year",">year")))


## change Surgical levels drop the non significant 3rd levels
md_data_new_1 %<>% 
  dplyr::mutate(Surgical = dplyr::if_else(Surgical == "Surgical","Surgical","Conservative"),
                outcome_ct = dplyr::if_else(outcome_ct == 1,"One","Two or more"))


md_data_new_1 %<>% 
  dplyr::mutate(weeks_to_smsend_cat = dplyr::if_else(
      weeks_to_smsend < 10,"Less Than 10 Weeks",
          "More than 10 Weeks"
      ),
  weeks_to_smsend_cat = factor(weeks_to_smsend_cat,levels = c("Less Than 10 Weeks",
                                                              "More than 10 Weeks"))) %>% 
  dplyr::select(-weeks_to_smsend)

md_data_new_1$int_sur_payer <- paste(md_data_new_1$Surgical,md_data_new_1$PayerCategory,sep = "-")
rm(list=setdiff(ls(), "md_data_new_1"))
