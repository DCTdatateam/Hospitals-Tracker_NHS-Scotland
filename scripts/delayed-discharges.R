##libraries
library(tidyverse)
library(dplyr)


##Health boards and special health boards

HB <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv") %>%
  select(c("HB","HBName")) %>% 
  rename(HBT = HB)


SHB <- read.csv("https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv") %>% 
  select(c("SHB","SHBName")) %>% 
  rename(HBT = SHB,
         HBName = SHBName)

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HBT", "HBName")

lookups <- rbind(healthboards,ScotID)

## Delayed discharges by health board (DDHB)

DDHB <- read.csv("https://www.opendata.nhs.scot/dataset/52591cba-fd71-48b2-bac3-e71ac108dfee/resource/fd354e4b-6211-48ba-8e4f-8356a5ed4215/download/2022-04_delayed-discharge-beddays-health-board.csv")

## compare to previous 

old <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/2022-09_delayed-discharge-beddays-health-board.csv")

## column compare
columns_equal <- setequal(names(old), names(DDHB))

columns_added <- setdiff(names(DDHB), names(old))
columns_dropped <- setdiff(names(old), names(DDHB))

added_empty <- identical(columns_added, character(0))
dropped_empty <- identical(columns_dropped, character(0))


if (dropped_empty == TRUE & added_empty == FALSE) {
  message= paste("Column(s) added: ", list(columns_added))
} else if (dropped_empty == FALSE & added_empty == TRUE) {
  message= paste("Column(s) removed: ", list(columns_dropped))
} else if (dropped_empty == FALSE & added_empty == FALSE) {
  message= paste("Column(s) removed: ", list(columns_dropped), 
                 "Column(s) added: ", list(columns_added))
} else {message <- NULL}

column_compare <- 
  if(columns_equal == FALSE) {
    message(paste("Warning: Column names changed in delayed discharges by HB", message))
  }else if(columns_equal == TRUE) {
    print('Delayed discharges by HB column names match')
  }

## group compare delay reason
group_equal <- setequal(DDHB$ReasonForDelay,old$ReasonForDelay)

group_added <- setdiff(DDHB$ReasonForDelay,old$ReasonForDelay)

group_removed <- setdiff(old$ReasonForDelay, DDHB$ReasonForDelay)

group_added_empty <- identical(group_added, character(0))
group_removed_empty <- identical(group_removed, character(0))

if (group_removed_empty == TRUE & group_added_empty == FALSE) {
  message= paste("Group(s) added: ", list(group_added))
} else if (group_removed_empty == FALSE & group_added_empty == TRUE) {
  message= paste("Group(s) removed: ", list(group_removed))
} else if (group_removed_empty == FALSE & group_added_empty == FALSE) {
  message= paste("Group(s) removed: ", list(group_removed), 
                 "Group(s) added: ", list(group_added))
} else {message <- NULL}

group_compare_reason <- 
  if(group_equal == FALSE) {
    message(paste("Warning: Reason for delay group change in delayed discharges.", message))
  }else if(group_equal == TRUE) {
    print('Delayed discharge reasons unchanged')
  }

## group compare age group
group_equal <- setequal(DDHB$AgeGroup,old$AgeGroup)

group_added <- setdiff(DDHB$AgeGroup,old$AgeGroup)

group_removed <- setdiff(old$AgeGroup, DDHB$AgeGroup)

group_added_empty <- identical(group_added, character(0))
group_removed_empty <- identical(group_removed, character(0))

if (group_removed_empty == TRUE & group_added_empty == FALSE) {
  message= paste("Group(s) added: ", list(group_added))
} else if (group_removed_empty == FALSE & group_added_empty == TRUE) {
  message= paste("Group(s) removed: ", list(group_removed))
} else if (group_removed_empty == FALSE & group_added_empty == FALSE) {
  message= paste("Group(s) removed: ", list(group_removed), 
                 "Group(s) added: ", list(group_added))
} else {message <- NULL}

group_compare_age <- 
  if(group_equal == FALSE) {
    message(paste("Warning: Age group change in delayed discharges.", message))
  }else if(group_equal == TRUE) {
    print('Delayed discharge age groups unchanged')
  }

## lookup area names
DDHB <- DDHB %>% 
  left_join(lookups)

## reorder Scotland first


DDHB$HBT <- factor(DDHB$HBT, levels = c("S92000003", "S08000015", "S08000016", "S08000017",
                                        "S08000018", "S08000019", "S08000020", "S08000021",
                                        "S08000022", "S08000023", "S08000024", "S08000025",
                                        "S08000026", "S08000027", "S08000028", "S08000029",
                                        "S08000030", "S08000031", "S08000032")) 

HB_order <- c("S92000003", "S08000015", "S08000016", "S08000017",
              "S08000018", "S08000019", "S08000020", "S08000021",
              "S08000022", "S08000023", "S08000024", "S08000025",
              "S08000026", "S08000027", "S08000028", "S08000029",
              "S08000030", "S08000031", "S08000032")

DDHB <- DDHB[ order(match(DDHB$HBT, HB_order)), ]

## date formatting

DDHB <- DDHB %>%
  rename(Date1 = MonthOfDelay) %>% 
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d'),
         Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y"),
         Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))

## daily average delayed discharges by age group

age_groups_daily <- DDHB %>%
  filter(ReasonForDelay == "All Delay Reasons") %>% 
  group_by(Date1, Date2, Month, Year, HBT, HBName, AgeGroup) %>%
  summarise(Daily_average = AverageDailyNumberOfDelayedBeds) %>% 
  pivot_wider(names_from = AgeGroup, values_from = Daily_average) %>% 
  mutate(`18-74 %`=round (`18-74`/`18plus`*100, digit=1),
         `75 plus %`=round (`75plus`/`18plus`*100, digit=1),
         `18-74Popup`=`18-74`,
         `Over75sPopup`=`75plus`) %>% 
  rename('All adults' = '18plus',
         '18-74 year olds' = '18-74',
         'Over 75s' = '75plus')

## monthly delayed discharges by reason for delay

reason_monthly <- DDHB %>%
  filter(AgeGroup == "18plus") %>% 
  group_by(Date1, Date2, Month, Year, HBT, HBName, ReasonForDelay) %>%
  summarise(Monthly_total = NumberOfDelayedBedDays) %>% 
  pivot_wider(names_from = ReasonForDelay, values_from = Monthly_total) %>% 
  mutate(`% Code 9 AWI`=round (`Code 9 AWI`/`All Delay Reasons`*100, digit=1),
         `% Code 9 non-AWI`=round (`Code 9 Non-AWI`/`All Delay Reasons`*100, digit=1),
         `% Health and social care reasons`=round (`Health and Social Care Reasons`/`All Delay Reasons`*100, digit=1),
         `% Patient and family related reasons`=round (`Patient and Family Related Reasons`/`All Delay Reasons`*100, digit=1),
         AWIPopup =`Code 9 AWI`,
         NonAWIPopup =`Code 9 Non-AWI`,
         HASCPopup =`Health and Social Care Reasons`,
         PAFRRPopup =`Patient and Family Related Reasons`) 

##exports
write.csv(reason_monthly, "data/delayed-discharges/DD_by_delay_reason_all_ages_monthly.csv", row.names = FALSE)
write.csv(age_groups_daily, "data/delayed-discharges/DD_by_age_group_all_delay_reasons_daily.csv", row.names = FALSE)  
