##libraries
library(tidyverse)

##Health boards and special health boards

HB <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")

HB <- HB[c("HB","HBName")]

names(HB)[names(HB) == 'HB'] <- 'HBT'

SHB <- read.csv("https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv")

SHB <- SHB[c("SHB","SHBName")]

names(SHB)[names(SHB) == 'SHB'] <- 'HBT'
names(SHB)[names(SHB) == 'SHBName'] <- 'HBName'

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HBT", "HBName")

lookups <- rbind(healthboards,ScotID)

## Delayed discharges by health board (DDHB)


DDHB <- read.csv("https://www.opendata.nhs.scot/dataset/52591cba-fd71-48b2-bac3-e71ac108dfee/resource/fd354e4b-6211-48ba-8e4f-8356a5ed4215/download/2022-04_delayed-discharge-beddays-health-board.csv")

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

names(DDHB)[names(DDHB) == 'MonthOfDelay'] <- 'Date1'
DDHB <- DDHB %>%
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d')) %>%
  mutate(Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y")) %>%
  mutate(Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))

## urn: mutate(URN=paste(Date1,HBT, sep="")) %>% 

## daily average delayed discharges by age group

age_groups_daily <- DDHB %>%
  filter(ReasonForDelay == "All Delay Reasons") %>% 
  group_by(Date1, Date2, Month, Year, HBT, HBName, AgeGroup) %>%
  summarise(Daily_average = AverageDailyNumberOfDelayedBeds) %>% 
  pivot_wider(names_from = AgeGroup, values_from = Daily_average) %>% 
  mutate(`18-74 %`=round (`18-74`/`18plus`*100, digit=1)) %>%
  mutate(`75 plus %`=round (`75plus`/`18plus`*100, digit=1)) %>%
  mutate(`18-74Popup`=`18-74`) %>%
  mutate(`Over75sPopup`=`75plus`)

names(age_groups_daily)[names(age_groups_daily) == '18plus'] <- 'All adults'
names(age_groups_daily)[names(age_groups_daily) == '18-74'] <- '18-74 year olds'
names(age_groups_daily)[names(age_groups_daily) == '75plus'] <- 'Over 75s'

## monthly delayed discharges by reason for delay

reason_monthly <- DDHB %>%
  filter(AgeGroup == "18plus") %>% 
  group_by(Date1, Date2, Month, Year, HBT, HBName, ReasonForDelay) %>%
  summarise(Monthly_total = NumberOfDelayedBedDays) %>% 
  pivot_wider(names_from = ReasonForDelay, values_from = Monthly_total) %>% 
  mutate(`% Code 9 AWI`=round (`Code 9 AWI`/`All Delay Reasons`*100, digit=1)) %>%
  mutate(`% Code 9 non-AWI`=round (`Code 9 Non-AWI`/`All Delay Reasons`*100, digit=1)) %>%
  mutate(`% Health and social care reasons`=round (`Health and Social Care Reasons`/`All Delay Reasons`*100, digit=1)) %>%
  mutate(`% Patient and family related reasons`=round (`Patient and Family Related Reasons`/`All Delay Reasons`*100, digit=1)) %>% 
  mutate(AWIPopup =`Code 9 AWI`) %>%
  mutate(NonAWIPopup =`Code 9 Non-AWI`) %>%
  mutate(HASCPopup =`Health and Social Care Reasons`) %>%
  mutate(PAFRRPopup =`Patient and Family Related Reasons`) 

##exports
write.csv(reason_monthly, "data/delayed-discharges/DD_by_delay_reason_all_ages_monthly.csv", row.names = FALSE)
write.csv(age_groups_daily, "data/delayed-discharges/DD_by_age_group_all_delay_reasons_daily.csv", row.names = FALSE)
