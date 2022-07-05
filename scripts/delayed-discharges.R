##libraries
library(curl)
library(tidyverse)

##Health boards and special health boards

temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

HB <- read.csv(temp)

HB <- HB[c("HB","HBName")]

temp1 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv"
temp1 <- curl_download(url=source, destfile=temp1, quiet=FALSE, mode="wb")

SHB <- read.csv(temp1)

SHB <- SHB[c("SHB","SHBName")]

names(HB)[names(HB) == 'HB'] <- 'HBT'
names(SHB)[names(SHB) == 'SHB'] <- 'HBT'
names(SHB)[names(SHB) == 'SHBName'] <- 'HBName'

healthboards <- rbind(HB, SHB)

## Scotland

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("HBT", "HBName")

lookups <- rbind(healthboards,ScotID)

## Delayed discharges by health board (DDHB)


temp2 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/52591cba-fd71-48b2-bac3-e71ac108dfee/resource/fd354e4b-6211-48ba-8e4f-8356a5ed4215/download/2022-04_delayed-discharge-beddays-health-board.csv"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

DDHB <- read.csv(temp2)

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


## split monthly and daily figures

MonthlyDD <- DDHB[c("Date1", "Date2", "Month","Year","HBT", "HBName", 
                    "AgeGroup","ReasonForDelay","NumberOfDelayedBedDays")]

DailyDD <- DDHB[c("Date1", "Date2", "Month","Year","HBT", "HBName", 
                  "AgeGroup", "ReasonForDelay","AverageDailyNumberOfDelayedBeds")]

## 1. Pivot age categories
## Monthly age category totals
## URN for age category joining

agegroupsmonthly <- MonthlyDD %>%
  mutate(agegroupID=paste(Date1,HBT,ReasonForDelay, sep=""))

## filter and rejoin to transpose age categories

AllAdultsmonthly <- agegroupsmonthly %>%
  filter(AgeGroup == "18plus") 

names(AllAdultsmonthly)[names(AllAdultsmonthly) == 'NumberOfDelayedBedDays'] <- 'alladultsMonthTotal'

Under75monthly <- agegroupsmonthly %>%
  filter(AgeGroup == "18-74")

names(Under75monthly)[names(Under75monthly) == 'NumberOfDelayedBedDays'] <- 'under75MonthTotal'

Over75monthly <- agegroupsmonthly %>%
  filter(AgeGroup == "75plus")

names(Over75monthly)[names(Over75monthly) == 'NumberOfDelayedBedDays'] <- 'over75MonthTotal'

finalagegroupsmonthly <- AllAdultsmonthly %>% 
  left_join(Under75monthly, by = 'agegroupID') %>% 
  left_join(Over75monthly, by = 'agegroupID')


finalagegroupsmonthly <- finalagegroupsmonthly[c("agegroupID", "Date1","Date2", 
                                                 "Month","Year","HBT", "HBName",
                                                 "ReasonForDelay","alladultsMonthTotal",
                                                 "under75MonthTotal","over75MonthTotal")]

## Daily average age category totals
## URN for age category joining
agegroupsdaily <- DailyDD %>%
  mutate(agegroupID=paste(Date1,HBT,ReasonForDelay, sep=""))

## filter and rejoin to transpose age categories

AllAdultsdaily <- agegroupsdaily %>%
  filter(AgeGroup == "18plus") 

names(AllAdultsdaily)[names(AllAdultsdaily) == 'AverageDailyNumberOfDelayedBeds'] <- 'alladultsDailyAvg'

Under75daily <- agegroupsdaily %>%
  filter(AgeGroup == "18-74")

names(Under75daily)[names(Under75daily) == 'AverageDailyNumberOfDelayedBeds'] <- 'under75DailyAvg'

Over75daily <- agegroupsdaily %>%
  filter(AgeGroup == "75plus")

names(Over75daily)[names(Over75daily) == 'AverageDailyNumberOfDelayedBeds'] <- 'over75DailyAvg'

finalagegroupsdaily <- AllAdultsdaily %>% 
  inner_join(Under75daily, by = 'agegroupID') %>% 
  inner_join(Over75daily, by = 'agegroupID')

finalagegroupsdaily <- finalagegroupsdaily[c("agegroupID", "Date1","Date2", "Month",
                                             "Year","HBT", "HBName",
                                             "ReasonForDelay","alladultsDailyAvg",
                                             "under75DailyAvg","over75DailyAvg")]

## join daily and monthly totals
DD_by_age_group <- finalagegroupsmonthly %>% 
  left_join(finalagegroupsdaily, by = 'agegroupID') 

DD_by_age_group <- DD_by_age_group[c("Date2.x","Month.x","Year.x","HBT.x", 
                               "HBName.x", "ReasonForDelay.x","alladultsMonthTotal",
                               "under75MonthTotal","over75MonthTotal",
                               "alladultsDailyAvg","under75DailyAvg","over75DailyAvg")] 

## daily %s 
DD_by_age_group <- DD_by_age_group %>%
  mutate(PercentageUnder75=round (under75DailyAvg/alladultsDailyAvg*100, digit=1)) %>%
  mutate(PercentageOver75=round (over75DailyAvg/alladultsDailyAvg*100, digit=1)) %>%
  mutate(`18-74Popup`=under75DailyAvg) %>%
  mutate(`Over75sPopup`=over75DailyAvg)

names(DD_by_age_group)[names(DD_by_age_group) == 'Date2.x'] <- 'Date'
names(DD_by_age_group)[names(DD_by_age_group) == 'Month.x'] <- 'Month'
names(DD_by_age_group)[names(DD_by_age_group) == 'Year.x'] <- 'Year'
names(DD_by_age_group)[names(DD_by_age_group) == 'HBT.x'] <- 'HB'
names(DD_by_age_group)[names(DD_by_age_group) == 'HBName.x'] <- 'Health Board'
names(DD_by_age_group)[names(DD_by_age_group) == 'ReasonForDelay.x'] <- 'Reason for delay'
names(DD_by_age_group)[names(DD_by_age_group) == 'alladultsMonthTotal'] <- 'All adults total'
names(DD_by_age_group)[names(DD_by_age_group) == 'under75MonthTotal'] <- '18-74 year olds total'
names(DD_by_age_group)[names(DD_by_age_group) == 'over75MonthTotal'] <- '75 plus total'
names(DD_by_age_group)[names(DD_by_age_group) == 'alladultsDailyAvg'] <- 'All adults'
names(DD_by_age_group)[names(DD_by_age_group) == 'under75DailyAvg'] <- '18-74 year olds'
names(DD_by_age_group)[names(DD_by_age_group) == 'over75DailyAvg'] <- 'Over 75s'
names(DD_by_age_group)[names(DD_by_age_group) == 'PercentageUnder75'] <- '18-74 %'
names(DD_by_age_group)[names(DD_by_age_group) == 'PercentageOver75'] <- '75 plus %'

DD_by_age_group_all_delay_reasons_daily <- DD_by_age_group %>%
  filter(`Reason for delay`== "All Delay Reasons")

## 2. Pivot delay reason categories
## Monthly delay category totals
## URN for delay category joining

delayreasonmonthly <- MonthlyDD %>%
  mutate(delayreasonID=paste(Date1,HBT,AgeGroup, sep=""))

## filter and rejoin to transpose age categories

AllReasonsmonthly <- delayreasonmonthly %>%
  filter(ReasonForDelay == "All Delay Reasons") 

names(AllReasonsmonthly)[names(AllReasonsmonthly) == 'NumberOfDelayedBedDays'] <- 'alldelaysMonthTotal'

Code9AWImonthly <- delayreasonmonthly %>%
  filter(ReasonForDelay == "Code 9 AWI")

names(Code9AWImonthly)[names(Code9AWImonthly) == 'NumberOfDelayedBedDays'] <- 'code9MonthAWITotal'

Code9_NON_AWImonthly <- delayreasonmonthly %>%
  filter(ReasonForDelay == "Code 9 Non-AWI")

names(Code9_NON_AWImonthly)[names(Code9_NON_AWImonthly) == 'NumberOfDelayedBedDays'] <- 'code9MonthNon-AWITotal'

HASCmonthly <- delayreasonmonthly %>%
  filter(ReasonForDelay == "Health and Social Care Reasons")

names(HASCmonthly)[names(HASCmonthly) == 'NumberOfDelayedBedDays'] <- 'HASCMonthTotal'

PAFRRmonthly <- delayreasonmonthly %>%
  filter(ReasonForDelay == "Patient and Family Related Reasons")

names(PAFRRmonthly)[names(PAFRRmonthly) == 'NumberOfDelayedBedDays'] <- 'PAFRRMonthTotal'

finaldelayreasonmonthly <- AllReasonsmonthly %>% 
  left_join(Code9AWImonthly, by = 'delayreasonID') %>% 
  left_join(Code9_NON_AWImonthly, by = 'delayreasonID') %>% 
  left_join(HASCmonthly, by = 'delayreasonID') %>% 
  left_join(PAFRRmonthly, by = 'delayreasonID')


finaldelayreasonmonthly <- finaldelayreasonmonthly[c("delayreasonID", "Date1.x","Date2.x", 
                                                     "Month.x","Year.x","HBT.x", "HBName.x",
                                                     "AgeGroup.x","alldelaysMonthTotal",
                                                     "code9MonthAWITotal", "code9MonthNon-AWITotal",
                                                     "HASCMonthTotal","PAFRRMonthTotal")]

## Daily average age category totals
## URN for age category joining
delayreasonsdaily <- DailyDD %>%
  mutate(delayreasonID=paste(Date1,HBT,AgeGroup, sep=""))

## filter and rejoin to transpose age categories

AllReasonsdaily <- delayreasonsdaily %>%
  filter(ReasonForDelay == "All Delay Reasons") 

names(AllReasonsdaily)[names(AllReasonsdaily) == 'AverageDailyNumberOfDelayedBeds'] <- 'AllReasonsdailyAvg'


Code9AWIdaily <- delayreasonsdaily %>%
  filter(ReasonForDelay == "Code 9 AWI")

names(Code9AWIdaily)[names(Code9AWIdaily) == 'AverageDailyNumberOfDelayedBeds'] <- 'code9AWIdaily'

Code9_NON_AWIdaily <- delayreasonsdaily %>%
  filter(ReasonForDelay == "Code 9 Non-AWI")

names(Code9_NON_AWIdaily)[names(Code9_NON_AWIdaily) == 'AverageDailyNumberOfDelayedBeds'] <- 'code9Non-AWIDaily'

HASCdaily <- delayreasonsdaily %>%
  filter(ReasonForDelay == "Health and Social Care Reasons")

names(HASCdaily)[names(HASCdaily) == 'AverageDailyNumberOfDelayedBeds'] <- 'HASCDailyAvg'

PAFRRdaily <- delayreasonsdaily %>%
  filter(ReasonForDelay == "Patient and Family Related Reasons")

names(PAFRRdaily)[names(PAFRRdaily) == 'AverageDailyNumberOfDelayedBeds'] <- 'PAFRRDailyAvg'

finaldelayreasonsdaily <- AllReasonsdaily %>% 
  left_join(Code9AWIdaily, by = 'delayreasonID') %>% 
  left_join(Code9_NON_AWIdaily, by = 'delayreasonID') %>% 
  left_join(HASCdaily, by = 'delayreasonID') %>% 
  left_join(PAFRRdaily, by = 'delayreasonID')


finaldelayreasonsdaily <- finaldelayreasonsdaily[c("delayreasonID", "Date1.x", "Date2.x",
                                                   "Month.x", "Year.x", "HBT.x",
                                                   "HBName.x", "AgeGroup.x",
                                                   "AllReasonsdailyAvg", "code9AWIdaily",
                                                   "code9Non-AWIDaily", "HASCDailyAvg",
                                                   "PAFRRDailyAvg")]



## join daily and monthly totals
DD_by_delay_reason <- finaldelayreasonmonthly %>% 
  left_join(finaldelayreasonsdaily, by = 'delayreasonID') 

DD_by_delay_reason <- DD_by_delay_reason[c("Date2.x.x","Month.x.x","Year.x.x","HBT.x.x", 
                                     "HBName.x.x", "AgeGroup.x.x","alldelaysMonthTotal",
                                     "code9MonthAWITotal", "code9MonthNon-AWITotal",
                                     "HASCMonthTotal",
                                     "PAFRRMonthTotal", "AllReasonsdailyAvg",
                                     "code9AWIdaily", "code9Non-AWIDaily","HASCDailyAvg",
                                     "PAFRRDailyAvg")] 
##monthly %s & dupe columns for popups
DD_by_delay_reason <- DD_by_delay_reason %>%
  mutate(PercentageCode9AWI=round (code9MonthAWITotal/alldelaysMonthTotal*100, digit=1)) %>%
  mutate(PercentageCode9NONAWI=round (`code9MonthNon-AWITotal`/alldelaysMonthTotal*100, digit=1)) %>%
  mutate(PercentageHASC=round (HASCMonthTotal/alldelaysMonthTotal*100, digit=1)) %>%
  mutate(PercentagePAFRR=round (PAFRRMonthTotal/alldelaysMonthTotal*100, digit=1)) %>%
  mutate(AWIPopup=code9MonthAWITotal)%>%
  mutate(NonAWIPopup=`code9MonthNon-AWITotal`) %>%
  mutate(HASCPopup=HASCMonthTotal)%>%
  mutate(PAFRRPopup=PAFRRMonthTotal)

names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'Date2.x.x'] <- 'Date'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'Month.x.x'] <- 'Month'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'Year.x.x'] <- 'Year'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'HBT.x.x'] <- 'HB'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'HBName.x.x'] <- 'Health Board'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'AgeGroup.x.x'] <- 'Age group'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'alldelaysMonthTotal'] <- 'Number of delayed bed days'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'code9MonthAWITotal'] <- 'Code 9 AWI reasons'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'code9MonthNon-AWITotal'] <- 'Code 9 Non-AWI reasons'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'HASCMonthTotal'] <- 'Health and social care reasons'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'PAFRRMonthTotal'] <- 'Patient and family related reasons'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'AllReasonsdailyAvg'] <- 'Daily average delayed bed days'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'code9AWIdaily'] <- 'Code 9 AWI daily average'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'code9Non-AWIDaily'] <- 'Code 9 non-AWI daily average'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'HASCDailyAvg'] <- 'Health and social care daily average'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'PAFRRDailyAvg'] <- 'Patient and family related daily average'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'PercentageCode9AWI'] <- '% Code 9 AWI'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'PercentageCode9NONAWI'] <- '% Code 9 non-AWI'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'PercentageHASC'] <- '% Health and social care reason delays'
names(DD_by_delay_reason)[names(DD_by_delay_reason) == 'PercentagePAFRR'] <- '% Patient and family related reason delays'

DD_by_delay_reason_all_ages <- DD_by_delay_reason %>%
  filter(`Age group`== "18plus")


##exports
write.csv(DD_by_delay_reason_all_ages, "data/delayed-discharges/DD_by_delay_reason_all_ages_monthly.csv", row.names = FALSE)
write.csv(DD_by_age_group_all_delay_reasons_daily, "data/delayed-discharges/DD_by_age_group_all_delay_reasons_daily.csv", row.names = FALSE)
