##libraries
library(tidyverse)
library(dplyr)


##Health boards and special health boards

HB <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv") %>%
  select(c("HB","HBName")) %>% 
  rename(HBT = HB)

## "https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv"
SHB <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/monthly_ae_waitingtimes_202204.csv") %>% 
  select(c("SHB","SHBName")) %>% 
  rename(HBT = SHB,
         HBName = SHBName)

healthboards <- rbind(HB, SHB)

## A&E waiting times data

AEWT <- read.csv("https://www.opendata.nhs.scot/dataset/997acaa5-afe0-49d9-b333-dcf84584603d/resource/2a4adc0a-e8e3-4605-9ade-61e13a85b3b9/download/monthly_ae_waitingtimes_202204.csv")

## compare to previous data

x <- AEWT        ## previous data 
y <- read_csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/test.csv")    ## new data

##x <- read_csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/monthly_ae_waitingtimes_202204.csv")        ## previous data 
##y <- AEWT    ## new data

columns_equal <- setequal(names(x), names(y))

columns_added <- setdiff(names(y), names(x))
columns_dropped <- setdiff(names(x), names(y))

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
    message(paste("Warning: Column names changed, A&E waiting times affected.", message))
  }else if(columns_equal == TRUE) {
    print('A&E waiting times column names match')
  }

## join to locations

AEWT <- AEWT %>%
  left_join(healthboards)

## Date formatting

AEWT <- AEWT %>%
  rename(Date1 = Month) %>% 
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d'),
         Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y"),
         Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))

## SECTION 1 - STANDARD LEVEL DATA
## Time series by HB and Scotland aggregate

Scotlandtimeseries <- AEWT %>% 
  group_by(Date2, Month, Year, Country) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate,
         PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1),
         PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1)) %>% 
  rename(HBT = Country) %>% 
  mutate(HBName="Scotland", .after = HBT)


Healthboardtimeseries <- AEWT %>%
  group_by(Date2, Month, Year, HBT, HBName) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate,
         PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1),
         PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1))

## bind spreadsheets for combined Scotland and HB breakdown
##duplicate columns for pop ups issue 
##change col names for Flourish legend

AEWt_4hrstandard_timeseries <- Scotlandtimeseries %>%
  rbind(Healthboardtimeseries) %>% 
  mutate(`Seen within target`=NumberMeetingTargetAggregate,
         `Waiting over 4 hrs`=NumberNotOnTarget)


## SECTION 2 - EPISODE LEVEL DATA
## filter episode level data for 8, 12hr waits and discharge destinations

episode_level_data <- AEWT %>%
  filter(NumberOfAttendancesEpisodeQF == "")


## Time series by HB and Scotland episode 

episode_Scotlandtimeseries <- episode_level_data %>% 
  group_by(Date2, Month, Year, Country) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate),
            AttendanceGreater8hrs = sum(AttendanceGreater8hrs),
            AttendanceGreater12hrs = sum(AttendanceGreater12hrs),
            DischargeDestinationAdmissionToSame = sum(DischargeDestinationAdmissionToSame),
            DischargeDestinationOtherSpecialty = sum(DischargeDestinationOtherSpecialty),
            DischargeDestinationResidence = sum(DischargeDestinationResidence),
            DischargeDestinationTransfer = sum(DischargeDestinationTransfer),
            DischargeDestinationUnknown = sum(DischargeDestinationUnknown)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate,
         PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1),
         PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1),
         `% Waiting over 8hrs`=round (AttendanceGreater8hrs/NumberOfAttendancesAggregate*100, digit=1),
         `% Waiting over 12hrs`=round (AttendanceGreater12hrs/NumberOfAttendancesAggregate*100, digit=1),
         PercentageAdmissionToSame=round (DischargeDestinationAdmissionToSame/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationOtherSpecialty=round (DischargeDestinationOtherSpecialty/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationResidence=round (DischargeDestinationResidence/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationTransfer=round (DischargeDestinationTransfer/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationUnknown=round (DischargeDestinationUnknown/NumberOfAttendancesAggregate*100, digit=1)) %>% 
  rename(HBT = Country) %>% 
  mutate(HBName="Scotland", .after = HBT)


episode_Healthboardtimeseries <- episode_level_data %>%
  group_by(Date2, Month, Year, HBT, HBName) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate),
            AttendanceGreater8hrs = sum(AttendanceGreater8hrs),
            AttendanceGreater12hrs = sum(AttendanceGreater12hrs),
            DischargeDestinationAdmissionToSame = sum(DischargeDestinationAdmissionToSame),
            DischargeDestinationOtherSpecialty = sum(DischargeDestinationOtherSpecialty),
            DischargeDestinationResidence = sum(DischargeDestinationResidence),
            DischargeDestinationTransfer = sum(DischargeDestinationTransfer),
            DischargeDestinationUnknown = sum(DischargeDestinationUnknown)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate,
         PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1),
         PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1),
         `% Waiting over 8hrs`=round (AttendanceGreater8hrs/NumberOfAttendancesAggregate*100, digit=1),
         `% Waiting over 12hrs`=round (AttendanceGreater12hrs/NumberOfAttendancesAggregate*100, digit=1),
         PercentageAdmissionToSame=round (DischargeDestinationAdmissionToSame/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationOtherSpecialty=round (DischargeDestinationOtherSpecialty/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationResidence=round (DischargeDestinationResidence/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationTransfer=round (DischargeDestinationTransfer/NumberOfAttendancesAggregate*100, digit=1),
         PercentageDestinationUnknown=round (DischargeDestinationUnknown/NumberOfAttendancesAggregate*100, digit=1))


AEWt_episode_timeseries <- episode_Scotlandtimeseries %>%
  rbind(episode_Healthboardtimeseries)                              ## No Orkney data until 2011

##change col names for Flourish legend

names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationAdmissionToSame'] <- 'Hospital - ICU'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationOtherSpecialty'] <- 'Hospital - other speciality'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationResidence'] <- 'Home'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationTransfer'] <- 'Hospital - transfer'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationUnknown'] <- 'Unknown'



## exports

write.csv(AEWt_4hrstandard_timeseries, "data/A&E-waiting-times/AEWt_4hrstandard_timeseries.csv", row.names = FALSE)
write.csv(AEWt_episode_timeseries, "data/A&E-waiting-times/AEWt_episode_timeseries.csv", row.names = FALSE)
