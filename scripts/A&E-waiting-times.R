##libraries
library(curl)
library(tidyverse)
library(dplyr)

## Hospital locations (open only)

temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current-hospital_flagged20211216.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

currenthospitals <- read.csv(temp)

## A&E sites

temp1 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/a877470a-06a9-492f-b9e8-992f758894d0/resource/1a4e3f48-3d9b-4769-80e9-3ef6d27852fe/download/hospital_site_list.csv"
temp1 <- curl_download(url=source, destfile=temp1, quiet=FALSE, mode="wb")

aesites <- read.csv(temp1)

## join

names(currenthospitals)[names(currenthospitals) == 'Location'] <- 'TreatmentLocationCode'

hospitalslookup <- currenthospitals %>%
  left_join(aesites)

##Health boards and special health boards

temp2 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

HB <- read.csv(temp2)

HB <- HB[c("HB","HBName")]

temp3 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv"
temp3 <- curl_download(url=source, destfile=temp3, quiet=FALSE, mode="wb")

SHB <- read.csv(temp3)

SHB <- SHB[c("SHB","SHBName")]

names(HB)[names(HB) == 'HB'] <- 'HBT'
names(SHB)[names(SHB) == 'SHB'] <- 'HBT'
names(SHB)[names(SHB) == 'SHBName'] <- 'HBName'

healthboards <- rbind(HB, SHB)


## Council areas
temp4 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv"
temp4 <- curl_download(url=source, destfile=temp4, quiet=FALSE, mode="wb")

CA <- read.csv(temp4)

CA <- CA %>%
  filter(is.na(CADateArchived)) %>%
  filter(is.na(HSCPDateArchived))%>%
  filter(is.na(HBDateArchived))

CA <- CA[c("CA","CAName")]

hospitalslookup <- hospitalslookup %>%
  left_join(CA)

hospitalslookup <- hospitalslookup[c(1:4,8,14:15,18,20:22)]

## A&E waiting times data

temp5 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/997acaa5-afe0-49d9-b333-dcf84584603d/resource/2a4adc0a-e8e3-4605-9ade-61e13a85b3b9/download/monthly_ae_waitingtimes_202204.csv"
temp5 <- curl_download(url=source, destfile=temp5, quiet=FALSE, mode="wb")

AEWT <- read.csv(temp5)

names(AEWT)[names(AEWT) == 'TreatmentLocation'] <- 'TreatmentLocationCode'

AEWT <- AEWT %>%
  left_join(hospitalslookup)

AEWT <- AEWT %>%
  left_join(healthboards)

## Date formatting
names(AEWT)[names(AEWT) == 'Month'] <- 'Date1'
AEWT <- AEWT %>%
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d')) %>%
  mutate(Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y")) %>%
  mutate(Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))


AEWT <- AEWT[c("Date2", "Month", "Year", "TreatmentLocationCode", "LocationName", 
               "HBT", "HBName", "Country", "DepartmentType", "NumberOfAttendancesAggregate",
               "NumberMeetingTargetAggregate", "AttendanceGreater8hrs", "AttendanceGreater12hrs",
               "DischargeDestinationAdmissionToSame", "DischargeDestinationOtherSpecialty",
               "DischargeDestinationResidence", "DischargeDestinationTransfer", 
               "DischargeDestinationUnknown", "AddressLine", "Postcode",
               "CA", "CAName", "XCoordinate", "YCoordinate", "Status", "Comments", 
               "NumberOfAttendancesEpisodeQF")]


## Summaries for latest month

latestmonth <- AEWT %>%
  filter(Date2==max(Date2)) 


Scotlandlatestsummary <- latestmonth %>% 
  group_by(Date2, Month, Year, Country) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1))


Healthboardlatestsummary <- latestmonth %>%
  group_by(Date2, Month, Year, HBT, HBName) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1))

## Time series by HB and Scotland aggregate

Scotlandtimeseries <- AEWT %>% 
  group_by(Date2, Month, Year, Country) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1))

Healthboardtimeseries <- AEWT %>%
  group_by(Date2, Month, Year, HBT, HBName) %>%                            
  summarise(NumberOfAttendancesAggregate = sum(NumberOfAttendancesAggregate), 
            NumberMeetingTargetAggregate = sum(NumberMeetingTargetAggregate)) %>%
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1))

ScotID <- data.frame("S92000003", "Scotland")  
names(ScotID) <- c("Country", "HBName")

Scotlandlatestsummary <- Scotlandlatestsummary %>%
  left_join(ScotID)
  

Scotlandtimeseries <- Scotlandtimeseries %>%
  left_join(ScotID)

Scotlandtimeseries <- Scotlandtimeseries[c("Date2","Month", "Year", "Country", "HBName",
                      "NumberOfAttendancesAggregate", "NumberMeetingTargetAggregate",
                      "NumberNotOnTarget", "PercentageOnTarget", "PercentageNotOnTarget")]

names(Scotlandtimeseries)[names(Scotlandtimeseries) == 'Country'] <- 'HBT'

AEWt_4hrstandard_timeseries <- Scotlandtimeseries %>%
  rbind(Healthboardtimeseries)

##duplicate columns for pop ups issue 
##change col names for Flourish legend

AEWt_4hrstandard_timeseries <- AEWt_4hrstandard_timeseries %>%
  mutate(Numberontargetpopup=NumberMeetingTargetAggregate) %>%
  mutate(Numbernotontargetpopup=NumberNotOnTarget)

names(AEWt_4hrstandard_timeseries)[names(AEWt_4hrstandard_timeseries) == 'NumberMeetingTargetAggregate'] <- 'Seen within target'
names(AEWt_4hrstandard_timeseries)[names(AEWt_4hrstandard_timeseries) == 'NumberNotOnTarget'] <- 'Waiting over 4 hrs'

## filter episode level data for 8, 12hr waits and discharge destinations

episode_level_data <- AEWT %>%
  filter(NumberOfAttendancesEpisodeQF == "")
write.csv(episode_level_data, "episode_level_data.csv", row.names = FALSE)

latestmonth_episode_level_data <- AEWT %>%
  filter(NumberOfAttendancesEpisodeQF == "") %>%
  filter(Date2==max(Date2)) 

Scotlandlatest_episodelevel_summary <- latestmonth_episode_level_data %>% 
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
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver8hrs=round (AttendanceGreater8hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver12hrs=round (AttendanceGreater12hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageAdmissionToSame=round (DischargeDestinationAdmissionToSame/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationOtherSpecialty=round (DischargeDestinationOtherSpecialty/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationResidence=round (DischargeDestinationResidence/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationTransfer=round (DischargeDestinationTransfer/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationUnknown=round (DischargeDestinationUnknown/NumberOfAttendancesAggregate*100, digit=1))

Healthboardlatest_episodelevel_summary <- episode_level_data %>% 
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
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver8hrs=round (AttendanceGreater8hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver12hrs=round (AttendanceGreater12hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageAdmissionToSame=round (DischargeDestinationAdmissionToSame/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationOtherSpecialty=round (DischargeDestinationOtherSpecialty/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationResidence=round (DischargeDestinationResidence/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationTransfer=round (DischargeDestinationTransfer/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationUnknown=round (DischargeDestinationUnknown/NumberOfAttendancesAggregate*100, digit=1))

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
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver8hrs=round (AttendanceGreater8hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver12hrs=round (AttendanceGreater12hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageAdmissionToSame=round (DischargeDestinationAdmissionToSame/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationOtherSpecialty=round (DischargeDestinationOtherSpecialty/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationResidence=round (DischargeDestinationResidence/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationTransfer=round (DischargeDestinationTransfer/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationUnknown=round (DischargeDestinationUnknown/NumberOfAttendancesAggregate*100, digit=1))

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
  mutate(NumberNotOnTarget=NumberOfAttendancesAggregate-NumberMeetingTargetAggregate) %>%
  mutate(PercentageOnTarget=round (NumberMeetingTargetAggregate/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageNotOnTarget=round (NumberNotOnTarget/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver8hrs=round (AttendanceGreater8hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageOver12hrs=round (AttendanceGreater12hrs/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageAdmissionToSame=round (DischargeDestinationAdmissionToSame/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationOtherSpecialty=round (DischargeDestinationOtherSpecialty/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationResidence=round (DischargeDestinationResidence/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationTransfer=round (DischargeDestinationTransfer/NumberOfAttendancesAggregate*100, digit=1)) %>%
  mutate(PercentageDestinationUnknown=round (DischargeDestinationUnknown/NumberOfAttendancesAggregate*100, digit=1))

episode_Scotlandtimeseries <- episode_Scotlandtimeseries %>%
  left_join(ScotID)

episode_Scotlandtimeseries <- episode_Scotlandtimeseries[c("Date2","Month", "Year", "Country", "HBName",
                                           "NumberOfAttendancesAggregate", "NumberMeetingTargetAggregate",
                                           "AttendanceGreater8hrs", "AttendanceGreater12hrs",
                                           "DischargeDestinationAdmissionToSame", "DischargeDestinationOtherSpecialty",
                                           "DischargeDestinationResidence", "DischargeDestinationTransfer",
                                           "DischargeDestinationUnknown",
                                           "NumberNotOnTarget", "PercentageOnTarget", "PercentageNotOnTarget",
                                           "PercentageOver8hrs", "PercentageOver12hrs", "PercentageAdmissionToSame",
                                           "PercentageDestinationOtherSpecialty", "PercentageDestinationResidence",
                                           "PercentageDestinationTransfer", "PercentageDestinationUnknown")]

names(episode_Scotlandtimeseries)[names(episode_Scotlandtimeseries) == 'Country'] <- 'HBT'

AEWt_episode_timeseries <- episode_Scotlandtimeseries %>%
  rbind(episode_Healthboardtimeseries)                              ## No Orkney data until 2011

##change col names for Flourish legend

names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'PercentageOver8hrs'] <- '% Waiting over 8hrs'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'PercentageOver12hrs'] <- '% Waiting over 12hrs'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationAdmissionToSame'] <- 'Hospital - ICU'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationOtherSpecialty'] <- 'Hospital - other speciality'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationResidence'] <- 'Home'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationTransfer'] <- 'Hospital - transfer'
names(AEWt_episode_timeseries)[names(AEWt_episode_timeseries) == 'DischargeDestinationUnknown'] <- 'Unknown'



## exports

write.csv(AEWt_4hrstandard_timeseries, "data/A&E-waiting-times/AEWt_4hrstandard_timeseries.csv", row.names = FALSE)
write.csv(AEWt_episode_timeseries, "data/A&E-waiting-times/AEWt_episode_timeseries.csv", row.names = FALSE)
