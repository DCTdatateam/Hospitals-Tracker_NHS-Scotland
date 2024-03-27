#install packages
#install.packages(tidyverse)

##libraries
library(tidyverse)
library(dplyr)
library(Rcpp)
library(sf)


## PHS Cancelled Planned operations

## 1. Cancelled operations by health board

CBHB <- read_csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/0f1cf6b1-ebf6-4928-b490-0a721cc98884/download/cancellations_by_board_february_2022.csv")

## compare to previous data

x <- read_csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_by_board_april_2022.csv")        ## previous data 
y <- CBHB   ## new data

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
    message(paste("Warning: Column names changed in cancellations by health board.", message))
  }else if(columns_equal == TRUE) {
    print('Cancellations by health board names match')
  }


## 2. Cancelled operations Scotland

CS <- read_csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/df65826d-0017-455b-b312-828e47df325b/download/cancellations_scotland_february_2022.csv")

## compare to previous data

x <- read_csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_scotland_april_2022.csv")        ## previous data 
y <- CS   ## new data

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
    message(paste("Warning: Column names changed in cancelled ops Scotland.", message))
  }else if(columns_equal == TRUE) {
    print('Cancellations Scotland names match')
  }

## Merge Scotland and boards

cancelledops <- CS %>% 
  rename(HBT = Country) %>% 
  rbind(CBHB)

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

## join
cancelledops <- cancelledops %>% 
  left_join(lookups)

##date formatting

cancelledops <- cancelledops %>%
  rename(Date1 = Month) %>% 
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d'),
         Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y"),
         Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))

##calculate %s and reformat for flourish

cancelledops <- cancelledops %>%
  mutate(Performed=TotalOperations-TotalCancelled,
         Cancelled_By_Patient_pc_of_planned_ops=round(CancelledByPatientReason/TotalOperations*100, digit = 1),
         Cancelled_clinical_reason_pc_of_planned_ops=round(ClinicalReason/TotalOperations*100, digit = 1),
         Non_clinical_capacity_reason_pc_of_planned_ops=round(NonClinicalCapacityReason/TotalOperations*100, digit =1),
         Other_Reason_pc_of_planned_ops=round(OtherReason/TotalOperations*100, digit = 1),
         Cancelled_By_Patient_pc_of_cancelled_ops=round(CancelledByPatientReason/TotalCancelled*100, digit = 1),
         Cancelled_clinical_reason_pc_of_cancelled_ops=round(ClinicalReason/TotalCancelled*100, digit = 1),
         Non_clinical_capacity_reason_pc_of_cancelled_ops=round(NonClinicalCapacityReason/TotalCancelled*100, digit = 1),
         Other_Reason_pc_of_cancelled_ops=round(OtherReason/TotalCancelled*100, digit = 1),
         Performed_PC=round(Performed/TotalOperations*100, digit = 1),
         Cancelled_PC=round(TotalCancelled/TotalOperations*100, digit = 1),
         PerformedPopUp=Performed,
         CancelledPopUp=TotalCancelled) %>%
  select(c("Date2", "Month","Year","HBT","HBName","TotalOperations",
           "TotalCancelled","Performed","Performed_PC", "Cancelled_PC",
           "CancelledByPatientReason",
           "ClinicalReason","NonClinicalCapacityReason","OtherReason",
           "Cancelled_By_Patient_pc_of_planned_ops","Cancelled_clinical_reason_pc_of_planned_ops",
           "Non_clinical_capacity_reason_pc_of_planned_ops","Other_Reason_pc_of_planned_ops",
           "Cancelled_By_Patient_pc_of_cancelled_ops","Cancelled_clinical_reason_pc_of_cancelled_ops",
           "Non_clinical_capacity_reason_pc_of_cancelled_ops","Other_Reason_pc_of_cancelled_ops", 
           "PerformedPopUp", "CancelledPopUp")) %>% 
  rename(Cancelled = TotalCancelled,
         `Cancelled by patient` = CancelledByPatientReason,
         `Capacity reason` = NonClinicalCapacityReason,
         `Other reason` = OtherReason,
         `Clinical reason` = ClinicalReason)

## 3. Cancelled operations by hospital

CBHOS <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/bcc860a4-49f4-4232-a76b-f559cf6eb885/download/cancellations_by_hospital_march_2022.csv")

## compare to previous data

x <- read_csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_by_hospital_april_2022.csv")        ## previous data 
y <- CBHOS   ## new data

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
    message(paste("Warning: Column names changed in cancellations by hospital", message))
  }else if(columns_equal == TRUE) {
    print('Cancellations by hospital names match')
  }


## remove duplicates issue by URN
CBHOS <- CBHOS %>%
  mutate(URN=paste(Month,Hospital, sep="")) %>%
  distinct(URN, .keep_all = TRUE)

## Hospital locations (open only)

currenthospitals <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS-YDp_Ws8kJHmih3sZ--4WhVE3VX2nmO-vCyADZQZcHqvQMwNHACCGHuSPV9qmYr-4f-NbIhU3Bg6p/pub?gid=0&single=true&output=csv")


## convert to lat / long
## excludes NA coordinates

hospitalslookup <- currenthospitals %>% 
  select(c("HospitalCode","HospitalName", 
           "Postcode","AddressLine1", "HealthBoard","CouncilArea","XCoordinate",
           "YCoordinate")) %>%
  filter(!is.na(XCoordinate)| !is.na(YCoordinate)) 


hospitalslookup <- st_as_sf(hospitalslookup, coords = c("XCoordinate", "YCoordinate"), crs = 7405) ##EPSG:7405: OSGB36 / British National Grid + ODN height

hospitalslookup <- st_transform(hospitalslookup, crs = 4326 ) 
hospitalslookup <- hospitalslookup %>%
  mutate( lon = st_coordinates(hospitalslookup)[,1],
          lat = st_coordinates(hospitalslookup)[,2])

## council areas

CA <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv") %>% 
  filter(is.na(CADateArchived), 
         is.na(HSCPDateArchived),
         is.na(HBDateArchived)) %>% 
  select(c("CA","CAName"))

##joins

hospitalslookup <- hospitalslookup %>%
  cross_join(CA)

CBHOS <- CBHOS %>% 
  rename(Location = Hospital) %>% 
  cross_join(hospitalslookup) 

##date formatting

CBHOS <- CBHOS %>%
  rename(Date1 = Month) %>% 
  mutate(Date2=as.Date(paste0(as.character(Date1), '01'), format='%Y%m%d'),
         Year=format(as.Date(Date2, format="%d/%m/%Y"),"%Y"),
         Month=format(as.Date(Date2, format="%d/%m/%Y"),"%b"))

##calculate %s and reformat for flourish

CBHOS <- CBHOS %>%
  mutate(Performed=TotalOperations-TotalCancelled,
         Cancelled_By_Patient_pc_of_planned_ops=round(CancelledByPatientReason/TotalOperations*100, digit = 1),
         Cancelled_clinical_reason_pc_of_planned_ops=round(ClinicalReason/TotalOperations*100, digit = 1),
         Non_clinical_capacity_reason_pc_of_planned_ops=round(NonClinicalCapacityReason/TotalOperations*100, digit =1),
         Other_Reason_pc_of_planned_ops=round(OtherReason/TotalOperations*100, digit = 1),
         Cancelled_By_Patient_pc_of_cancelled_ops=round(CancelledByPatientReason/TotalCancelled*100, digit = 1),
         Cancelled_clinical_reason_pc_of_cancelled_ops=round(ClinicalReason/TotalCancelled*100, digit = 1),
         Non_clinical_capacity_reason_pc_of_cancelled_ops=round(NonClinicalCapacityReason/TotalCancelled*100, digit = 1),
         Other_Reason_pc_of_cancelled_ops=round(OtherReason/TotalCancelled*100, digit = 1),
         Performed_PC=round(Performed/TotalOperations*100, digit = 1),
         Cancelled_PC=round(TotalCancelled/TotalOperations*100, digit = 1),
         Group=Cancelled_PC,
         PerformedPopUp=Performed,
         CancelledPopUp=TotalCancelled) %>% 
  select(c("Date2","Month","Year","Location", "HospitalName",
           "Postcode", "AddressLine1", "lon", "lat", "CAName", "TotalOperations",
           "TotalCancelled","Performed","Performed_PC", "Cancelled_PC", "Group",
           "CancelledByPatientReason",
           "ClinicalReason","NonClinicalCapacityReason","OtherReason",
           "Cancelled_By_Patient_pc_of_planned_ops","Cancelled_clinical_reason_pc_of_planned_ops",
           "Non_clinical_capacity_reason_pc_of_planned_ops","Other_Reason_pc_of_planned_ops",
           "Cancelled_By_Patient_pc_of_cancelled_ops","Cancelled_clinical_reason_pc_of_cancelled_ops",
           "Non_clinical_capacity_reason_pc_of_cancelled_ops","Other_Reason_pc_of_cancelled_ops", 
           "PerformedPopUp", "CancelledPopUp")) %>% 
  rename(Cancelled = TotalCancelled,
         `Cancelled by patient` = CancelledByPatientReason,
         `Capacity reason` = NonClinicalCapacityReason,
         `Other reason` = OtherReason,
         `Clinical reason` = ClinicalReason)

##exclude NA locations & locations without latest month data
## reorder chronologically

latestdate <- max(CBHOS$Date2)

CBHOS <- CBHOS %>%
  mutate(filename=gsub(" ", "", HospitalName)) %>%
  filter(lat != "")

time_series_hosp <- CBHOS %>%
  group_by(HospitalName) %>%
  filter(any(Date2==latestdate)) 

time_series_hosp <- time_series_hosp[order(as.Date(time_series_hosp$Date2, format="%d/%m/%Y")),]

## filter to latest date for map base
hospitalmapbase <- CBHOS %>% 
  filter(Date2==max(Date2))

## categories & flourish marker icons for % cancelled in latest month


markerlookup <- data.frame(Group=rep(c('0-10', '10-20', '20-30', '30-40', '50+')),
                           Marker=rep(c('https://public.flourish.studio/uploads/654810/6e42adcc-51f4-4a13-9a1b-2f4cb1c18b89.png', 
                                        'https://public.flourish.studio/uploads/654810/752b0d31-bca6-45d2-843c-a2b913e0aadd.png', 
                                        'https://public.flourish.studio/uploads/654810/102c623a-98e7-4419-b8a1-2acd7f49d14f.png', 
                                        'https://public.flourish.studio/uploads/654810/d51b7238-8264-4440-ad9c-4e7dad619021.png', 
                                        'https://public.flourish.studio/uploads/654810/80fa7955-f2fd-4fb2-a6e6-75ea8901a5df.png')))


hospitalmapbase$Group<-c( "0-10", "10-20", "20-30", "30-40", "40-50", "50+")[
  findInterval(hospitalmapbase$Group , c(-Inf, 9.99, 19.99, 29.99, 39.99, 49.99, Inf) ) ]

hospitalmapbase <- hospitalmapbase %>% 
  left_join(markerlookup)

## lookup flourish chart IDs

Chart_ID <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/Cancelled-operations-hospital-chart-ids_2024.csv")

hospitalmapbase <- hospitalmapbase %>%
  left_join(Chart_ID, by = 'HospitalName')

## exports 

## 1. cancelled ops by healthboard
write.csv(cancelledops, "data/cancelled-operations/scheduled_and_cancelled_ops_by_HB_NEW.csv", row.names = FALSE)

## 2. hospital map base 
write.csv(hospitalmapbase, "data/cancelled-operations/map_base.csv_NEW", row.names = FALSE) ## all hospitals

##3 hospitals time series
#time_series_hosp %>% 
#  group_by(filename) %>% 
#  group_walk(~ write_csv(.x, paste0("data/cancelled-operations/hospitals", .y$filename, ".csv")))
