library(tidyverse)

## 1. Locations lookups - all scripts 

location_lookups <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed in locations, all scripts affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Location column names match')
    }
  }
}


## healthboards 

HB_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv")

HB_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/hb14_hb19.csv")


location_lookups(HB_PHS,HB_GH)


## special health boards 

SHB_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv")

SHB_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/special-health-boards_19022021.csv")

location_lookups(SHB_PHS, SHB_GH)


## council areas 


CA_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv")

CA_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/ca11_ca19.csv")

location_lookups(CA_PHS, CA_GH)



## 2. Delayed discharges 

delayed_discharges <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed, delayed discharges affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Delayed discharge column names match')
    }
  }
}


DDHB_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/52591cba-fd71-48b2-bac3-e71ac108dfee/resource/fd354e4b-6211-48ba-8e4f-8356a5ed4215/download/2022-04_delayed-discharge-beddays-health-board.csv")


DDHB_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/2022-09_delayed-discharge-beddays-health-board.csv")

delayed_discharges(DDHB_PHS, DDHB_GH)

## check delay reason categories for changes in DDBH

delayed_discharge_reasons <- unique(DDHB_GH$ReasonForDelay)
delayed_discharge_reasons_new <- unique(DDHB_PHS$ReasonForDelay)

delay_reason_compare <- setequal(delayed_discharge_reasons, delayed_discharge_reasons_new)

{
  
  if(delay_reason_compare == TRUE) {print("delay reasons unchanged")}
  if(delay_reason_compare == FALSE) {stop("Warning: delay reasons changes in delayed discharges")}
  
}

## check age group categories for changes in DDBH

age_groups <- unique(DDHB_GH$AgeGroup)
age_groups_new <- unique(DDHB_PHS$AgeGroup)

age_groups_compare <- setequal(age_groups, age_groups_new)

{
  
  
  if(age_groups_compare == TRUE) {print("age groups unchanged")}
  if(age_groups_compare == FALSE) {stop("Warning: age groups category changes in delayed discharges")}
  
}



## 3. Beds information

beds_info <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed, beds info affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Beds info column names match')
    }
  }
}


BIHB_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/554b0e7a-ccac-4bb6-82db-1a8b306fcb36/resource/f272bb7d-5320-4491-84c1-614a2c064007/download/beds_by_nhs_board_of_treatment_and_specialty.csv")

BIHB_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/beds_by_nhs_board_of_treatment_and_specialty_Q3_2022.csv")

beds_info(BIHB_PHS, BIHB_GH)

## check beds info specialty groupings match

specialty_groups <- unique(BIHB_GH$SpecialtyName)
specialty_groups_new <- unique(BIHB_PHS$SpecialtyName)

specialty_groups_compare <- setequal(specialty_groups, specialty_groups_new)

{
  
  
  if(specialty_groups_compare == TRUE) {print("specialty groups unchanged")}
  if(specialty_groups_compare == FALSE) {stop("Warning: specialty groups changed in beds info")}
  
}

## 4. Hospital locations 

hospitals_lookup <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed for hospital locations, cancelled ops and A&E waits affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Hospital location column names match')
    }
  }
}

## Hospital locations


currenthospitals_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current-hospital_flagged20211216.csv")

currenthospitals_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/current-hospital_flagged20211216.csv")

hospitals_lookup(currenthospitals_PHS, currenthospitals_GH)


## 5. A&E waiting times 

aewt <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed, A&E waiting times affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('A&E waiting times column names match')
    }
  }
}



AEWT_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/997acaa5-afe0-49d9-b333-dcf84584603d/resource/2a4adc0a-e8e3-4605-9ade-61e13a85b3b9/download/monthly_ae_waitingtimes_202204.csv")


AEWT_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/monthly_ae_waitingtimes_202204.csv")

aewt(AEWT_PHS, AEWT_GH)

## 6. Cancelled Operations

cancelled_ops <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed, cancelled ops affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Cancelled ops column names match')
    }
  }
}




## Cancelled operations by health board


CBHB_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/0f1cf6b1-ebf6-4928-b490-0a721cc98884/download/cancellations_by_board_february_2022.csv")


CBHB_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_by_board_april_2022.csv")

cancelled_ops(CBHB_PHS,CBHB_GH)


## Cancelled operations Scotland


CS_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/df65826d-0017-455b-b312-828e47df325b/download/cancellations_scotland_february_2022.csv")


CS_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_scotland_april_2022.csv")

cancelled_ops(CS_PHS, CS_GH)

## Cancelled operations by hospital


CBHOS_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/bcc860a4-49f4-4232-a76b-f559cf6eb885/download/cancellations_by_hospital_march_2022.csv")


CBHOS_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_by_hospital_april_2022.csv")

cancelled_ops(CBHOS_PHS, CBHOS_GH)

## 7. Diagnostic waits

diagnostic_waits <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      stop('Warning: Column names changed, diagnostic waits affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Diagnostic waiting times column names match')
    }
  }
}

##diagnostic waits by health board

DWTHB_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/10dfe6f3-32de-4039-84c2-7e7794a06b31/download/diagnostics_by_board_june_2022.csv")

DWTHB_GH <- read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/diagnostics_by_board_june_2022_new.csv")


diagnostic_waits(DWTHB_PHS, DWTHB_GH)

## check test groups 

test_groups_HB <- unique(DWTHB_GH$DiagnosticTestDescription)
test_groups_new_HB <- unique(DWTHB_PHS$DiagnosticTestDescription)

test_groups_compare_HB <- setequal(test_groups_HB, test_groups_new_HB)

{


  if(test_groups_compare_HB == TRUE) {print("Diagnostic test groups unchanged")}
  if(test_groups_compare_HB == FALSE) {stop("Warning: Test description groups changed in diagnostic waits")}

}

## check waiting time groups

waiting_groups_HB <- unique(DWTHB_GH$WaitingTime)
waiting_groups_new_HB <- unique(DWTHB_PHS$WaitingTime)

waiting_groups_compare_HB <- setequal(waiting_groups_HB, waiting_groups_new_HB)

{


  if(waiting_groups_compare_HB == TRUE) {print("Diagnostic waiting intervals unchanged")}
  if(waiting_groups_compare_HB == FALSE) {stop("Warning: Waiting time groups changed in diagnostic waits")}

}

## diagnostic waits Scotland

DWTS_PHS <- read.csv("https://www.opendata.nhs.scot/dataset/3d1f49b2-f770-492f-82c9-ebefdc56ece4/resource/df75544f-4ba1-488d-97c7-30ab6258270d/download/diagnostics_scotland_june_2022.csv")
DWTS_GH <-  read.csv("https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/diagnostics_scotland_june_2022_new.csv")

diagnostic_waits(DWTS_PHS, DWTS_GH)  

## check test groups 

test_groups_S <- unique(DWTS_GH$DiagnosticTestDescription)
test_groups_new_S <- unique(DWTS_PHS$DiagnosticTestDescription)

test_groups_compare_S <- setequal(test_groups_S, test_groups_new_S)

{


  if(test_groups_compare_S == TRUE) {print("Diagnostic test groups unchanged")}
  if(test_groups_compare_S == FALSE) {stop("Warning: Test description groups changed in diagnostic waits")}

}

## check waiting time groups

waiting_groups_S <- unique(DWTS_GH$WaitingTime)
waiting_groups_new_S <- unique(DWTS_PHS$WaitingTime)

waiting_groups_compare_S <- setequal(waiting_groups_S, waiting_groups_new_S)

{


  if(waiting_groups_compare_S == TRUE) {print("Diagnostic waiting intervals unchanged")}
  if(waiting_groups_compare_S == FALSE) {stop("Warning: Waiting time groups changed in diagnostic waits")}

}
  
  
  
  
