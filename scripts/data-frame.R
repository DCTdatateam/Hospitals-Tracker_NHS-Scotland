##libraries
library(curl)
library(tidyverse)


## 1. Locations lookups - all scripts 

location_lookups <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same in locations, all scripts affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Location names match')
    }
  }
}


## healthboards
##PHS
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/hb14_hb19.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

HB_PHS <- read.csv(temp)

temp1 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/hb14_hb19.csv"
temp1 <- curl_download(url=source, destfile=temp1, quiet=FALSE, mode="wb")

HB_GH <- read.csv(temp1)


location_lookups(HB_PHS,HB_GH)


## special health boards 

temp2 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/65402d20-f0f1-4cee-a4f9-a960ca560444/resource/0450a5a2-f600-4569-a9ae-5d6317141899/download/special-health-boards_19022021.csv"
temp2 <- curl_download(url=source, destfile=temp2, quiet=FALSE, mode="wb")

SHB_PHS <- read.csv(temp2)

temp3 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/special-health-boards_19022021.csv"
temp3 <- curl_download(url=source, destfile=temp3, quiet=FALSE, mode="wb")

SHB_GH <- read.csv(temp3)

location_lookups(SHB_PHS, SHB_GH)


## council areas 

temp4 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv"
temp4 <- curl_download(url=source, destfile=temp4, quiet=FALSE, mode="wb")

CA_PHS <- read.csv(temp4)

temp5 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/ca11_ca19.csv"
temp5 <- curl_download(url=source, destfile=temp5, quiet=FALSE, mode="wb")

CA_GH <- read.csv(temp5)

location_lookups(CA_PHS, CA_GH)



## 2. Delayed discharges 

delayed_discharges <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same, delayed discharges affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Delayed discharche names match')
    }
  }
}

temp6 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/52591cba-fd71-48b2-bac3-e71ac108dfee/resource/fd354e4b-6211-48ba-8e4f-8356a5ed4215/download/2022-04_delayed-discharge-beddays-health-board.csv"
temp6 <- curl_download(url=source, destfile=temp6, quiet=FALSE, mode="wb")

DDHB_PHS <- read.csv(temp6)

temp7 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/2022-04_delayed-discharge-beddays-health-board.csv"
temp7 <- curl_download(url=source, destfile=temp7, quiet=FALSE, mode="wb")

DDHB_GH <- read.csv(temp7)

delayed_discharges(DDHB_PHS, DDHB_GH)

## 3. Beds information

beds_info <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same, beds info affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Beds info names match')
    }
  }
}


temp8 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/554b0e7a-ccac-4bb6-82db-1a8b306fcb36/resource/f272bb7d-5320-4491-84c1-614a2c064007/download/beds_by_nhs_board_of_treatment_and_specialty.csv"
temp8 <- curl_download(url=source, destfile=temp8, quiet=FALSE, mode="wb")

BIHB_PHS <- read.csv(temp8)

temp9 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/beds_by_nhs_board_of_treatment_and_specialty.csv"
temp9 <- curl_download(url=source, destfile=temp9, quiet=FALSE, mode="wb")

BIHB_GH <- read.csv(temp9)

beds_info(BIHB_PHS, BIHB_GH)

## 4. Hospital locations 

hospitals_lookup <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same for hospital locations, cancelled ops and A&E waits affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Hospital location names match')
    }
  }
}

## Hospital locations (open only)

temp10 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/cbd1802e-0e04-4282-88eb-d7bdcfb120f0/resource/c698f450-eeed-41a0-88f7-c1e40a568acc/download/current-hospital_flagged20211216.csv"
temp10 <- curl_download(url=source, destfile=temp10, quiet=FALSE, mode="wb")

currenthospitals_PHS <- read.csv(temp10)

temp11 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/current-hospital_flagged20211216.csv"
temp11 <- curl_download(url=source, destfile=temp11, quiet=FALSE, mode="wb")

currenthospitals_GH <- read.csv(temp11)

hospitals_lookup(currenthospitals_PHS, currenthospitals_GH)

## A&E sites

temp12 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/a877470a-06a9-492f-b9e8-992f758894d0/resource/1a4e3f48-3d9b-4769-80e9-3ef6d27852fe/download/hospital_site_list.csv"
temp12 <- curl_download(url=source, destfile=temp12, quiet=FALSE, mode="wb")

aesites_PHS <- read.csv(temp12)

temp13 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/hospital_site_list.csv"
temp13 <- curl_download(url=source, destfile=temp13, quiet=FALSE, mode="wb")

aesites_GH <- read.csv(temp13)

hospitals_lookup(aesites_PHS, aesites_GH)

## 5. A&E waiting times 

aewt <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same, A&E waiting times affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('A&E waiting times names match')
    }
  }
}


temp14 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/997acaa5-afe0-49d9-b333-dcf84584603d/resource/2a4adc0a-e8e3-4605-9ade-61e13a85b3b9/download/monthly_ae_waitingtimes_202204.csv"
temp14 <- curl_download(url=source, destfile=temp14, quiet=FALSE, mode="wb")

AEWT_PHS <- read.csv(temp14)

temp15 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/monthly_ae_waitingtimes_202204.csv"
temp15 <- curl_download(url=source, destfile=temp1, quiet=FALSE, mode="wb")

AEWT_GH <- read.csv(temp15)

aewt(AEWT_PHS, AEWT_GH)

## 6. Cancelled Operations

cancelled_ops <- function(x,y) {
  for (i in names(x)) {
    if (!(i %in% names(y))) {
      print('Warning: Names are not the same, cancelled ops scripts affected')
      break
    }  
    else if(i==tail(names(y),n=1)) {
      print('Cancelled Ops names match')
    }
  }
}


## Cancelled operations by health board

temp16 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/0f1cf6b1-ebf6-4928-b490-0a721cc98884/download/cancellations_by_board_february_2022.csv"
temp16 <- curl_download(url=source, destfile=temp16, quiet=FALSE, mode="wb")

CBHB_PHS <- read.csv(temp16)

temp17 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_by_board_april_2022.csv"
temp17 <- curl_download(url=source, destfile=temp17, quiet=FALSE, mode="wb")

CBHB_GH <- read.csv(temp17)

cancelled_ops(CBHB_PHS, CBHB_GH)

## Cancelled operations Scotland

temp18 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/df65826d-0017-455b-b312-828e47df325b/download/cancellations_scotland_february_2022.csv"
temp18 <- curl_download(url=source, destfile=temp18, quiet=FALSE, mode="wb")

CS_PHS <- read.csv(temp18)

temp19 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_scotland_april_2022.csv"
temp19 <- curl_download(url=source, destfile=temp19, quiet=FALSE, mode="wb")

CS_GH <- read.csv(temp19)

cancelled_ops(CS_PHS, CS_GH)

## Cancelled operations by hospital

temp20 <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/479848ef-41f8-44c5-bfb5-666e0df8f574/resource/bcc860a4-49f4-4232-a76b-f559cf6eb885/download/cancellations_by_hospital_march_2022.csv"
temp20 <- curl_download(url=source, destfile=temp20, quiet=FALSE, mode="wb")

CBHOS_PHS <- read.csv(temp20)

temp21 <- tempfile()
source <- "https://github.com/DCTdatateam/Hospitals-Tracker_NHS-Scotland/raw/main/data/source-data/cancellations_by_hospital_april_2022.csv"
temp21 <- curl_download(url=source, destfile=temp21, quiet=FALSE, mode="wb")

CBHOS_GH <- read.csv(temp21)

cancelled_ops(CBHOS_PHS, CBHOS_GH)

