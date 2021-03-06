library(DBI)
library(RPostgreSQL)
library(magrittr)
library(lubridate)
library(tidyr)
library(dplyr)
library(utils)
library(zoo)
library(pool)
library(data.table)


# Shiny libraries
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)


library(DT)
library(bcp)
library(changepoint)


source( '../shared_script/helpfunctions.r')
source('../shared_script/uihelpers.R') 
source('../shared_script/serverhelpers.R')
source('../shared_script/jstats.R')


data_date   <- "20160630" 

hcopen<- dbPool(drv=RPostgreSQL::PostgreSQL(),
                host = "shiny.hc.local",
                dbname = "hcopen",
                user = "hcreader",
                password = "canada1")


cv_drug_rxn_meddra <- hcopen %>% tbl("cv_drug_rxn_meddra")%>%select(REPORT_ID,DRUGNAME,ing,PT_NAME_ENG,month)%>%as.data.table()
cv_indication<-hcopen%>%tbl("cv_report_drug_indication_joined_20160630")%>%
                        select(REPORT_ID,INDICATION_NAME_ENG)%>%
                        filter(!is.null(INDICATION_NAME_ENG))    

#count_monthly_hlt<-count(cv_drug_rxn_2006, ing, HLT_NAME_ENG, month) %>% as.data.frame()

#dropdown list for PT term
pt_choices<- cv_drug_rxn_meddra%>%filter(!is.na(PT_NAME_ENG))%>%distinct(PT_NAME_ENG)%>%`$`("PT_NAME_ENG")%>%sort()

