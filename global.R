library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(readr)
library(ggplot2)


world <- map_data("world")

data <- read_csv("CompleteData.csv")

data_long <-gather(data, key = "Year", value = "Value", 5:10 )
data_long
gdp_literacy <- read_csv("GDPandLiteracyRates.csv")

world <- map_data("world",regions=c("Brazil","Peru","Colombia","Bolivia","Chile","Venezuela","Argentina","Uruguay","Paraguay","Ecuador","Guyana","Suriname","French Guiana"))


data_long$Year <- as.numeric(as.character(data_long$Year))
data_long$Value <- as.numeric(as.character(data_long$Value))
gdp_literacy$Year <- as.numeric(as.character(gdp_literacy$Year))
gdp_literacy$Literacy <-as.numeric(as.character(gdp_literacy$Literacy))
gdp_literacy$GDPPerCapita <-as.numeric(as.character(gdp_literacy$GDPPerCapita))


# primary net enrollment data
both_genders_PrimNetEnrollment <- filter(data_long, data_long$Code=="SE.PRM.TENR")
male_PrimNetEnrollment <- filter(data_long, data_long$Code=="SE.PRM.TENR.MA")
female_PrimNetEnrollment <- filter(data_long, data_long$Code=="SE.PRM.TENR.FE")

# primary completion rate data
both_genders_PrimCompletionRate <- filter(data_long, data_long$Code=="SE.PRM.CMPT.ZS")
male_PrimCompletionRate <- filter(data_long, data_long$Code=="SE.PRM.CMPT.FE.ZS")
female_PrimCompletionRate <- filter(data_long, data_long$Code=="SE.PRM.CMPT.MA.ZS")

# secondary net enrollment data
both_genders_SecNetEnrollment <- filter(data_long, data_long$Code=="UIS.NER.2")
male_SecNetEnrollment <- filter(data_long, data_long$Code=="UIS.NER.2.M")
female_SecNetEnrollment <- filter(data_long, data_long$Code=="UIS.NER.2.F")

# secondary completion rate data
both_genders_SecCompletionRate <-  filter(data_long, data_long$Code=="SE.SEC.CMPT.LO.ZS")
male_SecCompletionRate <- filter(data_long, data_long$Code=="SE.SEC.CMPT.LO.MA.ZS")
female_SecCompletionRate <- filter(data_long, data_long$Code=="SE.SEC.CMPT.LO.FE.ZS")

