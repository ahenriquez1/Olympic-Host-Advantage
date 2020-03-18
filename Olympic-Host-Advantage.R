# Olympic Host Advantage
# Larry Yang, Angel Henriquez

rm(list = ls())
library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(readstata13)
library(sandwich)
library(stargazer)
library(ISwR)
library(chron)
library(htmltab)
library(readxl)

options(scipen = 9)
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

# reading in excel file
britainmedalcount <- read_excel("britainmedalcount.xlsx")

# calculating britain medal percentage
britainmedalcount$medal_percentage <- britainmedalcount$medal_count / britainmedalcount$total
britainmedalcount$medal_percentage <- britainmedalcount$medal_percentage * 100
britainmedalcount$medal_percentage <- as.numeric(britainmedalcount$medal_percentage)
britainmedalcount$expected_percentage <- as.numeric(britainmedalcount$expected_percentage)

reg1 <- lm(expected_percentage ~ medal_percentage, data = britainmedalcount)
reg2 <- lm(expected_percentage ~ medal_percentage + expected_change, data = britainmedalcount)
stargazer(reg1,reg2, type = "text", covariate.labels = c("percentage medal count","expected percentage change"))
