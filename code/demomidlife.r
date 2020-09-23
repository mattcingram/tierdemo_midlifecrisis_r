# Change plot size to 4 x 4
options(repr.plot.width=4, repr.plot.height=4)

#library(pacman)
#p_load(ggplot2, data.table, xtable, stargazer, repr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rio)
library(texreg)
library(pacman)
library(xtable)
library(stargazer)
library(data.table)
library(repr)
library(tidyverse)

#getwd()
#path <- '/home1/s/m/mi122167/OpenStats/replication1lacina'
#setwd(path)

# dir.create checks whether subdirectories exist; if missing, creates them
dir.create("../data",showWarnings = TRUE)
dir.create("../data/original",showWarnings = TRUE)
dir.create("../data/working",showWarnings = TRUE)
dir.create("../figures",showWarnings = TRUE)
dir.create("../output",showWarnings = TRUE)
dir.create("../tables",showWarnings = TRUE)

# IMPORT WDI DATA
# Indicator: NE.CON.GOVT.ZS (General government final consumption expenditure (% of GDP))
# Countries (ISO-2 codes): China (CN), India (IN), Indonesia (ID), Jordan (JO), Pakistan (PK), Russia (RU), United States (US)
# Year: 2002

exp_dat <- read.csv("../data/original/API_NE.CON.GOVT.ZS_DS2_en_csv_v2_9913909.csv", skip=4)
str(exp_dat)

# Keep relevant variables
exp_dat <- select(exp_dat, -`Country.Code`, -`Indicator.Name`, -`Indicator.Code`, -`X`)

# Reshape from wide to long using tidyr
exp_dat_long <- gather(exp_dat, Year, Value, -`Country.Name`)
exp_dat_long$exp <- as.numeric(exp_dat_long$Value)

# Rename variables
exp_dat_long <- rename(exp_dat_long, CountryName = `Country.Name`)

# Filter for relevant observations
exp_dat_long <- filter(exp_dat_long, 
  CountryName %in% c("China", "India", "Indonesia", 
                  "Jordan", "Pakistan", "Russian Federation", "United States") &
   Year == 2002)

# Keep relevant variables
exp_dat_2002 <- select(exp_dat_long,
                       CountryName, exp)

# IMPORT WDI DATA
# Indicator: NY.GDP.PCAP.CD (GDP per capita
# Countries (ISO-2 codes): China (CN), India (IN), Indonesia (ID), Jordan (JO), Pakistan (PK), Russia (RU), United States (US)
# Year: 2002

inc_dat <- read.csv("../data/original/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_9908784.csv", skip=4)
str(inc_dat)

# Keep relevant variables
inc_dat <- select(inc_dat, -`Country.Code`, -`Indicator.Name`, -`Indicator.Code`, -`X`)

# Reshape from wide to long using tidyr
inc_dat_long <- gather(inc_dat, Year, Value, -`Country.Name`)
inc_dat_long$inc <- as.numeric(inc_dat_long$Value)

# Rename variables
inc_dat_long <- rename(inc_dat_long, CountryName = `Country.Name`)

# Filter for relevant observations
inc_dat_long <- filter(inc_dat_long, 
                       CountryName %in% c("China", "India", "Indonesia", 
                                          "Jordan", "Pakistan", "Russian Federation", "United States") &
                         Year == 2002)

# Keep relevant variables
inc_dat_2002 <- select(inc_dat_long,
                       CountryName, inc)

# Combine both WDI variables
wdi_dat_2002 <- bind_cols(inc_dat_2002, exp_dat_2002)
original_wdi <- select(wdi_dat_2002, CountryName, inc, exp)

# Alternative: retrieve WDI data via API using the WDI package

# library("WDI")
# 
# original_wdi <- WDI(country = c("CN", "IN", "ID", "JO", "PK", "RU", "US"),
#                     indicator = c("NY.GDP.PCAP.CD", "NE.CON.GOVT.ZS"),
#                     start = 2002,
#                     end = 2002)
# 
# str(original_wdi)

# original_wdi <- rename(original_wdi, inc = NY.GDP.PCAP.CD, 
                        # exp = NE.CON.GOVT.ZS,
                        # CountryName = country)

# SO THAT WE CAN MERGE THIS WDI DATA WITH THE PEW DATA,
# GENERATE A VARIABLE country THAT CODES THE COUNTRIES
# IN THE SAME WAY AS THE PEW DATA.

original_wdi$ccode_pew <- -99
original_wdi[original_wdi$CountryName == "China", ]$ccode_pew <- 8 
original_wdi[original_wdi$CountryName == "India", ]$ccode_pew <- 17 
original_wdi[original_wdi$CountryName == "Indonesia", ]$ccode_pew <- 18 
original_wdi[original_wdi$CountryName == "Jordan", ]$ccode_pew <- 45 
original_wdi[original_wdi$CountryName == "Pakistan", ]$ccode_pew <- 27 
original_wdi[original_wdi$CountryName == "Russian Federation", ]$ccode_pew <- 31
original_wdi[original_wdi$CountryName == "United States", ]$ccode_pew <- 40

table(original_wdi$ccode_pew)

# Comment out the next line if we want to save importable data

# export(original_wdi, file = "../Importable-Data/wdi.csv")

# OPEN THE ORIGINAL PEW DATA
original_pew <- rio::import("../data/original/original-pew.sav")

# KEEP ONLY THE VARIABLES THAT WILL BE USED FOR 
# THIS STUDY
pew <- original_pew[, c("country", "q2", "q74")]
pew <- select(original_pew, country, q2, q74) # tidyverse equivalent

# GIVE THE VARIABLES NICER NAMES AND LABELS
pew <- rename(pew, satis = q2, age = q74)

# FOR VARIABLES satis AND age
# RECODE "DON'T KNOW" and "REFUSED" AS NA

# To see how missing values and refusals are coded for satis
table(pew$satis)

# Consult ../Original-Data/metadata/supplements/2-Pew-GAP-Survey-details 1.1.pdf
# page 6:
# Range 0-10
# 11 Dont know (DO NOT READ)
# 12 Refused (DO NOT READ)

pew$satis <- ifelse(pew$satis == 11, NA, pew$satis)
pew$satis <- ifelse(pew$satis == 12, NA, pew$satis)

table(pew$satis)

# To see how missing values and refusals are coded for age
table(pew$age)

# Consult ../Original-Data/metadata/supplements/1-Pew-GAP-Survey-details 1.1.pdf
# page 6:
# Range 18-96
# 97 97 or older
# 98 Dont know (DO NOT READ)
# 99 Refused (DO NOT READ)

pew$age <- ifelse(pew$age == 98, NA, pew$age)
pew$age <- ifelse(pew$age == 99, NA, pew$age)

table(pew$age)

# DROP OBSERVATIONS FOR ALL INDIVIDUALS FOR WHOM 
# THE VALUE OF THE VARIABLE satis IS MISSING OR REFUSED
pew <- pew[!is.na(pew$satis), ]
pew <- filter(pew, !is.na(satis)) # tidyverse equivalent

# DROP OBSERVATIONS FOR ALL INDIVIDUALS WHO ARE 
# LESS THAN 21 OR MORE THAN 70 YEARS OF AGE
# OR FOR WHOM THE VALUE OF THE VARIABLE age IS MISSING

# To drop observations where the value of age is either
# greater than 70, or missing or refused
pew <- pew[pew$age <= 70, ]
pew <- filter(pew, age <= 70) # tidyverse equivalent

# To drop observations where the value of age is less than 21
pew <- pew[pew$age >= 21, ]
pew <- filter(pew, age >= 21) # tidyverse equivalent


# KEEP DATA ONLY FOR COUNTRIES WITH AT LEAST 900 OBSERVATIONS
# REMAINING IN THE SAMPLE AFTER REMOVAL OF INDIVIDUALS WITH 
# MISSING OR REFUSED VALUES OF satis 0R age, AS WELL AS REMOVAL
# OF INDIVIDUALS UNDER 21 OR OVER 70 YEARS OF AGE

# First, generate a variable called country_n that, for each
# individual, equals the total number of observations that remain 
# in the sample representing individuals from her/his own country

pew <- mutate(group_by(pew, country),
              country_n = n())

# Then drop all individuals for whom country_n is less than 900
pew <- filter(pew, country_n >= 900)

table(pew$country)

# The variable country_n is no longer needed, so drop it
pew <- select(pew, -country_n)

# Rename variables
pew <- rename(pew, ccode_pew = country)

# IN SOME OF THE ANALYSIS I WILL WANT TO USE BOTH AGE AND
# THE SQUARE OF AGE.  SO GENERATE A NEW VARIABLE age2 EQUAL
# TO THE SQUARE OF AGE.
pew$age2 <- pew$age^2

# Comment out the next line if we want to save importable data

# export(pew, file = "../Importable-Data/pew.csv")

# MERGE both datasets
di <- merge(x = pew, y = original_wdi, by = "ccode_pew")


# SAVE THE INDIVIDUAL-LEVEL DATA WITH THE NAME individual-analysis.ext
# Save this file in the "Analysis-Data" folder.
export(di, file = "../data/working/individual-analysis.csv")

# CREATE A COUNTRY-LEVEL DATA SET 
# WITH COUNTRIES IDENTIFIED BY countryname AND country 
# INCLUDING THE VARIABLES satis exp AND inc
# IN WHICH

	# THE VALUE OF cm_satis IS EQUAL TO THE MEAN VALUE
	# FOR ALL INDIVIDUALS IN THE COUNTRY
	
	# THE VALUES OF exp AND inc ARE THE VALUES
	# OF THESE VARIABLES FOR THE COUNTRY IN THE YEAR 2002
	
dc <- summarize(group_by(di, ccode_pew),
                         CountryName = first(CountryName),
                         cm_satis = mean(satis),
                         exp = mean(exp),
                         inc = mean(inc))
 


# SAVE THE COUNTRY-LEVEL DATA WITH THE NAME country-analysis.ext
# Save this file in the "Analysis-Data" folder.
export(dc, file = "../data/working/country-analysis.csv")

## ----dataprep, eval = FALSE, echo = FALSE, results = "hide"--------------
## # Note: This script creates the data used in this paper.
## # eval is set to FALSE because the script does not need to be run each
## # time the paper is compiled.
## source("../Command-Files/1-processing.R")

## ----loaddata, echo = FALSE----------------------------------------------
dc <- import("../data/working/country-analysis.csv")
di <- import("../data/working/individual-analysis.csv")

## ----table1, echo = FALSE------------------------------------------------
table1 <- select(dc, -ccode_pew)
table1 <- arrange(table1, CountryName)
names(table1) <- c("Country", "Mean Satis.", "GDP per capita", "Gov. Cons.")
kable(table1, 
      digits = 2,
      caption = "Mean Satisfaction, GPD per capita and Government Consumption by Country")

## ----regressions, echo = FALSE-------------------------------------------
r1 <- lm(satis ~ age + age2, data = di)
r2 <- lm(satis ~ age + age2 + factor(CountryName), data = di)

## ----table2-pdf, eval = FALSE, results = 'asis', echo = FALSE------------
## texreg(list(r1, r2),
##        custom.coef.names = c("Constant", "Age", "Age squared"),
##        omit.coef = "factor",
##        digits = 3,
##        caption = "Regression results",
##        caption.above = TRUE,
##        bold = 0.05, stars = 0,
##        custom.note = "Standard errors in parentheses. Coefficients with $p < 0.05$ in bold font.")

## ----table2-html, eval = TRUE, results = 'asis', echo = FALSE------------
htmlreg(list(r1, r2),
       custom.coef.names = c("Constant", "Age", "Age squared"),
       omit.coef = "factor",
       digits = 3,
       caption = "Regression results",
       caption.above = TRUE,
       custom.model.names = c("Pooled", "Country fixed effects"),
       bold = 0.05, stars = 0,
       custom.note = "Standard errors in parentheses. Coefficients with p < 0.05 in bold font.")

## ----figure1, echo = FALSE, fig.cap = "Scatterplot of GDP per capita and mean satisfaction", fig.align = "center"----
ggplot(data = dc, aes(x = inc, y = cm_satis)) + 
  geom_point() + 
  geom_text(aes(label = CountryName), vjust = "inward", hjust = "inward") + 
  xlab("GDP per capita (current [2002] $ US)") + 
  ylab("Country mean satisfaction")

## ----figure2, echo = FALSE, fig.cap = "Scatterplot of General Government Consumption and mean satisfaction", fig.align = "center"----
ggplot(data = dc, aes(x = exp, y = cm_satis)) + 
  geom_point() + 
  geom_text(aes(label = CountryName), vjust = "inward", hjust = "inward") + 
  xlab("Government consumption, % of GDP") + 
  ylab("Country mean satisfaction")

## ----extractrcode, eval = FALSE, echo = FALSE----------------------------
## # Note: this command extracts the R code from this .Rmd file and
## # creates a pure R script, then places it in the
## # Command-Files folder. Highlight and run the next two lines if you wish to
## # generate the R script.
## purl(input = "2-Midlife-Crisis-paper.Rmd", output = "2-analysis.R")
## file.rename("2-analysis.R", "../Command-Files/2-analysis.R")
