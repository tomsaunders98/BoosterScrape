library(tidyverse)
library(InewsTheme)
library(openxlsx)
library(lubridate)


#
#  Step 1 --> Scrape new data
#


dl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-"

today <- format(Sys.Date()-days(1), "%d-%B-%Y")

booster <- paste0(dl, today, ".xlsx")


dailydata <- tempfile()
dailydata <- curl::curl_download(url=booster, destfile=dailydata, quiet=FALSE, mode="wb")


### Download data


bost <- readxl::read_excel(dailydata, range = "B11:F22") %>%
  select(!2) %>%
  drop_na() %>%
  rename(
    name = 1,
    one_dose = 2,
    two_dose = 3,
    booster = 4
  ) %>%
  mutate(
    date = format(Sys.Date(), "%d/%m/%Y")
  )



bost_by_age <- readxl::read_excel(dailydata, sheet = 2, range = "B11:F29") %>%
  select(!2) %>%
  drop_na() %>%
  rename(
    age = 1,
    one_dose = 2,
    two_dose = 3,
    booster = 4
  ) %>%
  mutate(
    date = format(Sys.Date(), "%d/%m/%Y")
  )

#
# Step 2 --> format data correctly (Clean dates, remove non-numbers)
#

# Clean text that may be picked up:
bost <- bost %>%
  mutate(
    st = stringr::str_extract(one_dose, "[^0-9.]"),
    st = ifelse(is.na(st), "g", NA)
  ) %>% drop_na(st) %>% select(!st)

bost_by_age <- bost_by_age %>%
  mutate(
    st = stringr::str_extract(one_dose, "[^0-9.]"),
    st = ifelse(is.na(st), "g", NA)
  ) %>% drop_na(st) %>% select(!st)



#
# Step 3 --> Build output: Region + age CSVs, + Master excel with three sheets: England, Region, Age (all pivot wide)
#

### 1)Update CSVs

#Regions

all_reg <- read.csv("all_region.csv")
all_reg <- merge(all_reg, bost, by=c("name", "one_dose", "two_dose", "booster", "date"), all=T) 

england <- all_reg %>%
  filter(
    name == "England4"
  ) %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y")
  ) %>%
  arrange(desc(date)) %>%
  mutate(
    date = format(date, "%d/%m/%Y"),
    name = "England"
  )

write.csv(england, "all_england.csv", row.names = F)



all_reg <- all_reg %>%
  filter(
    name != "England4"
  )  %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y")
  ) 
write.csv(all_reg, "all_region.csv", row.names = F)

#Population


all_age <- read.csv("all_age.csv")

all_age <- merge(all_age, bost_by_age, by=c("age", "one_dose", "two_dose", "booster", "date"), all=T) %>%
  filter(
    age != "England5"
  )  %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y")
  ) 

write.csv(all_age, "all_age.csv", row.names = F)





### 2) Develop pretty excel from base csvs

ons_pop <- read.csv("ons_pop.csv")

ages <- merge(all_age, ons_pop, by="age") %>%
  mutate(
    age = as.factor(age)
  )

ages$age <- factor(ages$age, levels = c("Under 18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",  "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))

ages <- ages %>%
  arrange(desc(date), age) %>%
  mutate(
    date = format(date, "%d/%m/%Y")
  )


regions <- all_reg %>%
  select(!c(one_dose, two_dose)) %>%
  pivot_wider(names_from = "name", values_from="booster") %>%
  arrange(desc(date)) %>%
  mutate(
    date = format(date, "%d/%m/%Y")
  )

  
# out <- createWorkbook()
# 
# # Add some sheets to the workbook
# addWorksheet(out, "England")
# addWorksheet(out, "Regions")
# addWorksheet(out, "Ages")
# 
# # Write the data to the sheets
# writeData(out, sheet = "England", x = england)
# writeData(out, sheet = "Regions", x = regions)
# writeData(out, sheet = "Ages", x = ages)
# 
# #fname <- paste0("Booster_", format(Sys.Date(), "%d-%m-%Y"), ".xlsx")
# 
# saveWorkbook(out, file = "Booster.xlsx", overwrite = TRUE)

gs4_deauth()

gs4_auth(
  path = 'auth.json'
) 

ss = "102tDE1n0QfjUJEFvFRozCbNksbhtp8CpDyR6DuDZ85U"



googlesheets4::sheet_write( 
  data  = england,
  ss    = ss,
  sheet = "England"
)

googlesheets4::sheet_write( 
  data  = regions,
  ss    = ss,
  sheet = "Regions"
)

googlesheets4::sheet_write( 
  data  = ages,
  ss    = ss,
  sheet = "Ages"
)

