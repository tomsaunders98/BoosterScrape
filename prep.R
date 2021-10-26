library(tidyverse)
library(InewsTheme)
library(sf)

library(googlesheets4)

pop <- read.csv("ukpopestimates.csv") %>%
  filter(Name == "ENGLAND") %>%
  select(X0:X90.) %>%
  pivot_longer(everything(), names_to="ages", values_to="pop") %>%
  mutate(
    ages = as.numeric(gsub("X", "", ages)),
    pop = as.numeric(gsub(",", "", pop))
  )

pop$range <- as.character(cut(pop$ages, c(0,17,24,29,34,39,44,49,54,59,64,69,74,79, 90), include.lowest = T))

pop <- pop %>%
  drop_na(range)

convcat <- function(x) {
  ages <- str_match_all(x, "\\d+")
  f_year <- as.numeric(ages[[1]][[1]])
  l_year <- as.numeric(ages[[1]][[2]])
  f_year = f_year + 1
  group <- paste0(as.character(f_year), "-", as.character(l_year))
  return(group)
}
pop$range <- sapply(pop$range, convcat)

pop <- pop %>%
  mutate(
    range = ifelse(range == "80-90", "80+", range),
    range = ifelse(range == "1-17", "Under 18", range)
  ) %>%
  group_by(range) %>%
  summarise(
    pop = sum(pop)
  ) %>%
  drop_na() %>%
  rename(
    age = range
  )

write.csv(pop, "ons_pop.csv", row.names = F)


###### Generate base files

library(tidyverse)
library(InewsTheme)
library(lubridate)


all_reg <- data.frame(
  name = character(),
  one_dose = integer(),
  two_dose = integer(),
  booster = integer(),
  date = character()
)

all_age <- data.frame(
  age = character(),
  one_dose = integer(),
  two_dose = integer(),
  booster = integer(),
  date = character()
)


for (x in 1:28){
  
  dl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/10/COVID-19-daily-announced-vaccinations-"
  
  today <- format(Sys.Date() - days(x), "%d-%B-%Y")
  
  booster <- paste0(dl, today, ".xlsx")
  
  
  dailydata <- tempfile()
  dailydata <- curl::curl_download(url=booster, destfile=dailydata, quiet=FALSE, mode="wb")
  
  
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
      date = Sys.Date() - days(x)
    )
  

  
  all_reg <- merge(all_reg, bost, by=c("name", "one_dose", "two_dose", "booster", "date"), all=T)

  
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
      date = Sys.Date() - days(x)
    )
  

  
  all_age <- merge(all_age, bost_by_age, by=c("age", "one_dose", "two_dose", "booster", "date"), all=T)

  
  
}

all_reg <- all_reg  %>%
  mutate(
    st = stringr::str_extract(one_dose, "[^0-9.]"),
    st = ifelse(is.na(st), "g", NA)
  ) %>% drop_na(st) %>% select(!st) %>%
  mutate(
    date = as.numeric(date),
    date = as.Date(date, origin="1970-01-01"),
    date = ifelse(is.na(date), Sys.Date() - days(1), date),
    date = as.Date(date, origin="1970-01-01"),
    date = format(date, "%d/%m/%Y")
  )

write.csv(all_reg, "all_region.csv", row.names = F)

all_age <- all_age  %>%
  mutate(
    st = stringr::str_extract(one_dose, "[^0-9.]"),
    st = ifelse(is.na(st), "g", NA)
  ) %>% drop_na(st) %>% select(!st) %>%
  mutate(
    date = as.numeric(date),
    date = as.Date(date, origin="1970-01-01"),
    date = ifelse(is.na(date), Sys.Date() - days(1), date),
    date = as.Date(date, origin="1970-01-01"),
    date = format(date, "%d/%m/%Y")
  )

write.csv(all_age, "all_age.csv", row.names = F)






gs4_deauth()

gs4_auth(
  path = 'auth.json'
) 

read_sheet("1XBQ5kwrHt7v28WWnuU_1iWUA5SWnS9yYjnxYX_WdSdo")



googlesheets4::sheets_write( 
  data  = your_data_frame, 
  ss    = 'google_sheets_document_id', 
  sheet = 1 
)


read_sheet("https://docs.google.com/spreadsheets/d/1XBQ5kwrHt7v28WWnuU_1iWUA5SWnS9yYjnxYX_WdSdo/edit#gid=0")
gs4_deauth()