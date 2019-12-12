library(tidyverse)
library(lubridate)
library(quantmod)

# Read in data (already combined excel files for 2018 and 2019)
snap_ads = read_csv("../Datasets/PoliticalAds.csv") 

glimpse(snap_ads)

# Convert date columns to date format
snap_ads = snap_ads %>% 
  mutate_at(vars(contains("Date")), as_date) %>% 
  replace_na(list(EndDate = today()))

# Count the targeted interests after splitting them by commas
str_split(snap_ads$Interests, pattern = ",") %>% 
  flatten() %>% 
  discard(is.na) %>% 
  flatten_chr() %>% 
  tibble(interests = .) %>% 
  count(interests) %>% 
  write_csv("../Datasets/snap_interests.csv")

# region groups
r_south_america = c("argentina", "brazil", "chile")
r_north_america = c("united states", "canada", "puerto rico")
r_europe = c("austria", "belgium", "denmark", "finland", 
             "france", "germany", "ireland", "lithuania", 
             "netherlands", "norway", "poland", "sweden", "switzerland", 
             "turkey", "united kingdom")
r_middle_east_africa = c("iraq", "kuwait", "nigeria", "south africa", "united arab emirates")
r_asia_oceania = c("australia", "india", "new zealand")

# Get currency data for conversion
fxData = do.call(merge.xts,lapply(paste0(unique(snap_ads$`Currency Code`), "/USD"),function(x) 
  getFX(x,auto.assign=FALSE)))

colnames(fxData) = str_remove_all(colnames(fxData),".USD")

fxData_DF = data.frame(date=index(fxData),coredata(fxData),stringsAsFactors=FALSE) %>% 
  gather("currency", "conversion_factor", -date) %>% 
  as_tibble()


# Apply regions and currency conversion
snap_ads_region = snap_ads %>% 
  mutate(region = case_when(CountryCode %in% r_south_america ~ "South America",
                            CountryCode %in% r_north_america ~ "North America",
                            CountryCode %in% r_europe ~ "Europe",
                            CountryCode %in% r_middle_east_africa ~ "Middle East & Africa",
                            CountryCode %in% r_asia_oceania ~ "Asia & Oceania"),
         CandidateBallotInformation = str_squish(CandidateBallotInformation)) %>% 
  left_join(fxData_DF, by = c("StartDate" = "date", "Currency Code" = "currency")) %>% 
  arrange(`Currency Code`, StartDate) %>% 
  # getFX() can only get past 180 days of exchange rates
  # For dates prior, use oldest exchange rate (imperfect)
  fill(conversion_factor, .direction = "up") %>% 
  mutate(Spend_usd = Spend*conversion_factor,
         CountryCode = str_to_title(CountryCode))

snap_ads_region %>% 
  write_csv("../Datasets/PoliticalAds_region.csv")


# Use this code to explore upticks
snap_ads_region %>% 
  mutate(year = year(StartDate),
         month = month(StartDate),
         week = week(StartDate)) %>% 
  filter(year == 2018, month == 10, region == "North America") %>%
  top_n(10, wt = Spend_usd) %>% 
  count(PayingAdvertiserName, wt = Spend) %>% 
  arrange(-n)


         
         
