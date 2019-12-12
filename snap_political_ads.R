library(tidyverse)

snap_ads = read_csv("../Datasets/PoliticalAds.csv")

str_split(snap_ads$Interests, pattern = ",") %>% 
  flatten() %>% 
  discard(is.na) %>% 
  flatten_chr() %>% 
  tibble(interests = .) %>% 
  count(interests) %>% 
  write_csv("../Datasets/snap_interests.csv")


r_south_america = c("argentina", "brazil", "chile")
r_north_america = c("united states", "canada", "puerto rico")
r_europe = c("austria", "belgium", "denmark", "finland", 
             "france", "germany", "ireland", "lithuania", 
             "netherlands", "norway", "poland", "sweden", "switzerland", 
             "turkey", "united kingdom")
r_middle_east_africa = c("iraq", "kuwait", "nigeria", "south africa", "united arab emirates")
r_asia_oceania = c("australia", "india", "new zealand")
snap_ads %>% 
  mutate(region = case_when(CountryCode %in% r_south_america ~ "South America",
                            CountryCode %in% r_north_america ~ "North America",
                            CountryCode %in% r_europe ~ "Europe",
                            CountryCode %in% r_middle_east_africa ~ "Middle East & Africa",
                            CountryCode %in% r_asia_oceania ~ "Asia & Oceania"),
         CandidateBallotInformation = str_squish(CandidateBallotInformation)) %>% 
  count(CandidateBallotInformation)
  write_csv("../Datasets/PoliticalAds_region.csv")
         
         
