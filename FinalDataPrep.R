# 1/30/2019 Joins for DO rules --------------------------------------------
# revised 4/19/2019
# Anna Withington
# anna.withington@gmail.com

library(tidyverse)
library(lubridate)

#original data, called dta2
load("data/ShinyAllData.Rdata")

# stations list with geodata from Lesley Merrick
stations <- read_csv("data/TEP_StationsInfo.csv")

# the DO lookup table, from Travis Pritchard
DO_lvls <- read_csv("data/DO_crit.csv")
# Note: some revisions are handcoded below.

# spawning dates, also from Lesley Merrick.
# dates may come in in weird format
spn <- read_csv("data/IRDatabase_CriteriaTables.csv")

#removes stations without a lasar/MLocID id
#we are waiting for these to get coded
#refers specifically to two TEP sampling sites
dta3 <- dta2 %>% 
  filter(!is.na(lasar_id))

#join the station list to the spawn codes
spawnjoin <- left_join(stations, spn, by = c("SpawnCode" = "SpawnCode"))
# started with 14 stations, ended with 14 stations

# change station keys to factors for join with dta3
spawnjoin$station_key <- as.factor(spawnjoin$station_key)

#join spawn dates to all of the continuous DO data
dataspawn <- left_join(dta3, spawnjoin, by = c("lasar_id" = "station_key"))

#join DO baseline criteria to above
DO_base <- left_join(dataspawn, DO_lvls, by = "DO_code")

#----
#A test case for spawn date coding, see actual case below
# short <- DO_base %>% 
#   select(datetime, SpawnStart, SpawnEnd, crit_Instant)
# 
# # Adapted from Travis Prichard:
# # https://github.com/TravisPritchardODEQ/IR2018/blob/master/Parameters/DO/fun_DO_spawn.R
# nt <- short %>% 
#   mutate(SpawnStart = ifelse(!is.na(SpawnStart) & SpawnStart != "NULL", paste0(SpawnStart, "/", year(datetime) ), SpawnStart ),
#          SpawnEnd = ifelse(!is.na(SpawnEnd) & SpawnEnd != "NULL", paste0(SpawnEnd, "/", year(datetime)), SpawnEnd),
#          SpawnStart = mdy(SpawnStart),
#          SpawnEnd = mdy(SpawnEnd),
#          SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd),
#          in_spawn = ifelse(datetime >= SpawnStart & datetime <= SpawnEnd & !is.na(SpawnStart), 1, 0 ))
#----

# most of this is the same as Travis's code
# R doesn't like to work with day/month without year associated, so
# this is a way around that

# changed the last few lines so that if the spawning season was active, 
# the do limit would be 11, and otherwise would be the value assigned from
# the DO_code to crit_Instant (either 6.5 or 8 for grab samples, for these sites)

#the last step compares the actual DO to the DO recorded
# and assigns pass, fail, or other
# assigned other to evaluate any unexpected cases
# allowed pass for greater than OR EQUAL TO.

dtasp <- DO_base %>% 
  mutate(SpawnStart = ifelse(!is.na(SpawnStart) & SpawnStart != "NULL", paste0(SpawnStart, "-", year(datetime) ), SpawnStart ),
         SpawnEnd = ifelse(!is.na(SpawnEnd) & SpawnEnd != "NULL", paste0(SpawnEnd, "-", year(datetime)), SpawnEnd),
         SpawnStart = dmy(SpawnStart),
         SpawnEnd = dmy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd),
         #if the datetime is between spawnstart and spawn and and spawnstart is not NA,
         # code in_spawn column as 1. in other words, 1 means you are in the spawning season
         # zero means you are not in the spawning season
         in_spawn = ifelse(datetime >= SpawnStart & datetime <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         #if you're in the spawning season, your DO limit is 11,
         #otherwise your DO limit is whatever the crit instant says it is
         DO_lim = ifelse(in_spawn == 1, 11, crit_Instant),
         # if your DO is below the limit, excursion
         # if your DO is above or equal to the limit, it meets the criteria
         DO_status = ifelse(do < DO_lim, "Excursion",
                            ifelse(do >= DO_lim, "Meets criteria", "other")))
#results in a "failed to parse warning" for around 49k records
#this is because those don't have spawn dates. it should be ok but double check
#to make sure the columns were correctly generated

summary(as.factor(dtasp$DO_status))
# check results
# results in NAs, these items have DO sat only
# no "other"



names(dtasp)


sites <- dtasp %>%
  group_by(MLocID, StationDes, Lat_DD, Long_DD, LLID, RiverMile, Spawn_dates, MonLocType) %>% 
  count() %>% 
  rename(`Station Description` = StationDes,
         Lat = Lat_DD,
         Long = Long_DD,
         Type = MonLocType) %>% 
  ungroup()

#removes columns with DO sat only
#removes columns we don't need in the continuous data
dtasp1 <- dtasp %>% 
  filter(!is.na(DO_status)) %>% 
  select(-c(lasar_id, Datum, CollMethod, MapScale, AU_ID,
            Comments, STATE, COUNTY, T_R_S, EcoRegion3, EcoRegion4, HUC4_Name,
            HUC6_Name, HUC8_Name, HUC10_Name, HUC12_Name, HUC8, HUC10, HUC12,
            ELEV_Ft, GNIS_Name, Reachcode, Measure, SnapDate, ReachRes,
            SnapDist_ft, Conf_Score, QC_Comm, UseNHD_LL,FishCode, SpawnCode,
            WaterTypeCode,WaterBodyCode,BacteriaCode, DO_code, ben_use_code,
            pH_code, DO_SpawnCode, OWRD_Basin, TimeZone, EcoRegion2, UserName,
            Created_Date, crit_30D, crit_7Mi, crit_Min)) %>% 
  select(MLocID, StationDes, everything())

#filters NAs, creates identifier "Site"
dta1 <- dtasp1 %>%
  filter(!is.na(datetime)) %>% 
  mutate(Site = paste(MLocID, StationDes))

save(dta1, file = "data/dataforwqapp.RData")
save(sites, file = "data/sitesummary.RData")

