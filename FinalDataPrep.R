# 1/30/2019 Joins for DO rules --------------------------------------------
library(tidyverse)
library(lubridate)

#original data, called dta2
load("ShinyAllData.Rdata")
# stations list with geodata from Lesley Merrick
stations <- read_csv("TEP_StationsInfo_anna.csv")
# the DO lookup table, from Travis Pritchard
DO_lvls <- read_csv("DO_crit.csv")
# spawning dates, also from Lesley Merrick.
# dates may come in in weird format
spn <- read_csv("IRDatabase_CriteriaTables.csv")

#removes stations without a lasar id, might have to be remedied later
dta3 <- dta2 %>% 
  filter(!is.na(lasar_id))

#join the station list to the spawn codes
spawnjoin <- left_join(stations, spn, by = c("SpawnCode" = "SpawnCode"))

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
  mutate(SpawnStart = ifelse(!is.na(SpawnStart) & SpawnStart != "NULL", paste0(SpawnStart, "/", year(datetime) ), SpawnStart ),
         SpawnEnd = ifelse(!is.na(SpawnEnd) & SpawnEnd != "NULL", paste0(SpawnEnd, "/", year(datetime)), SpawnEnd),
         SpawnStart = mdy(SpawnStart),
         SpawnEnd = mdy(SpawnEnd),
         SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd),
         in_spawn = ifelse(datetime >= SpawnStart & datetime <= SpawnEnd & !is.na(SpawnStart), 1, 0 ),
         DO_lim = ifelse(in_spawn == 1, 11, crit_Instant),
         DO_status = ifelse(do < DO_lim, 0,
                            ifelse(do >= DO_lim, 1, "other")))
#results in a "failed to parse warning" - this should be ok

summary(as.factor(dtasp$DO_status))
# results in NAs, these items have DO sat only

#example table for York
# d <- dtasp[1:6,]
# write.csv(d, "tablesample.csv")

names(dtasp)
names(dta1)

dtasp1 <- dtasp %>% 
  filter(!is.na(DO_status)) %>% 
  select(-c(lasar_id, CollMethod, MonLocType, AU_ID, STATE, Reachcode, Measure, LLID, ReachRes,
            crit_30D, crit_7Mi, crit_Min, Spawn_dates, Created_Date, UserName, EcoRegion2,
            OWRD_Basin, UseNHD_LL, FishCode, SpawnCode, WaterTypeCode, WaterBodyCode,
            BacteriaCode, DO_code, ben_use_code, pH_code, DO_SpawnCode, QC_Comm,
            Conf_Score, SnapDist_ft, SnapDate, HUC10, HUC8, HUC4_Name, HUC6_Name,
            HUC12, EcoRegion3, T_R_S, Comments)) %>% 
  select(MLocID, StationDes, everything())


dta1 <- dtasp1 %>%
  filter(!is.na(datetime)) %>% 
  mutate(Site = paste(MLocID, StationDes))

save(dta1, file = "dataforwqapp.RData")

# ddd <- dtasp %>% 
#   group_by(month = floor_date(datetime, "month")) %>% 
#   mutate(n_samples = n()) %>% 
#   mutate(season = factor(month.abb[month(datetime)],
#                          levels = c("May", "Jun", "Jul", "Aug", "Oct")))
