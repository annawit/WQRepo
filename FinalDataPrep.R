# Notes upfront ----
# 1/30/2019 Joins for DO rules
# revised 4/19/2019
# by Anna Withington | anna.withington@gmail.com
# revised 7/9/2019 Correction to the DO satuation and status
# by Yuan Grund | grund.yuan@deq.state.or.us

## install.packages("tidyverse"); install.packages("lubridate"); install.packages("dplyr")

library("tidyverse"); library("lubridate"); library("dplyr")

# original data, called dta2
## setwd("~/PROJECTS/20190709_DanSobota_NorthCoastContinuousDissolvedOxygenVisualizer/NorthCoastDO")
load("data/ShinyAllData.Rdata")

# stations list with geodata from Lesley Merrick
stations <- read_csv("data/TEP_StationsInfo.csv")

# the DO lookup table, from Travis Pritchard
DO_lvls <- read_csv("data/DO_crit.csv")
# Note: some revisions are handcoded below.

# spawning dates, also from Lesley Merrick.
# dates may come in in weird format
spn <- read_csv("data/IRDatabase_CriteriaTables.csv")

# removes stations without a lasar/MLocID id
# we are waiting for these to get coded
# refers specifically to two TEP sampling sites
dta3 <- dta2 %>% 
  filter(!is.na(lasar_id))

# join the station list to the spawn codes
spawnjoin <- left_join(stations, spn, by = c("SpawnCode" = "SpawnCode"))
# started with 14 stations, ended with 14 stations

# change station keys to factors for join with dta3
spawnjoin$station_key <- as.factor(spawnjoin$station_key)

# join spawn dates to all of the continuous DO data
dataspawn <- left_join(dta3, spawnjoin, by = c("lasar_id" = "station_key"))

# join DO baseline criteria to above
DO_base <- left_join(dataspawn, DO_lvls, by = "DO_code")

# remove samples with negetive DO value
DO_base$do <- replace(DO_base$do,which(DO_base$do <= 0),NA)

# Correct DO_sat: re-cal DO_sat and update "-" values to NA
DO_base$do_sat_cor <- if_else(
  !is.na(DO_base$do) == T, 
  as.numeric(DO_base$do * 100 / ((exp(-139.34411 + 
                                        (157570.1/(DO_base$temp +273.15)) -
                                        (66423080/((DO_base$temp +273.15)^2)) +
                                        (12438000000/((DO_base$temp +273.15)^3)) -
                                        (862194900000/((DO_base$temp +273.15)^4)))) *
                                   (1-(0.0001148 * DO_base$ELEV_Ft * 0.3048)))),
  DO_base$do_sat)
DO_base$do_sat_cor <- ifelse(DO_base$do_sat_cor<0, NA, round(as.numeric(DO_base$do_sat_cor),2))

# A test case for spawn date coding, see actual case below
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

# DO status decision & generate datasets for shiny app ----
# most of this is the same as Travis's code

dta4 <- DO_base %>% 
  mutate(# correct to dmy format
    SpawnStart = ifelse(!is.na(SpawnStart) & SpawnStart != "NULL", paste0(SpawnStart, "-", year(datetime) ), SpawnStart ),
    SpawnEnd = ifelse(!is.na(SpawnEnd) & SpawnEnd != "NULL", paste0(SpawnEnd, "-", year(datetime)), SpawnEnd),
    SpawnStart = lubridate::dmy(SpawnStart),
    SpawnEnd = lubridate::dmy(SpawnEnd),
    SpawnEnd = if_else(SpawnEnd < SpawnStart, SpawnEnd + years(1), SpawnEnd),
    # if the datetime is between spawnstart and spawnend, and spawnstart is not NA,
    # code in_spawn column as 1. in other words, 1 means you are in the spawning season
    # zero means you are not in the spawning season
    in_spawn = ifelse(datetime >= SpawnStart & datetime <= SpawnEnd & !is.na(SpawnStart), TRUE, FALSE),
    DO_lim = ifelse(in_spawn == 1, 11, crit_Instant),
    DO_sat_lim = ifelse(MonLocType == "River/Stream" & in_spawn == 1, "95%",
                        ifelse(MonLocType == "River/Stream" & in_spawn == 0, "90%", NA)),
    # in estuary, if DO > 6.5, meets criteria, otherwise, DO excursion
    # in river/stream, 
    # (1) during spawning (in_spawn = 1), if DO > 11 or DO_sat > 95%, meets criteria,
    # (2) during non-spawning (in_spawn = 0), if DO > 8 or DO_sat > 90%, meets criteria,
    # (3) otherwise, DO excursion
    DO_status = ifelse(
      MonLocType == "Estuary" & do >= 6.5, "Meets criteria",
      ifelse(MonLocType == "River/Stream" & in_spawn == 1 & ( do >= 11 || do_sat_cor >= 95) , "Meets criteria",
             ifelse(MonLocType == "River/Stream" & in_spawn == 0 & ( do >= 8 || do_sat_cor >= 90), "Meets criteria",
                    "Excursion")))
    )

# results in a "49566 failed to parse", because those are Estuary & No spawn dates
# count No spawn dates
## summary(as.factor(dta4$Spawn_dates))

# check DO status
## summary(as.factor(dta4$DO_status))
# results in (1) 597 NA when Estuary & DO=NA (estuary critiria is DO only); 
#            (2) 70 negetive DO values replaced by NA
# count DO=NA
## summary(as.factor(dta4$do))

## names(dta4)

sites <- dta4 %>%
  group_by(MLocID, StationDes, Lat_DD, Long_DD, RiverMile, Spawn_dates, MonLocType) %>% 
  count() %>% 
  rename(`Station Description` = StationDes,
         Lat = Lat_DD,
         Long = Long_DD,
         Type = MonLocType) %>% 
  ungroup()

# removes NA DO_status
# removes columns we don't need in the continuous data

dta1 <- dta4 %>% 
  filter(!is.na(DO_status)) %>% 
  select(MLocID, StationDes, datetime, temp, grade_temp, ph, grade_ph, cond,
         grade_cond, do, grade_do, do_sat_cor, grade_do_sat, data_source,
         Lat_DD, Long_DD, MonLocType, RiverMile, Spawn_dates, SpawnStart, 
         SpawnEnd, in_spawn, DO_lim, DO_sat_lim, DO_status) %>% 
  mutate(Site = paste(MLocID," ",StationDes),
         Date = as.Date(format(datetime, "%Y-%m-%d")))
  

dta <- dta1 %>%
  select(MLocID, StationDes, datetime, temp, grade_temp, ph, grade_ph, cond,
         grade_cond, do, grade_do, do_sat_cor, grade_do_sat, data_source,
         Lat_DD, Long_DD, MonLocType, RiverMile, Spawn_dates, SpawnStart, 
         SpawnEnd, in_spawn, DO_lim, DO_sat_lim, DO_status, Site, Date) %>%
  rename(`Station Description` = StationDes,
         `Sample Time` = datetime,
         "Temperature (\u00B0C)" = temp,
         `Temperature Grade` = grade_temp,
         `pH` = ph,
         `pH Grade` = grade_ph,
         "Conductivity (\u03BCS)" = cond,
         `Conductivity Grade` = grade_cond,
         `Dissolved Oxygen (mg/L)` = do,
         `Dissolved Oxygen Grade` = grade_do,
         `Dissolved Oxygen Saturation (%)` = do_sat_cor,
         `Dissolved Oxygen Saturation Grade` = grade_do_sat,
         `Data Source` = data_source,
         `Latitude` = Lat_DD,
         `Longitude` = Long_DD,
         `Waterbody Type` = MonLocType,
         `River Miles` = RiverMile,
         `Spawn Dates` = Spawn_dates,
         `Spawn Start` = SpawnStart,
         `Spawn End` = SpawnEnd,
         `During Spawning` = in_spawn,
         `DO Criteria` = DO_lim,
         `DO Saturation Criteria` = DO_sat_lim,
         `DO Status` = DO_status)

# Gives percent meeting criteria at each site
meets <- dta1 %>%
  group_by(MLocID,DO_status) %>%
  count() %>%
  group_by(MLocID) %>% 
  mutate(pctmeets = n/sum(n)) %>% 
  filter(DO_status == "Meets criteria") %>% 
  mutate(pctbin = ifelse(pctmeets == 1,"All samples meeting criteria", "Some samples not meeting criteria"))


s <- left_join(sites, meets, by= "MLocID")

save.image(file = "finaldata.RData")
