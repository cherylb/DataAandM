

library(dplyr)
library(tidyr)
library(sqldf)

# Load 3 data tables
path <- "C:/Users/Cheryl/Downloads/UCS_Satellite_Database.csv"
df_UCS <- tbl_df(read.csv(path, header=TRUE, stringsAsFactors = FALSE))
colnames(df_UCS) <- gsub("\\.","_",colnames(df_UCS))
(str(df_UCS))

path <- "C:/Users/Cheryl/Downloads/NORAD_Catalog1990.csv"
df_NORAD<- tbl_df(read.csv(path, header=TRUE, stringsAsFactors = FALSE))
colnames(df_NORAD) <- gsub("\\.","_",colnames(df_NORAD))
(str(df_NORAD))

path <- "C:/Users/Cheryl/Downloads/Satellite_Maneuver_History.csv"
df_move<- tbl_df(read.csv(path, header=TRUE, stringsAsFactors = FALSE))
colnames(df_move) <- gsub("\\.","_",colnames(df_move))
(str(df_move))


#combine data into frames needed
#where norad = UCS , fields
#where move = ucs, left
head(df_NORAD)

sql1 <- "SELECT df_UCS.*, df_NORAD.Deacy_Time, 
          df_NORAD.Decay_, 
          df_NORAD.Debris_, 
          df_NORAD.Rocket_,
          df_NORAD.NORAD_ID
        FROM df_UCS inner join df_NORAD 
          ON df_UCS.COSPAR_Number = df_NORAD.COSPAR_ID"

df_UCS_decay <- sqldf(sql1, stringsAsFactors = FALSE)

sql2 <- "SELECT df_UCS_decay.*, 
          df_move.Date, 
          df_move.Delta_Semimajor_Axis__m_ AS Delta_Semi_m,
          df_move.Delta_Eccentricity, 
          df_move.Delta_Inclination__deg_ AS Delta_Incl_deg
        FROM df_UCS_decay left join df_move
          ON df_UCS_decay.NORAD_ID = df_move.CatalogID"

df_UCS_move<- sqldf(sql2, stringsAsFactors = FALSE)

save(df_UCS_move, file = "df_move.Rdata")

## organize 
# by contractor

(all_contractor <- df_UCS_move %>%
  select(Contractor,NORAD_ID)%>% 
  group_by(Contractor)%>%
  summarise(count = n_distinct(NORAD_ID))%>%
  arrange(desc(count)))

(decay_by_cont <- df_UCS_move %>%
  select(NORAD_ID, Contractor, Decay_)%>% 
  filter(Decay_ == TRUE)%>%
  group_by(Contractor)%>%summarise(count = n_distinct(NORAD_ID)))


#who is wobbely
(Wobble_Country <- df_UCS_move %>%
  select(Country_Of_Operator_Owner,NORAD_ID,Delta_Semi_m)%>%
  mutate(abs_Delta_s = abs(Delta_Semi_m))%>%
  filter(abs_Delta_s > 0) %>%
  group_by(Country_Of_Operator_Owner)%>%
  summarise(Delta_semi=sum(abs_Delta_s), count = n())%>%
  mutate(wobble_ratio = Delta_semi/count)%>%
  arrange(desc(wobble_ratio)))
  head(wobble_Country
 
 (Wobbel_Contractor <- df_UCS_move %>%
    select(Contractor,NORAD_ID,Delta_Semi_m)%>%
    mutate(abs_Delta_s = abs(Delta_Semi_m))%>%
    filter(abs_Delta_s > 0) %>%
    group_by(Contractor)%>%
    summarise(Delta_semi=sum(abs_Delta_s), count = n())%>%
    mutate(wobble_ratio = Delta_semi/count)%>%
    arrange(desc(wobble_ratio)))
  
  head(Wobbel_Contractor)
 
 
 #### Send all of this over to Mongodb
 lst1 <- df(USF.)
