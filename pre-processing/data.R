"
Preparation of data to generate shiny application

Author: Pedro Alencar

13.01.2023
"

pacman::p_load(dplyr, tidyr, ggplot2, magrittr, shiny, shinythemes, plotly,
               leaflet, RColorBrewer, fst, tibble, tibbletime, lubridate)

df <- read.csv("./data_hanna/crop_Brandenburg.csv", sep = ";") %>%
  mutate(A_ha =  as.numeric(A_ha),
         P_per_A =  as.numeric(P_per_A),
         P_t =  as.numeric(P_t))
# View(df)

df2001 <- df %>% filter(Year == "2001-2006") %>%
  group_by(Landkreis,Crop,Variety)
df2006 <- df %>% filter(Year == "2006") %>%
  group_by(Landkreis,Crop,Variety)

df2005 <- left_join(df2001, df2006, suffix = c(".x", ".y"), by = c("Landkreis","Crop","Variety")) %>%
  mutate(A_ha = as.numeric(NA),
         P_t = as.numeric(NA),
         P_per_A = (6*P_per_A.x - P_per_A.y)/5,
         Year = "2001-2005") %>%
  select(Landkreis, Crop, Variety,A_ha,P_per_A,P_t,Year )

df_complete <- rbind(df2005, df) %>%
  filter(Year != "2001-2006") %>%
  mutate(P_per_A = P_per_A/10) %>%
  mutate(P_per_A = ifelse(is.na(P_per_A), P_t/A_ha, P_per_A))

# View(df_complete)

fst::write_fst(df_complete, "crop_bb_complete.bin")
data.table::fwrite(df_complete, "crop_bb_complete.csv")
