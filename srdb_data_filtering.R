library(readr)
library(RCurl)
library(tidyverse)
library("tidylog", warn.conflicts = FALSE)

# download 20191022, 
# commit 8df6b9e394bb0f8c44a62e535ec9c09836756b46

srdb_table <- read.csv(text = getURL("https://raw.githubusercontent.com/bpbond/srdb/master/srdb-data.csv"))

srdb_table_temperate_forests <- tbl_df(srdb_table) %>% 
  filter(Quality_flag != "Q10") %>% # filter irrelevant quality flags
  filter(Quality_flag != "Q11") %>%
  filter(Quality_flag != "Q12") %>%
  filter(Quality_flag != "Q13") %>%
  filter(Quality_flag != "Q14") %>% 
  filter(Manipulation == "None") %>% # only unmanipulated sites
  filter(Ecosystem_type == "Forest") %>%  
  drop_na(Rs_annual) %>% # omit NA in annual Rs
  group_by(Biome) %>%
  summarise(median_g_C_m_yr = median(Rs_annual), mean_g_C_m_yr = mean(Rs_annual), sd = sd(Rs_annual), n = n(), se = (sd/sqrt(n))) %>% 
  mutate(median_Mg_C_ha_yr = median_g_C_m_yr/100, mean_Mg_C_ha_yr = mean_g_C_m_yr/100) %>% 
  print
  
