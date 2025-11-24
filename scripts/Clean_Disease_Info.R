
library(tidyverse)

setwd("C:/Users/mabes/Desktop/dcdmgp/")

file <- "C:/Users/mabes/Desktop/dcdmgp/Disease_information.txt"

#--- 1. READ THE RAW TXT FILE----
df <- read.table(
  "Disease_information.txt",
  header = TRUE,
  sep = "\t",   # any trim white spaces with ,
  quote = "\"", 
  stringsAsFactors = FALSE, # to keep text as texts
  fill = TRUE,   #it fills emmtry values with NA
  comment.char = ""
)

#-- 2. CLEANING STEPS ---
df_clean <- df %>%
  mutate(across(everything(), trimws)) %>% #remove extra white sxpaces around all fields
  mutate(across(everything(), ~na_if(., ""))) %>% #conver emtry string into proper NA
  rename(
    do_id = DO.Disease.ID,
    disease_name = DO.Disease.Name,
    omim_ids = OMIM.IDs,
    mgi_id = Mouse.MGI.ID
  )%>%
  distinct()  #to remove extra duplicate rows

#--- 3. WRITE TO CSV ---
  write.csv(df_clean, "Disease_information_cleaned.csv", row.names = FALSE)
  
  
