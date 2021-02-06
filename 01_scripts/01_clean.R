# This script creates a clean data set that contains only farm and residential class parcels own by single-parcel, out-of-state owners


# Load package and data
library(tidyverse)
york_parcels <- read_csv("00_data/Parcels.csv")
colnames(york_parcels) <- tolower(colnames(york_parcels))

colnames(york_parcels)
york_parcels %>% select(contains("addr")) %>% head(.)
york_parcels %>% select(contains("addr2")) %>% unique(.) %>% head(.) %>% print(.)


# Break the address column with city, state, and zip into separate columns
york_parcels <- york_parcels %>% 
        separate(col = mail_addr3, into = c("city", "state", "zip"), sep = " ", remove = FALSE)

# Use the length of strings to find and separate all state and non-state values 
unq_state_col_vals <- unique(york_parcels %>% select(state))
states <- unq_state_col_vals[(unq_state_col_vals$state %>% str_length(.) < 3),]
non_states <- unq_state_col_vals %>% filter(!state %in% states$state)

# Use the non-state values to clean up the state column
for (value in unique(non_states$state)) {
        pttrn <- paste(" ", value, sep = "")
        rplcmnt <- paste("_", value, sep = "")
        york_parcels <- york_parcels %>% 
                mutate(mail_addr3 = str_replace(mail_addr3, pttrn, rplcmnt))
}

# Again, break the (cleaned) address column with city, state, and zip into separate columns
york_parcels <- york_parcels %>% 
        separate(col = mail_addr3, into = c("city", "state", "zip"), sep = " ", remove = FALSE)
york_parcels <- york_parcels %>% 
        separate(col = state, into = c("state", "zip"), sep = "_", remove = FALSE)

# Remove all in-state owners
non_pa_owners <- york_parcels %>% filter(state != "PA")

nobsnss <- non_pa_owners %>% 
        filter(!class %in% c("A", "C", "I", "E")) %>% 
        filter(!str_detect(own_name1, "^EXELON|^BIF III|^COPART|^STONE FENCE|^NAIR GOPALAKRISHNAN|^TROYER|^PIXELLE|^GRIMMEL|^NORRIS|^RIVER RUN|^SAFE HARBOR|^HARMONY|^UNITED STATES|^REPUBLIC SERVICES")) %>% 
        filter(!str_detect(own_name1, "LLC$|L L C$|LLC ET AL$|INC$|CO$|TRUST$|^U S|LLC &$|CORP$|LIMITED LIABILITY COMPANY$|ASSOCIATES$|PENN BANK$|CORPORATION$|PIPELINE$|CONSERVANCY$|LAW$|
                           |MID-ATLANTIC$|RHODA$|COMPANY$|PARTICIPATION TR$|PARTICIPATION TRST$|JOINT VENTURE$|ESTATES LP$|BUTTONWOOD LP$|ROBERT A TR$
                           |KING SAMUEL B$|ING SAMUEL B$|MCKEON STEPHEN M$|KUTCHER ALBERT M$|ZOOK DOUGLAS L$"))

nobsnss %>% group_by(own_name1) %>% count() %>% arrange(desc(n))

# Remove real estate investors and other landowners with many properties
single_parcels_owners <- nobsnss %>%  
        group_by(own_name1) %>% 
        count(.) %>% filter(n==1) %>% 
        select(own_name1)

nobsnss_or_reis <- nobsnss %>% filter(own_name1 %in% single_parcels_owners[[1]])

# Clean data set contains only farm and residential class parcels own by single-parcel, out-of-state owners
write_csv(nobsnss_or_reis, "00_data/Parcels_clean.csv")

library(googlesheets4)
library(googledrive)

table <- "https://docs.google.com/spreadsheets/d/1Kig0CtHJrGr-cl5rHW0I8PqsqhEBFq_IVNAV9YGSTZ0/edit?usp=sharing"

#drive_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


sheet_append(table, nobsnss_or_reis)


read_sheet(table)
