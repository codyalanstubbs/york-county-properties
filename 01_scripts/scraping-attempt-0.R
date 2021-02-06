# Load the Library
library(RSelenium)
library(XML)
library(tidyverse)

eCaps <- list("chrome_binary" = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
# start the server and browser(you can use other browsers here)
rD <- rsDriver(browser=c("chrome"), chromever="87.0.4280.88", extraCapabilities = eCaps, port = 4602L)
driver <- rD$client

# retrieve all math textbooks----
# navigate to textbook subjects page
driver$navigate("http://assessments.yorkcountypa.gov/Search?parcelID=0&ownerName=&address=")

# Click Accept button on Disclaimer page
accept_button <- driver$findElement(using = 'css selector', "button.btn.btn-default")
accept_button$clickElement()

# Type "0" into Parcel ID search box
parcel_id_search <- driver$findElement(using = "css", "[name = 'parcelID']")
parcel_id_search$sendKeysToElement(list("0"))

# Hit "Submit"
submit_button <- driver$findElement(using = 'css selector', "input.btn.btn-default")
submit_button$clickElement()

# find and retrieve all the links on the page
parcel_table_elements <- driver$findElements("css selector","[class = 'tableCells']")
parcel_table_text <- unlist(sapply(parcel_table_elements, function(x){x$getElementText()}))
parcel_ids <- data.frame(parcel_table_text) %>% 
        filter(str_detect(parcel_table_text, "..-...-..-....\\...-.....")) %>% 
        rename( ids = parcel_table_text)

# retrieve table of contents data from all textbooks ----
parcel_data <- data.frame() # create empty data frame for collecting

for (id in parcel_ids$ids){
        
        # Click on the "Details" button for a specific parcel ID
        details_button <- driver$findElement("css selector", paste("[data-assigned-id='", id, "']", sep = ""))
        details_button$clickElement()
        Sys.sleep(0.75)
        
        # Extract parcel data
        details_elements <- driver$findElements("css selector","[class = 'col-lg-12']")
        details_data <- unlist(sapply(details_elements, function(x){x$getElementText()}))
        Sys.sleep(0.5)
        
        parcel_data <- parcel_data %>% 
                rbind(
                        data.frame(extract_date = Sys.Date(), parcel_id = id, data = details_data)
                )
        
}

processed_parcel_data <<- parcel_data %>% 
        separate_rows(data,sep = "\n") %>% 
        filter(str_detect(data, ":")) %>% 
        separate(data, into = c("metric", "value"), sep = ": ")

write.csv(processed_parcel_data, "processed_parcel_data.csv", row.names = FALSE)

scrape_pages <- function(pages) {
        
        for (page in pages){
                
                # Navigate to the specific page
                page_link <- paste("http://assessments.yorkcountypa.gov/Search?page=", pages[1], "&parcelID=0&agreed=True", sep = "")
                driver$navigate(page_link)      
                
                # find and retrieve all the links on the page
                parcel_table_elements <- driver$findElements("css selector","[class = 'tableCells']")
                parcel_table_text <- unlist(sapply(parcel_table_elements, function(x){x$getElementText()}))
                parcel_ids <- data.frame(parcel_table_text) %>% 
                        filter(str_detect(links, "..-...-..-....\\...-.....")) %>% 
                        rename( ids = parcel_table_text)
                
                for (id in parcel_ids$ids){
                        
                        # Click on the "Details" button for a specific parcel ID
                        details_button <- driver$findElement("css selector", paste("[data-assigned-id='", id, "']", sep = ""))
                        details_button$clickElement()
                        Sys.sleep(0.75)
                        
                        # Extract parcel data
                        details_elements <- driver$findElements("css selector","[class = 'col-lg-12']")
                        details_data <- unlist(sapply(details_elements, function(x){x$getElementText()}))
                        Sys.sleep(0.5)
                        
                        parcel_data <- parcel_data %>% 
                                rbind(
                                        data.frame(parcel_id = id, data = details_data)
                                )
                        
                }
        }
        
        processed_parcel_data <<- parcel_data %>% 
                separate_rows(data,sep = "\n") %>% 
                filter(str_detect(data, ":")) %>% 
                separate(data, into = c("metric", "value"), sep = ": ")
        
        write.csv(processed_parcel_data, "processed_parcel_data.csv", row.names = FALSE, append = TRUE)
}

#close the driver
driver$close()

#close the server
rD$server$stop()

# final modifications and write to csv ----
all_toc_text$book <- gsub("^https://openstax.org/details/books/", "", x = all_toc_text$book) 
write.csv(all_toc_text, "data/mathtext_tocs.csv", row.names = FALSE)
