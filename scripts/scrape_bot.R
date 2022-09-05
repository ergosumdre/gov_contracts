# Author Dre Dyson
# Github link: https://github.com/ergosumdre
# 
# 
# 
# This script will return All Sold Items on Ebay given a Sold/Completed URL. 
# Pseudo code:
# 1. go to page
#  # get number of pages
# 2. scrape listings
#  # add the following to dataframe: title, price, date sold, link
# 3. click on next page
#  # repeat 1 and 2. Append rows to previous dataframe

library(RSelenium)
library(lubridate)
library(rvest)
library(stringr)
library(dplyr)





# Pagination
main_page <- "https://sam.gov/search/?index=opp&sort=-modifiedDate&page=1&pageSize=100&sfm%5BsimpleSearch%5D%5BkeywordRadio%5D=ANY&sfm%5Bstatus%5D%5Bis_active%5D=true&sfm%5BserviceClassificationWrapper%5D%5Bpsc%5D%5B0%5D%5Bkey%5D=84&sfm%5BserviceClassificationWrapper%5D%5Bpsc%5D%5B0%5D%5Bvalue%5D=84%20-%20CLOTHING,%20INDIVIDUAL%20EQUIPMENT,%20INSIGNA,%20AND%20JEWELRY"
rD <- rsDriver(browser="firefox", port=2848L, verbose=F)
remDr <- rD[["client"]]
remDr$navigate(main_page)
html <- remDr$getPageSource()[[1]]


remDr$findElement("css selector", "usa-icon.ng-tns-c72-1")$clickElement()


numOfPages <- read_html(html) %>%
  html_nodes("span.sds-pagination__total") %>%
  html_text() %>%
  gsub(" of ", "", .) %>%
  as.numeric()


linkFirstHalf <- "https://sam.gov/search/?index=opp&sort=-modifiedDate&page="
linkSecondHalf <- "&pageSize=100&sfm%5BsimpleSearch%5D%5BkeywordRadio%5D=ANY&sfm%5Bstatus%5D%5Bis_active%5D=true&sfm%5BserviceClassificationWrapper%5D%5Bpsc%5D%5B0%5D%5Bkey%5D=84&sfm%5BserviceClassificationWrapper%5D%5Bpsc%5D%5B0%5D%5Bvalue%5D=84%20-%20CLOTHING,%20INDIVIDUAL%20EQUIPMENT,%20INSIGNA,%20AND%20JEWELRY"


for(i in 1:numOfPages){
  numOfPages[i] <- paste0(linkFirstHalf, i, linkSecondHalf)
}

# use CSS Selector

listings_details <- function(pageLink){
  remDr$navigate(pageLink) # go to page
  Sys.sleep(5)
  print(paste0("Going to page: ", pageLink))
  html <- remDr$getPageSource()[[1]]
  
  # create empty dataframe
  details <- data.frame("listing_title" = character(),
                        "listing_agency" = character(),
                        "listing_sub_tier" = character(),
                        "listing_published_date" = character(),
                        "listing_due_date" = character(),
                        "listing_product_service_code" = character(),
                        "listing_place_performance_city" = character(),
                        "listing_place_performance_state" = character(),
                        "listing_place_performance_zip" = character(),
                        "listing_place_performance_country" = character(),
                        "listing_description" = character(),
                        "listing_contact_primary_name" = character(),
                        "listing_contact_primary_email" = character(),
                        "listing_contact_primary_ph_number" = character(),
                        "listing_contact_secondary_name" = character(),
                        "listing_contact_secondary_email" = character(),
                        "listing_contact_secondary_ph_number" = character())
  
  
  title <- read_html(html) %>% # parse HTML
    html_nodes("h1") %>% # type of table/class and title of table/class
    rvest::html_text() %>% # get text
    stringr::str_squish()
  
  agency <- read_html(html) %>%
    html_node("#header-hierarchy-level > div:nth-child(1) > div:nth-child(2)") %>%
    html_text() %>% 
    stringr::str_squish()
  
  
  sub_tier <- read_html(html) %>%
    html_node("div.description:nth-child(4)") %>%
    html_text() %>% 
    stringr::str_squish()
  
  published_date <- read_html(html) %>%
    html_node("#general-original-published-date") %>%
    html_text() %>%
    gsub("Original Published Date: ", "", .) %>%
    lubridate::mdy_hm()
    
  
  due_date <- read_html(html) %>%
    html_node("#general-response-date") %>%
    html_text() %>%
    gsub("Updated Date Offers Due: ", "", .) %>%
    lubridate::mdy_hm()
  
  # some listings have due dates, others do not
  #reminding_time <- (due_date - published_date) %>% gsub("Time difference of ", "", .) %>% paste0(., " days")
  
  product_service_code <- read_html(html) %>%
    html_node("#classification-classification-code") %>%
    html_text() %>%
    gsub("Product Service Code: ", "", .) %>%
    stringr::str_squish()
  
  place_performance_city <- read_html(html) %>%
    html_node("#classification-pop-city") %>%
    html_text() %>%
    stringr::str_squish()
  
  place_performance_state <- read_html(html) %>%
    html_node("#classification-pop-state") %>%
    html_text() %>%
    stringr::str_squish()
  
  place_performance_zip <- read_html(html) %>%
    html_node("#classification-pop-zip") %>%
    html_text() %>%
    stringr::str_squish()
  
  place_performance_country <- read_html(html) %>%
    html_node("#classification-pop-country") %>%
    html_text() %>%
    stringr::str_squish()
  
  listing_description <- read_html(html) %>%
    html_node("#description") %>%
    html_text() %>%
    stringr::str_squish()
    
  contact_primary_name <- read_html(html) %>%
    html_node("#contact-primary-poc-full-name") %>%
    html_text() %>%
    stringr::str_squish()
  
  contact_primary_email <- read_html(html) %>%
    html_node("#contact-primary-poc-email") %>%
    html_text() %>%
    stringr::str_squish()
  
  contact_primary_ph_number <- read_html(html) %>%
    html_node("#contact-primary-poc-phone") %>%
    html_text() %>%
    stringr::str_squish() %>%
    gsub("Phone Number ", "", .)
  
  contact_secondary_name <- read_html(html) %>%
    html_node("#contact-secondary-poc-full-name") %>%
    html_text() %>%
    stringr::str_squish()
  
  contact_secondary_email <- read_html(html) %>%
    html_node("#contact-secondary-poc-email") %>%
    html_text() %>%
    stringr::str_squish()
  
  contact_secondary_ph_number <- read_html(html) %>%
    html_node("#contact-secondary-poc-phone") %>%
    html_text() %>%
    stringr::str_squish() %>%
    gsub("Phone Number ", "", .)

  details <- data.frame(listing_title = title,
                       listing_agency = agency,
                       listing_sub_tier = sub_tier,
                       listing_published_date = published_date,
                       listing_due_date = due_date,
                       listing_product_service_code = product_service_code,
                       listing_place_performance_city = place_performance_city,
                       listing_place_performance_state = place_performance_state,
                       listing_place_performance_zip = place_performance_zip,
                       listing_place_performance_country = place_performance_country,
                       listing_description = listing_description,
                       listing_contact_primary_name = contact_primary_name,
                       listing_contact_primary_email = contact_primary_email,
                       listing_contact_primary_ph_number = contact_primary_ph_number,
                       listing_contact_secondary_name = contact_secondary_name,
                       listing_contact_secondary_email = contact_secondary_email,
                       listing_contact_secondary_ph_number = contact_secondary_ph_number)
  return(details)
  
}
  
  
  
  
  
  


listings <- function(link){
  remDr$navigate(link) # go to page
  Sys.sleep(5)
  print(paste0("Going to page: ", link))
  html <- remDr$getPageSource()[[1]]
  #Sys.sleep(5)
  title <- read_html(html) %>% # parse HTML
    html_nodes("h3.margin-y-0") %>% # type of table/class and title of table/class
    rvest::html_text() %>% # get text
    stringr::str_squish() # remove any whitespaces
  
  
  link <- read_html(html) %>%
    html_nodes("a.usa-link") %>%
    rvest::html_attr("href") %>%
    grep("/opp/", ., value = T) %>%
    paste0("https://sam.gov", .)
  
  one_listing <- lapply(link, listings_details)
  one_listing <-  do.call(rbind.data.frame, one_listing)

  return(one_listing)
}



all_listings <- lapply(numOfPages, listings) # do function

listings <- do.call(rbind.data.frame, all_listings) # unlist data into dataframe
write.csv(listings, "/Users/dre/Downloads/govContracts_bot/gov_contracts.csv")
