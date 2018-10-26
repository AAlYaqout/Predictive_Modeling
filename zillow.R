library(rvest)
library(tidyr)
library(ZillowR)
library(XML)

base_url <- 'https://www.zillow.com/homes/for_sale/Miami-FL/pmf,pf_pt/12700_rid/globalrelevanceex_sort/25.864321,-80.117798,25.700473,-80.341301_rect/11_zm/'
suffix <- '_p/'

zillow.df <- data.frame(Zid = numeric(20*25),
                        Address = numeric(20*25),
                        Price = numeric(20*25),
                        Beds = numeric(20*25),
                        Baths = numeric(20*25),
                        HouseArea = numeric(20*25))
start = 1
for(i in 1:20){
        end = 25*i
        url <- paste0(base_url, i, suffix)
        page <- read_html(url)
        
        houses <- page %>%
                html_nodes(".photo-cards li article")
        
        z_id <- houses %>% html_attr("id")
        
        address <- houses %>%
                html_node(".zsg-photo-card-address") %>%
                html_text()
        
        price <- houses %>%
                html_node(".zsg-photo-card-price") %>%
                html_text() %>%
                readr::parse_number()
        
        params <- houses %>%
                html_node(".zsg-photo-card-info") %>%
                html_text() %>%
                strsplit("\u00b7")
        
        beds <- params %>% purrr::map_chr(1) %>% readr::parse_number()
        baths <- params %>% purrr::map_chr(2) %>% readr::parse_number()
        house_area <- params %>% purrr::map_chr(3) %>% readr::parse_number()
        
        zillow.df$Zid[start:end] <- z_id
        zillow.df$Address[start:end] <- address
        zillow.df$Price[start:end] <- price
        zillow.df$Beds[start:end] <- beds
        zillow.df$Baths[start:end] <- baths
        zillow.df$HouseArea[start:end] <- house_area
        
        start = start + 25
        Sys.sleep(7)
}

page <- read_html("http://www.zillow.com/homes/for_sale/Miami-FL/")

houses <- page %>%
  html_nodes(".photo-cards li article")

z_id <- houses %>% html_attr("id")

address <- houses %>%
  html_node(".zsg-photo-card-address") %>%
  html_text()

price <- houses %>%
  html_node(".zsg-photo-card-price") %>%
  html_text() %>%
  readr::parse_number()

params <- houses %>%
  html_node(".zsg-photo-card-info") %>%
  html_text() %>%
  strsplit("\u00b7")

beds <- params %>% purrr::map_chr(1) %>% readr::parse_number()
baths <- params %>% purrr::map_chr(2) %>% readr::parse_number()
house_area <- params %>% purrr::map_chr(3) %>% readr::parse_number()


#### Zillow API
myZWSID <- 'X1-ZWz19es2knvrpn_1acr8' # substitute your Zillow API key here
set_zillow_web_service_id('X1-ZWz19es2knvrpn_1acr8')

output123 = GetDeepSearchResults(address = '2898 Romana', citystatezip =  'Cincinnati, OH 45209', zws_id = getOption("ZillowR-zws_id"), url = "http://www.zillow.com/webservice/GetSearchResults.htm")
results <- xmlToList(output123$response[["results"]])
getValRange <- function(x, hilo) {
        ifelse(hilo %in% unlist(dimnames(x)), x["text",hilo][[1]], NA)
}

out <- apply(results, MAR=2, function(property) {
        zpid <- property$zpid
        links <- unlist(property$links)
        address <- unlist(property$address)
        z <- property$zestimate
        zestdf <- list(
                amount=ifelse("text" %in% names(z$amount), z$amount$text, NA),
                lastupdated=z$"last-updated",
                valueChange=ifelse(length(z$valueChange)==0, NA, z$valueChange),
                valueLow=getValRange(z$valuationRange, "low"),
                valueHigh=getValRange(z$valuationRange, "high"),
                percentile=z$percentile)
        list(id=zpid, links, address, zestdf)
})

data <- as.data.frame(do.call(rbind, lapply(out, unlist)),
                      row.names=seq_len(length(out)))


# GET DEEP COMPS
dc = GetDeepComps(myZWSID, zpid=43828618, count=25, rentzestimate=TRUE)
results <- xmlToList(dc$response[["properties"]])
results$principal
results$comparables


# GET DEEP SEARCH RESULTS
dsr <- GetDeepSearchResults(myZWSID, address = '2898 Romana Pl.', 
                            citystatezip = 'Cincinnati, OH 45209', 
                            rentzestimate = TRUE)
dsr_results <- xmlToList(dsr$response[["results"]])
dsr_results[1]
dsr_results[2] # possibly use these to mine for more data if necessary
dsr_results[3]
dsr_results[4]
dsr_results[5]
dsr_results[6]
dsr_results[7]
dsr_results[8]
dsr_results[9]
dsr_results[10]
dsr_results[11]
dsr_results[12]
dsr_results[13]
dsr_results[14]
dsr_results[15]
dsr_results[16]
dsr_results[17] # rentZestimate
dsr_results[15] # last sold price


# UPDATED PROPERTY DETAILS
xml <- GetUpdatedPropertyDetails(zpid=43828618, zws_id=myZWSID,
                                 url = "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm")

names(xml$response)


xmlToList(xml$response[["pageViewCount"]])
xmlToList(xml$response[["address"]])
xmlToList(xml$response[["posting"]])
xmlToList(xml$response[["price"]])
xmlToList(xml$response[["links"]])
xmlToList(xml$response[["images"]])
xmlToList(xml$response[["editedFacts"]])
xmlToList(xml$response[["homeDescription"]])
xmlToList(xml$response[["whatOwnerLoves"]])





getValRange <- function(x, hilo) {
        ifelse(hilo %in% unlist(dimnames(x)), x["text",hilo][[1]], NA)
}

out <- apply(results, MAR=2, function(property) {
        zpid <- property$zpid
        links <- unlist(property$links)
        address <- unlist(property$address)
        z <- property$zestimate
        zestdf <- list(
                amount=ifelse("text" %in% names(z$amount), z$amount$text, NA),
                lastupdated=z$"last-updated",
                valueChange=ifelse(length(z$valueChange)==0, NA, z$valueChange),
                valueLow=getValRange(z$valuationRange, "low"),
                valueHigh=getValRange(z$valuationRange, "high"),
                percentile=z$percentile)  
        list(id=zpid, address, zestdf)
})

data <- as.data.frame(do.call(rbind, lapply(out, unlist)), 
                      row.names=seq_len(length(out)))
