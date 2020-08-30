
##### Selenium & PhantomJS
rm(list=ls())
library(RSelenium)
library(rvest)
library(wdman)
library(mongolite)
library(tidyverse)

id <- "49168" #"48973" # Geschäfts ID

pjs <- phantomjs(
  port = 4444L,
  version = "2.1.1",
  check = TRUE,
  loglevel = c("INFO", "ERROR", "WARN", "DEBUG"),
  verbose = TRUE,
  retcommand = FALSE
)

Sys.sleep(5) # give the phantom binary a moment
remDr <- remoteDriver(browserName = 'phantomjs')
remDr$open()

remDr$navigate(paste("https://www.parlament.ch/de/ratsbetrieb/amtliches-bulletin/amtliches-bulletin-die-verhandlungen?SubjectId=", id, sep=''))
Sys.sleep(5) # give the website a moment to load
# remDr$screenshot(display = FALSE, useViewer = F, file = "screen.png")

subject.contents <- remDr$findElements(using = 'css selector', value = paste("[subjectid='", id, "']", sep="")) 
results <- vector()

# v1: Parse as selenium WebElements..
# for(i in subject.contents){
#   
#   # exclude votes etc. - we just want the spoken statements
#   if(length(i$findChildElements("css selector", "em")) == 0 && length(i$findChildElements("css selector", "strong")) == 0){
#     
#     # remove page break indicators
#     spans <- i$findChildElements("css selector", "span.page-break")
#     if(length(spans) > 0){
#       result <- i$getElementText()[[1]]
#       for(j in spans){
#         result <- gsub(j$getElementText()[[1]], '', result)
#       }
#       results <- append(results, result)
#     }else{
#       results <- append(results, i$getElementText()[[1]])
#     }
#   }
# }
# df <- data.frame(results)

# v2: Parse as rvest nodes?

src <- remDr$getPageSource()[[1]]
html <- read_html(src)

subject.contents <- html %>% 
  html_nodes(paste("[subjectid='", id, "']", sep="")) #  '[subjectid="48973"]')

results <- vector()

for(i in subject.contents){
  
  # exclude votes etc. - we just want the spoken statements
  if(length(i %>% html_nodes("em")) == 0 && length(i %>% html_nodes("strong")) == 0){
    
    # remove page break indicators
    spans <- i %>% html_nodes("span.page-break")
    xml_remove(spans)
    results <- append(results, i %>% html_text())
  }
}
df2 <- data.frame(results) # same result as with selenium webElements, but cleaner code & quicker
df2 <- df2 %>% rename(text = results)

# close connection to Selenium and stop PhantomJS
rD$close()
pjs$stop()


# Save to MongoDB

db <- mongo(db = "swisscovid_parl", collection = "tweet", url = "mongodb://localhost", verbose = FALSE, options = ssl_options())
# ID, text, datetime
db$insert(df2)

rm(db)
gc()

#-------------------------------------------------------------------


##### Selenium & Firefox

# library(RSelenium)
# library(rvest)
# library(wdman)
# rD <- rsDriver(browser="firefox")
# remDr <- rD[["client"]]
# remDr$navigate(paste("https://www.parlament.ch/de/ratsbetrieb/amtliches-bulletin/amtliches-bulletin-die-verhandlungen?SubjectId=", id, sep=''))
# remDr$screenshot(display = FALSE, useViewer = F, file = "screen.png")
# webElem4 <- remDr$findElements(using = 'css selector', value = paste("[subjectid='", id, "']", sep="")) 
# 
# for(i in webElem4){print(i$getElementText()[[1]])}
# result <- webElem4[[3]]
# result <- result$getElementText()[[1]]
# result
# 
# rD$close()

