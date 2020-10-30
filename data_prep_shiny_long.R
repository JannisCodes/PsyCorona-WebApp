lib <- c("psych", "ggplot2", "ggthemes", "haven", "data.table", "dplyr", "tidyr", "Hmisc", "mada",
         "knitr", "kableExtra", "naniar", "stats", "readxl", "matrixStats", "ISOcodes", "pander",
         "scales", "lubridate", "rnaturalearth", "rnaturalearthdata")

invisible(lapply(lib, library, character.only = TRUE))
lapply(lib, library, character.only = TRUE)
rm(lib)

### Load in data ###
load("data/shinyDataShinyPrep.RData")
vars <- read.csv("data/vars_long.csv")
mvars <- as.character(vars$mvars)

# prevent NAs from thworing duplicate error for rownames...
shiny_prep <- subset(shiny_prep,!is.na(shiny_prep$ResponseId))

### Waves definition. ###

# Fix decoding error for startdate

colnames(shiny_prep)[colnames(shiny_prep) =="ï..StartDate"] <- "StartDate"

# Change names for w0

colnames(shiny_prep)[colnames(shiny_prep) %in% mvars] <- paste0("w0_",mvars)
colnames(shiny_prep)[colnames(shiny_prep) %in% c("StartDate","EndDate")] <- paste0("w0_",c("StartDate","EndDate"))

waves <- c("w0","w1", "w2", "w3", "w4", "w5",
           "w6", "w7", "w8", "w9", "w10",
           "w11")

### combine waves with varnames ###

# just crude processing here, I need to refine this.

longmvarV <- c() # for reduced frame: a vector
longmvar <- list() # a list of vectors for reshape
index <- 1 # to keep track of list indexing
for(var in mvars) {
  longSingleVar <- sapply(waves,
                          function(x,var){return(paste0(x,"_",var))},
                          var)
  longmvarV <- c(longmvarV,longSingleVar)
  longmvar[[index]] <- longSingleVar
  index = index + 1
  
  # There are some variables that have not been measured in every
  # wave.
  # Update: added a .csv file so that again variables can just be added
  # this partially addresses this concern as we can easily include/exclude vars.
  notCollected <- longSingleVar[!longSingleVar %in% names(shiny_prep)]

  for (missingVar in notCollected) {
    print(missingVar)
    shiny_prep[missingVar] <- NaN
  }
  
}

### Adding start and end dates for each wave. ###

startDates <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, "StartDate")
endDates <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, "EndDate")
longmvarV <- c(longmvarV, startDates, endDates)

longmvar[[index]] <- startDates
longmvar[[index + 1]] <- endDates

### Create reduced frame ###

reducedF <- shiny_prep[,c(longmvarV,c("ResponseId","coded_country"))]

### Convert to long format ###

reducedFL<-reshape(reducedF, direction = "long",
                    varying = longmvar,
                    timevar = "wave",
                    times = waves,
                    v.names=c(mvars,c("StartDate", "EndDate")),
                    idvar = c("ResponseId",
                              "coded_country")
                    ) %>% filter(!is.na(EndDate))

### Follow-up Histogram ###

# ### Follow-up Checks: ###
# 
# # Wide frame all data on lone for wave 9 in the US
# length(reducedF$w9_lone01[!is.na(reducedF$w9_lone01) &
#                             reducedF$coded_country == "United States of America"])
# 
# # subset long frame
# US <- reducedFL[reducedFL$coded_country =="United States of America",]
# 
# # all data on US in long frame (individual values and length should match)
# length(US$lone01[US$wave =="w9"])
# 
# # Convert dates to allow for comparisons.
# US$StartDate <- lubridate::parse_date_time(US$StartDate,"%m/%d/%Y %H:%M:%S")
# 
# 
# max(US$StartDate)
# 
# length(US$lone01[US$wave == "w0"])
# length(US$lone01[US$wave == "w2"])
# length(US$lone01[US$wave == "w3"])
# length(US$lone01[US$wave == "w4"])
# length(US$lone01[US$wave == "w5"])
# length(US$lone01[US$wave == "w6"])
# length(US$lone01[US$wave == "w7"])
# length(US$lone01[US$wave == "w8"])
# length(US$lone01[US$wave == "w9"])
# length(US$lone01[US$wave == "w10"])
# length(US$lone01[US$wave == "w11"])
# 
# length(US$lone01[US$StartDate > lubridate::parse_date_time("6/15/2020 0:0:1","%m/%d/%Y %H:%M:%S") &
#                    US$StartDate < lubridate::parse_date_time("7/8/2020 0:0:1","%m/%d/%Y %H:%M:%S")])
# 
# length(US$lone01[US$StartDate > lubridate::parse_date_time("6/15/2020 0:0:1","%m/%d/%Y %H:%M:%S") &
#                    US$StartDate < lubridate::parse_date_time("6/22/2020 0:0:1","%m/%d/%Y %H:%M:%S")])

hist(as.POSIXct(strptime(reducedFL$EndDate, "%m/%d/%Y %H:%M:%S")),
     "days", las=2, freq=T, format = "%d %b", xlab=NULL,
     main="Participation Follow-up")
