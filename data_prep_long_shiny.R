lib <- c("psych", "ggplot2", "ggthemes", "haven", "data.table", "dplyr", "tidyr", "Hmisc", "mada",
         "knitr", "kableExtra", "naniar", "stats", "readxl", "matrixStats", "ISOcodes", "pander", "scales", "lubridate", "rnaturalearth", "rnaturalearthdata")

invisible(lapply(lib, library, character.only = TRUE))
lapply(lib, library, character.only = TRUE)
rm(lib)
#load("data/shinyDataAggregated.RData")

world.data <- ne_countries(scale = "medium", returnclass = "sf")
world.data$iso_a2[world.data$admin=="Kosovo"] <- "XK"
raw.data <- read.csv("data/rawdatalabels.csv", na.strings = " ") # raw data provided
#raw.data$coded_country[raw.data$coded_country == " "] <- NA # set empty cells in coded_country column to NA
raw.data <- raw.data %>% drop_na(coded_country) # drop participants with missing counties
unique(raw.data$coded_country)[!unique(raw.data$coded_country) %in% world.data$admin] # check whether all country names are spelled correctly

### Variables to consider ###
vars <- read.csv("data/vars.csv")
mvars <- as.character(vars$mvars)
rm(vars)

### re-code EDU levels ###
levels(raw.data$edu) <- gsub("General s", "S", levels(raw.data$edu))

# all ISO-2 country code to dataframe and flags
shiny_prep <- merge(x = raw.data, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T)
shiny_prep$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(shiny_prep$iso_a2))

# Waves definition.

waves <- c("w1", "w2", "w3", "w4", "w5",
           "w6", "w7", "w8", "w9", "w10",
           "w11")

# Example for simplicity (using just AffAnx waves):

longmvar1 <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, mvars[1])



# Reshape seems to be computationally very expensive and I found
# that first reducing the frame gives me the fastest results.
reducedF <- shiny_prep[,c(longmvar1,c("ResponseId","coded_country"))]
reducedF <- reducedF %>% filter(!is.na(coded_country))

reducedFL<-reshape(reducedF, 
                   direction = "long",
                   varying = longmvar1,
                   timevar = "wave",
                   times = waves,
                   v.names=c("affAnx"),
                   idvar = c("ResponseId", "coded_country")
                    )

head(reducedFL)

# Subject check (This works)

reducedF[reducedF$ResponseId == "0068dc3e",longmvar1]
reducedFL$affAnx[reducedFL$ResponseId == "0068dc3e"]

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
  # wave (?) Currently I just create NA filled columns for those
  # since reshape does not really like anything else.
  
  # So basically the question is, do we need all of the variables
  # named in vars? Some are sum-scores that we created in the
  # script and I do not think that for those there are corresponding
  # longitudinal values.
  notCollected <- longSingleVar[!longSingleVar %in% names(shiny_prep)]

  for (missingVar in notCollected) {
    shiny_prep[missingVar] <- NaN
  }
  
}

# Adding start and end dates for each wave.

startDates <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, "StartDate")
endDates <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, "EndDate")
longmvarV <- c(longmvarV, startDates, endDates)

longmvar[[index]] <- startDates
longmvar[[index + 1]] <- endDates


reducedF <- shiny_prep[,c(longmvarV,c("ResponseId","coded_country"))]
reducedF <- reducedF %>% filter(!is.na(coded_country))

reducedFL<-reshape(reducedF, direction = "long",
                    varying = longmvar,
                    timevar = "wave",
                    times = waves,
                    v.names=c(mvars,c("StartDate", "EndDate")),
                    idvar = c("ResponseId",
                              "coded_country")
                    )

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

reducedFL[is.nan(reducedFL)] <- NA

reducedFLRed <- reducedFL[rowSums(!is.na(reducedFL[ , 4:33])) != 0,]

reducedFLRed2 <- reducedFL %>% filter(!is.na(EndDate))

hist(as.POSIXct(strptime(reducedFLRed$EndDate, "%m/%d/%Y %H:%M:%S")), "days", las=2, freq=T, format = "%d %b", xlab=NULL,main="Participation Follow-up")

# Just some example stats, are those numbers what you would expect?

length(na.omit(reducedFL$affAnx[reducedFL$wave == "w1"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w2"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w3"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w4"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w5"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w6"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w7"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w8"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w9"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w10"]))
length(na.omit(reducedFL$affAnx[reducedFL$wave == "w11"]))

# Some subject (still seems to work):

reducedF[reducedF$ResponseId == "0068dc3e",longmvar1]
reducedFL$affAnx[reducedFL$ResponseId == "0068dc3e"]
