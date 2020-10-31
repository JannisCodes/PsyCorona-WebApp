lib <- c("psych", "ggplot2", "ggthemes", "haven", "data.table", "dplyr", "tidyr", "Hmisc", "mada",
         "knitr", "kableExtra", "naniar", "stats", "readxl", "matrixStats", "ISOcodes", "pander",
         "scales", "lubridate", "rnaturalearth", "rnaturalearthdata", "highcharter")

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
           "w11","w12","w13","w14", "w15")

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

# fix factors first
mycols <- paste0(c("lone01", "para01", "consp01", "isoFriends_inPerson", "isoOthPpl_inPerson", "isoFriends_online", "isoOthPpl_online"), 
                 '$', 
                 collapse = '|')
reducedFL <- reducedF %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate_at(vars(matches(mycols)),
            function (x) x-1)

reducedFL <- reshape(reducedFL, direction = "long",
                    varying = longmvar,
                    timevar = "wave",
                    times = waves,
                    v.names=c(mvars,c("StartDate", "EndDate")),
                    idvar = c("ResponseId",
                              "coded_country")
                    ) %>% filter(!is.na(EndDate))

### Create country summaries (for time window) ###

countryWeekly <- reducedFL %>% 
  mutate(Dates = lubridate::parse_date_time(EndDate, "%Y/%m/%d %H:%M:%S"),
         week = isoweek(Dates), 
         # some weird inconsistencies in the function the added options ensure that Sunday is the last day of the week
         weekLab = paste0(floor_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 1)), " - ", 
                          ceiling_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 7), change_on_boundary = FALSE)),
         weekDate = floor_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 1))) %>%
  arrange(Dates) %>%
  group_by(coded_country, week, weekLab, weekDate) %>%
  summarise_at(vars(mvars), list(~ mean(., na.rm = T), 
                                 ~ sd(., na.rm = T), 
                                 n = ~ sum(!is.na(.)), 
                                 se = ~ sd(.,na.rm=TRUE)/sqrt(sum(!is.na(.))),
                                 lwr = ~ mean(., na.rm = T) - 1.96*sd(.,na.rm=TRUE)/sqrt(sum(!is.na(.))),
                                 upr = ~ mean(., na.rm = T) + 1.96*sd(.,na.rm=TRUE)/sqrt(sum(!is.na(.)))
                                 )
               ) %>%
  arrange(coded_country, week) %>%
  mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .)))

# countryWeekly %>%
#   filter(coded_country == "United States of America") %>%
#   dplyr::select(weekDate, ends_with("_n")) %>%
#   View(.)

# remove any that measures that have less than 20 participants
#  there is probably a better way of doing this but quick and dirty here we go.
#  better solution probably for-loop or spread and gather stuff: https://stackoverflow.com/questions/48600340/change-column-value-to-na-based-on-other-column-condition
countryWeeklyRed <- countryWeekly

meanCols <- grep("_mean", names(countryWeekly))
sdCols <- grep("_sd", names(countryWeekly))
seCols <- grep("_se", names(countryWeekly))
lwrCols <- grep("_lwr", names(countryWeekly))
uprCols <- grep("_upr", names(countryWeekly))
nCols <- grep("_n", names(countryWeekly))

countryWeeklyRed[meanCols][countryWeeklyRed[nCols] <= 9] <- NA
countryWeeklyRed[sdCols][countryWeeklyRed[nCols] <= 9] <- NA
countryWeeklyRed[seCols][countryWeeklyRed[nCols] <= 9] <- NA
countryWeeklyRed[lwrCols][countryWeeklyRed[nCols] <= 9] <- NA
countryWeeklyRed[uprCols][countryWeeklyRed[nCols] <= 9] <- NA
countryWeeklyRed[nCols][countryWeeklyRed[nCols] <= 9] <- NA

# remove all rows that have missingness on all
# alternative: weeklyOut3 <- weeklyOut[!rowSums(is.na(weeklyOut[nCols])),]
countryWeeklyRed <- countryWeeklyRed %>%
  #filter_at(vars(nCols),all_vars(!is.na(.))) %>%
  group_by(coded_country) %>%
  filter(n()>2) %>% # filter all countries that 3 or more measurement weeks
  ungroup()

# load world data for maps and iso codes
world.data <- ne_countries(scale = "medium", returnclass = "sf")
world.data$iso_a2[world.data$admin=="Kosovo"] <- "XK"

countryWeeklyRed <- merge(x = countryWeeklyRed, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T) %>% 
  dplyr::select(-geometry)
countryWeeklyRed$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(countryWeeklyRed$iso_a2))

globalWeekly <- reducedFL %>% 
  mutate(Dates = lubridate::parse_date_time(EndDate, "%Y/%m/%d %H:%M:%S"),
         week = isoweek(Dates), 
         # some weird inconsistencies in the function the added options ensure that Sunday is the last day of the week
         weekLab = paste0(floor_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 1)), " - ", 
                          ceiling_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 7), change_on_boundary = FALSE)),
         weekDate = floor_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 1))) %>%
  #arrange(Dates) %>%
  #filter(paste(.$coded_country, .$week) %in% paste(countryWeeklyRed$coded_country, countryWeeklyRed$week)) %>% # only include country and week if also in country Weekly Red
  mutate(coded_country = "global") %>%
  group_by(coded_country, week, weekLab, weekDate) %>%
  summarise_at(vars(mvars), list(~ mean(., na.rm = T), 
                                 ~ sd(., na.rm = T), 
                                 n = ~ sum(!is.na(.)), 
                                 se = ~ sd(.,na.rm=TRUE)/sqrt(sum(!is.na(.))),
                                 lwr = ~ mean(., na.rm = T) - 1.96*sd(.,na.rm=TRUE)/sqrt(sum(!is.na(.))),
                                 upr = ~ mean(., na.rm = T) + 1.96*sd(.,na.rm=TRUE)/sqrt(sum(!is.na(.)))
                                 )
               ) %>%
  arrange(coded_country, week) %>%
  #mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .))) %>%
  mutate(iso_a2 = NA,
         flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")

globalWeeklyRed <- globalWeekly

meanCols <- grep("_mean", names(globalWeekly))
sdCols <- grep("_sd", names(globalWeekly))
seCols <- grep("_se", names(globalWeekly))
lwrCols <- grep("_lwr", names(globalWeekly))
uprCols <- grep("_upr", names(globalWeekly))
nCols <- grep("_n", names(globalWeekly))

globalWeeklyRed[meanCols][globalWeeklyRed[nCols] <= 9] <- NA
globalWeeklyRed[sdCols][globalWeeklyRed[nCols] <= 9] <- NA
globalWeeklyRed[seCols][globalWeeklyRed[nCols] <= 9] <- NA
globalWeeklyRed[lwrCols][globalWeeklyRed[nCols] <= 9] <- NA
globalWeeklyRed[uprCols][globalWeeklyRed[nCols] <= 9] <- NA
globalWeeklyRed[nCols][globalWeeklyRed[nCols] <= 9] <- NA

globalWeeklyRed <- globalWeeklyRed %>%
  filter_at(vars(nCols),all_vars(!is.na(.)))

weekly <- rbind(globalWeeklyRed, countryWeeklyRed)

### Follow-up Histogram ###

hist(ymd_hms(reducedFL$EndDate),
     "days", las=2, freq=T, format = "%d %b", xlab=NULL,
     main="Participation over time")

ggplot(countryWeekly, aes(x=week, y=affBor_n)) +
  geom_line() +
  geom_point() +
  facet_wrap(~coded_country, scales = "free_y") +
  ggtitle("Participation Affect Boredom over Time per Country (individual y axes)")

ggplot(countryWeekly, aes(x=week, y=affBor_n)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(trans='log2') +
  facet_wrap(~coded_country) + #, scales = "free_y"
  ggtitle("Participation Affect Boredom over Time per Country")

### Save for Shiny ###

weeklyRegions <- weekly %>% 
  ungroup() %>%
  dplyr::select(coded_country, flag) %>%
  distinct()
weeklyCountries <- weekly %>% 
  ungroup() %>%
  dplyr::select(coded_country, flag) %>%
  distinct() %>%
  filter(coded_country != "global")

surveyN <- max(colSums(globalWeeklyRed[nCols]), na.rm = T)

save(weekly, weeklyRegions, weeklyCountries, mvars, waves, surveyN, file = "data/shinyDataLongitudinal.RData")
