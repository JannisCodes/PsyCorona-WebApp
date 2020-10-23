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

### Create country summaries (for time window) ###

countryWeekly <- reducedFL %>% 
  mutate(Dates = lubridate::parse_date_time(EndDate, "%m/%d/%Y %H:%M:%S"),
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
  mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .))) %>%
  filter(sum(c_across(ends_with("_n"))) > length(mvars)) # drop all rows with only one participant (calculation of sd and se not possible)

world.data <- ne_countries(scale = "medium", returnclass = "sf")
world.data$iso_a2[world.data$admin=="Kosovo"] <- "XK"

countryWeekly <- merge(x = countryWeekly, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T) %>% 
  dplyr::select(-geometry)
countryWeekly$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(countryWeekly$iso_a2))

globalWeekly <- reducedFL %>% 
  mutate(coded_country = "global",
         Dates = lubridate::parse_date_time(EndDate, "%m/%d/%Y %H:%M:%S"),
         week = isoweek(Dates), 
         # some weird inconsistencies in the function the added options ensure that Sunday is the last day of the week
         weekLab = paste0(floor_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 1)), " - ", 
                          ceiling_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 7), change_on_boundary = FALSE)),
         weekDate = floor_date(as_date(Dates), unit="week", week_start = getOption("lubridate.week.start", 1))) %>%
  #arrange(Dates) %>%
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
  mutate_if(is.numeric, funs(ifelse(is.nan(.), NA, .))) %>%
  mutate(iso_a2 = NA,
         flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")

weekly <- rbind(globalWeekly, countryWeekly)

# input <- list(long_ctrs_country_selection = c("United States of America", "Germany", "Netherlands"), 
#               long_ctrs_var = c("affBor"),
#               ci = TRUE)
# 
# weeklySelection <- weekly %>% 
#   ungroup() %>% 
#   filter(coded_country %in% input$long_ctrs_country_selection) %>% 
#   dplyr::select(coded_country, weekDate, value = one_of(paste0(input$long_ctrs_var, "_mean"))) %>%
#   group_by(coded_country) %>%
#   do(ds = list(
#     data = highcharter::list_parse2(data.frame(datetime_to_timestamp(.$weekDate), .$value))
#   )) %>% 
#   {purrr::map2(.$coded_country, .$ds, function(x, y){
#     append(list(name = x), y)
#   })}
# 
# weeklySelectionCI <- weekly %>% 
#   ungroup() %>% 
#   filter(coded_country %in% input$long_ctrs_country_selection) %>% 
#   dplyr::select(coded_country, weekDate, lwr = one_of(paste0(input$long_ctrs_var, "_lwr")), upr = one_of(paste0(input$long_ctrs_var, "_upr"))) %>%
#   group_by(coded_country) %>%
#   do(ds = list(data = highcharter::list_parse2(data.frame(datetime_to_timestamp(.$weekDate), .$lwr, .$upr)),
#                type = 'arearange',
#                fillOpacity = 0.3,
#                lineWidth = 0,
#                name = "95% Confidence Interval",
#                zIndex = 0
#                )) %>% 
#   {purrr::map2(.$coded_country, .$ds, function(x, y){
#     append(list(name = x), y)
#   })}


### Follow-up Histogram ###

hist(as.POSIXct(strptime(reducedFL$EndDate, "%m/%d/%Y %H:%M:%S")),
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

save(weekly, weeklyRegions, weeklyCountries, mvars, waves, file = "data/longitudinal.RData")
