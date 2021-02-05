lib <- c("psych", "ggplot2", "ggthemes", "haven", "data.table", "dplyr", "tidyr", "Hmisc", "mada",
         "knitr", "kableExtra", "naniar", "stats", "readxl", "matrixStats", "ISOcodes", "pander",
         "scales", "lubridate", "rnaturalearth", "rnaturalearthdata", "highcharter")

invisible(lapply(lib, library, character.only = TRUE))
lapply(lib, library, character.only = TRUE)
rm(lib)

standardize_long <- function(raw_data, vars, respSetMeans, respSetSDs){
  # extension of standardize function included in the cross-sectional script.
  # Receives the raw data and a list of vars that contains all wave names
  # for a single harmonized variable. RespSetMeans and respSetSDs contains
  # the grand means and standard deviations for each participant.
  # 
  # Indexing in the across context retrieves the correct wave (corresponding
  # to the index of the current column since means/sds and variables match in
  # dimensions) from the grand mean and sd matrix.
  
  vars_h <- paste(vars,"_Harmonized",sep="")
  col_names <- colnames(raw_data)

  standardized_data <- raw_data %>% dplyr::mutate(
    dplyr::across({{vars_h}},
                  ~((.-{{respSetMeans}}[,{{vars_h}} == cur_column()])/
                       {{respSetSDs}}[,{{vars_h}} == cur_column()]),
                   .names= "{col}_S")
  )
  for (var in vars) {
    var_h <- paste(var,"_Harmonized_S",sep = "")
    colnames(standardized_data)[names(standardized_data)==var_h] <- paste(var,"_Standardized",sep = "")
  }
  
  return(standardized_data)
}

country_summarize <- function(df) {
  ### Create country summaries (for time window) ###
  
  countryWeekly <- df %>% 
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
  
  return(countryWeekly)
}

clean_country <- function(df_country, minWeekN) {
  # remove any that measures that have less than minWeekN participants
  
  countryWeeklyRed <- df_country
  
  meanCols <- grep("_mean", names(df_country))
  sdCols <- grep("_sd", names(df_country))
  seCols <- grep("_se", names(df_country))
  lwrCols <- grep("_lwr", names(df_country))
  uprCols <- grep("_upr", names(df_country))
  nCols <- grep("_n", names(df_country))
  
  countryWeeklyRed[meanCols][countryWeeklyRed[nCols] < minWeekN] <- NA
  countryWeeklyRed[sdCols][countryWeeklyRed[nCols] < minWeekN] <- NA
  countryWeeklyRed[seCols][countryWeeklyRed[nCols] < minWeekN] <- NA
  countryWeeklyRed[lwrCols][countryWeeklyRed[nCols] < minWeekN] <- NA
  countryWeeklyRed[uprCols][countryWeeklyRed[nCols] < minWeekN] <- NA
  countryWeeklyRed[nCols][countryWeeklyRed[nCols] < minWeekN] <- NA
  
  # remove all rows that have missingness on all
  # alternative: weeklyOut3 <- weeklyOut[!rowSums(is.na(weeklyOut[nCols])),]
  countryWeeklyRed <- countryWeeklyRed %>%
    #filter(across(nCols, ~ !is.na(.x))) %>% 
    filter_at(vars(nCols),all_vars(!is.na(.))) %>% # should be the same as before
    group_by(coded_country) %>%
    filter(n()>2) %>% # filter all countries that 3 or more measurement weeks
    ungroup()
  
  # load world data for maps and iso codes
  world.data <- ne_countries(scale = "medium", returnclass = "sf")
  world.data$iso_a2[world.data$admin=="Kosovo"] <- "XK"
  
  countryWeeklyRed <- merge(x = countryWeeklyRed, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T) %>% 
    dplyr::select(-geometry)
  countryWeeklyRed$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(countryWeeklyRed$iso_a2))
  
  return(countryWeeklyRed)
}

global_summarize <- function(df){
  ### Create global summary (for time window) ###
  
  globalWeekly <- df %>% 
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
  
  return(globalWeekly)
}

clean_global <- function(df_global, minWeekN) {
  # remove any that measures that have less than minWeekN participants
  
  globalWeeklyRed <- df_global
  
  meanCols <- grep("_mean", names(df_global))
  sdCols <- grep("_sd", names(df_global))
  seCols <- grep("_se", names(df_global))
  lwrCols <- grep("_lwr", names(df_global))
  uprCols <- grep("_upr", names(df_global))
  nCols <- grep("_n", names(df_global))
  
  globalWeeklyRed[meanCols][globalWeeklyRed[nCols] < minWeekN] <- NA
  globalWeeklyRed[sdCols][globalWeeklyRed[nCols] < minWeekN] <- NA
  globalWeeklyRed[seCols][globalWeeklyRed[nCols] < minWeekN] <- NA
  globalWeeklyRed[lwrCols][globalWeeklyRed[nCols] < minWeekN] <- NA
  globalWeeklyRed[uprCols][globalWeeklyRed[nCols] < minWeekN] <- NA
  globalWeeklyRed[nCols][globalWeeklyRed[nCols] < minWeekN] <- NA
  
  
  globalWeeklyRed <- globalWeeklyRed %>%
    filter_at(vars(nCols),all_vars(!is.na(.)))
  
  return(globalWeeklyRed)
}

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
colnames(shiny_prep)[colnames(shiny_prep) %in% paste0(mvars,"_Harmonized")] <- paste0("w0_",colnames(shiny_prep)[colnames(shiny_prep) %in% paste0(mvars,"_Harmonized")])
colnames(shiny_prep)[colnames(shiny_prep) %in% c("StartDate","EndDate")] <- paste0("w0_",c("StartDate","EndDate"))
colnames(shiny_prep)[colnames(shiny_prep) %in% c("respSetMean", "respSetSd")] <- paste0("w0_",c("respSetMean", "respSetSd"))

waves <- c("w0","w1", "w2", "w3", "w4", "w5",
           "w6", "w7", "w8", "w9", "w10",
           "w11","w12","w13","w14", "w15",
           "w16", "w17", "w18")

### prepare set of respMeans and respSds per wave ###

respSetMeanWaves <- shiny_prep[,sapply(waves,
                               function(x,var){return(paste0(x,"_",var))},
                               var="respSetMean")]

respSetSDsWaves <- shiny_prep[,sapply(waves,
                              function(x,var){return(paste0(x,"_",var))},
                              var="respSetSd")]

### create harominzed variables for friends/people online/inPerson ###

for (var in c("isoFriends_inPerson", "isoOthPpl_inPerson",
              "isoFriends_online", "isoOthPpl_online")) {
  
  shiny_prep[,paste0(waves,"_",var,"_Harmonized")] <- shiny_prep[,paste0(waves,"_",var)] %>%
                                                                  mutate_all(as.numeric) %>%
                                                                  mutate_all(~ scales::rescale(., to = c(1,7)))
  
}

### combine waves with varnames ###

# just crude processing here, I need to refine this.

# For non-standardized varriables
longmvarV <- c() # for reduced frame: a vector
longmvar <- list() # a list of vectors for reshape
# For standardized variables
longmvarV_S <- c() # for reduced frame: a vector
longmvar_S <- list() # a list of vectors for reshape

index <- 1 # to keep track of list indexing
#var <- "affAnx"
for(var in mvars) {
  longSingleVar <- sapply(waves,
                          function(x,var){return(paste0(x,"_",var))},
                          var)
  longSingleVar_S <- sapply(longSingleVar,
                            function(x,var){return(paste0(x,var))},
                            var="_Standardized")
  
  longmvarV <- c(longmvarV,longSingleVar)
  longmvarV_S <- c(longmvarV_S,longSingleVar_S)
  
  longmvar[[index]] <- longSingleVar
  longmvar_S[[index]] <- longSingleVar_S
  
  index = index + 1
  
  # There are some variables that have not been measured in every
  # wave.
  # Update: added a .csv file so that again variables can just be added
  # this partially addresses this concern as we can easily include/exclude vars.
  notCollected <- longSingleVar[!longSingleVar %in% names(shiny_prep)]
  
  longSingleVar_Harmonized <- sapply(longSingleVar,
                                     function(x,var){return(paste0(x,var))},
                                     var="_Harmonized")
  notCollected_Harmonized <- longSingleVar_Harmonized[!longSingleVar_Harmonized %in% names(shiny_prep)]
  
  # Unfortunately missing variables != missing harmonized variable
  # Hence two loops are necessary since in some cases there is
  # an existing variable but no harmonized equivalent.
  
  for (missingVar in notCollected) {
    print(paste0("Missing variable: ",missingVar))
    shiny_prep[missingVar] <- NaN
  }
  
  for (missingVar in notCollected_Harmonized) {
    print(paste0("Missing harmonized variable: ",missingVar))
    shiny_prep[missingVar] <- NaN
  }
  
  # Add all standardized waves to frame.
  shiny_prep <- standardize_long(shiny_prep,
                                 longSingleVar,
                                 respSetMeanWaves,
                                 respSetSDsWaves)
  
}

### Adding start and end dates for each wave. ###

startDates <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, "StartDate")
endDates <- sapply(waves, function(x,var){return(paste0(x,"_",var))}, "EndDate")
longmvarV <- c(longmvarV, startDates, endDates)
longmvarV_S <- c(longmvarV_S, startDates, endDates)

longmvar[[index]] <- startDates
longmvar[[index + 1]] <- endDates

longmvar_S[[index]] <- startDates
longmvar_S[[index + 1]] <- endDates

### Create reduced frame ###

reducedF <- shiny_prep[,c(longmvarV,c("ResponseId","coded_country"))]
reducedF_S <- shiny_prep[,c(longmvarV_S,c("ResponseId","coded_country"))]

### Convert to long format ###

# fix factors first
mycols <- paste0(c("lone01", "para01", "consp01",
                   "isoFriends_inPerson", "isoOthPpl_inPerson",
                   "isoFriends_online", "isoOthPpl_online"), 
                 '$', 
                 collapse = '|')

reducedFL <- reducedF %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate_at(vars(matches(mycols)),
            function (x) x-1)

reducedFL_S <- reducedF_S %>%
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

reducedFL_S <- reshape(reducedFL_S, direction = "long",
                      varying = longmvar_S,
                      timevar = "wave",
                      times = waves,
                      v.names=c(mvars,c("StartDate", "EndDate")),
                      idvar = c("ResponseId",
                                "coded_country")
                      ) %>% filter(!is.na(EndDate))


### Summarize on country and global level for standardized and regular ###

# Regular Sample

countryWeekly <- country_summarize(reducedFL)
countryWeeklyRed <- clean_country(countryWeekly, 10)

globalWeekly <- global_summarize(reducedFL)
globalWeeklyRed <- clean_global(globalWeekly, 10)

weekly <- rbind(globalWeeklyRed, countryWeeklyRed)

# Standardized Sample

countryWeekly_S <- country_summarize(reducedFL_S)
countryWeeklyRed_S <- clean_country(countryWeekly_S, 10)

globalWeekly_S <- global_summarize(reducedFL_S)
globalWeeklyRed_S <- clean_global(globalWeekly_S, 10)

weekly_S <- rbind(globalWeeklyRed_S, countryWeeklyRed_S)

### Save for Shiny ###

# Regular Sample

weeklyRegions <- weekly %>% 
  ungroup() %>%
  dplyr::select(coded_country, flag) %>%
  distinct()
weeklyCountries <- weekly %>% 
  ungroup() %>%
  dplyr::select(coded_country, flag) %>%
  distinct() %>%
  filter(coded_country != "global")

# Standardized Sample

weeklyRegions_S <- weekly_S %>% 
  ungroup() %>%
  dplyr::select(coded_country, flag) %>%
  distinct()
weeklyCountries_S <- weekly_S %>% 
  ungroup() %>%
  dplyr::select(coded_country, flag) %>%
  distinct() %>%
  filter(coded_country != "global")

if(isTRUE(all.equal(weeklyRegions,weeklyRegions_S))) {
  cat("OK. Standardized and Unstandardized regions the same")
  } else {
  stop("Standardized and Unstandardized regions are not the same!")
  }
 
if(isTRUE(all.equal(weeklyCountries,weeklyCountries_S))) {
  cat("OK. Standardized and Unstandardized countries the same")
  } else {
  stop("Standardized and Unstandardized countries are not the same!")
  }


# anti_join(weekly, 
#           weekly_S, 
#           by=c("coded_country", "week")) %>% 
#   View(.)

surveyN <- max(colSums(globalWeeklyRed[grep("_n", names(countryWeekly))]),
               na.rm = T)

save(weekly, weeklyRegions, weeklyCountries,
     weekly_S, weeklyRegions_S, weeklyCountries_S,
     mvars, waves, surveyN,
     file = "data/shinyDataLongitudinal.RData")
