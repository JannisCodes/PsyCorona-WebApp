## **Data for Shiny App**
# Dataframe for Shiny App


#renv::init() # new project 
#renv::snapshot() # save changes
#renv::restore() # load latest library (make sure packages are reproducible)
# require(pacman)
# 
# pacman::p_load(psych, ggplot2, ggthemes, haven, data.table, dplyr, tidyr, Hmisc, mada,
#                knitr, kableExtra, naniar, stats, readxl, matrixStats, ISOcodes, pander,
#                scales, haven, lubridate, naniar, stats, rnaturalearth, rnaturalearthdata)
#
lib <- c("psych", "ggplot2", "ggthemes", "haven", "data.table", "dplyr", "tidyr", "Hmisc", "mada",
         "knitr", "kableExtra", "naniar", "stats", "readxl", "matrixStats", "ISOcodes", "pander",
         "scales", "lubridate", "rnaturalearth", "rnaturalearthdata")

invisible(lapply(lib, library, character.only = TRUE))
lapply(lib, library, character.only = TRUE)
rm(lib)

filter_cntry <- function(filter_vector, mean_vars, sd_vars=c(), custom_names=c(),...) {
  ### Initial summary based on user-input vector ###
  
  data <- filter_vector %>% mutate_if(is.factor, as.numeric) %>%
    dplyr::summarize(
      n = n(),
      dplyr::across( {{ mean_vars }}, list(~mean(.,na.rm=TRUE),~sd(.,na.rm=TRUE),~sd(.,na.rm=TRUE)/sqrt(n)), .names= "{col}.{fn}"),
      ...
    )
  
  if (length(sd_vars)){
    ### Drop SD and SE columns for the only mean variables and rename all variables ###
    for (var in mean_vars) {
      m1var <- paste(var,".1",sep = "")
      svar <- paste(var,".2",sep = "")
      evar <- paste(var,".3",sep = "")
      
      colnames(data)[names(data)==m1var] <- var
      
      if(!var %in% sd_vars){
        to_be_dropped <- c(svar,evar)
        data <- data[, !names(data) %in% to_be_dropped]
      } else {
        colnames(data)[names(data)==svar] <- paste(var,".sd",sep = "")
        colnames(data)[names(data)==evar] <- paste(var,".se",sep = "")
      }
    }
  }
  
  if (length(custom_names)) {
    ### Replace Names with custom Names ###
    for (name_index in 1:length(custom_names[,2])) {
      colnames(data)[names(data)==custom_names[name_index,1]] <- custom_names[name_index,2]
      if(custom_names[name_index,1] %in% sd_vars){
        svar <- paste(custom_names[name_index,1],".sd",sep = "")
        evar <- paste(custom_names[name_index,1],".se",sep = "")
        colnames(data)[names(data)==svar] <- paste(custom_names[name_index,2],".sd",sep = "")
        colnames(data)[names(data)==evar] <- paste(custom_names[name_index,2],".se",sep = "")
      }
    }
  }
  
  return(data)
  
}

categorical_filter <- function(filter,filter_glob,keyword){
  c <- filter %>%
    tally() %>%
    tidyr::spread(keyword, n)
  c.glob <- filter_glob  %>%
    tally() %>%
    tidyr::spread(keyword, n)
  return(rbind(c.glob, c))
}

standardize <- function(raw_data, vars){
  # uses mvars instead of _h for renaming handled below. Uses mutate to add
  # new variables rather than overwriting the ones that are manipulated.
  # We could use transmute to overwrite the harmonized variables if we do not
  # need them in the final data frame to save some memory.
  
  # no as.numeric necessary here since the harmonized variables are already
  # guaranteed to be numeric. This is good since mutating from factor to
  # numeric would also affect parts of the raw.data we do not want to change.
  
  vars_h <- paste(vars,"_Harmonized",sep="")
  standardized_data <- raw_data %>% dplyr::mutate(
    dplyr::across( {{ vars_h }}, ~((.-respSetMean)/respSetSd), .names= "{col}_S")
  )
  
  for (var in vars) {
    var_h <- paste(var,"_Harmonized_S",sep = "")
    colnames(standardized_data)[names(standardized_data)==var_h] <- paste(var,"_Standardized",sep = "")
  }

  return(standardized_data)
}

data_prep <- function(){
  print("Preparing Data")
  world.data <- ne_countries(scale = "medium", returnclass = "sf")
  world.data$iso_a2[world.data$admin=="Kosovo"] <- "XK"
  raw.data <- read.csv("data/rawdatalabels.csv", na.strings = " ") # raw data provided
  #raw.data$coded_country[raw.data$coded_country == " "] <- NA # set empty cells in coded_country column to NA
  raw.data <- raw.data %>% drop_na(coded_country) # drop participants with missing counties
  #raw.data <- raw.data[raw.data$EndDate < "5/1/2020",] # drop responses after April
  unique(raw.data$coded_country)[!unique(raw.data$coded_country) %in% world.data$admin] # check whether all country names are spelled correctly
  
  #table(raw.data$consp01)
  #Re-order factor levels and make numeric variables with the right range
  raw.data <- raw.data %>%
    mutate_at(.vars = vars(matches("^(?=.*c19perBeh)(?!.*harmonized)|^(?=.*c19Hope)(?!.*harmonized)|^(?=.*c19Eff)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("Strongly disagree", "Somewhat disagree", "Disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*c19perBeh)(?!.*harmonized|.*label)|^(?=.*c19Hope)(?!.*harmonized|.*label)|^(?=.*c19Eff)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("Strongly disagree", "Somewhat disagree", "Disagree", "Neither agree nor disagree", "Somewhat agree", "Agree", "Strongly agree")))-4)) %>%
    
    mutate_at(.vars = vars(matches("^(?=.*aff)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("Very slightly or not at all", "A little", "Moderately", "Quite a bit", "Extremely")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*aff)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("Very slightly or not at all", "A little", "Moderately", "Quite a bit", "Extremely"))))) %>%
    
    mutate_at(.vars = vars(matches("^(?=.*lone)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("Never", "Rarely", "Sometimes", "Often", "All the time")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*lone)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"))))) %>%
    
    mutate_at(.vars = vars(matches("^(?=.*para)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("Not at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very much")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*para)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("Not at all", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Very much")))-1)) %>%
    
    mutate_at(.vars = vars(matches("^(?=.*consp)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("Certainly not 0%", "10%", "20%", "30%", "40%", "Undecided 50%", "60%", "70%", "80%", "90%", "Certainly 100%")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*consp)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("Certainly not 0%", "10%", "20%", "30%", "40%", "Undecided 50%", "60%", "70%", "80%", "90%", "Certainly 100%")))-1)) %>%

    mutate_at(.vars = vars(matches("^(?=.*extC19Msg)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("1: Messages are completely unclear/ ambiguous", "2", "3", "4", "5", "6: Messages are very clear/ unambiguous")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*extC19Msg)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("1: Messages are completely unclear/ ambiguous", "2", "3", "4", "5", "6: Messages are very clear/ unambiguous"))))) %>%
    
    mutate_at(.vars = vars(matches("^(?=.*c19IsStrict)(?!.*harmonized)|^(?=.*c19IsPunish)(?!.*harmonized)|^(?=.*c19IsOrg)(?!.*harmonized)", perl = T)),
              .funs = list(label = ~factor(., levels = c("1: not at all", "2", "3", "4", "5", "6: Very much")))) %>%
    mutate_at(.vars = vars(matches("^(?=.*c19IsStrict)(?!.*harmonized|.*label)|^(?=.*c19IsPunish)(?!.*harmonized|.*label)|^(?=.*c19IsOrg)(?!.*harmonized|.*label)", perl = T)),
              .funs = list(~as.numeric(factor(., levels = c("1: not at all", "2", "3", "4", "5", "6: Very much")))))
  
  ### Variables to consider ###
  vars <- read.csv("data/vars.csv")
  mvars <- as.character(vars$mvars)
  mvars_s <- paste(mvars,"_Standardized",sep="")
  sdvars <- as.character(na.omit(vars$svars))
  sdvars_s <- paste(sdvars,"_Standardized",sep="")
  custom_names <- na.omit(data.frame(vars$mvars,vars$custom_names) %>% mutate_all(as.character))
  custom_names_s <- custom_names
  custom_names_s$vars.mvars <- paste(custom_names_s$vars.mvars,"_Standardized",sep="")
  custom_names_s$vars.custom_names <- paste(custom_names_s$vars.custom_names,"_Standardized",sep="")
  rm(vars)
  
  ### Calculate compound scores ###
  raw.data$affHighPos.m <- scoreItems(keys=c(1,1,1), items = raw.data  %>% dplyr::select(affEnerg, affExc, affInsp), min = 1, max = 5)$scores
  raw.data$affHighNeg.m <- scoreItems(keys=c(1,1), items = raw.data %>% dplyr::select(affAnx, affNerv), min = 1, max = 5)$scores
  raw.data$affLowPos.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(affCalm, affContent, affRel), min = 1, max = 5)$scores
  raw.data$affLowNeg.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(affBor, affExh, affDepr), min = 1, max = 5)$scores
  raw.data$lone.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(matches("^lone[0][[:digit:]]$")), min = 1, max = 5)$scores # changed selection to use RE
  raw.data$isoPers.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(ends_with("inPerson"), -starts_with("w")), min = 0, max = 7)$scores
  raw.data$isoOnl.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(ends_with("online"), -starts_with("w")), min = 0, max = 7)$scores
  raw.data$para.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(matches("^para[0][[:digit:]]$")), min = 0, max = 10)$scores # changed selection to use RE
  raw.data$consp.m <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(matches("^consp[0][[:digit:]]$")), min = 0, max = 10)$scores # changed selection to use RE
  
  ### Calculate compound scores for Harmonized ###
  raw.data$affHighPos.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(affEnerg_Harmonized, affExc_Harmonized, affInsp_Harmonized), min = 1, max = 5)$scores
  raw.data$affHighNeg.m_Harmonized <- scoreItems(keys=c(1,1), items = raw.data %>% dplyr::select(affAnx_Harmonized, affNerv_Harmonized), min = 1, max = 5)$scores[1]
  raw.data$affLowPos.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(affCalm_Harmonized, affContent_Harmonized, affRel_Harmonized), min = 1, max = 5)$scores
  raw.data$affLowNeg.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(affBor_Harmonized, affExh_Harmonized, affDepr_Harmonized), min = 1, max = 5)$scores
  raw.data$lone.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(matches("^lone[0][[:digit:]]_Harmonized$")), min = 1, max = 5)$scores # changed selection to use RE
  raw.data$isoPers.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(ends_with("inPerson"), -starts_with("w")), min = 0, max = 7)$scores # No harmonized one
  raw.data$isoOnl.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(ends_with("online"), -starts_with("w")), min = 0, max = 7)$scores # No harmonized one
  raw.data$para.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(matches("^para[0][[:digit:]]_Harmonized$")), min = 0, max = 10)$scores # changed selection to use RE
  raw.data$consp.m_Harmonized <- scoreItems(keys=c(1,1,1), items = raw.data %>% dplyr::select(matches("^consp[0][[:digit:]]_Harmonized$")), min = 0, max = 10)$scores # changed selection to use RE
  
  
  ### Add standardized variables ###
  raw.data <- standardize(raw.data,mvars)
  raw.data <- raw.data %>% dplyr::na_if("NaN")
  for (j in 1:ncol(raw.data)) set(raw.data, which(is.infinite(raw.data[[j]])), j, NA)
  
  ### re-code EDU levels ###
  levels(raw.data$edu) <- gsub("General s", "S", levels(raw.data$edu))
  
  # all ISO-2 country code to dataframe and flags
  shiny_prep <- merge(x = raw.data, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T)
  shiny_prep$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(shiny_prep$iso_a2))
  
  ### export shiny frame for longitudinal setup ###
  
  save(shiny_prep, 
       file = "data/shinyDataShinyPrep.RData")
  
  # exclude participants only for remaining cross-sectional analysis
  shiny_prep <- shiny_prep[shiny_prep$EndDate < "5/1/2020",]
  
  ### Creating scales for entire Sample and standardized entire sample ###
  ctry.scales <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country)) %>% dplyr::group_by(coded_country),mvars,sdvars,custom_names)
  global.scales <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country)),mvars,sdvars,custom_names,coded_country = "global",iso_a2 = NA,
                                flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")
  
  ctry.scales_s <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country)) %>% dplyr::group_by(coded_country),mvars_s,sdvars_s,custom_names_s)
  global.scales_s <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country)),mvars_s,sdvars_s,custom_names_s,coded_country = "global",iso_a2 = NA,
                                flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")
  
  ### Creating scales for representative Sample and standardized represenatative sample ###
  ctry.scales.representative <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country) & representative == "Yes") %>% dplyr::group_by(coded_country),mvars,sdvars,custom_names)
  global.scales.representative <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country) & representative == "Yes"),mvars,sdvars,custom_names,coded_country = "global",iso_a2 = NA,
                                               flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")
  
  ctry.scales.representative_s <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country) & representative == "Yes") %>% dplyr::group_by(coded_country),mvars_s,sdvars_s,custom_names_s)
  global.scales.representative_s <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country) & representative == "Yes"),mvars_s,sdvars_s,custom_names_s,coded_country = "global",iso_a2 = NA,
                                               flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")
  
  ctry.scales <- merge(x = ctry.scales, y = unique(shiny_prep %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO
  ctry.scales_s <- merge(x = ctry.scales_s, y = unique(shiny_prep %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO
  
  ctry.scales.representative <- merge(x = ctry.scales.representative, y = unique(shiny_prep %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO
  ctry.scales.representative_s <- merge(x = ctry.scales.representative_s, y = unique(shiny_prep %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO
  
  ### Merger for entire sample with global variable and again for standardized entire sample ###
  ctry.scales <- rbind(global.scales, ctry.scales); rm(global.scales)
  ctry.scales_s <- rbind(global.scales_s, ctry.scales_s); rm(global.scales_s)
  
  ### Merger for representative sample with global (representative) variable and again for standardized representative sample ###
  ctry.scales.representative <- rbind(global.scales.representative, ctry.scales.representative); rm(global.scales.representative)
  ctry.scales.representative_s <- rbind(global.scales.representative_s, ctry.scales.representative_s); rm(global.scales.representative_s)
  
  
  scramble20 <- function(x) {ifelse(x<20, abs(x+sample(-2:2, 1, replace = T)), x)}
  scramble50 <- function(x) {ifelse(x<50, abs(x+sample(-5:5, 1, replace = T)), x)}
  
  ### Categorical filters for entire sample and representative sample ###
  
  ## Languages ##
  
  ### fill empty Dutch Language (potentially from )
  shiny_prep$language[shiny_prep$coded_country=="Netherlands" & shiny_prep$language==""] <- "Dutch"
  
  languages <- categorical_filter(shiny_prep %>% dplyr::select(coded_country, language) %>%
                                  group_by(language, coded_country),
                                  shiny_prep %>% dplyr::select(language) %>% mutate(coded_country = "global") %>%
                                  group_by(language, coded_country),
                                  "language")
  
  names(languages)[names(languages) != "coded_country"] = paste0("languages_", names(languages)[names(languages) != "coded_country"])
  languages[,-1] <- lapply(languages[,-1], scramble20)
  
  languages.rep <- categorical_filter(shiny_prep %>% filter(representative == "Yes") %>% dplyr::select(coded_country, language) %>%
                                      group_by(language, coded_country),
                                      shiny_prep %>% filter(representative == "Yes") %>% dplyr::select(language) %>% mutate(coded_country = "global") %>%
                                      group_by(language, coded_country),
                                      "language")
  
  names(languages.rep)[names(languages.rep) != "coded_country"] = paste0("languages_", names(languages.rep)[names(languages.rep) != "coded_country"])
  languages.rep[,-1] <- lapply(languages.rep[,-1], scramble20)
  
  ## Gender ##
  gender <- categorical_filter(data.frame(coded_country = shiny_prep$coded_country, 
                               gender = as_factor(shiny_prep$gender)) %>%
                               group_by(gender, coded_country),
                               data.frame(coded_country = "global", 
                               gender = as_factor(shiny_prep$gender)) %>%
                               group_by(gender, coded_country),
                               "gender")
  
  names(gender)[names(gender) != "coded_country"] = paste0("gender_", names(gender)[names(gender) != "coded_country"])
  gender[,-1] <- lapply(gender[,-1], scramble20)
  
  
  gender.rep <- categorical_filter(data.frame(coded_country = (shiny_prep %>% filter(representative == "Yes"))$coded_country, 
                                   gender = as_factor((shiny_prep %>% filter(representative == "Yes"))$gender)) %>%
                                   group_by(gender, coded_country),
                                   data.frame(coded_country = "global", 
                                   gender = as_factor((shiny_prep %>% filter(representative == "Yes"))$gender)) %>%
                                   group_by(gender, coded_country),
                                   "gender")
  
  names(gender.rep)[names(gender.rep) != "coded_country"] = paste0("gender_", names(gender.rep)[names(gender.rep) != "coded_country"])
  gender.rep[,-1] <- lapply(gender.rep[,-1], scramble20)
  
  
  ## Age ##
  
  age <- categorical_filter(data.frame(coded_country = shiny_prep$coded_country, 
                            age = as_factor(shiny_prep$age)) %>%
                            group_by(age, coded_country),
                            data.frame(coded_country = "global", 
                            age = as_factor(shiny_prep$age)) %>%
                            group_by(age, coded_country),
                            "age")
  
  names(age)[names(age) != "coded_country"] = paste0("age_", names(age)[names(age) != "coded_country"])
  age[,-1] <- lapply(age[,-1], scramble20)
  
  age.rep <- categorical_filter(data.frame(coded_country = (shiny_prep %>% filter(representative == "Yes"))$coded_country, 
                                age = as_factor((shiny_prep %>% filter(representative == "Yes"))$age)) %>%
                                group_by(age, coded_country),
                                data.frame(coded_country = "global", 
                                age = as_factor((shiny_prep %>% filter(representative == "Yes"))$age)) %>%
                                group_by(age, coded_country),
                                "age")
  
  names(age.rep)[names(age.rep) != "coded_country"] = paste0("age_", names(age.rep)[names(age.rep) != "coded_country"])
  age.rep[,-1] <- lapply(age.rep[,-1], scramble20)
  
  ## Education ##
  
  edu <- categorical_filter(data.frame(coded_country = shiny_prep$coded_country, 
                            edu = as_factor(shiny_prep$edu)) %>%
                            group_by(edu, coded_country),
                            data.frame(coded_country = "global", 
                            edu = as_factor(shiny_prep$edu)) %>%
                            group_by(edu, coded_country),
                            "edu")
  
  names(edu)[names(edu) != "coded_country"] = paste0("education_", names(edu)[names(edu) != "coded_country"])
  edu[,-1] <- lapply(edu[,-1], scramble20)
  
  edu.rep <- categorical_filter(data.frame(coded_country = (shiny_prep %>% filter(representative == "Yes"))$coded_country, 
                                edu = as_factor((shiny_prep %>% filter(representative == "Yes"))$edu)) %>%
                                group_by(edu, coded_country),
                                data.frame(coded_country = "global", 
                                edu = as_factor((shiny_prep %>% filter(representative == "Yes"))$edu)) %>%
                                group_by(edu, coded_country),
                                "edu")
  
  names(edu.rep)[names(edu.rep) != "coded_country"] = paste0("education_", names(edu.rep)[names(edu.rep) != "coded_country"])
  edu.rep[,-1] <- lapply(edu.rep[,-1], scramble20)
  
  ## Political ##
  
  pol <- categorical_filter(shiny_prep %>% dplyr::select(coded_country, PolOrCat) %>%
                            mutate(PolOrCat = na_if(PolOrCat, "Libertarian LeftLibertarian Right")) %>%
                            mutate(PolOrCat = na_if(PolOrCat, "Authoritarian RightLibertarian Right")) %>%
                            group_by(PolOrCat, coded_country),
                            shiny_prep %>% dplyr::select(PolOrCat) %>%
                            mutate(PolOrCat = na_if(PolOrCat, "Libertarian LeftLibertarian Right")) %>%
                            mutate(PolOrCat = na_if(PolOrCat, "Authoritarian RightLibertarian Right")) %>%
                            mutate(coded_country = "global") %>%
                            group_by(PolOrCat, coded_country),
                            "PolOrCat")
  
  names(pol)[names(pol) != "coded_country"] = paste0("political_", names(pol)[names(pol) != "coded_country"])
  pol[,-1] <- lapply(pol[,-1], scramble50)
  
  pol.rep <- categorical_filter(shiny_prep %>% filter(representative == "Yes") %>% dplyr::select(coded_country, PolOrCat) %>%
                                mutate(PolOrCat = na_if(PolOrCat, "Libertarian LeftLibertarian Right")) %>%
                                mutate(PolOrCat = na_if(PolOrCat, "Authoritarian RightLibertarian Right")) %>%
                                group_by(PolOrCat, coded_country),
                                shiny_prep %>% filter(representative == "Yes") %>% dplyr::select(PolOrCat) %>%
                                mutate(PolOrCat = na_if(PolOrCat, "Libertarian LeftLibertarian Right")) %>%
                                mutate(PolOrCat = na_if(PolOrCat, "Authoritarian RightLibertarian Right")) %>%
                                mutate(coded_country = "global") %>%
                                group_by(PolOrCat, coded_country),
                                "PolOrCat")
  
  names(pol.rep)[names(pol.rep) != "coded_country"] = paste0("political_", names(pol.rep)[names(pol.rep) != "coded_country"])
  pol.rep[,-1] <- lapply(pol.rep[,-1], scramble50)
  
  ### Merger Ctry with categorical and Ctry.rep with categorical.rep
  
  ## Entire sample and standardized entire sample ##
  ctry.scales <- plyr::join(x=ctry.scales, y=languages, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=gender, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=age, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=edu, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=pol, by="coded_country")
  
  ctry.scales_s <- plyr::join(x=ctry.scales_s, y=languages, by="coded_country")
  ctry.scales_s <- plyr::join(x=ctry.scales_s, y=gender, by="coded_country")
  ctry.scales_s <- plyr::join(x=ctry.scales_s, y=age, by="coded_country")
  ctry.scales_s <- plyr::join(x=ctry.scales_s, y=edu, by="coded_country")
  ctry.scales_s <- plyr::join(x=ctry.scales_s, y=pol, by="coded_country")
  rm(languages, gender, age, edu, pol)
  
  ## Representative Sample and standardized representative sample ##
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=languages.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=gender.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=age.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=edu.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=pol.rep, by="coded_country")
  
  ctry.scales.representative_s <- plyr::join(x=ctry.scales.representative_s, y=languages.rep, by="coded_country")
  ctry.scales.representative_s <- plyr::join(x=ctry.scales.representative_s, y=gender.rep, by="coded_country")
  ctry.scales.representative_s <- plyr::join(x=ctry.scales.representative_s, y=age.rep, by="coded_country")
  ctry.scales.representative_s <- plyr::join(x=ctry.scales.representative_s, y=edu.rep, by="coded_country")
  ctry.scales.representative_s <- plyr::join(x=ctry.scales.representative_s, y=pol.rep, by="coded_country")
  rm(languages.rep, gender.rep, age.rep, edu.rep, pol.rep)
  
  ### Remaining Selection and export ###
  
  # sample size per country (including NA)
  world.n <- shiny_prep %>% 
    dplyr::select(coded_country, iso_a2, flag) %>%
    dplyr::group_by(coded_country, iso_a2, flag) %>%
    dplyr::summarize(
      n = n()
    )
  
  ctry.red <- ctry.scales %>%
    dplyr::select(coded_country, iso_a2, flag, n) %>%
    filter(n >= 20) #, coded_country != "global"
  ctry.only.red <- ctry.scales %>%
    dplyr::select(coded_country, iso_a2, flag, n) %>%
    filter(n >= 20, coded_country != "global")
  
  ctry.scales <- ctry.scales %>%
    filter(n>=20)
  
  ctry.scales_s <- ctry.scales_s %>%
    filter(n>=20)
  
  latest.DateTime <- format(max(ymd_hms(shiny_prep$EndDate, tz = "CET"), na.rm=T), "%d %B, %Y - %H:%M %Z")
  
  ## export for Shiny ##
  save(ctry.scales, ctry.scales.representative, ctry.scales_s, ctry.scales.representative_s, world.n, ctry.red, ctry.only.red, latest.DateTime, 
       file = "data/shinyDataAggregated.RData")
}
