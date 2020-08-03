## **Data for Shiny App**
# Dataframe for Shiny App

require(pacman)

pacman::p_load(psych, ggplot2, ggthemes, haven, data.table, dplyr, tidyr, Hmisc, mada,
               knitr, kableExtra, naniar, stats, readxl, matrixStats, ISOcodes, pander,
               scales, haven, lubridate, naniar, stats, rnaturalearth, rnaturalearthdata)

lib <- c("psych", "ggplot2", "ggthemes", "haven", "data.table", "dplyr", "tidyr", "Hmisc", "mada", 
         "knitr", "kableExtra", "naniar", "stats", "readxl", "matrixStats", "ISOcodes", "pander", "scales", "lubridate", "rnaturalearth", "rnaturalearthdata")

invisible(lapply(lib, library, character.only = TRUE))  
lapply(lib, library, character.only = TRUE)
rm(lib)

filter_cntry <- function(filter_vector,...) {
  data <- filter_vector %>%
    dplyr::summarize(
      ...,
      n = n(),
      
      affAnx = mean(affAnx, na.rm = T),
      affBor = mean(affBor, na.rm = T),
      affCalm = mean(affCalm, na.rm = T),
      affContent = mean(affContent, na.rm = T),
      affDepr = mean(affDepr, na.rm = T),
      affEnerg = mean(affEnerg, na.rm = T),
      affExc = mean(affExc, na.rm = T),
      affNerv = mean(affNerv, na.rm = T),
      affExh = mean(affExh, na.rm = T),
      affInsp = mean(affInsp, na.rm = T),
      affRel = mean(affRel, na.rm = T),
      affHighPos = mean(affHighPos.m, na.rm = T),
      affHighNeg = mean(affHighNeg.m, na.rm = T),
      affLowPos = mean(affLowPos.m, na.rm = T),
      affLowNeg = mean(affLowNeg.m, na.rm = T),
      
      #ext = mean(ext.m, na.rm = T),
      
      gov = mean(extC19Msg, na.rm = T),
      gov.sd = sd(extC19Msg, na.rm = T),
      gov.se = gov.sd/sqrt(n),
      
      comRule = mean(c19IsStrict, na.rm = T),
      comRule.sd = sd(c19IsStrict, na.rm = T),
      comRule.se = comRule.sd/sqrt(n),
      
      comPunish = mean(c19IsPunish, na.rm = T),
      comPunish.sd = sd(c19IsPunish, na.rm = T),
      comPunish.se = comPunish.sd/sqrt(n),
      
      comOrg = mean(c19IsOrg, na.rm = T),
      comOrg.sd = sd(c19IsOrg, na.rm = T),
      comOrg.se = comOrg.sd/sqrt(n),
      
      lone = mean(lone.m, na.rm = T),
      lone.sd = sd(lone.m, na.rm = T),
      lone.se = lone.sd/sqrt(n),
      
      #bor = mean(bor.m, na.rm = T),
      isoPers = mean(isoPers.m, na.rm = T),
      isoPers.sd = sd(isoPers.m, na.rm = T),
      isoPers.se = isoPers.sd/sqrt(n),
      
      isoOnl = mean(isoOnl.m, na.rm = T),
      isoOnl.sd = sd(isoOnl.m, na.rm = T),
      isoOnl.se = isoOnl.sd/sqrt(n),
      
      #beh = mean(beh.m, na.rm = T),
      behWash = mean(c19perBeh01, na.rm = T),
      behWash.sd = sd(c19perBeh01, na.rm = T),
      behWash.se = behWash.sd/sqrt(n),
      
      behAvoid = mean(c19perBeh02, na.rm = T),
      behAvoid.sd = sd(c19perBeh02, na.rm = T),
      behAvoid.se = behAvoid.sd/sqrt(n),
      
      covidHope = mean(c19Hope, na.rm = T),
      covidHope.sd = sd(c19Hope, na.rm = T),
      covidHope.se = covidHope.sd/sqrt(n),
      
      covidEff = mean(c19Eff, na.rm = T),
      covidEff.sd = sd(c19Eff, na.rm = T),
      covidEff.se = covidEff.sd/sqrt(n),
      
      para = mean(para.m, na.rm = T),
      para.sd = sd(para.m, na.rm = T),
      para.se = para.sd/sqrt(n),
      
      consp = mean(consp.m, na.rm = T),
      consp.sd = sd(consp.m, na.rm = T),
      consp.se = consp.sd/sqrt(n)
      
      #jobinsec = mean(jobinsec.m, na.rm = T),
      #pfs = mean(pfs.m, na.rm = T),
    )
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

data_prep <- function(){
  print("Preparing Data")
  
  world.data <- ne_countries(scale = "medium", returnclass = "sf")
  world.data$iso_a2[world.data$admin=="Kosovo"] <- "XK"
  dt5newVars <- read.csv("data/rawdata.csv") # raw data provided
  
  dt5newVars$coded_country[dt5newVars$coded_country == ""] <- NA # set empty cells in coded_country column to NA
  unique(dt5newVars$coded_country)[!unique(dt5newVars$coded_country) %in% world.data$admin] # check whether all country names are spelled correctly
  
  
  ### Calculate compound scores ###
  
  dt5newVars$affHighPos.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(affEnerg, affExc, affInsp), min = 1, max = 5)$scores
  dt5newVars$affHighNeg.m <- scoreItems(keys=c(1,1), items = dt5newVars %>% dplyr::select(affAnx, affNerv), min = 1, max = 5)$scores
  dt5newVars$affLowPos.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(affCalm, affContent, affRel), min = 1, max = 5)$scores
  dt5newVars$affLowNeg.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(affBor, affExh, affDepr), min = 1, max = 5)$scores
  dt5newVars$lone.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(matches("^lone[0][[:digit:]]$")), min = 1, max = 5)$scores # changed selection to use RE
  dt5newVars$isoPers.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(ends_with("inPerson"), -starts_with("w")), min = 0, max = 7)$scores
  dt5newVars$isoOnl.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(ends_with("online"), -starts_with("w")), min = 0, max = 7)$scores
  dt5newVars$para.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(matches("^para[0][[:digit:]]$")), min = 0, max = 10)$scores # changed selection to use RE
  dt5newVars$consp.m <- scoreItems(keys=c(1,1,1), items = dt5newVars %>% dplyr::select(matches("^consp[0][[:digit:]]$")), min = 0, max = 10)$scores # changed selection to use RE
  
  # all ISO-2 country code to dataframe and flags
  shiny_prep <- merge(x = dt5newVars, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T)
  shiny_prep$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(shiny_prep$iso_a2))
  
  ### Creating scales for entire Sample ###
  ctry.scales <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country)) %>% dplyr::group_by(coded_country))
  global.scales <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country)),coded_country = "global",iso_a2 = NA,
                                flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")
  
  ### Creating scales for representative Sample ###
  ctry.scales.representative <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country) & representative == "Yes") %>% dplyr::group_by(coded_country))
  representative_global.scales <- filter_cntry(shiny_prep %>% filter(!is.na(coded_country) & representative == "Yes"),coded_country = "global",iso_a2 = NA,
                                               flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg")
  
  
  ctry.scales <- merge(x = ctry.scales, y = unique(shiny_prep %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO
  ctry.scales.representative <- merge(x = ctry.scales.representative, y = unique(shiny_prep %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO
  
  ### Merger for entire sample with global variable ###
  ctry.scales <- rbind(global.scales, ctry.scales); rm(global.scales)
  
  ### Merger for representative sample with global (representative) variable ###
  ctry.scales.representative <- rbind(representative_global.scales, ctry.scales.representative); rm(representative_global.scales)
  
  
  scramble20 <- function(x) {ifelse(x<20, abs(x+sample(-2:2, 1, replace = T)), x)}
  scramble50 <- function(x) {ifelse(x<50, abs(x+sample(-5:5, 1, replace = T)), x)}
  
  ### Categorical filters for entire sample and representative sample ###
  
  ## Languages ##
  
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
  
  ## Entire sample ##
  #ctry.scales <- merge(x=ctry.scales, y=languages, by="coded_country", all.x=TRUE)
  ctry.scales <- plyr::join(x=ctry.scales, y=languages, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=gender, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=age, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=edu, by="coded_country")
  ctry.scales <- plyr::join(x=ctry.scales, y=pol, by="coded_country")
  rm(languages, gender, age, edu, pol)
  
  ## Representative Sample ##
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=languages.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=gender.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=age.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=edu.rep, by="coded_country")
  ctry.scales.representative <- plyr::join(x=ctry.scales.representative, y=pol.rep, by="coded_country")
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
  
  latest.DateTime <- format(max(ymd_hms(shiny_prep$EndDate, tz = "CET"), na.rm=T), "%d %B, %Y - %H:%M %Z")
  
  ## export for Shiny ##
  save(ctry.scales, ctry.scales.representative, world.n, ctry.red, ctry.only.red, latest.DateTime, 
       file = "data/shinyDataAggregated.RData")
}