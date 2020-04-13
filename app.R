library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(ggthemes)
library(stats)
library(shinydashboard)
#library(dygraphs)
#library(RColorBrewer)
#library(dichromat)
#library(zoo)
#library(xts)
#library(visNetwork)
#library(geomnet)
#library(igraph)
library(stringr)
#library(knitr)
#library(DT)
library(shinyjs)
library(shinyWidgets)
library(r2d3)
#library(forcats)
#library(rlang)
#library(plotly)
#library(ggiraph)
#library(gapminder)
#library(maps)
#library(rworldmap)
library(radarchart)
library(haven)
#library(leaflet)
library(highcharter)
library(rgeos)
library(scales)
library(grDevices)

# R Studio Clean-Up
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc() # garbage collector
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# truncate data:
dt <- readRDS("data/reducedData.rds")

# # load geo spatial data
# library(rnaturalearth)
# library(rnaturalearthdata)
# world.data <- ne_countries(scale = "medium", returnclass = "sf")
# unique(dt$coded_country)[!unique(dt$coded_country) %in% world.data$admin] # check whether all country names are spelled correctly
# 
# # all ISO-2 country code to dataframe and flags
# dt <- merge(x = dt, y = world.data %>% dplyr::select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T)
# dt$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(dt$iso_a2))

# add survey data to world map data
ctry.scales <- dt %>%
  filter(!is.na(coded_country)) %>%
  group_by(coded_country) %>%
  dplyr::summarize(
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
    comRule = mean(extC19Rules, na.rm = T),
    comPunish = mean(extC19Punish, na.rm = T),
    comOrg = mean(extC19Org, na.rm = T),
    lone = mean(lone.m, na.rm = T),
    #bor = mean(bor.m, na.rm = T),
    isoPers = mean(isoPers.m, na.rm = T),
    isoOnl = mean(isoOnl.m, na.rm = T),
    #beh = mean(beh.m, na.rm = T),
    behWash = mean(c19perBeh01, na.rm = T),
    behAvoid = mean(c19perBeh02, na.rm = T),
    c19Hope = mean(c19Hope, na.rm = T),
    c19Eff = mean(c19Eff, na.rm = T),
    para = mean(para.m, na.rm = T),
    consp = mean(consp.m, na.rm = T),
    #jobinsec = mean(jobinsec.m, na.rm = T),
    #pfs = mean(pfs.m, na.rm = T),
  )
ctry.scales <- merge(x = ctry.scales, y = unique(dt %>% dplyr::select(coded_country, iso_a2, flag)), all.x = T) # add flags and ISO

global.scales <- dt %>%
  filter(!is.na(coded_country)) %>%
  dplyr::summarize(
    coded_country = "global",
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
    comRule = mean(extC19Rules, na.rm = T),
    comPunish = mean(extC19Punish, na.rm = T),
    comOrg = mean(extC19Org, na.rm = T),
    lone = mean(lone.m, na.rm = T),
    #bor = mean(bor.m, na.rm = T),
    isoPers = mean(isoPers.m, na.rm = T),
    isoOnl = mean(isoOnl.m, na.rm = T),
    #beh = mean(beh.m, na.rm = T),
    behWash = mean(c19perBeh01, na.rm = T),
    behAvoid = mean(c19perBeh02, na.rm = T),
    c19Hope = mean(c19Hope, na.rm = T),
    c19Eff = mean(c19Eff, na.rm = T),
    para = mean(para.m, na.rm = T),
    consp = mean(consp.m, na.rm = T),
    #jobinsec = mean(jobinsec.m, na.rm = T),
    #pfs = mean(pfs.m, na.rm = T),
    iso_a2 = NA,
    flag = "https://rawcdn.githack.com/FortAwesome/Font-Awesome/4e6402443679e0a9d12c7401ac8783ef4646657f/svgs/solid/globe.svg"
  )
ctry.scales <- rbind(global.scales, ctry.scales); rm(global.scales)

# world.data <- merge(x=world.data, y=ctry.scales, by.x = "admin", by.y="coded_country", all.x=TRUE)
# # world.data$n[is.na(world.data$n)] <- 0
world.n <- ctry.scales %>% dplyr::select(coded_country, iso_a2, n)
# #world.cog <- world.data %>% dplyr::dplyr::select(admin, iso_a2, c19Hope, c19Eff, lone, para, consp)

cognitive <- rbind(data.frame(coded_country = "global",
                              c19Hope = as.numeric(dt$c19Hope), 
                              c19Eff = as.numeric(dt$c19Eff), 
                              lone = as.numeric(dt$lone.m), 
                              para = as.numeric(dt$para.m), 
                              consp = as.numeric(dt$consp.m)),
                   data.frame(coded_country = as.character(dt$coded_country),
                              c19Hope = as.numeric(dt$c19Hope), 
                              c19Eff = as.numeric(dt$c19Eff), 
                              lone = as.numeric(dt$lone.m), 
                              para = as.numeric(dt$para.m), 
                              consp = as.numeric(dt$consp.m)))
government <- rbind(data.frame(coded_country = "global",
                               gov = as.numeric(dt$extC19Msg)),
                    data.frame(coded_country = as.character(dt$coded_country),
                               gov = as.numeric(dt$extC19Msg)))
#gov.box$global <- boxplot.stats(as.numeric(dt$extC19Msg))
test <- government %>%
  group_by(coded_country) %>%
  summarise(boxplot= list(setNames(boxplot.stats(gov)$stats,
                                   c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker')))) %>%
  unnest_wider(boxplot)

community <- rbind(data.frame(coded_country = "global",
                              extC19Rules = as.numeric(dt$extC19Rules), 
                              extC19Punish = as.numeric(dt$extC19Punish), 
                              extC19Org = as.numeric(dt$extC19Org)),
                   data.frame(coded_country = as.character(dt$coded_country),
                              extC19Rules = as.numeric(dt$extC19Rules), 
                              extC19Punish = as.numeric(dt$extC19Punish), 
                              extC19Org = as.numeric(dt$extC19Org)))
behavior <- rbind(data.frame(coded_country = "global",
                             wash = as.numeric(dt$c19perBeh01),
                             avoid = as.numeric(dt$c19perBeh02),
                             #quarantine = as.numeric(dt$c19perBeh03),
                             isoPers = as.numeric(dt$isoPers.m),
                             isoOnl = as.numeric(dt$isoOnl.m)),
                  data.frame(coded_country = as.character(dt$coded_country),
                             wash = as.numeric(dt$c19perBeh01),
                             avoid = as.numeric(dt$c19perBeh02),
                             #quarantine = as.numeric(dt$c19perBeh03),
                             isoPers = as.numeric(dt$isoPers.m),
                             isoOnl = as.numeric(dt$isoOnl.m)))

ctry.red <- ctry.scales %>%
  filter(n >= 20) %>% #, coded_country != "global"
  dplyr::select(coded_country, n, flag)
ctry.only.red <- ctry.scales %>%
  filter(n >= 20, coded_country != "global") %>% #, coded_country != "global"
  dplyr::select(coded_country, n, flag)

overview <- data.frame(language = as.character(dt$language), 
                       gender = as.character(as_factor(dt$gender)), 
                       age = as.character(as_factor(dt$age)), 
                       education = as.character(as_factor(dt$edu)),
                       political = as.character(dt$PolOrCat) %>% 
                         str_replace_all(c("Libertarian LeftLibertarian Right" = NA_character_)) %>%
                         str_replace_all(c("Authoritarian RightLibertarian Right" = NA_character_)),
                       coded_country = as.character(dt$coded_country),
                       flag = as.character(dt$flag))

r2d3_script <- "
// !preview r2d3 data= data.frame(y = 0.1, ylabel = '1%', fill = '#E69F00', mouseover = 'green', label = 'one', id = 1)
function svg_height() {return parseInt(svg.style('height'))}
function svg_width()  {return parseInt(svg.style('width'))}
function col_top()  {return svg_height() * 0.05; }
function col_left() {return svg_width()  * 0.20; }
function actual_max() {return d3.max(data, function (d) {return d.y; }); }
function col_width()  {return (svg_width() / actual_max()) * 0.55; }
function col_heigth() {return svg_height() / data.length * 0.95; }

  var bars = svg.selectAll('rect').data(data);
  bars.enter().append('rect')
      .attr('x',      col_left())
      .attr('y',      function(d, i) { return i * col_heigth() + col_top(); })
      .attr('width',  function(d) { return d.y * col_width(); })
      .attr('height', col_heigth() * 0.9)
      .attr('fill',   function(d) {return d.fill; })
      .attr('id',     function(d) {return (d.label); })
      .on('click', function(){
        Shiny.setInputValue('bar_clicked', d3.select(this).attr('id'), {priority: 'event'});
      })
      .on('mouseover', function(){
        d3.select(this).attr('fill', function(d) {return d.mouseover; });
      })
      .on('mouseout', function(){
        d3.select(this).attr('fill', function(d) {return d.fill; });
      });
  bars.transition()
    .duration(500)
      .attr('x',      col_left())
      .attr('y',      function(d, i) { return i * col_heigth() + col_top(); })
      .attr('width',  function(d) { return d.y * col_width(); })
      .attr('height', col_heigth() * 0.9)
      .attr('fill',   function(d) {return d.fill; })
      .attr('id',     function(d) {return d.label; });
  bars.exit().remove();
  
  // Identity labels
  var txt = svg.selectAll('text').data(data);
  txt.enter().append('text')
      .attr('x', width * 0.01)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .text(function(d) {return d.label; })
      .style('font-family', 'sans-serif');
  txt.transition()
      .duration(1000)
      .attr('x', width * 0.01)
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .text(function(d) {return d.label; });
  txt.exit().remove();
  
  // Numeric labels
  var totals = svg.selectAll().data(data);
  totals.enter().append('text')
      .attr('x', function(d) { return ((d.y * col_width()) + col_left()) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.ylabel; });
  totals.transition()
      .duration(1000)
      .attr('x', function(d) { return ((d.y * col_width()) + col_left()) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr('d', function(d) { return d.x; })
      .text(function(d) {return d.ylabel; });
  totals.exit().remove();
"
r2d3_file <- tempfile()
writeLines(r2d3_script, r2d3_file)

ui <- dashboardPage(
  title = "Psycorona Campaign: Data Tool",
  dashboardHeader(title = "PsyCorona Data Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Our Sample", tabName = "sample", icon = icon("fas fa-users")),
      menuItem("Psychological Variables", tabName = "Variables", icon = icon("fas fa-pencil-ruler")),
      menuItem("Development", tabName = "development", icon = icon("fas fa-chart-line"), badgeLabel = "coming soon", badgeColor = "orange"),
      menuItem("Data", tabName = "data", icon = icon("fas fa-share-square")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  
  dashboardBody(
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$style(
      type = 'text/css', 
      '.bg-aqua {background-color: #3c8dbe!important; }
               .bttn-simple.bttn-primary {background-color: #3c8dbe!important; }'
    ),
    tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
    tags$script(HTML("
                            var openTab = function(tabName){
                              $('a', $('.sidebar')).each(function() {
                                if(this.getAttribute('data-value') == tabName) {
                                  this.click()
                                };
                              });
                            }
                          ")),
    
    tabItems(
      tabItem(tabName = "sample",
              h3("Our Sample"),
              fluidRow(
                box(width = 12,
                    radioGroupButtons(
                      inputId = "var", 
                      label = "Participant characteristics:", 
                      #choices = c("language", "gender", "age", "education", "political"), 
                      selected = "language",
                      justified = TRUE, 
                      status = "primary",
                      choiceNames = c("Survey language", "Gender", "Age", "Education", "Political orientation"),
                      choiceValues = c("language", "gender", "age", "education", "political")
                      #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                    ),
                    h3(textOutput("sample.bar.NA"), align = "center"),
                    d3Output("d3.bar")
                )
              ),
              fluidRow(
                box(
                  status = "primary",
                  width = 6,
                  tags$strong("World Map (sample sizes)"),
                  highchartOutput("freqPlot")
                ),
                box(
                  shinyjs::useShinyjs(),
                  id = "sample-controls",
                  width = 6,
                  #height = "600px",
                  title = "Controls",
                  solidHeader = T,
                  status = "primary",
                  #"Use these controls to ",
                  #br(), 
                  
                  multiInput(
                    inputId = "sample_country_selection",
                    label = "Countries:", 
                    choices = NULL,
                    choiceNames = lapply(seq_along(unique(na.omit(overview$coded_country))), 
                                         function(i) tagList(tags$img(src = unique(na.omit(overview$flag))[i],
                                                                      width = 20, 
                                                                      height = 15), unique(na.omit(overview$coded_country))[i])),
                    choiceValues = unique(na.omit(overview$coded_country)),
                    selected = unique(na.omit(overview$coded_country))
                  ),
                  hr(),
                  
                  div(style="display:inline-block;width:100%;text-align: center;",
                      actionBttn(
                        inputId = "sample_country_none", 
                        label = "None",
                        style = "simple", 
                        color = "primary",
                        size = "sm"),
                      HTML("&nbsp;&nbsp;"),
                      actionBttn(
                        inputId = "sample_country_all", 
                        label = "All",
                        style = "simple", 
                        color = "primary",
                        size = "sm")
                      
                  )
                )
              )
      ),
      tabItem(tabName = "Variables",
              box(width = 12, solidHeader = TRUE,
                  navbarPage(title = "Domain:",
                             id = "dataTabs",
                             tabPanel("Government Reponse",
                                      value = 1,
                                      #"extMsg",
                                      highchartOutput("boxGov")
                                      
                                      # multiInput(
                                      #   inputId = "gov_country_selection",
                                      #   label = "Select Countries (from all countries n > 20):", 
                                      #   choices = NULL,
                                      #   choiceNames = lapply(seq_along(ctry.red$coded_country), 
                                      #                        function(i) tagList(tags$img(src = ctry.red$flag[i],
                                      #                                                     width = 20, 
                                      #                                                     height = 15), 
                                      #                                            ctry.red$coded_country[i],
                                      #                                            paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")"))),
                                      #   choiceValues = ctry.red$coded_country,
                                      #   selected = "global"
                                      # )
                             ),
                             tabPanel("Community Response", 
                                      value = 2,
                                      #"ext",
                                      highchartOutput("boxCom")
                                      # radioGroupButtons(
                                      #   inputId = "ComVars", 
                                      #   label = "Variable:", 
                                      #   #choices = c("language", "gender", "age", "education", "political"), 
                                      #   selected = "extC19Rules",
                                      #   justified = TRUE, 
                                      #   status = "primary",
                                      #   choiceNames = c("Rules", "Punishment", "Organization"),
                                      #   choiceValues = c("extC19Rules", "extC19Punish", "extC19Org")
                                      #   #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                      # ),
                                      
                                      # multiInput(
                                      #   inputId = "com_country_selection",
                                      #   label = "Select Countries (from all countries n > 20):", 
                                      #   choices = NULL,
                                      #   choiceNames = lapply(seq_along(ctry.red$coded_country), 
                                      #                        function(i) tagList(tags$img(src = ctry.red$flag[i],
                                      #                                                     width = 20, 
                                      #                                                     height = 15), 
                                      #                                            ctry.red$coded_country[i],
                                      #                                            paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")"))),
                                      #   choiceValues = ctry.red$coded_country,
                                      #   selected = "global"
                                      # )
                             ),
                             tabPanel("Cognitive Response",
                                      value = 3,
                                      #Financial strain(?), job insecurity (?)",
                                      highchartOutput("boxCog")
                                      
                                      # radioGroupButtons(
                                      #   inputId = "CogVars", 
                                      #   label = "Variable:", 
                                      #   #choices = c("language", "gender", "age", "education", "political"), 
                                      #   selected = "c19Hope",
                                      #   justified = TRUE, 
                                      #   status = "primary",
                                      #   choiceNames = c("Hope", "Efficacy", "Loneliness", "Paranoia", "Conspiracy"),
                                      #   choiceValues = c("c19Hope", "c19Eff", "lone", "para", "consp")
                                      #   #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                      # ),
                                      
                                      # multiInput(
                                      #   inputId = "cog_country_selection",
                                      #   label = "Select Countries (from all countries n > 20):", 
                                      #   choices = NULL,
                                      #   choiceNames = lapply(seq_along(ctry.red$coded_country), 
                                      #                        function(i) tagList(tags$img(src = ctry.red$flag[i],
                                      #                                                     width = 20, 
                                      #                                                     height = 15), 
                                      #                                            ctry.red$coded_country[i],
                                      #                                            paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")"))),
                                      #   choiceValues = ctry.red$coded_country,
                                      #   selected = "global"
                                      # )
                                      
                             ),
                             tabPanel("Behavioral Response",
                                      value = 4,
                                      #"isolation, beh",
                                      htmlOutput("boxBeh")
                                      
                                      # radioGroupButtons(
                                      #   inputId = "BehVars", 
                                      #   label = "Variable:", 
                                      #   #choices = c("language", "gender", "age", "education", "political"), 
                                      #   selected = "wash",
                                      #   justified = TRUE, 
                                      #   status = "primary",
                                      #   choiceNames = c("Washing", "Avoiding", "Social Contact"),
                                      #   choiceValues = c("wash", "avoid", "iso")
                                      #   #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                      # ),
                                      
                                      # multiInput(
                                      #   inputId = "beh_country_selection",
                                      #   label = "Select Countries (from all countries n > 20):", 
                                      #   choices = NULL,
                                      #   choiceNames = lapply(seq_along(ctry.red$coded_country), 
                                      #                        function(i) tagList(tags$img(src = ctry.red$flag[i],
                                      #                                                     width = 20, 
                                      #                                                     height = 15), 
                                      #                                            ctry.red$coded_country[i],
                                      #                                            paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")"))),
                                      #   choiceValues = ctry.red$coded_country,
                                      #   selected = "global"
                                      # )
                             ),
                             tabPanel("Emotional Response",
                                      value = 5,
                                      # sidebarLayout(
                                      #   sidebarPanel(
                                      #     h4("Choose Display Option:"),
                                      #     tags$b("Individual Emotions "),
                                      #     prettySwitch(
                                      #       inputId = "categorySwitch",
                                      #       label = tags$b("Emotional Categories"), 
                                      #       status = "success",
                                      #       fill = TRUE,
                                      #       inline = TRUE,
                                      #       value = TRUE
                                      #     ),
                                      #     
                                      #     h4("Select the Relevant Regions:"),
                                      #     multiInput(
                                      #       inputId = "sample_country_affect",
                                      #       label = "Countries (all countries n > 20):", 
                                      #       choices = NULL,
                                      #       choiceNames = lapply(seq_along(ctry.red$coded_country), 
                                      #                            function(i) tagList(tags$img(src = ctry.red$flag[i],
                                      #                                                         width = 20, 
                                      #                                                         height = 15), 
                                      #                                                ctry.red$coded_country[i],
                                      #                                                paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")"))),
                                      #       choiceValues = ctry.red$coded_country,
                                      #       selected = "global"
                                      #     )
                                      #   ),
                                      chartJSRadarOutput('affect', height = "125")
                                      #)
                             ),
                             tabPanel("Cross Domain Relationships",
                                      value = 6,
                                      sidebarLayout(
                                        sidebarPanel(
                                          # h4("Choose Display Option:"),
                                          # tags$b("Scatter "),
                                          # prettySwitch(
                                          #   inputId = "corSwitch",
                                          #   label = tags$b("Bubble"), 
                                          #   status = "success",
                                          #   fill = TRUE,
                                          #   inline = TRUE,
                                          #   value = TRUE
                                          # ),
                                          #hr(),
                                          h4("Select Variables:"),
                                          pickerInput(
                                            inputId = "CorX",
                                            label = "X Axis",
                                            choices = list(
                                              Government = c("Clear Government Information" = "gov"),
                                              Community = c("Rules"="comRule", "Punishment"="comPunish", "Organization"="comOrg"),
                                              Cognitive = c("Hope"="c19Hope", "Efficacy"="c19Eff", "Loneliness"="lone", "Paranoia"="para", "Conspiracy"="consp"),
                                              Behavior = c("Washing"="behWash", "Avoiding"="behAvoid", "Isolation Offline"="isoPers", "Isolation Online"="isoOnl"),
                                              Emotion = c("Anxious"="affAnx", "Bored"="affBor", "Calm"="affCalm", "Content"="affContent", "Depressed"="affDepr", "Energetic"="affEnerg", 
                                                          "Excited"="affExc", "Nervous"="affNerv", "Exhausted"="affExh", "Inspired"="affInsp", "Relaxed"="affRel"),
                                              EmtionCat = c("High Arousal Positive"="affHighPos", "High Arousal Negative"="affHighNeg", 
                                                            "Low Arousal Positive"="affLowPos", "Low Arousal Negative"="affLowNeg")
                                            )
                                          ),
                                          pickerInput(
                                            inputId = "CorY",
                                            label = "Y Axis",
                                            choices = list(
                                              Government = c("Clear Government Information" = "gov"),
                                              Community = c("Rules"="comRule", "Punishment"="comPunish", "Organization"="comOrg"),
                                              Cognitive = c("Hope"="c19Hope", "Efficacy"="c19Eff", "Loneliness"="lone", "Paranoia"="para", "Conspiracy"="consp"),
                                              Behavior = c("Washing"="behWash", "Avoiding"="behAvoid", "Isolation Offline"="isoPers", "Isolation Online"="isoOnl"),
                                              Emotion = c("Anxious"="affAnx", "Bored"="affBor", "Calm"="affCalm", "Content"="affContent", "Depressed"="affDepr", "Energetic"="affEnerg", 
                                                          "Excited"="affExc", "Nervous"="affNerv", "Exhausted"="affExh", "Inspired"="affInsp", "Relaxed"="affRel"),
                                              EmtionCat = c("High Arousal Positive"="affHighPos", "High Arousal Negative"="affHighNeg", 
                                                            "Low Arousal Positive"="affLowPos", "Low Arousal Negative"="affLowNeg")),
                                            selected = "comPunish"
                                          ),
                                          hr(),
                                          h4("Select Region:"),
                                          multiInput(
                                            inputId = "cor_country_selection",
                                            label = "Countries (all countries n > 20):", 
                                            choices = NULL,
                                            choiceNames = lapply(seq_along(ctry.only.red$coded_country), 
                                                                 function(i) tagList(tags$img(src = ctry.only.red$flag[i],
                                                                                              width = 20, 
                                                                                              height = 15), 
                                                                                     ctry.only.red$coded_country[i],
                                                                                     paste0(" (n=",prettyNum(ctry.only.red$n[i], big.mark=",", scientific=FALSE),")"))),
                                            choiceValues = ctry.only.red$coded_country,
                                            selected = ctry.only.red$coded_country
                                          ),
                                          div(style="display:inline-block;width:100%;text-align: center;",
                                              actionBttn(
                                                inputId = "cor_country_none", 
                                                label = "None",
                                                style = "simple", 
                                                color = "primary",
                                                size = "sm"),
                                              HTML("&nbsp;&nbsp;"),
                                              actionBttn(
                                                inputId = "cor_country_all", 
                                                label = "All",
                                                style = "simple", 
                                                color = "primary",
                                                size = "sm")
                                          )
                                        ),
                                        mainPanel(
                                          highchartOutput("cor")
                                        )
                                      )
                             )
                  )
              ),
              conditionalPanel(
                condition = "input.dataTabs != 6",
                box(width = 12, 
                    solidHeader = TRUE,
                    uiOutput("var.settings"),
                    multiInput(
                      inputId = "psych_country_selection",
                      label = "Countries (all countries n > 20):", 
                      choices = NULL,
                      choiceNames = lapply(seq_along(ctry.red$coded_country), 
                                           function(i) tagList(tags$img(src = ctry.red$flag[i],
                                                                        width = 20, 
                                                                        height = 15), 
                                                               ctry.red$coded_country[i],
                                                               paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")"))),
                      choiceValues = ctry.red$coded_country,
                      selected = "global"
                    )
                )
             )
      ),
      tabItem(tabName = "development",
              h2("Development over Time"),
              fluidRow(
                box(#title = "Explore The Data", 
                  width = 12, 
                  heigth = "500px",
                  solidHeader = TRUE,
                  tags$br(),
                  tags$br(),
                  h4(HTML(paste("<center>","As our efforts grow over time we will share data describing developments over time.", "</center>"))),
                  h4(HTML(paste("<center>","As soon as we have multiple data waves you can explore this data here.", "</center>"))),
                  tags$br()
                )
              )
      ),
      tabItem(tabName = "data",
              #h3("Dashboard tab content"),
              fluidRow(
                box(title = "Data Protection",
                    width = 12, 
                    solidHeader = TRUE,
                    "To protect the privacy and confidentiality of our participants, data access will never be made available without thorough vetting by our 
                              editorial board. ... WILL BE UPDATED SHORTLY.",
                    tags$br(),
                    tags$br()
                ),
                box(title = "Data Collaboration",
                    width = 12, 
                    solidHeader = TRUE,
                    "One major aim of the PsyCorona initiative is to combine psychologolical reactions with local, regional, and national
                              data on the Covid-19 spread and governmental reactions towards it. In our efforts we collaborate with ",
                    tags$a(href="https://dataversuscorona.com/", 
                           target="_blank",
                           "Data Scientists Against Corona"),
                    "and are working on bringing together the ",
                    tags$a(href="https://github.com/cjvanlissa/COVID19_metadata", 
                           target="_blank",
                           "Covid-19 Metadata"),
                    "repository with anonymized regional data from the PsyCorona initiative.",
                    tags$br(),
                    tags$br()
                ),
                box(title = "Data Sharing",
                    width = 12, 
                    solidHeader = TRUE,
                    "The aim of the PsyCorona initiative is to build a collaborative research network. If you are interested in becoming
                              part of the PsyCorona initiative you contact us via our website: ",
                    tags$a(href="https://psycorona.org/", 
                           target="_blank",
                           "www.psycorona.org/."),
                    
                    tags$br(),
                    tags$br(),
                    "Part of data sharing is also open availability of code. The code to this web applet is available at",
                    tags$a(href="https://github.com/JannisCodes/PsyCorona-WebApp", 
                           target="_blank",
                           "our git repository."),
                    tags$br(),
                    tags$br()
                )
              )
              # tabsetPanel(type = "tab",
              #             tabPanel("Twitter",
              #                      "stuff for tab 1"
              #             ),
              #             tabPanel("Interview",
              #                      "stuff for tab 2"
              #             )
              # )
      ),
      tabItem(tabName = "about",
              h3("Welcome to the PsyCorona Data Tool"),
              br(),
              fluidRow(
                box(#title = "Explore The Data", 
                  width = 8, 
                  heigth = "500px",
                  solidHeader = TRUE,
                  
                  h4("The Initiative:"),
                  #tags$b("The Project:"),
                  "Psychology and culture could affect the spread of the virus; human psychology could also change in response to the pandemic. 
                            We aim to mobilize behavioral and data scientists to identify targets for rapid intervention to slow the spread of the pandemic and minimize its social damage. 
                            All the scientists on the team are operating on a largely volunteer basis, relying on existing infrastructure and their own resources.
                            This is a global project based at New York University-Abu Dhabi and the University of Groningen (The Netherlands). 
                            We have evolved into a distributed team across the world, with autonomous work groups in numerous countries, each of whom understands the PsyCorona mission goals and needs. 
                            We aim to ensure global involvement, so we are translating the survey into more languages on a daily basis.
                            Currently more than 100 international social scientists are working together to collect immediate and longitudinal information on 
                            the key social science factors that might predict the spread of COVID-19. The project, is documented in detail our",
                  tags$a(href="https://www.psycorona.org", 
                         target="_blank",
                         "PsyCorona website"),
                  ". We pair social and data scientists to connect data across multiple layers—individual survey reports from", prettyNum(nrow(dt), big.mark=" ", scientific=FALSE), 
                  "participants from more than",length(unique(dt$coded_country)),"countries, satellite data documenting social distancing, and World Health Organization data on county level spread 
                             of the disease.",
                  br(),
                  "You can find the PsyCorona Initiative on: ",
                  tags$a(href="https://www.facebook.com/PsyCorona-100726584925027", 
                         target="_blank",
                         icon("facebook")),
                  HTML("&nbsp"),
                  tags$a(href="https://github.com/JannisCodes/PsyCorona-WebApp", 
                         target="_blank", 
                         icon("github")),
                  br(),
                  br(),
                  h4("What You Can Do Here:"),
                  "This applet has ",
                  tags$b("four main interactive sections"),
                  " that enable visitors to directly interact with the PsyCorona data: ",
                  tags$ul(
                    tags$li("The ",
                            a("Data", onclick = "openTab('data')", href="#"),
                            "tab provides information on how we deal with our participants' data and how you can get involved in data analysis.
                                      Here we also share information on our open source code and connections to meta data repositories we are aiming to 
                                      connect to the psychological responses we measure during the PsyCorona initiative.")),
                  "The remaining three tabs offer tools to visualize the psychological data we collect in this project.",
                  tags$ul(
                    tags$li("The ",
                            a("Our Sample", onclick = "openTab('sample')", href="#"),
                            " tab offers an insight into the diversity of our participants. We share compound information on some demographic variables,
                                      as well as the number of participants we have reached in each country. Please note that to protect the privacy and anonymity
                                      of our participants most data visualizations are only available for selections of more then 25 people."),
                    tags$li("The ",
                            a("Psychological Variables", onclick = "openTab('Variables')", href="#"),
                            " tab offers an interactive interface to explore the psychological variables we collect in the initiative's baseline survey. 
                                      This survey is open to anyone interested at",
                            tags$a(href="https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", 
                                   target="_blank",
                                   "tiny.cc/corona_survey"),
                            "and currently includes over", prettyNum(nrow(dt), big.mark=" ", scientific=FALSE), 
                            "participants. You can explore psychological reactions to the coronavirus at five different levels: (1) Governmental Response, 
                                      (2) Community Response, (3) Cognitive Response, (4) Behavioral Response, as well as (5) Emotional Reponse. Additionally, we offer a
                                      tool to explore the mean level relationship between different variables for different countries. Please note that to protect the 
                                      privacy and anonymity of our participants we only provide country-level visualizations once we have data for more than 20 people from 
                                      any particular country."),
                    tags$li("The ",
                            a("Development", onclick = "openTab('development')", href="#"),
                            " tab gives you the possibility to interactively explore how different areas are evolving over time. This section is currently under
                                      construction, but will be available as soon as we have a dataset of developmental data that can be modeled over time.")
                  )
                ),
                box(width = 4,
                    HTML("<a class=\"twitter-timeline\" data-height=\"500\" href=\"https://twitter.com/FortuneMagazine/lists/coronavirus-updates?ref_src=twsrc%5Etfw\">A Twitter List by FortuneMagazine</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                    #HTML("<a class=\"twitter-timeline\" data-lang=\"en\" data-height=\"500\" href=\"https://twitter.com/ReMatriate?ref_src=twsrc%5Etfw\">Tweets by ReMatriate</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                )
              ),
              fluidRow(
                valueBox(prettyNum(nrow(dt), big.mark=" ", scientific=FALSE), "Participants", icon = icon("user-edit"), width = 4),
                valueBox(length(unique(dt$language)), "Languages", icon = icon("language"), width = 4),
                valueBox("100+", "Researchers", icon = icon("user-graduate"), width = 4)#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$sample.bar.NA <- renderText({
    test <- overview %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      mutate(label = !!sym(input$var)) %>%
      group_by(label) %>%
      tally() %>%
      na.omit() 
    ifelse(sum(test$n)<20, "Not enough data to display summary","")
  })
  
  output$d3.bar <- renderD3({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    #input <- list(var = "gender", sample_country_selection = c("Poland", "Romania", "Albania"))
    
    overview %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      mutate(label = !!sym(input$var)) %>%
      group_by(label) %>%
      tally() %>%
      arrange(desc(n)) %>%
      na.omit() %>%
      filter(sum(n)>=20) %>%
      mutate(
        y = n,
        ylabel = scales::percent(n/sum(n), accuracy = 0.01), #prettyNum(n/sum(n)*100, big.mark = ",", format = "f", digits = 2),
        fill = "#3b738f", #ifelse(label != input$val, "#E69F00", "red"),
        mouseover = "#2a5674"
      ) %>%
      r2d3(r2d3_file)
  })
  
  output$freqPlot <- renderHighchart({
    hcmap(download_map_data = FALSE,
          data = world.n %>% filter(coded_country %in% input$sample_country_selection), 
          value = "n",
          joinBy = c("iso-a2", "iso_a2"), name = "sample size",
          #dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.1,
          tooltip = list(valueDecimals = 0, valuePrefix = "n = "))%>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(minColor = "#c4e6c3", maxColor = "#1d4f60", type = "logarithmic") 
  })
  #Color schemes: https://carto.com/carto-colors/
  
  output$boxGov <- renderHighchart({
    # for testing:
    # input = list(gov_country_selection = c("Germany", "France"))
    
    governmentRed <- government %>%
      filter(coded_country %in% input$psych_country_selection,
             !is.na(gov))
    
    categories <- c("0" , "1<br>unclear", "2", "3", "4", "5", "6<br>clear")
    
    hcboxplot(x = governmentRed$gov, var = governmentRed$coded_country,
              color = "#1d4f60", fillColor = "#3b738f", medianColor = "#4e99bf",
              allowPointSelect = T, outliers = T) %>% 
      hc_title(text = "To what extent are you getting clear, unambiguous messages about what to do about the Coronavirus?") %>%
      hc_yAxis(showFirstLabel = T,
               showLastLabel = T,
               min = 1,
               max = 6,
               #step = 1,
               #list(formatter = JS(gov.labs)), 
               #rotation = 0,
               categories = categories,
               #align = "center",
               tickmarkPlacement = seq(1,6,1))
    
    
    
  })
  
  output$boxCom <- renderHighchart({
    # for testing:
    # input = list(CogVars = "c19Hope", cog_country_selection = c("Germany", "France"))
    
    communityRed <- community %>%
      dplyr::select(coded_country, 
                    label = one_of(input$ComVars)) %>%
      filter(coded_country %in% input$psych_country_selection,
             !is.na(label))
    title.txt <- list(extC19Rules = "To what extent is your commmunity developing strict rules in response to the Coronavirus?", 
                      extC19Punish = "To what extent is your commmunity punishing people who deviate from the rules that have been put in place in response to the Coronavirus?", 
                      extC19Org = "To what extent is your commmunity well organized in responding to the Coronavirus?")
    
    categories <- c("0" , "1<br>not at all", "2", "3", "4", "5", "6<br>very much")
    
    
    hcboxplot(x = communityRed$label, var = communityRed$coded_country,
              color = "#1d4f60", fillColor = "#3b738f", medianColor = "#4e99bf",
              allowPointSelect = T,
              outliers = T) %>%
      hc_title(text = title.txt[[input$ComVars]]) %>% 
      hc_yAxis(showFirstLabel = T,
               showLastLabel = T,
               min = 1,
               max = 6,
               #step = 1,
               #list(formatter = JS(gov.labs)), 
               #rotation = 0,
               categories = categories,
               #align = "center",
               tickmarkPlacement = seq(1,6,1))
  })
  
  output$boxCog <- renderHighchart({
    # for testing:
    # input = list(CogVars = "c19Hope", cog_country_selection = c("Germany", "France"))
    
    cognitiveRed <- cognitive %>%
      dplyr::select(coded_country, 
                    label = one_of(input$CogVars)) %>%
      filter(coded_country %in% input$psych_country_selection,
             !is.na(label))
    
    title.txt <- list(c19Hope = "I have high hopes that the situation regarding coronavirus will improve.", 
                      c19Eff = "I think that this country is able to fight the Coronavirus.",
                      lone = "Mean Loneliness Scores",
                      para = "Mean State Paranoia Scores",
                      consp = "Mean Conspiracy Scores")
    y.min <- list(c19Hope = -3, 
                  c19Eff = -3,
                  lone = 1,
                  para = 0,
                  consp = 0)
    y.max <- list(c19Hope = 3, 
                  c19Eff = 3,
                  lone = 5,
                  para = 10,
                  consp = 10)
    lab.ticks <- list(c19Hope = c("0"), 
                      c19Eff = c("0"), 
                      lone = c("0"),
                      para = c("0"),
                      consp = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"))
    lab.breaks <- list(c19Hope = seq(0,7,1), 
                       c19Eff = seq(0,7,1), 
                       lone = seq(1,5,1),
                       para = seq(0,10,1),
                       consp = seq(0,10,1))
    
    lab.ends <- list(c19Hope = c("-3<br>disagree", "3<br>agree"), 
                     c19Eff = c("-3<br>disagree", "3<br>agree"), 
                     lone = c("1<br>Never", "5<br>All the time"),
                     para = c("0<br>Not at all", "10<br>Very much"),
                     consp = c("0%", "100%"))
    lab.ends.js <- paste0("function(){console.log(this);
                                        if(this.isFirst){
                                        return '",
                          lab.ends[input$CogVars][[1]][1],
                          "'} else if(this.isLast) {return '",
                          lab.ends[input$CogVars][[1]][2],
                          "'} else {
                                        return this.value
                                        }
                                        }")
    
    hcboxplot(x = cognitiveRed$label, var = cognitiveRed$coded_country,
              color = "#1d4f60", fillColor = "#3b738f", medianColor = "#4e99bf",
              allowPointSelect = T,
              outliers = T) %>%
      #hc_add_series_labels_values(labels = lab.ticks[[input$CogVars]],
      #                            values = lab.breaks[[input$CogVars]])
      hc_title(text = title.txt[[input$CogVars]]) %>%
      hc_yAxis(showFirstLabel = T,
               showLastLabel = T,
               min = y.min[[input$CogVars]],
               max = y.max[[input$CogVars]],
               tickInterval = 1,
               #step = 1,
               labels = list(formatter = JS(lab.ends.js)), 
               #rotation = 0,
               categories = lab.ticks[[input$CogVars]],
               #align = "center",
               tickmarkPlacement = lab.breaks[[input$CogVars]])
  })
  
  output$boxBeh <- renderUI({
    # for testing:
    # input = list(BehVars = "avoid", beh_country_selection = c("Germany", "France"))
    
    if (input$BehVars == "iso") {
      behaviorRedIso <- behavior %>%
        dplyr::select(coded_country, 
                      isoPers, isoOnl) %>%
        filter(coded_country %in% input$psych_country_selection)
      
      
      hcPers <- hcboxplot(x = behaviorRedIso$isoPers, var = behaviorRedIso$coded_country,
                          color = "#1d4f60", fillColor = "#3b738f", medianColor = "#4e99bf",
                          allowPointSelect = T, outliers = T) %>%
        hc_title(text = "Number of days per week with in-person contacts") %>%
        hc_yAxis(min = 0,
                 max = 7,
                 categories = seq(0,7,1),
                 tickmarkPlacement = seq(0,7,1))
      hcOnli <- hcboxplot(x = behaviorRedIso$isoOnl, var = behaviorRedIso$coded_country,
                          color = "#1d4f60", fillColor = "#3b738f", medianColor = "#4e99bf",
                          allowPointSelect = T, outliers = T) %>%
        hc_title(text = "Number of days per week with online contacts") %>%
        hc_yAxis(min = 0,
                 max = 7,
                 categories = seq(0,7,1),
                 tickmarkPlacement = seq(0,7,1))
      lst <- list(hcPers, hcOnli)  
      
      hw_grid(lst, ncol = 2, rowheight = "400")
      
    } else {
      behaviorRed <- behavior %>%
        dplyr::select(coded_country, 
                      label = one_of(input$BehVars)) %>%
        filter(coded_country %in% input$psych_country_selection,
               !is.na(label))
      
      title.txt <- list(wash = "To minimize my chances of getting corona virus I wash my hands mroe often", 
                        avoid = "To minimize my chances of getting corona virus I avoid crowded spaces")
      
      y.min <- list(wash = -3, 
                    avoid = -3)
      y.max <- list(wash = 3, 
                    avoid = 3)
      
      lab.ends <- list(wash = c("-3<br>disagree", "3<br>agree"), 
                       avoid = c("-3<br>disagree", "3<br>agree"))
      lab.ends.js <- paste0("function(){console.log(this);
                                        if(this.isFirst){
                                        return '",
                            lab.ends[input$BehVars][[1]][1],
                            "'} else if(this.isLast) {return '",
                            lab.ends[input$BehVars][[1]][2],
                            "'} else {
                            return this.value
                            }
                            }")
      
      hcboxplot(x = behaviorRed$label, var = behaviorRed$coded_country,
                color = "#1d4f60", fillColor = "#3b738f", medianColor = "#4e99bf",
                allowPointSelect = T, outliers = T) %>%
        hc_title(text = title.txt[[input$BehVars]]) %>%
        hc_yAxis(showFirstLabel = T,
                 showLastLabel = T,
                 min = y.min[[input$BehVars]],
                 max = y.max[[input$BehVars]],
                 tickInterval = 1,
                 labels = list(formatter = JS(lab.ends.js)), 
                 categories = c("0"),
                 tickmarkPlacement = seq(0,7,1)) %>%
        hw_grid(ncol = 1, rowheight = "400")
    }
  })
  
  
  output$affect <- renderChartJSRadar({
    # for testing:
    # input = list(categorySwitch = TRUE, sample_country_affect = c("global", "Germany"))
    
    if (input$categorySwitch == TRUE) {
      labs <- c("High Arousal Positive", "High Arousal Negative", "Low Arousal Positive", "Low Arousal Negative")
      vars <- c("affHighPos", "affHighNeg", "affLowPos", "affLowNeg")
    } else {
      labs <- c("Inspired", "Excited", 
                "Nervous", "Anxious", 
                "Calm", "Content", "Relaxed", 
                "Bored", "Depressed", "Exhausted",
                "Energetic")
      vars <- c("affInsp", "affExc", 
                "affNerv", "affAnx", 
                "affCalm", "affContent", "affRel", 
                "affBor", "affDepr", "affExh",
                "affEnerg")
    }
    
    radar <- data.frame("label" = labs, 
                        t(
                          ctry.scales %>%
                            filter(coded_country %in% input$psych_country_selection) %>% 
                            dplyr::select(one_of(vars))
                        )
    )
    names(radar) <- c("label", input$psych_country_selection)
    chartJSRadar(radar, maxScale = 5, showToolTipLabel=TRUE, showLegend = T) 
  })
  
  output$cor <- renderHighchart({
    # for testing:
    # input = list(CorX = "c19Hope", CorY = "c19Eff")
    
    cor.dat <- ctry.scales %>%
      dplyr::select(coded_country, n,
                    xvar = one_of(input$CorX),
                    yvar = one_of(input$CorY)) %>%
      filter(coded_country != "global",
             coded_country %in% input$cor_country_selection)
    
    varLab <- c("gov"="Clear Government Information",
                "comRule"="Community Rules", "comPunish"="Community Punishment", "comOrg"="Community Organization",
                "c19Hope"="Hope", "c19Eff"="Efficacy", "lone"="Loneliness", "para"="State Paranoia", "consp"="Conspiracy",
                "behWash"="Washing", "behAvoid"="Avoiding", "isoPers"="Isolation Offline", "isoOnl"="Isolation Online",
                "affAnx"="Anxious", "affBor"="Bored", "affCalm"="Calm", "affContent"="Content", "affDepr"="Depressed", "affEnerg"="Energetic", 
                "affExc"="Excited", "affNerv"="Nervous", "affExh"="Exhausted", "affInsp"="Inspired", "affRel"="Relaxed",
                "affHighPos"="High Arousal Positive", "affHighNeg"="High Arousal Negative",
                "affLowPos"="Low Arousal Positive", "affLowNeg"="Low Arousal Negative")
    
    if (nrow(cor.dat) == 0) {
      highchart() %>%
        hc_title(text = "Select Countries to Display")
    } else {
      tooltipJS <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> ",varLab[input$CorX],": ' + Math.round((this.x+ Number.EPSILON) * 100) / 100 + 
                      ' <br> ",varLab[input$CorY],": ' + Math.round((this.y+ Number.EPSILON) * 100) / 100 + 
                      ' <br> Sample Size: ' + this.point.n)
                      }")
      
      highchart() %>%
        hc_add_series(data = cor.dat,
                      type = "bubble",
                      mapping = hcaes(x = xvar, y = yvar, z = n),
                      color = "#3b738f", alpha = 0.5, 
                      minSize = 8, maxSize = "30%", showInLegend = F
        ) %>%
        hc_title(text = "PsyCorona Bubble Chart") %>%
        hc_xAxis(title = list(text = as.character(varLab[input$CorX]))) %>%
        hc_yAxis(title = list(text = as.character(varLab[input$CorY]))) %>%
        hc_tooltip(formatter = JS(tooltipJS))
    }
  })
  
  output$var.settings <- renderUI({
    if (input$dataTabs == 1) {
      
    } else if (input$dataTabs == 2) {
      radioGroupButtons(
        inputId = "ComVars", 
        label = "Variable:", 
        #choices = c("language", "gender", "age", "education", "political"), 
        selected = "extC19Rules",
        justified = TRUE, 
        status = "primary",
        choiceNames = c("Rules", "Punishment", "Organization"),
        choiceValues = c("extC19Rules", "extC19Punish", "extC19Org")
        #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      )
    } else if (input$dataTabs == 3) {
      radioGroupButtons(
        inputId = "CogVars", 
        label = "Variable:", 
        #choices = c("language", "gender", "age", "education", "political"), 
        selected = "c19Hope",
        justified = TRUE, 
        status = "primary",
        choiceNames = c("Hope", "Efficacy", "Loneliness", "Paranoia", "Conspiracy"),
        choiceValues = c("c19Hope", "c19Eff", "lone", "para", "consp")
        #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      )
    } else if (input$dataTabs == 4) {
      radioGroupButtons(
        inputId = "BehVars", 
        label = "Variable:", 
        #choices = c("language", "gender", "age", "education", "political"), 
        selected = "wash",
        justified = TRUE, 
        status = "primary",
        choiceNames = c("Washing", "Avoiding", "Social Contact"),
        choiceValues = c("wash", "avoid", "iso")
        #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      )
    } else if (input$dataTabs == 5) {
      list(h4("Choose Display Option:"),
           tags$b("Individual Emotions "),
           prettySwitch(
             inputId = "categorySwitch",
             label = tags$b("Emotional Categories"), 
             status = "success",
             fill = TRUE,
             inline = TRUE,
             value = TRUE
           ),
           h4("Select Relevant Regions:")
         )
    } else {
      
    }
    
  })
  
  
  
  observeEvent(input$reset_input_ctry, {
    shinyjs::reset("country_controls")
  })
  
  observeEvent(input$sample_country_all, {
    updateMultiInput(
      session = session,
      inputId = "sample_country_selection",
      selected = unique(overview$coded_country)
    )
  })
  
  observeEvent(input$sample_country_none, {
    updateMultiInput(
      session = session,
      inputId = "sample_country_selection",
      selected = character(0)
    )
  })
  
  observeEvent(input$cor_country_all, {
    updateMultiInput(
      session = session,
      inputId = "cor_country_selection",
      selected = ctry.only.red$coded_country
    )
  })
  
  observeEvent(input$cor_country_none, {
    updateMultiInput(
      session = session,
      inputId = "cor_country_selection",
      selected = character(0)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
