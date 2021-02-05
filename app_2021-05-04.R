library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
library(shinydashboard)
library(shinythemes)
library(metathis)
library(stringr)
library(shinyjs)
library(shinyWidgets)
library(r2d3)
library(radarchart)
library(haven)
library(highcharter)
library(rgeos)
library(scales)
library(grDevices)
library(shinyalert)
library(shinyBS)
library(htmltools)
library(purrr)
library(RColorBrewer)
library(lubridate)
source("R/dependencies.R")
source("R/input-multi.R")
source("R/utils.R")
#source("data_prep_shiny_labels.R")

# R Studio Clean-Up:
#cat("\014") # clear console
#rm(list=ls()) # clear workspace (better way: Ctrl+Shift+F10 but make sure that you are not saving your workspace to your .Rdata file)
#gc() # garbage collector
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # usually set by project

# load data:
# data creation not possible in docker container (i.e., on server)
# if (!file.exists("data/shinyDataAggregated.RData")) {
#   data_prep()
# }
load("data/shinyDataAggregated.RData")
load("data/shinyDataLongitudinal.RData")


updateMultiInput_2 <- function (session, inputId, label = NULL, selected = NULL, choices = NULL, choiceValues = NULL, choiceNames = NULL) {
  if (is.null(choices)) {
    if (is.null(choiceValues))
      stop("If choices = NULL, choiceValues must be not NULL")
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    choiceValues <- as.list(choiceValues)
    choiceNames <- as.list(choiceNames)
    choices_2 <- tagList(
      lapply(
        X = seq_along(choiceNames),
        FUN = function(i) {
          htmltools::tags$option(value = choiceValues[[i]], as.character(choiceNames[[i]]),
                                 selected = if(choiceValues[[i]] %in% selected) "selected")
        }
      )
    )
  }
  else {
    choices_2 <- if (!is.null(choices))
      choicesWithNames(choices_2)
  }
  if (!is.null(selected))
    selected <- validateSelected(selected, choices_2, inputId)
  options <- as.character(makeChoices(choices = choices, choiceValues = choiceValues, choiceNames = choiceNames, selected = selected))
  message <- dropNulls(list(label = label, options = options, value = selected))
  session$sendInputMessage(inputId, message)
}


r2d3_script <- "
// !preview r2d3 data= data.frame(y = 0.1, ylabel = '1%', fill = '#E69F00', mouseover = 'green', label = 'one', id = 1)
function svg_height() {return parseInt(svg.style('height'))}
function svg_width()  {return parseInt(svg.style('width'))}
function col_top()  {return svg_height() * 0.05; }
function col_left() {return svg_width()  * 0.25;} 
function actual_max() {return d3.max(data, function (d) {return d.y; }); }
function col_width()  {return (svg_width() / actual_max()) * 0.60; }
function col_heigth() {return svg_height() / data.length * 0.95; }

  var bars = svg.selectAll('rect').data(data);
  bars.enter().append('rect')
      .attr('x',      170)
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
      .attr('x',      170)
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
      .attr('x', function(d) { return ((d.y * col_width()) + 170) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.ylabel; });
  totals.transition()
      .duration(1000)
      .attr('x', function(d) { return ((d.y * col_width()) + 170) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr('d', function(d) { return d.x; })
      .text(function(d) {return d.ylabel; });
  totals.exit().remove();
"
r2d3_file <- tempfile()
writeLines(r2d3_script, r2d3_file)


all_valid_ctry <- ctry.scales$coded_country
all_valid_ctry.only.red <- all_valid_ctry[! all_valid_ctry %in% c("global")]
repr_valid_ctry <- ctry.scales.representative$coded_country
repr_valid_ctry.only.red <- repr_valid_ctry[! repr_valid_ctry %in% c("global")]

# TODO: TEMPORARY: ADD TO DATA_PREP()
ctry.red.repr <- ctry.scales.representative %>% dplyr::select(coded_country, iso_a2, flag, n)
ctry.only.red.repr <- ctry.red.repr[!ctry.red.repr$coded_country %in% c("global"),]
# TODO: temporary: make sure that the columns of ctry.scales and ctry.scales.representative are identical
ctry.scales <- ctry.scales[names(ctry.scales.representative)]
ctry.scales_s <- ctry.scales_s[names(ctry.scales.representative_s)]
# TODO: rename column names
names(ctry.scales_s) <- sub("_Standardized", "", names(ctry.scales_s))
names(ctry.scales.representative_s) <- sub("_Standardized", "", names(ctry.scales.representative_s))

z.min <- min(ctry.scales_s %>% dplyr::select_if(., is.numeric), na.rm = T)
z.max <- max(ctry.scales_s %>% dplyr::select_if(., is.numeric), na.rm = T)

# Default
select_ctry_sample <- all_valid_ctry
select_ctry_psych <- "global"
select_sample <- "full"
select_transformation <- "raw"
latest.DateTime <- file.info("app.R")$mtime

# -----
ui <- dashboardPage(
  title = "PsyCorona: Data Visualization",
  dashboardHeader(title=span( icon("fas fa-virus"), "PsyCorona Data Tool") #HTML(paste(icon("virus"), "PsyCorona Data Tool")),
                  # dropdownMenu(type = "notifications",
                  #              notificationItem(text = "Data is currenlty not accurate",
                  #                               icon = icon("warning"),
                  #                               status = "warning")
                  #              )
                  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebarMenu",
      menuItem("The Sample", tabName = "sample", icon = icon("fas fa-users")),
      menuItem("Psychological Variables", tabName = "Variables", icon = icon("fas fa-pencil-ruler")),
      menuItem("Development", tabName = "development", icon = icon("fas fa-chart-line"), badgeLabel = "new", badgeColor = "blue"),
      menuItem("Data", tabName = "data", icon = icon("fas fa-share-square")),
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem(HTML(paste0("Take the survey now ", icon("external-link"))), icon=icon("fas fa-file-signature"), href = "https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", newtab = T),
      uiOutput("dynamic_content")),
    shinyjs::useShinyjs(),
    tags$footer(HTML("<strong>Copyright &copy; 2020 <a href=\"https://psycorona.org/about/\" target=\"_blank\">PsyCorona</a>.</strong> 
                   <br>This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.
                   <br><a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\" target=\"_blank\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png\" /></a>
                   <br>Last updated:<br>"), 
                  latest.DateTime,
                id = "sideFooter",
                  align = "left",
                  style = "
                  position:absolute;
                  bottom:0;
                  width:100%;
                  padding: 10px;
                  "
                  )
      #HTML("<a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png\" /></a><br />This work is licensed under a <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/4.0/\">Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License</a>.")
      #)
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('sidebar-mini');")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$head(tags$meta(name = "viewport", content = "width=1600"), uiOutput("body")),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    #tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/JannisCodes/PsyCorona-WebApp/master/www/faviconData.png")),
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    tags$style(
      type = 'text/css',
      '.bg-aqua {background-color: #3c8dbe!important; }
      .bttn-simple.bttn-primary {background-color: #3c8dbe!important; }
      .btn.radiobtn.btn-primary {float: center!important;
                                 display: block;
                                 width: 160px}
      '
    ),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.13.0/css/all.css);"),
    tags$script(src = "https://code.highcharts.com/mapdata/custom/world.js"),
    tags$script(HTML("
                            var openTab = function(tabName){
                              $('a', $('.sidebar')).each(function() {
                                if(this.getAttribute('data-value') == tabName) {
                                  this.click()
                                };
                              });
                            };
                            $('.sidebar-toggle').attr('id','menu');
                            var dimension = [0, 0];
                                $(document).on('shiny:connected', function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange('dimension', dimension);
                                });
                          ")),
    meta() %>%
      meta_social(
        title = "PsyCorona: Data Visualization",
        description = paste0("A tool to explore the patterns of psychological reactions to the Covid-19 epidemic across ", nrow(ctry.only.red), " countries."),
        url = "https://psycorona.shinyapps.io/WebApp/",
        image = "https://raw.githubusercontent.com/JannisCodes/PsyCorona-WebApp/master/www/media.png",
        image_alt = "PsyCorona Data Tool",
        twitter_creator = "@JannisWrites",
        twitter_card_type = "summary",
        twitter_site = "@JannisWrites"
        ),
    
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "sample",
              useShinyalert(),
              h3("The Sample"),
              bsAlert("dataAlert"),
              
              fluidRow(
                box(width = 12,
                    div(style="display:inline-block;width:100%;text-align:center;",
                        radioGroupButtons(
                          inputId = "var", 
                          label = "Participant characteristics:", 
                          selected = "languages",
                          status = "primary",
                          #justified = T,
                          #individual = T,
                          choiceNames = c("Survey language", "Gender", "Age", "Education", "Political orientation"),
                          choiceValues = c("languages", "gender", "age", "education", "political")
                          #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                        )
                    ),
                    h3(textOutput("sample.bar.NA"), align = "center"),
                    d3Output("d3.bar"),
                    textOutput("SampleTxt"), align = "center")
                #)
              ),
              fluidRow(
                box(
                  status = "primary",
                  width = 6,
                  tags$strong("World Map (sample sizes of initial survey round)"),
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
                    label = "Please select the countries you are interested in (all countries n > 20):", 
                    choices = NULL,
                    choiceNames = lapply(seq_along(ctry.only.red$coded_country), 
                                         function(i) tagList(tags$img(src = ctry.only.red$flag[i],
                                                                      width = 20, 
                                                                      height = 15), 
                                                             ctry.only.red$coded_country[i],
                                                             paste0(" (n=",prettyNum(ctry.only.red$n[i], big.mark=",", scientific=FALSE),")"))),
                    choiceValues = ctry.only.red$coded_country,
                    selected = all_valid_ctry
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
                                      highchartOutput("boxGov"),
                                      "Note: Every person who took the survey could answer the above question. Here we present the average rating level (i.e., mean) of a given country, 
                                      as well as an interval that indicates the uncertainty around the average value given the sample (i.e., confidence interval). 
                                      Please keep in mind that (a) one country being higher or lower than another country can have a multitude of reasons (including the date in which they answered the question), 
                                      and (b) the data might not always be representative of the entire country’s population."
                             ),
                             tabPanel("Community Response", 
                                      value = 2,
                                      div(style="display:inline-block;width:100%;text-align: center;",
                                          radioGroupButtons(
                                            inputId = "ComVars", 
                                            #label = "", 
                                            selected = "comRule",
                                            #justified = TRUE, 
                                            status = "primary",
                                            choiceNames = c("Rules", "Punishment", "Organization"),
                                            choiceValues = c("comRule", "comPunish", "comOrg")
                                            #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                          )
                                      ),
                                      highchartOutput("boxCom"),
                                      "Note: Every person who took the survey could answer the above question. Here we present the average rating level (i.e., mean) 
                                      of the countries you choose to select as well as an interval that indicates the uncertainty around the average value given the sample 
                                      (i.e., confidence interval - range of values that is likely to encompass the true value). Please keep in mind that 
                                      (a) one country being higher or lower than another country can have a multitude of reasons (including, when most people answered the question), and 
                                      (b) that the data might not always be representative the entire country."
                             ),
                             tabPanel("Cognitive Response",
                                      value = 3,
                                      #Financial strain(?), job insecurity (?)",
                                      div(style="display:inline-block;width:100%;text-align: center;",
                                          radioGroupButtons(
                                            inputId = "CogVars", 
                                            #label = "Variable:", 
                                            selected = "covidHope",
                                            #justified = TRUE, 
                                            status = "primary",
                                            choiceNames = c("Hope", "Efficacy", "Loneliness", "Paranoia", "Conspiracy"),
                                            choiceValues = c("covidHope", "covidEff", "lone", "para", "consp")
                                            #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                          )
                                      ),
                                      highchartOutput("boxCog"),
                                      "Note: Every person who took the survey could answer the above question. Here we present the average rating level (i.e., mean) 
                                      of the countries you choose to select as well as an interval that indicates the uncertainty around the average value given the sample 
                                      (i.e., confidence interval - range of values that is likely to encompass the true value). Please keep in mind that 
                                      (a) one country being higher or lower than another country can have a multitude of reasons (including, when most people answered the question), and 
                                      (b) that the data might not always be representative the entire country."
                             ),
                             tabPanel("Behavioral Response",
                                      value = 4,
                                      #"isolation, beh",
                                      div(style="display:inline-block;width:100%;text-align: center;",
                                          radioGroupButtons(
                                            inputId = "BehVars", 
                                            #label = "Variable:", 
                                            selected = "behWash",
                                            #justified = TRUE, 
                                            status = "primary",
                                            choiceNames = c("Washing", "Avoiding", "Social Contact"),
                                            choiceValues = c("behWash", "behAvoid", "iso")
                                          )
                                      ),
                                      htmlOutput("boxBeh"),
                                      "Note: Every person who took the survey could answer the above question. Here we present the average rating level (i.e., mean) 
                                      of the countries you choose to select as well as an interval that indicates the uncertainty around the average value given the sample 
                                      (i.e., confidence interval - range of values that is likely to encompass the true value). Please keep in mind that 
                                      (a) one country being higher or lower than another country can have a multitude of reasons (including, when most people answered the question), and 
                                      (b) that the data might not always be representative the entire country."
                             ),
                             tabPanel("Emotional Response",
                                      value = 5,
                                      column(12, align="center",
                                             tags$b("Individual Emotions "),
                                            prettySwitch(
                                                inputId = "categorySwitch",
                                                label = tags$b("Emotional Categories"), 
                                                status = "success",
                                                fill = TRUE,
                                                inline = TRUE,
                                                value = FALSE
                                              )
                                            ),
                                      chartJSRadarOutput('affect', height = "125", width = "400"),
                                      tags$br(),
                                      "Note: Here you can see the emotional reactions of people. There are two ways to look at them: Emotional Categories and Individual Emotions. 
                                      For the individual emotions, people answered how much they felt that emotion over the last week. For the emotional categories, we averaged together 
                                      the emotions that are either high or low on arousal as well as positive or negative (negative high arousal: anxiety and nervous; negative low arousal: 
                                      bored, exhausted, depressed; positive high arousal: energetic, excited, inspired; positive low arousal: calm, content, relaxed)"
                             ),
                             tabPanel("Cross Domain Relationships",
                                      value = 6,
                                      highchartOutput("cor"),
                                      "Note: Here you can select two variables and plot them against each other. One bubble represents one country. 
                                      The bigger the bubble, the more people from that country answered the question. The position of the bubble represents the average levels of the variables 
                                      you select. This allows you to examine relationships between variables. For example, do countries that are higher in one variable also tend to be higher 
                                      on the other variable."
                             )
                  )
              ),
              conditionalPanel(
                condition = "input.dataTabs != 6",
                box(width = 12, 
                    solidHeader = T,
                    status = "primary",
                    multiInput(
                      inputId = "psych_country_selection",
                      label = "Please select the countries you are interested in (all countries n > 20):", 
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
             ),
             conditionalPanel(
               condition = "input.dataTabs == 6",
               box(width = 12,
                   solidHeader = T,
                   status = "primary",
                   h4("Select Variables:"),
                   column(width = 6,
                     pickerInput(
                       inputId = "CorX",
                       label = "X Axis:",
                       #width = "80%",
                       choices = list(
                         Government = c("Clear Government Information" = "gov"),
                         Community = c("Rules"="comRule", "Punishment"="comPunish", "Organization"="comOrg"),
                         Cognitive = c("Hope"="covidHope", "Efficacy"="covidEff", "Loneliness"="lone", "Paranoia"="para", "Conspiracy"="consp"),
                         Behavior = c("Washing"="behWash", "Avoiding"="behAvoid", "Isolation Offline"="isoPers", "Isolation Online"="isoOnl"),
                         Emotion = c("Anxious"="affAnx", "Bored"="affBor", "Calm"="affCalm", "Content"="affContent", "Depressed"="affDepr", "Energetic"="affEnerg", 
                                     "Excited"="affExc", "Nervous"="affNerv", "Exhausted"="affExh", "Inspired"="affInsp", "Relaxed"="affRel"),
                         EmtionCat = c("High Arousal Positive"="affHighPos", "High Arousal Negative"="affHighNeg", 
                                       "Low Arousal Positive"="affLowPos", "Low Arousal Negative"="affLowNeg")
                       )
                     )
                    ),
                   column(width = 6,
                     pickerInput(
                       inputId = "CorY",
                       label = "Y Axis:",
                       #width = "80%",
                       choices = list(
                         Government = c("Clear Government Information" = "gov"),
                         Community = c("Rules"="comRule", "Punishment"="comPunish", "Organization"="comOrg"),
                         Cognitive = c("Hope"="covidHope", "Efficacy"="covidEff", "Loneliness"="lone", "Paranoia"="para", "Conspiracy"="consp"),
                         Behavior = c("Washing"="behWash", "Avoiding"="behAvoid", "Isolation Offline"="isoPers", "Isolation Online"="isoOnl"),
                         Emotion = c("Anxious"="affAnx", "Bored"="affBor", "Calm"="affCalm", "Content"="affContent", "Depressed"="affDepr", "Energetic"="affEnerg", 
                                     "Excited"="affExc", "Nervous"="affNerv", "Exhausted"="affExh", "Inspired"="affInsp", "Relaxed"="affRel"),
                         EmtionCat = c("High Arousal Positive"="affHighPos", "High Arousal Negative"="affHighNeg", 
                                       "Low Arousal Positive"="affLowPos", "Low Arousal Negative"="affLowNeg")),
                       selected = "comPunish"
                     )
                   ),
                   hr(),
                   h4("Select Region:"),
                   multiInput(
                     inputId = "cor_country_selection",
                     label = "Please select the countries you are interested in (all countries n > 20):", 
                     choices = NULL,
                     choiceNames = lapply(seq_along(ctry.only.red$coded_country), 
                                          function(i) tagList(tags$img(src = ctry.only.red$flag[i],
                                                                       width = 20, 
                                                                       height = 15), 
                                                              ctry.only.red$coded_country[i],
                                                              paste0(" (n=",prettyNum(ctry.only.red$n[i], big.mark=",", scientific=FALSE),")"))),
                     choiceValues = ctry.only.red$coded_country,
                     selected = all_valid_ctry
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
                 
               )
             )
      ),
      tabItem(tabName = "development",
              #h2("Development over Time"),
              navbarPage("Focus on:", #theme = shinytheme("flatly"),
                         tabPanel("Multiple countries",
                                  #h2("Countries tab"),
                                  bsAlert("longitudinalAlert"),
                                  box(#title = "Explore The Data", 
                                    width = 12, 
                                    #heigth = "500px",
                                    solidHeader = TRUE,
                                    highchartOutput("long_ctrs_hc"),
                                    tags$br()
                                    ),
                                  box(width = 12,
                                      solidHeader = T,
                                      status = "primary",
                                      #h4("Select Variables:"),
                                      fluidRow(
                                      column(width = 8, style = "margin-top: 6px;",
                                             pickerInput(
                                               inputId = "long_ctrs_var",
                                               label = "Variable selection:",
                                               #width = "80%",
                                               choices = list(
                                                 Cognitive = c("Loneliness"="lone01", "Paranoia"="para01", "Conspiracy"="consp01"),
                                                 Behavior = c("Isolation Friends Offline"="isoFriends_inPerson", "Isolation Others Offline"="isoOthPpl_inPerson", 
                                                              "Isolation Friends Online"="isoFriends_online", "Isolation Others Online"="isoOthPpl_online"),
                                                 Emotion = c("Anxious"="affAnx", "Bored"="affBor", "Calm"="affCalm", "Depressed"="affDepr", "Energetic"="affEnerg", 
                                                             "Nervous"="affNerv", "Exhausted"="affExh", "Inspired"="affInsp", "Relaxed"="affRel")
                                               )
                                             )
                                             ),
                                      column(width = 4, style = "margin-top: 0px;",
                                             h5(tags$b("Confidence Interval:")),
                                             switchInput(
                                               inputId = "long_ctrs_CiSwitch",
                                               onStatus = "success",
                                               offStatus = "danger",
                                               size = "mini",
                                               value = FALSE
                                             )
                                      )),
                                      hr(),
                                      h4("Select Region:"),
                                      multiInput(
                                        inputId = "long_ctrs_country_selection",
                                        label = "Please select the countries you are interested in (all countries with at least three weeks of n > 10):", 
                                        choices = NULL,
                                        choiceNames = lapply(seq_along(weeklyRegions$coded_country), 
                                                             function(i) tagList(tags$img(src = weeklyRegions$flag[i],
                                                                                          width = 20, 
                                                                                          height = 15), 
                                                                                 weeklyRegions$coded_country[i])),
                                        choiceValues = weeklyRegions$coded_country,
                                        selected = "global"
                                      ),
                                      div(style="display:inline-block;width:100%;text-align: center;",
                                          actionBttn(
                                            inputId = "long_ctrs_none", 
                                            label = "None",
                                            style = "simple", 
                                            color = "primary",
                                            size = "sm"),
                                          HTML("&nbsp;&nbsp;"),
                                          actionBttn(
                                            inputId = "long_ctrs_all", 
                                            label = "All",
                                            style = "simple", 
                                            color = "primary",
                                            size = "sm")
                                      )
                                      )
                                  ),
                         tabPanel("Multiple Variables",
                                  div(style="display:inline-block;width:100%;text-align:center;",
                                      box(width = 12, 
                                          solidHeader = T,
                                          h3("We are working hard to bring you new content."),
                                          h5("We have hit a bit of a snag but we are already testing for bugs. Soon you will be able to directly compare multiple variables in their development over time."),
                                          br(),
                                          h3("Estimated Time until release:"),
                                          h2(textOutput('eventTimeRemaining')),
                                          br()
                                          )
                                      )
                                  # box(#title = "Explore The Data", 
                                  #     width = 12, 
                                  #     heigth = "500px",
                                  #     solidHeader = TRUE,
                                  #     highchartOutput("long_vars_hc"),
                                  #     tags$br()
                                  #   ),
                                  # box(width = 12,
                                  #     solidHeader = T,
                                  #     status = "primary",
                                  #     h4("Select Variables:"),
                                  #     fluidRow(
                                  #       column(width = 8, style = "margin-top: 6px;",
                                  #              pickerInput(inputId = "long_vars_country",
                                  #                          label = "country selection:",
                                  #                           #width = "80%",
                                  #                           choices = weeklyRegions$coded_country,
                                  #                           choicesOpt = list(content =  
                                  #                                               mapply(weeklyRegions$coded_country, weeklyRegions$flag, FUN = function(country, flagUrl) {
                                  #                                                 HTML(paste(
                                  #                                                   tags$img(src=flagUrl, width=20, height=15),
                                  #                                                   country
                                  #                                                 ))
                                  #                                               }, SIMPLIFY = FALSE, USE.NAMES = FALSE)),
                                  #                           selected = "global"
                                  #                          )),
                                  #       column(width = 4, style = "margin-top: 0px;",
                                  #              h5(tags$b("Confidence Interval:")),
                                  #              switchInput(
                                  #                inputId = "long_vars_CiSwitch",
                                  #                onStatus = "success",
                                  #                offStatus = "danger",
                                  #                size = "mini",
                                  #                value = FALSE
                                  #              )
                                  #       )),
                                  #     hr(),
                                  #     h4("Select Variable(s):"),
                                  #     multiInput(
                                  #       inputId = "long_vars_variables",
                                  #       label = "Please select the variables you are interested in:", 
                                  #       choices = NULL,
                                  #       choiceNames = c( "Anxious", "Bored", "Calm", "Depressed", "Energetic", 
                                  #                        "Nervous", "Exhausted", "Inspired", "Relaxed", 
                                  #                        "Loneliness", "Paranoia", "Conspiracy", 
                                  #                        "Isolation Friends Offline", "Isolation Others Offline", 
                                  #                        "Isolation Friends Online", "Isolation Others Online"),
                                  #       choiceValues = mvars,
                                  #       selected = "affAnx"
                                  #     ),
                                  #     div(style="display:inline-block;width:100%;text-align: center;",
                                  #         actionBttn(
                                  #           inputId = "long_vars_none", 
                                  #           label = "None",
                                  #           style = "simple", 
                                  #           color = "primary",
                                  #           size = "sm"),
                                  #         HTML("&nbsp;&nbsp;"),
                                  #         actionBttn(
                                  #           inputId = "long_vars_all", 
                                  #           label = "All",
                                  #           style = "simple", 
                                  #           color = "primary",
                                  #           size = "sm")
                                  #     )
                                  # )
                         )
              ),
              
              
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Data Protection",
                    width = 12, 
                    solidHeader = TRUE,
                    HTML("To protect the privacy and confidentiality of our participants, this application only uses aggregate, anonymized data (i.e., no individual person is identifiable).
                    <br><b>Anonymization:</b><br>  
                    <ol>
                    <li>We use <b>data aggregation</b> as the main method of data anonymization. This means that we never show data of a single person or small groups of people; instead, 
                    we combine data of hundreds-to-thousands of people to show country-level summary statistics. As an example, you can see the average level of hope in a country and how much 
                    variation there was in the responses, but you cannot see the rating of any individual respondent.
                    <br>Importantly, the application only has access to the country-level summaries, which means that data cannot be linked or combined to single out individuals 
                    (e.g., you cannot see the level of hope for U.S. women, aged 25-34, who speak Farsi and have a doctorate degree).
                    <br><i>Note:</i> In order for aggregate data to be anonymous, we need to combine the responses of enough people. This is why we never display data of countries with less than 20 respondents.</li>
                    
                    <li>When we show summaries of categorical data (e.g., percentage of people identifying as female), we additionally apply <b>data perturbations</b> for small groups. This means that the counts and percentages 
                    for groups that comprise less than 20 people (less than 50 for political orientation) have been slightly changed (e.g., a random number between -2 and 2 or between -5 and 5 has been added; 
                    this process is also sometimes referred to as 'artificial noise addition'). 
                    <br>Please note that this also means that the numbers are not always 100% accurate. However, with this method, we can represent the full diversity of our sample whilst still protecting the identities of people in 
                    small or vulnerable groups.</li>
                    </ol>
                    These are the main ways in which we protect the personal data of our participants and make sure that no individual is identifiable within the application. If you have any questions or concerns, please feel
                    free to contact us at <a href=\"mailto:psycorona@rug.nl?Subject=Data%20web%20application\" target=\"_top\">psycorona@rug.nl</a>.
                    <br><b>Access:</b> <br>
                    Access to person-level data will never be made available without thorough vetting by our editorial board (see data sharing below). And even then, we only share
                    anonymized or pseudonymized data with active academic collaborators."),
                    tags$br(),
                    tags$br()
                ),
                box(title = "Data Collaboration",
                    width = 12, 
                    solidHeader = TRUE,
                    "One major aim of the PsyCorona initiative is to combine psychological reactions with local, regional, 
                    and national data on the Covid-19 spread and governmental reactions. In our efforts we collaborate with ",
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
                              part of the PsyCorona collaboration, you can contact us via our website: ",
                    tags$a(href="https://psycorona.org/", 
                           target="_blank",
                           "www.psycorona.org."),
                    
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
      ),
      tabItem(tabName = "about",
              h3("Welcome to the PsyCorona Data Tool"),
              br(),
              fluidRow(
                box(#title = "Explore The Data", 
                  width = 8, 
                  heigth = "500px",
                  solidHeader = TRUE,
                  
                  h4("The Collaboration:"),
                  "Psychology and culture could affect the spread of the virus; human psychology could also change in response to the pandemic. Early in the pandemic, 
                  we sought to mobilize behavioral and data scientists to identify psychological factors that could slow the spread of the pandemic and minimize its social damage. 
                  All the scientists in the collaboration operate on a largely volunteer basis, relying on existing infrastructure and their own resources. 
                  This is an international project based at New York University-Abu Dhabi and the University of Groningen (The Netherlands). We have evolved into a distributed team across the world, 
                  with autonomous work groups in numerous countries, each of whom understands the PsyCorona mission goals and needs. We aim to ensure global involvement, 
                  so we translated the survey into 30 languages and worked with more than 100 international social scientists to collect immediate and longitudinal information on 
                  psychological factors that might predict the spread of COVID-19. Further details are available at the",
                  tags$a(href="https://www.psycorona.org", 
                         target="_blank",
                         "PsyCorona website."),
                  br(),
                  "You can find the PsyCorona collaboration on: ",
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
                            Here we also share information on our open-source code and on the connections to metadata repositories we aim to connect to the psychological responses.")),
                  "The remaining three tabs offer tools to visualize the psychological data we collect in this project.",
                  tags$ul(
                    tags$li("The ",
                            a("The Sample", onclick = "openTab('sample')", href="#"),
                            "tab offers an insight into the diversity of our participants. We share compound information on some demographic variables, as well as the number of respondents in each country. 
                            Please note that to protect the privacy and anonymity of our participants, data visualizations are only available for selections of more than 20 people."),
                    tags$li("The ",
                            a("Psychological Variables", onclick = "openTab('Variables')", href="#"),
                            " tab offers an interactive interface to explore the psychological variables we collect in the initiative's baseline survey. 
                                      This survey is open to anyone interested at",
                            tags$a(href="https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", 
                                   target="_blank",
                                   "tiny.cc/corona_survey"),
                            "and currently includes over 50 000 participants. You can explore psychological reactions to the coronavirus at five different levels: 
                            (1) Governmental Response, (2) Community Response, (3) Cognitive Response, (4) Behavioral Response, as well as (5) Emotional Response. 
                            Additionally, we offer a tool to explore the mean level relationship between different variables for different countries. Please note that to protect the 
                            privacy and anonymity of our participants we only provide country-level visualizations once we have data for more than 20 people from any particular country."),
                    tags$li("The ",
                            a("Development", onclick = "openTab('development')", href="#"),
                            " tab gives you the possibility to interactively explore how different areas are evolving over time. This section is currently partly under
                                      construction, but will be fully available soon.")
                  )
                ),
                box(width = 4,
                    HTML("<a class=\"twitter-timeline\" data-height=\"600\" href=\"https://twitter.com/FortuneMagazine/lists/coronavirus-updates?ref_src=twsrc%5Etfw\">A Twitter List by FortuneMagazine</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                )
              ),
              fluidRow(
                valueBox(prettyNum(sum(world.n$n), big.mark=" ", scientific=FALSE), "Participants", icon = icon("user-edit"), width = 3),
                valueBox(prettyNum(surveyN, big.mark=" ", scientific=FALSE), "Total Survey Responses", icon = icon("edit"), width = 3),
                valueBox(sum(str_count(names(ctry.scales), c("language")))-1, "Languages", icon = icon("language"), width = 3),
                valueBox("100+", "Researchers", icon = icon("user-graduate"), width = 3)#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              )
      )
    )
    )
  )
# -----
  
server <- function(input, output, session) {
  # -----
  # observeEvent(input$dimension[1], {
  #   if (input$dimension[1] <= 767) {
  #     shinyalert(title = "Small Screen Detected", 
  #               text = "Some elements of this application are not optimized for smaller screens.
  #               If you have problems viewing some of the graphical displays please try to use a desktop or tablet device with a larger screens.
  #               Some graphs might be visible if you use your device in landscape mode.",
  #               type = "info",
  #               animation = TRUE,
  #               confirmButtonCol = "#3b738f"
  #               )
  #   } else {
  #   }
  # })

  
  # shinyalert(title = "Preview Version",
  #            text = "This application is currently in development.
  #            To protect the privacy and confidentiality of our participants this beta version relies on simulated data.",
  #            type = "warning",
  #            animation = TRUE,
  #            confirmButtonCol = "#3b738f"
  #            )
  
  createAlert(session = session,
              anchorId = "dataAlert",
              #alertId="a1",
              title = paste(icon("warning"),"Data Notification"),
              content="To protect the privacy of everyone who took our survey, this application only uses aggregate, anonymized data (i.e., no individual person is identifiable). 
              For further information, see our <a href='#' onclick=\"openTab('data')\">data description section</a>. Bear in mind that we display data collected over the past weeks. 
              This means the data might not be representative of how countries are doing right now. 
              Where possible, we also provide <b> nationally representative, standardized, and developmental (longitudinal) displays of the data</b>. 
              You can find these options in the left sidebar.",
              style = "warning")
  
  createAlert(session = session,
              anchorId = "longitudinalAlert",
              #alertId="a1",
              title = paste(icon("warning"),"Preliminary Data"),
              content="You are beta testing this section. We aim to provide access to more data from more countries. However, to protect the privacy of our participants, 
              we will never release visualizations of data if we cannot be certain that the data are anonymous. 
              Additionally, standardized visualization options will be available soon for all developmental visualizations.",
              style = "warning")
  
  output$dynamic_content <- renderUI({
    
    if (input$sidebarMenu == "sample") {
      
      dyn_ui <- awesomeRadio(inputId = "switch_sample", 
                             label = "Sample Selection:",
                             choices = c("Full sample" = "full",
                                         "Representative sample" = "representative"),
                             selected = select_sample)
      
    } 
    else if (input$sidebarMenu == "Variables") {
      
      dyn_ui <- list(awesomeRadio(inputId = "switch_sample", 
                                  label = "Sample Selection:",
                                  choices = c("Full sample" = "full", 
                                              "Representative sample" = "representative"),
                                  selected = select_sample),
                     awesomeRadio(inputId = "switch_transformation", 
                                  label = "Transformation:",
                                  choices = c("Raw data" = "raw", 
                                              "Standardized" = "standardized"),
                                  selected = select_transformation)
      )
    }
    else {dyn_ui <- NULL}
    
    return(dyn_ui)
  })


  
  output$sample.bar.NA <- renderText({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    test <- reactive_ctry.scales() %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      select(starts_with(input$var)) %>%
      t() %>%
      as.data.frame()
    # colnames(test) <- input$sample_country_selection
    test <- test %>%
      mutate(n = rowSums(., na.rm=TRUE),
             label = str_replace(rownames(.), ".*_", "")) %>%
      filter(n>0,
             label != "<NA>")
    
    ifelse(sum(test$n)<20, "Not enough data to display summary","")
    # ifelse(sum(test$n)<20, "","")
  })
  
  output$SampleTxt <- renderText({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    
    explanation <- list(languages = "Note: The languages people used to answer the survey. Below, you can select the countries you are interested in.", 
                        gender = "Note: The gender people identified with.",
                        age = "Note: The age of people who filled out the survey. ",
                        education = "Note: The education level of people who filled out the survey. ",
                        political = "Note: The political orientation of people who filled out the survey.")
    explanation[[input$var]]
  })
  
  output$d3.bar <- renderD3({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    #input <- list(var = "gender", sample_country_selection = c("Poland", "Romania", "Albania"))
    
    dem <- reactive_ctry.scales() %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      select(starts_with(input$var)) %>%
      t() %>%
      as.data.frame()
    # colnames(dem) <- input$sample_country_selection
    dem %>%
      mutate(n = rowSums(., na.rm=TRUE),
             label = str_replace(rownames(.), ".*_", "")) %>%
      arrange(desc(n)) %>%
      filter(n > 0,
             label != "<NA>") %>%
      mutate(y = n,
             ylabel = scales::percent(n/sum(n), accuracy = 0.01), #prettyNum(n/sum(n)*100, big.mark = ",", format = "f", digits = 2),
             fill = "#3b738f", #ifelse(label != input$val, "#E69F00", "red"),
             mouseover = "#2a5674") %>%
      r2d3(r2d3_file)
  })
  
  output$freqPlot <- renderHighchart({
    hcmap(download_map_data = FALSE,
          data = reactive_ctry.scales() %>% filter(coded_country %in% input$sample_country_selection),
          #data = ctry.only.red,
          value = "n",
          joinBy = c("iso-a2", "iso_a2"), name = "sample size",
          #dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.1,
          tooltip = list(valueDecimals = 0, valuePrefix = "n = "),
          margin = 0) %>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(minColor = "#c4e6c3", maxColor = "#1d4f60", type = "logarithmic", endOnTick=FALSE, maxPadding=0)
  })
  #Color schemes: https://carto.com/carto-colors/
  

  # -----
  # Government Response
  output$boxGov <- renderHighchart({
    # for testing:
    # input = list(psych_country_selection = c("global"))
    
    governmentRed <- reactive_ctry.scales() %>%
      select(coded_country, starts_with("gov")) %>%
      filter(coded_country %in% input$psych_country_selection) %>%
      mutate(mean = gov,
             low = gov-1.96*gov.se,
             high = gov+1.96*gov.se) %>% 
      arrange(desc(mean))
    
    tooltipJS <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> Mean: ' + Math.round((this.point.mean + Number.EPSILON) * 100) / 100 + 
                      ' <br> Lower Limit: ' + Math.round((this.point.low + Number.EPSILON) * 100) / 100 + 
                      ' <br> Upper Limit: ' + Math.round((this.point.high + Number.EPSILON) * 100) / 100)
                      }")
    categories <- c("0", "1<br>unclear", "2", "3", "4", "5", "6<br>clear")
    
    if (nrow(governmentRed) == 0) {
      highchart() %>%
        hc_title(text = "Select Countries to Display")
    } else {
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_add_series(governmentRed, "errorbar", 
                    hcaes(x = coded_country, low = low, high = high),
                    color = "#3b738f") %>% 
      hc_add_series(governmentRed, "scatter",
                    hcaes(x = coded_country, y = mean),
                    color = "#3b738f",
                    marker = list(symbol = "diamond", radius = 5, enabled = TRUE),
                    animation = list(duration = 1900),
                    showInLegend = FALSE) %>%
      hc_title(text = "To what extent are you getting clear, unambiguous messages about what to do about the Coronavirus? [Mean and 95%CI]") %>%
      hc_xAxis(categories = as.list(governmentRed$coded_country)) %>% 
      hc_tooltip(formatter = JS(tooltipJS)) -> out
      
      if (select_transformation == "raw") {
        # categories <- c("0" , "1<br>unclear", "2", "3", "4", "5", "6<br>clear")
        out %>% 
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
      }
      else {
        #categoriesS <- c("-3", "-2", "-1", "0", "1", "2", "3")
        out %>% hc_yAxis(showFirstLabel = T,
                         showLastLabel = T,
                         min = -3,
                         max = 3,
                         categories =seq(-3,-3,1),
                         step = 1,
                         #round=F,
                         #list(formatter = JS(gov.labs)), 
                         #rotation = 0,
                         #align = "center",
                         tickmarkPlacement = seq(-3,-3,1)
        )
      }
      
    }
  })
  
  
  # -----
  # Community Response
  output$boxCom <- renderHighchart({
    # for testing:
    # input = list(ComVars = "comRule", psych_country_selection = c("Germany", "France"))
    
    communityRed <- reactive_ctry.scales() %>%
      dplyr::select(coded_country, 
                    n,
                    mean = one_of(input$ComVars),
                    #sd = one_of(paste0(input$ComVars,".sd")),
                    se = one_of(paste0(input$ComVars,".se"))) %>%
      filter(coded_country %in% input$psych_country_selection) %>%
      mutate(low = mean - qt(1 - (0.05 / 2), n - 1) * se,
             high = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      arrange(desc(mean))
    
    tooltipJS <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> Mean: ' + Math.round((this.point.mean + Number.EPSILON) * 100) / 100 + 
                      ' <br> Lower Limit: ' + Math.round((this.point.low + Number.EPSILON) * 100) / 100 + 
                      ' <br> Upper Limit: ' + Math.round((this.point.high + Number.EPSILON) * 100) / 100)
                      }")
    
    title.txt <- list(comRule = "To what extent is your commmunity developing strict rules in response to the Coronavirus? [Mean and 95%CI]", 
                      comPunish = "To what extent is your commmunity punishing people who deviate from the rules that have been put in place in response to the Coronavirus? [Mean and 95%CI]", 
                      comOrg = "To what extent is your commmunity well organized in responding to the Coronavirus? [Mean and 95%CI]")
    
    categories <- c("0", "1<br>not at all", "2", "3", "4", "5", "6<br>very much")
    
    if (nrow(communityRed) == 0) {
      highchart() %>%
        hc_title(text = "Select Countries to Display")
    } else {
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_add_series(communityRed, "errorbar", 
                    hcaes(x = coded_country, low = low, high = high),
                    color = "#3b738f") %>% 
      hc_add_series(communityRed, "scatter",
                    hcaes(x = coded_country, y = mean),
                    color = "#3b738f",
                    marker = list(symbol = "diamond", radius = 5, enabled = TRUE),
                    animation = list(duration = 1900),
                    showInLegend = FALSE) %>%
      hc_title(text = title.txt[[input$ComVars]]) %>% 
      hc_xAxis(categories = as.list(communityRed$coded_country)) %>%
      hc_tooltip(formatter = JS(tooltipJS)) -> out
      
      if (select_transformation == "raw") {
        out %>% hc_yAxis(showFirstLabel = T,
                         showLastLabel = T,
                         min = 1,
                         max = 6,
                         #step = 1,
                         #list(formatter = JS(gov.labs)), 
                         #rotation = 0,
                         categories = categories,
                         #align = "center",
                         tickmarkPlacement = seq(1,6,1))
          
      }
      else {
        # categories <- c("-2", "-1<br>unclear", "0", "1<br>clear")
        out %>% hc_yAxis(showFirstLabel = T,
                         showLastLabel = T,
                         min = -3,
                         max = 3,
                         step = 1,
                         #list(formatter = JS(gov.labs)), 
                         #rotation = 0,
                         categories = seq(-3,-3,1),
                         #align = "center",
                         tickmarkPlacement = seq(-3,-3,1)
        )
      }
      
    }
  })
  
  
  # -----
  # Cognitive Response
  output$boxCog <- renderHighchart({
    # for testing:
    # input = list(CogVars = "covidHope", cog_country_selection = c("Germany", "France"))
    
    cognitiveRed <- reactive_ctry.scales() %>%
      dplyr::select(coded_country, 
                    n,
                    mean = one_of(input$CogVars),
                    #sd = one_of(paste0(input$ComVars,".sd")),
                    se = one_of(paste0(input$CogVars,".se"))) %>%
      filter(coded_country %in% input$psych_country_selection) %>%
      mutate(low = mean - qt(1 - (0.05 / 2), n - 1) * se,
             high = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
      arrange(desc(mean))
    
    tooltipJS <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> Mean: ' + Math.round((this.point.mean + Number.EPSILON) * 100) / 100 + 
                      ' <br> Lower Limit: ' + Math.round((this.point.low + Number.EPSILON) * 100) / 100 + 
                      ' <br> Upper Limit: ' + Math.round((this.point.high + Number.EPSILON) * 100) / 100)
                      }")
    
    title.txt <- list(covidHope = "I have high hopes that the situation regarding coronavirus will improve. [Mean and 95%CI]", 
                      covidEff = "I think that this country is able to fight the Coronavirus. [Mean and 95%CI]",
                      lone = "Mean Loneliness Scores [Mean and 95%CI]",
                      para = "Mean State Paranoia Scores [Mean and 95%CI]",
                      consp = "Mean Conspiracy Scores [Mean and 95%CI]")
    
    if (select_transformation == "raw"){
      y.min = list(covidHope = -3, 
                   covidEff = -3,
                   lone = 1,
                   para = 0,
                   consp = 0)
      y.max <- list(covidHope = 3, 
                    covidEff = 3,
                    lone = 5,
                    para = 10,
                    consp = 10)
      lab.ticks <- list(covidHope = c("0"), 
                        covidEff = c("0"), 
                        lone = c("0"),
                        para = c("0"),
                        consp = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"))
      lab.breaks <- list(covidHope = seq(0,7,1), 
                         covidEff = seq(0,7,1), 
                         lone = seq(1,5,1),
                         para = seq(0,10,1),
                         consp = seq(0,10,1))
      lab.ends <- list(covidHope = c("-3<br>disagree", "3<br>agree"), 
                       covidEff = c("-3<br>disagree", "3<br>agree"), 
                       lone = c("1<br>Never", "5<br>All the time"),
                       para = c("0<br>Not at all", "10<br>Very much"),
                       consp = c("0%", "100%"))}
    else if (select_transformation == "standardized"){
      y.min = list(covidHope = -3, 
                   covidEff = -3,
                   lone = -3,
                   para = -3,
                   consp = -3)
      y.max <- list(covidHope = 3, 
                    covidEff = 3,
                    lone = 3,
                    para = 3,
                    consp = 3)
      lab.ticks <- list(covidHope = c("0"), 
                        covidEff = c("0"), 
                        lone = c("0"),
                        para = c("0"),
                        consp = c("0"))
      lab.breaks <- list(covidHope = seq(-3,3,1), 
                         covidEff = seq(-3,3,1), 
                         lone = seq(-3,3,1),
                         para = seq(-3,3,1),
                         consp = seq(-3,3,1))
      lab.ends = list(covidHope = c("-3", "3"), 
                      covidEff = c("-3", "3"), 
                      lone = c("-3", "3"),
                      para = c("-3", "3"),
                      consp = c("-3", "3"))}
    
    
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
    
    if (nrow(cognitiveRed) == 0) {
      highchart() %>%
        hc_title(text = "Select Countries to Display")
    } else {
    highchart() %>% 
      hc_chart(type = "bar") %>% 
      hc_add_series(cognitiveRed, "errorbar", 
                    hcaes(x = coded_country, low = low, high = high),
                    color = "#3b738f") %>% 
      hc_add_series(cognitiveRed, "scatter",
                    hcaes(x = coded_country, y = mean),
                    color = "#3b738f",
                    marker = list(symbol = "diamond", radius = 5, enabled = TRUE),
                    animation = list(duration = 1900),
                    showInLegend = FALSE) %>%
      hc_title(text = title.txt[[input$CogVars]]) %>% 
      hc_xAxis(categories = as.list(cognitiveRed$coded_country)) %>%
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
               tickmarkPlacement = lab.breaks[[input$CogVars]]) %>%
      hc_tooltip(formatter = JS(tooltipJS))
    }
  })
  
  
  # -----
  # Behavioral Response
  output$boxBeh <- renderUI({
    # for testing:
    # input = list(BehVars = "avoid", beh_country_selection = c("Germany", "France"))
    
    if (input$BehVars == "iso") {
      
      behaviorRedIso <- reactive_ctry.scales() %>%
        dplyr::select(coded_country, 
                      n,
                      isoPers,
                      isoOnl,
                      isoPers.se,
                      isoOnl.se) %>%
        filter(coded_country %in% input$psych_country_selection) %>%
        mutate(lowPers = isoPers - qt(1 - (0.05 / 2), n - 1) * isoPers.se,
               highPers = isoPers + qt(1 - (0.05 / 2), n - 1) * isoPers.se,
               lowOnl = isoOnl - qt(1 - (0.05 / 2), n - 1) * isoOnl.se,
               highOnl = isoOnl + qt(1 - (0.05 / 2), n - 1) * isoOnl.se) %>% 
        arrange(desc(isoPers))
      
      tooltipJSPers <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> Mean: ' + Math.round((this.point.isoPers + Number.EPSILON) * 100) / 100 + 
                      ' <br> Lower Limit: ' + Math.round((this.point.lowPers + Number.EPSILON) * 100) / 100 + 
                      ' <br> Upper Limit: ' + Math.round((this.point.highPers + Number.EPSILON) * 100) / 100)
                      }")
      tooltipJSOnl <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> Mean: ' + Math.round((this.point.isoOnl + Number.EPSILON) * 100) / 100 + 
                      ' <br> Lower Limit: ' + Math.round((this.point.lowOnl + Number.EPSILON) * 100) / 100 + 
                      ' <br> Upper Limit: ' + Math.round((this.point.highOnl + Number.EPSILON) * 100) / 100)
                      }")
      
      if (nrow(behaviorRedIso) == 0) {
        highchart() %>%
          hc_title(text = "Select Countries to Display") %>%
        hw_grid(ncol = 1, rowheight = "400")
      } else {
      hcPers <- highchart() %>% 
        hc_chart(type = "bar") %>% 
        hc_add_series(behaviorRedIso, "errorbar", 
                      hcaes(x = coded_country, low = lowPers, high = highPers),
                      color = "#3b738f") %>% 
        hc_add_series(behaviorRedIso, "scatter",
                      hcaes(x = coded_country, y = isoPers),
                      color = "#3b738f",
                      marker = list(symbol = "diamond", radius = 5, enabled = TRUE),
                      animation = list(duration = 1900),
                      showInLegend = FALSE) %>%
        hc_title(text = "Number of days per week with <b>in-person</b> contacts") %>%
        hc_xAxis(categories = as.list(behaviorRedIso$coded_country)) %>%
        hc_tooltip(formatter = JS(tooltipJSPers))

      
      if (select_transformation == "raw") {
        hcPers <- hcPers %>% hc_yAxis(min = 0,
                         max = 7,
                         categories = seq(0,7,1),
                         tickmarkPlacement = seq(0,7,1))
          
      }
      else {
        hcPers <- hcPers %>% hc_yAxis(min = -3,
                                      max = 3,
                                      step = 1,
                                      #list(formatter = JS(gov.labs)), 
                                      #rotation = 0,
                                      categories = seq(-3,-3,1),
                                      #align = "center",
                                      tickmarkPlacement = seq(-3,-3,1))
      }
      
      
      hcOnli <- highchart() %>% 
        hc_chart(type = "bar") %>% 
        hc_add_series(behaviorRedIso, "errorbar", 
                      hcaes(x = coded_country, low = lowOnl, high = highOnl),
                      color = "#3b738f") %>% 
        hc_add_series(behaviorRedIso, "scatter",
                      hcaes(x = coded_country, y = isoOnl),
                      color = "#3b738f",
                      marker = list(symbol = "diamond", radius = 5, enabled = TRUE),
                      animation = list(duration = 1900),
                      showInLegend = FALSE) %>%
        hc_title(text = "Number of days per week with <b>online</b> contacts") %>%
        hc_xAxis(categories = as.list(behaviorRedIso$coded_country)) %>%
        hc_tooltip(formatter = JS(tooltipJSOnl))
      
      
      if (select_transformation == "raw") {
        hcOnli <- hcOnli %>% hc_yAxis(min = 0,
                                      max = 7,
                                      categories = seq(0,7,1),
                                      tickmarkPlacement = seq(0,7,1))
          
      }
      else {
        hcOnli <- hcOnli %>% hc_yAxis(min = -3,
                                      max = 3,
                                      step = 1,
                                      #list(formatter = JS(gov.labs)), 
                                      #rotation = 0,
                                      categories = seq(-3,-3,1),
                                      #align = "center",
                                      tickmarkPlacement = seq(-3,-3,1))
          
      }
      lst <- list(hcPers, hcOnli)
      hw_grid(lst, ncol = 2, rowheight = "400")
      
      }
      
    } else {
      
      behaviorRed <- reactive_ctry.scales() %>%
        dplyr::select(coded_country, 
                      n,
                      mean = one_of(input$BehVars),
                      #sd = one_of(paste0(input$ComVars,".sd")),
                      se = one_of(paste0(input$BehVars,".se"))) %>%
        filter(coded_country %in% input$psych_country_selection) %>%
        mutate(low = mean - qt(1 - (0.05 / 2), n - 1) * se,
               high = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
        arrange(desc(mean))
      
      tooltipJS <- paste0("function(){
                      return ('Country: ' + this.point.coded_country + 
                      ' <br> Mean: ' + Math.round((this.point.mean + Number.EPSILON) * 100) / 100 + 
                      ' <br> Lower Limit: ' + Math.round((this.point.low + Number.EPSILON) * 100) / 100 + 
                      ' <br> Upper Limit: ' + Math.round((this.point.high + Number.EPSILON) * 100) / 100)
                      }")
      
      
      title.txt <- list(behWash = "To minimize my chances of getting corona virus I wash my hands more often.  [Mean and 95%CI]", 
                        behAvoid = "To minimize my chances of getting corona virus I avoid crowded spaces.  [Mean and 95%CI]")
      
      y.min <- list(behWash = -3, 
                    behAvoid = -3)
      y.max <- list(behWash = 3, 
                    behAvoid = 3)
      
      lab.ends <- list(behWash = c("-3<br>disagree", "3<br>agree"), 
                       behAvoid = c("-3<br>disagree", "3<br>agree"))
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
      
      if (nrow(behaviorRed) == 0) {
        highchart() %>%
          hc_title(text = "Select Countries to Display") %>%
        hw_grid(ncol = 1, rowheight = "400")
      } else {
      highchart() %>% 
        hc_chart(type = "bar") %>% 
        hc_add_series(behaviorRed, "errorbar", 
                      hcaes(x = coded_country, low = low, high = high),
                      color = "#3b738f") %>% 
        hc_add_series(behaviorRed, "scatter",
                      hcaes(x = coded_country, y = mean),
                      color = "#3b738f",
                      marker = list(symbol = "diamond", radius = 5, enabled = TRUE),
                      animation = list(duration = 1900),
                      showInLegend = FALSE) %>%
        hc_title(text = title.txt[[input$BehVars]]) %>% 
        hc_xAxis(categories = as.list(behaviorRed$coded_country)) %>%
        hc_tooltip(formatter = JS(tooltipJS)) -> out
        
        if (select_transformation == "raw") {
          out %>% hc_yAxis(showFirstLabel = T,
                           showLastLabel = T,
                           min = y.min[[input$BehVars]],
                           max = y.max[[input$BehVars]],
                           tickInterval = 1,
                           labels = list(formatter = JS(lab.ends.js)), 
                           categories = c("0"),
                           tickmarkPlacement = seq(0,7,1)) %>% 
            hw_grid(ncol = 1, rowheight = "400")
        }
        else {
          out %>% hc_yAxis(showFirstLabel = T,
                           showLastLabel = T,
                           min = y.min[[input$BehVars]],
                           max = y.max[[input$BehVars]],
                           #tickInterval = 1,
                           steps = 1,
                           categories = c("0"),
                           tickmarkPlacement = seq(-3,3,by=1)) %>% 
            hw_grid(ncol = 1, rowheight = "400")
        }

        
      }
    }
  })
  
  
  # -----
  # Emotional Response
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
                          reactive_ctry.scales() %>%
                            filter(coded_country %in% input$psych_country_selection) %>% 
                            dplyr::select(one_of(vars))
                        )
    )
    names(radar) <- c("label", input$psych_country_selection)
    
    if (select_transformation == "raw") {
      chartJSRadar(radar, maxScale = 5, showToolTipLabel=TRUE, showLegend = T, responsive = T, 
                   labelSize = 12) 
    }
    else {
      chartJSRadar(radar, maxScale = 3, showToolTipLabel=TRUE, showLegend = T, responsive = T, 
                   labelSize = 12, scaleStartValue = -3)
    }
  })
  
  
  # -----
  # Cross Domain Relationships
  output$cor <- renderHighchart({
    # for testing:
    # input = list(CorX = "comRule", CorY = "covidHope")
    
    cor.dat <- reactive_ctry.scales() %>%
      dplyr::select(coded_country, n,
                    xvar = one_of(input$CorX),
                    yvar = one_of(input$CorY)) %>%
      filter(coded_country != "global")
    
    varLab <- c("gov"="Clear Government Information",
                "comRule"="Community Rules", "comPunish"="Community Punishment", "comOrg"="Community Organization",
                "covidHope"="Hope", "covidEff"="Efficacy", "lone"="Loneliness", "para"="State Paranoia", "consp"="Conspiracy",
                "behWash"="Washing", "behAvoid"="Avoiding", "isoPers"="Isolation Offline", "isoOnl"="Isolation Online",
                "affAnx"="Anxious", "affBor"="Bored", "affCalm"="Calm", "affContent"="Content", "affDepr"="Depressed", "affEnerg"="Energetic", 
                "affExc"="Excited", "affNerv"="Nervous", "affExh"="Exhausted", "affInsp"="Inspired", "affRel"="Relaxed",
                "affHighPos"="High Arousal Positive", "affHighNeg"="High Arousal Negative",
                "affLowPos"="Low Arousal Positive", "affLowNeg"="Low Arousal Negative")
    
    if (nrow(cor.dat) == 0) {
      highchart() %>%
        hc_title(text = "Select Countries to Display")
    } else {
      
      
      if (select_transformation == "raw") {
        min <- list(gov = 1,
                   comRule = 1,
                   comPunish = 1,
                   comOrg = 1,
                   covidHope = -3,
                   covidEff = -3,
                   lone = 1,
                   para = 0,
                   consp = 0,
                   behWash = -3,
                   behAvoid = -3,
                   isoPers = 0,
                   isoOnl = 0,
                   affAnx = 1,
                   affBor = 1,
                   affCalm = 1,
                   affContent = 1,
                   affDepr = 1,
                   affEnerg = 1,
                   affExc = 1,
                   affNerv = 1,
                   affExh = 1,
                   affInsp = 1,
                   affRel = 1,
                   affHighPos = 1,
                   affHighNeg = 1,
                   affLowPos = 1,
                   affLowNeg = 1)
        
        max <- list(gov = 6,
                    comRule = 6,
                    comPunish = 6,
                    comOrg = 6,
                    covidHope = 3,
                    covidEff = 3,
                    lone = 5,
                    para = 10,
                    consp = 10,
                    behWash = 3,
                    behAvoid = 3,
                    isoPers = 7,
                    isoOnl = 7,
                    affAnx = 5,
                    affBor = 5,
                    affCalm = 5,
                    affContent = 5,
                    affDepr = 5,
                    affEnerg = 5, 
                    affExc = 5, 
                    affNerv = 5, 
                    affExh = 5, 
                    affInsp = 5, 
                    affRel = 5,
                    affHighPos = 5, 
                    affHighNeg = 5,
                    affLowPos = 5, 
                    affLowNeg = 5)
        lab.x.ends <- list(gov = c("1<br>unclear", "6<br>clear"), 
                         comRule = c("1<br>not at all", "6<br>very much"),
                         comPunish = c("1<br>not at all", "6<br>very much"),
                         comOrg = c("1<br>not at all", "6<br>very much"),
                         covidHope = c("-3<br>disagree", "3<br>agree"), 
                         covidEff = c("-3<br>disagree", "3<br>agree"), 
                         lone = c("1<br>Never", "5<br>All the time"),
                         para = c("0<br>Not at all", "10<br>Very much"),
                         consp = c("0%", "100%"),
                         behWash = c("-3<br>disagree", "3<br>agree"), 
                         behAvoid = c("-3<br>disagree", "3<br>agree"),
                         isoPers = c(1,7),
                         isoOnl = c(1,7),
                         affAnx = c("1<br>not at all", "5<br>very much"),
                         affBor = c("1<br>not at all", "5<br>very much"),
                         affCalm = c("1<br>not at all", "5<br>very much"),
                         affContent = c("1<br>not at all", "5<br>very much"),
                         affDepr = c("1<br>not at all", "5<br>very much"),
                         affEnerg = c("1<br>not at all", "5<br>very much"),
                         affExc = c("1<br>not at all", "5<br>very much"),
                         affNerv = c("1<br>not at all", "5<br>very much"),
                         affExh = c("1<br>not at all", "5<br>very much"),
                         affInsp = c("1<br>not at all", "5<br>very much"),
                         affRel = c("1<br>not at all", "5<br>very much"),
                         affHighPos = c("1<br>not at all", "5<br>very much"),
                         affHighNeg = c("1<br>not at all", "5<br>very much"),
                         affLowPos = c("1<br>not at all", "5<br>very much"),
                         affLowNeg = c("1<br>not at all", "5<br>very much"))
        lab.y.ends <- list(gov = c("unclear 1", "clear 6"), 
                           comRule = c("not at all 1", "very much 6"),
                           comPunish = c("not at all 1", "very much 6"),
                           comOrg = c("not at all 1", "very much 6"),
                           covidHope = c("disagree -3", "agree 3"), 
                           covidEff = c("disagree -3", "agree 3"), 
                           lone = c("Never 1", "All the time 5"),
                           para = c("Not at all 0", "Very much 10"),
                           consp = c("0%", "100%"),
                           behWash = c("disagree -3", "agree 3"), 
                           behAvoid = c("disagree -3", "agree 3"), 
                           isoPers = c(1,7),
                           isoOnl = c(1,7),
                           affAnx = c("not at all 1", "very much 5"),
                           affBor = c("not at all 1", "very much 5"),
                           affCalm = c("not at all 1", "very much 5"),
                           affContent = c("not at all 1", "very much 5"),
                           affDepr = c("not at all 1", "very much 5"),
                           affEnerg = c("not at all 1", "very much 5"),
                           affExc = c("not at all 1", "very much 5"),
                           affNerv = c("not at all 1", "very much 5"),
                           affExh = c("not at all 1", "very much 5"),
                           affInsp = c("not at all 1", "very much 5"),
                           affRel = c("not at all 1", "very much 5"),
                           affHighPos = c("not at all 1", "very much 5"),
                           affHighNeg = c("not at all 1", "very much 5"),
                           affLowPos = c("not at all 1", "very much 5"),
                           affLowNeg = c("not at all 1", "very much 5"))
      }
      else {
        min <- list(gov = -2,
                    comRule = -2,
                    comPunish = -2,
                    comOrg = -2,
                    covidHope = -2,
                    covidEff = -2,
                    lone = -2,
                    para = -2,
                    consp = -2,
                    behWash = -2,
                    behAvoid = -2,
                    isoPers = -2,
                    isoOnl = -2,
                    affAnx = -2,
                    affBor = -2,
                    affCalm = -2,
                    affContent = -2,
                    affDepr = -2,
                    affEnerg = -2,
                    affExc = -2,
                    affNerv = -2,
                    affExh = -2,
                    affInsp = -2,
                    affRel = -2,
                    affHighPos = -2,
                    affHighNeg = -2,
                    affLowPos = -2,
                    affLowNeg = -2)
        
        max <- list(gov = 2,
                    comRule = 2,
                    comPunish = 2,
                    comOrg = 2,
                    covidHope = 2,
                    covidEff = 2,
                    lone = 2,
                    para = 2,
                    consp = 2,
                    behWash = 2,
                    behAvoid = 2,
                    isoPers = 2,
                    isoOnl = 2,
                    affAnx = 2,
                    affBor = 2,
                    affCalm = 2,
                    affContent = 2,
                    affDepr = 2,
                    affEnerg = 2, 
                    affExc = 2, 
                    affNerv = 2, 
                    affExh = 2, 
                    affInsp = 2, 
                    affRel = 2,
                    affHighPos = 2, 
                    affHighNeg = 2,
                    affLowPos = 2, 
                    affLowNeg = 2)
        lab.x.ends <- list(gov = c("-2", "2"), 
                           comRule = c("-2", "2"),
                           comPunish = c("-2", "2"),
                           comOrg = c("-2", "2"),
                           covidHope = c("-2", "2"), 
                           covidEff = c("-2", "2"), 
                           lone = c("-2", "2"),
                           para = c("-2", "2"),
                           consp = c("-2", "2"),
                           behWash = c("-2", "2"), 
                           behAvoid = c("-2", "2"),
                           isoPers = c("-2", "2"),
                           isoOnl = c("-2", "2"),
                           affAnx = c("-2", "2"),
                           affBor = c("-2", "2"),
                           affCalm = c("-2", "2"),
                           affContent = c("-2", "2"),
                           affDepr = c("-2", "2"),
                           affEnerg = c("-2", "2"),
                           affExc = c("-2", "2"),
                           affNerv = c("-2", "2"),
                           affExh = c("-2", "2"),
                           affInsp = c("-2", "2"),
                           affRel = c("-2", "2"),
                           affHighPos = c("-2", "2"),
                           affHighNeg = c("-2", "2"),
                           affLowPos = c("-2", "2"),
                           affLowNeg = c("-2", "2"))
        lab.y.ends <- list(gov = c("-2", "2"), 
                           comRule = c("-2", "2"),
                           comPunish = c("-2", "2"),
                           comOrg = c("-2", "2"),
                           covidHope = c("-2", "2"), 
                           covidEff = c("-2", "2"), 
                           lone = c("-2", "2"),
                           para = c("-2", "2"),
                           consp = c("-2", "2"),
                           behWash = c("-2", "2"), 
                           behAvoid = c("-2", "2"), 
                           isoPers = c("-2", "2"),
                           isoOnl = c("-2", "2"),
                           affAnx = c("-2", "2"),
                           affBor = c("-2", "2"),
                           affCalm = c("-2", "2"),
                           affContent = c("-2", "2"),
                           affDepr = c("-2", "2"),
                           affEnerg = c("-2", "2"),
                           affExc = c("-2", "2"),
                           affNerv = c("-2", "2"),
                           affExh = c("-2", "2"),
                           affInsp = c("-2", "2"),
                           affRel = c("-2", "2"),
                           affHighPos = c("-2", "2"),
                           affHighNeg = c("-2", "2"),
                           affLowPos = c("-2", "2"),
                           affLowNeg = c("-2", "2"))
      }
      
      
      title.txt <- list(gov = "To what extent are you getting clear, unambiguous messages about what to do about the Coronavirus?",
                        comRule = "To what extent is your commmunity developing strict rules in response to the Coronavirus?", 
                        comPunish = "To what extent is your commmunity punishing people who deviate from the rules that have been put in place in response to the Coronavirus?", 
                        comOrg = "To what extent is your commmunity well organized in responding to the Coronavirus?",
                        covidHope = "I have high hopes that the situation regarding coronavirus will improve.", 
                        covidEff = "I think that this country is able to fight the Coronavirus.",
                        lone = "Mean Loneliness Scores",
                        para = "Mean State Paranoia Scores",
                        consp = "Mean Conspiracy Scores",
                        behWash = "To minimize my chances of getting corona virus I wash my hands more often.", 
                        behAvoid = "To minimize my chances of getting corona virus I avoid crowded spaces.",
                        isoPers = "Number of days per week with <b>in-person</b> contacts", 
                        isoOnl = "Number of days per week with <b>online</b> contacts",
                        affAnx = "Anxious", 
                        affBor = "Bored", 
                        affCalm = "Calm", 
                        affContent = "Content", 
                        affDepr = "Depressed", 
                        affEnerg = "Energetic", 
                        affExc = "Excited", 
                        affNerv = "Nervous", 
                        affExh = "Exhausted", 
                        affInsp = "Inspired", 
                        affRel = "Relaxed",
                        affHighPos = "High Arousal Positive", 
                        affHighNeg = "High Arousal Negative",
                        affLowPos = "Low Arousal Positive", 
                        affLowNeg = "Low Arousal Negative")
      
      # lab.ticks <- list(covidHope = c("0"), 
      #                   covidEff = c("0"), 
      #                   lone = c("0"),
      #                   para = c("0"),
      #                   consp = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"))
      # lab.breaks <- list(covidHope = seq(0,7,1), 
      #                    covidEff = seq(0,7,1), 
      #                    lone = seq(1,5,1),
      #                    para = seq(0,10,1),
      #                    consp = seq(0,10,1))
      
      lab.ends.x.js <- paste0("function(){console.log(this);
                                        if(this.isFirst){
                                        return '",
                            lab.x.ends[input$CorX][[1]][1],
                            "'} else if(this.isLast) {return '",
                            lab.x.ends[input$CorX][[1]][2],
                            "'} else {
                                        return this.value
                                        }
                                        }")
      
      lab.ends.y.js <- paste0("function(){console.log(this);
                                        if(this.isFirst){
                                        return '",
                              lab.y.ends[input$CorY][[1]][1],
                              "'} else if(this.isLast) {return '",
                              lab.y.ends[input$CorY][[1]][2],
                              "'} else {
                                        return this.value
                                        }
                                        }")
      
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
        hc_xAxis(title = list(text = as.character(varLab[input$CorX])),
                 min = min[[input$CorX]],
                 max = max[[input$CorX]],
                 tickInterval = 1,
                 labels = list(formatter = JS(lab.ends.x.js))) %>%
        hc_yAxis(title = list(text = as.character(varLab[input$CorY])),
                 min = min[[input$CorY]],
                 max = max[[input$CorY]],
                 tickInterval = 1,
                 labels = list(formatter = JS(lab.ends.y.js))) %>%
        hc_tooltip(formatter = JS(tooltipJS)) # %>%
        # hc_exporting(
        #   enabled = TRUE, # always enabled
        #   width = '1200',
        #   filename = "Correlation01"
        # )
    }
  })

  # -----
  # Longitudinal 
  
  titleTxtLong <- list(affAnx = "Anxious", 
                       affBor = "Bored", 
                       affCalm = "Calm", 
                       affDepr = "Depressed", 
                       affEnerg = "Energetic",  
                       affNerv = "Nervous", 
                       affExh = "Exhausted", 
                       affInsp = "Inspired", 
                       affRel = "Relaxed",
                       lone01 = "During the past week, did you feel lonely?",
                       para01 = "I need to be on my guard against others.",
                       consp01 = "I think that many very important things happen in the world, which the public is never informed about.",
                       isoFriends_inPerson = "Number of days per week with <b>in-person</b> contacts with friends.", 
                       isoOthPpl_inPerson = "Number of days per week with <b>in-person</b> contacts with other people.", 
                       isoFriends_online = "Number of days per week with <b>online</b> contacts with friends.",
                       isoOthPpl_online = "Number of days per week with <b>online</b> contacts with other people.")
  
  varLabLong <- c("affAnx"="Anxious", 
                  "affBor"="Bored", 
                  "affCalm"="Calm", 
                  "affDepr"="Depressed", 
                  "affEnerg"="Energetic",
                  "affNerv"="Nervous", 
                  "affExh"="Exhausted", 
                  "affInsp"="Inspired", 
                  "affRel"="Relaxed",
                  "lone01"="Loneliness", 
                  "para01"="State Paranoia", 
                  "consp01"="Conspiracy",
                  "isoFriends_inPerson"="Friends Isolation Offline", 
                  "isoOthPpl_inPerson"="Others Isolation Offline", 
                  "isoFriends_online"="Friends Isolation Online",
                  "isoOthPpl_online"="Others Isolation Online")
  
  minLong <- list(affAnx = 1,
                  affBor = 1,
                  affCalm = 1,
                  affDepr = 1,
                  affEnerg = 1,
                  affNerv = 1,
                  affExh = 1,
                  affInsp = 1,
                  affRel = 1,
                  lone01 = 0,
                  para01 = 0,
                  consp01 = 0,
                  isoFriends_inPerson = 0,
                  isoOthPpl_inPerson = 0,
                  isoFriends_online = 0,
                  isoOthPpl_online = 0)
  
  maxLong <- list(affAnx = 5,
                  affBor = 5,
                  affCalm = 5,
                  affDepr = 5,
                  affEnerg = 5, 
                  affNerv = 5, 
                  affExh = 5, 
                  affInsp = 5, 
                  affRel = 5,
                  lone01 = 5,
                  para01 = 10,
                  consp01 = 10,
                  isoFriends_inPerson = 7,
                  isoOthPpl_inPerson = 7,
                  isoFriends_online = 7,
                  isoOthPpl_online = 7)
  
  lab.y.ends.long <- list(affAnx = c("not at all", "very much"),
                          affBor = c("not at all", "very much"),
                          affCalm = c("not at all", "very much"),
                          affDepr = c("not at all", "very much"),
                          affEnerg = c("not at all", "very much"),
                          affNerv = c("not at all", "very much"),
                          affExh = c("not at all", "very much"),
                          affInsp = c("not at all", "very much"),
                          affRel = c("not at all", "very much"),
                          lone01 = c("Never", "All the time"),
                          para01 = c("Not at all", "Very much"),
                          consp01 = c("0%", "100%"),
                          isoFriends_inPerson = c(),
                          isoOthPpl_inPerson = c(),
                          isoFriends_online = c(),
                          isoOthPpl_online = c())
  
  output$long_ctrs_hc <- renderHighchart({
    # for testing:
    # input <- list(long_ctrs_country_selection = c("United States of America"),
    #               long_ctrs_var = c("lone01"),
    #               long_vars_country = c("United States of America"),
    #               long_vars_variables = c("affBor", "affDepr"),
    #               long_ctrs_CiSwitch = TRUE)
  
    weeklySelection <- weekly %>% 
      ungroup() %>% 
      dplyr::select(coded_country, weekDate, value = one_of(paste0(input$long_ctrs_var, "_mean"))) %>%
      filter(coded_country %in% input$long_ctrs_country_selection, !is.na(value)) %>% 
      group_by(coded_country) %>%
      do(ds = list(
        data = highcharter::list_parse2(data.frame(datetime_to_timestamp(.$weekDate), .$value)),
        type = "spline",
        showInLegend = TRUE
      )) %>% 
      {purrr::map2(.$coded_country, .$ds, function(x, y){
        append(list(name = x), y)
      })}
    
    weeklySelectionCI <- weekly %>% 
      ungroup() %>% 
      dplyr::select(coded_country, weekDate, lwr = one_of(paste0(input$long_ctrs_var, "_lwr")), upr = one_of(paste0(input$long_ctrs_var, "_upr"))) %>%
      filter(coded_country %in% input$long_ctrs_country_selection, !is.na(lwr)) %>% 
      group_by(coded_country) %>%
      do(ds = list(data = highcharter::list_parse2(data.frame(datetime_to_timestamp(.$weekDate), .$lwr, .$upr)),
                   type = 'areasplinerange',
                   fillOpacity = 0.3,
                   lineWidth = 0,
                   name = "95% Confidence Interval",
                   zIndex = 0, 
                   showInLegend = FALSE,
                   marker = list(enabled = FALSE,
                                 states = list(hover = list(enabled = FALSE))),
                   visible = as.logical(input$long_ctrs_CiSwitch)
      )) %>% 
      {purrr::map2(.$coded_country, .$ds, function(x, y){
        append(list(name = x), y)
      })}
    
    highchart() %>% 
      hc_chart(zoomType = "x") %>% 
      hc_add_series_list(weeklySelection) %>% 
      hc_add_series_list(weeklySelectionCI) %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_yAxis(title = list(text = as.character(varLabLong[input$long_ctrs_var])),
               min = minLong[[input$long_ctrs_var]],
               max = maxLong[[input$long_ctrs_var]],
               showLastLabel = T,
               showFirstLabel = T,
               tickInterval = 1,
               opposite = F,
               plotLines = list(
                 list(label = list(text = lab.y.ends.long[input$long_ctrs_var][[1]][1]),
                      color = "#'FF0000",
                      width = 2,
                      value = minLong[[input$long_ctrs_var]]),
                 list(label = list(text = lab.y.ends.long[input$long_ctrs_var][[1]][2]),
                      color = "#'FF0000",
                      width = 2,
                      value = maxLong[[input$long_ctrs_var]]))) %>%
      hc_plotOptions(
        spline = list(
          #color = brewer.pal(length(input$long_ctrs_country_selection), "Dark2"),
          dataLabels = list(enabled = F),
          enableMouseTracking = TRUE, 
          showInNavigator = TRUE),
        arearange = list(
          #color = brewer.pal(length(input$long_ctrs_country_selection), "Dark2"),
          enableMouseTracking = FALSE, 
          marker = list(enabled = FALSE)),
        series = list(events = list(legendItemClick = JS("function(e) { return false; } ")))
        ) %>%
      hc_navigator(enabled = TRUE,
                   maskFill = 'rgba(180, 198, 220, 0.25)',
                   series = list(
                     type = "spline" # you can change the type
                     )) %>%
      hc_colors(brewer.pal(length(input$long_ctrs_country_selection), "Dark2")[1:length(input$long_ctrs_country_selection)]) %>%
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_title(text = titleTxtLong[[input$long_ctrs_var]]) %>%
      hc_tooltip(crosshairs = TRUE) %>%
      hc_rangeSelector( 
        enabled = TRUE,
        verticalAlign = "top",
        buttons = list(
          list(type = 'all', text = 'All'),
          list(type = 'month', count = 3, text = '4m'),
          list(type = 'month', count = 2, text = '2m'),
          list(type = 'week', count = 6, text = '6w')
        )) #%>%
      # hc_size(., width = 1000, height = 500) %>%
      # hc_exporting(
      #   enabled = TRUE, # always enabled
      #   filename = "hc-longitudinal"
      # )
  })
  
  output$long_vars_hc <- renderHighchart({
    # for testing:
    # input <- list(long_ctrs_country_selection = c("United States of America", "Germany"), long_ctrs_var = c("affBor_mean"))
    
    weeklySelection <- weekly %>%
      ungroup() %>%
      filter(coded_country %in% input$long_vars_country) %>%
      dplyr::select(coded_country, weekDate, one_of(paste0(input$long_vars_variables, "_mean"))) %>%
      reshape(., 
              direction = "long",
              varying = list(paste0(input$long_vars_variables, "_mean")),
              timevar = "variable",
              v.names = "value",
              idvar = c("coded_country", "weekDate"),
              times = input$long_vars_variables) %>%
      group_by(variable) %>%
      do(ds = list(
        data = highcharter::list_parse2(data.frame(datetime_to_timestamp(.$weekDate), .$value)),
        type = "spline",
        showInLegend = TRUE
      )) %>%
      {purrr::map2(.$variable, .$ds, function(x, y){
        append(list(name = x), y)
      })}
    
    weeklySelectionCI <- weekly %>%
      ungroup() %>%
      filter(coded_country %in% input$long_vars_country) %>%
      dplyr::select(coded_country, weekDate, one_of(sort(c(paste0(input$long_vars_variables, "_lwr"), paste0(input$long_vars_variables, "_upr"))))) %>%
      reshape(., 
              direction='long', 
              varying=sort(c(paste0(input$long_vars_variables, "_lwr"), paste0(input$long_vars_variables, "_upr"))), 
              timevar='variable',
              times=input$long_vars_variables,
              v.names=c('lwr', 'upr'),
              idvar=c("coded_country", "weekDate")) %>%
      group_by(variable) %>%
      do(ds = list(data = highcharter::list_parse2(data.frame(datetime_to_timestamp(.$weekDate), .$lwr, .$upr)),
                   type = 'arearange',
                   fillOpacity = 0.3,
                   lineWidth = 0,
                   name = "95% Confidence Interval",
                   zIndex = 0,
                   showInLegend = FALSE,
                   visible = as.logical(input$long_vars_CiSwitch)
      )) %>%
      {purrr::map2(.$variable, .$ds, function(x, y){
        append(list(name = x), y)
      })}
    
    highchart() %>% 
      hc_chart(zoomType = "x") %>% 
      hc_add_series_list(weeklySelection) %>% 
      hc_add_series_list(weeklySelectionCI) %>% 
      hc_xAxis(type = "datetime") %>% 
      # hc_yAxis(title = list(text = as.character(varLabLong[input$long_ctrs_var])),
      #          min = minLong[[input$long_ctrs_var]],
      #          max = maxLong[[input$long_ctrs_var]],
      #          showLastLabel = T,
      #          showFirstLabel = T,
      #          opposite = F,
      #          plotLines = list(
      #            list(label = list(text = lab.y.ends.long[input$long_ctrs_var][[1]][1]),
      #                 color = "#'FF0000",
      #                 width = 2,
      #                 value = minLong[[input$long_ctrs_var]]),
      #            list(label = list(text = lab.y.ends.long[input$long_ctrs_var][[1]][2]),
      #                 color = "#'FF0000",
      #                 width = 2,
      #                 value = maxLong[[input$long_ctrs_var]]))) %>%
      hc_plotOptions(
        spline = list(
          #color = brewer.pal(length(input$long_ctrs_country_selection), "Dark2"),
          dataLabels = list(enabled = F),
          enableMouseTracking = TRUE, 
          showInNavigator = TRUE),
        arearange = list(
          #color = brewer.pal(length(input$long_ctrs_country_selection), "Dark2"),
          enableMouseTracking = FALSE, 
          marker = list(enabled = FALSE)),
        series = list(events = list(legendItemClick = JS("function(e) { return false; } ")))
      ) %>%
      hc_navigator(enabled = TRUE,
                   maskFill = 'rgba(180, 198, 220, 0.25)',
                   series = list(
                     type = "spline" # you can change the type
                   )) %>%
      hc_colors(brewer.pal(length(input$long_vars_variables), "Dark2")[1:length(input$long_vars_variables)]) %>%
      hc_legend(enabled = TRUE) %>%
      hc_tooltip(valueDecimals = 2) %>%
      hc_title(text = input$long_vars_country) %>%
      hc_tooltip(crosshairs = TRUE) %>%
      hc_rangeSelector( 
        enabled = TRUE,
        verticalAlign = "top",
        buttons = list(
          list(type = 'all', text = 'All'),
          list(type = 'month', count = 3, text = '4m'),
          list(type = 'month', count = 2, text = '2m'),
          list(type = 'week', count = 6, text = '6w')
        ))
  })
  
  # -----
  reactive_ctry.scales <- eventReactive(
    c(input$switch_sample, input$switch_transformation),
    {
      # 1 & 2
      if( ((input$switch_sample == "full") && (is.null(input$switch_transformation))) || 
          ((input$switch_sample == "full") && (input$switch_transformation == "raw")) ){
        print("RUNNING FULL OR FULL+RAW")
        select_ctry_sample <<- all_valid_ctry
        select_ctry_psych <<- input$psych_country_selection
        select_sample <<- "full"
        select_transformation <<- "raw"
        
        
        choiceNames_sample = lapply(seq_along(ctry.only.red$coded_country), 
                                    function(i) tagList(tags$img(src = ctry.only.red$flag[i], width = 20, height = 15), 
                                                        ctry.only.red$coded_country[i],
                                                        paste0(" (n=",prettyNum(ctry.only.red$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_sample = ctry.only.red$coded_country
        
        choiceNames_psych = lapply(seq_along(ctry.red$coded_country), 
                                   function(i) tagList(tags$img(src = ctry.red$flag[i], width = 20, height = 15), 
                                                       ctry.red$coded_country[i],
                                                       paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_psych = ctry.red$coded_country
        
        df <- ctry.scales
        
      }
      
      
      # 3 & 4
      else if( ((input$switch_sample == "representative") && (is.null(input$switch_transformation))) ||
               ((input$switch_sample == "representative") && (input$switch_transformation == "raw")) ){
        print("RUNNING REPRESENTATIVE OR REPRESENTATIVE+RAW")
        select_ctry_sample <<- sort(unique(append(repr_valid_ctry, input$psych_country_selection))) 
        select_ctry_psych <<- input$psych_country_selection
        select_sample <<- "representative"
        select_transformation <<- "raw" 
        
        
        ctry.red.repr_with_selection = ctry.red[(ctry.red$coded_country %in% ctry.red.repr$coded_country) | (ctry.red$coded_country %in% input$psych_country_selection),]
        
        choiceNames_sample = lapply(seq_along(ctry.only.red.repr$coded_country), 
                                    function(i) tagList(tags$img(src = ctry.only.red.repr$flag[i], width = 20,  height = 15), 
                                                        ctry.only.red.repr$coded_country[i],
                                                        paste0(" (n=",prettyNum(ctry.only.red.repr$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_sample = ctry.only.red.repr$coded_country
        
        choiceNames_psych = lapply(seq_along(ctry.red.repr$coded_country), 
                                   function(i) tagList(tags$img(src = ctry.red.repr$flag[i], width = 20, height = 15), 
                                                       ctry.red.repr$coded_country[i],
                                                       paste0(" (n=",prettyNum(ctry.red.repr$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_psych = ctry.red.repr$coded_country
        
        
        
        # df <- rbind(ctry.scales.representative[ctry.scales.representative$coded_country %in% c("global"),], 
        #             ctry.scales[(ctry.scales$coded_country %in% ctry.scales.representative$coded_country[!ctry.scales.representative$coded_country %in% c("global")]) |
        #                           (ctry.scales$coded_country %in% select_ctry_psych[!select_ctry_psych %in% c("global")]),]
        #             )
        df <- ctry.scales.representative
      }

      
      # 5
      else if( (input$switch_sample == "full") && (input$switch_transformation == "standardized")) {
        print("RUNNING FULL+ STANDARDIZED")
        select_ctry_sample <<- all_valid_ctry
        select_ctry_psych <<- input$psych_country_selection
        select_sample <<- "full"
        select_transformation <<- "standardized"
        
        
        choiceNames_sample = lapply(seq_along(ctry.only.red$coded_country), 
                                    function(i) tagList(tags$img(src = ctry.only.red$flag[i], width = 20, height = 15), 
                                                        ctry.only.red$coded_country[i],
                                                        paste0(" (n=",prettyNum(ctry.only.red$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_sample = ctry.only.red$coded_country
        
        choiceNames_psych = lapply(seq_along(ctry.red$coded_country), 
                                   function(i) tagList(tags$img(src = ctry.red$flag[i], width = 20, height = 15), 
                                                       ctry.red$coded_country[i],
                                                       paste0(" (n=",prettyNum(ctry.red$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_psych = ctry.red$coded_country
        
        df <- ctry.scales_s
      }
      
      
      # 6
      else if( (input$switch_sample == "representative") && (input$switch_transformation == "standardized") ){
        print("RUNNING REPRESENTATIVE + STANDARDIZED")
        select_ctry_sample <<- sort(unique(append(repr_valid_ctry, input$psych_country_selection))) 
        select_ctry_psych <<- input$psych_country_selection
        select_sample <<- "representative"
        select_transformation <<- "standardized"
        
        
        ctry.red.repr_with_selection = ctry.red.repr[(ctry.red.repr$coded_country %in% ctry.red.repr$coded_country) | (ctry.red.repr$coded_country %in% input$psych_country_selection),]
        
        choiceNames_sample = lapply(seq_along(ctry.only.red.repr$coded_country), 
                                    function(i) tagList(tags$img(src = ctry.only.red.repr$flag[i], width = 20,  height = 15), 
                                                        ctry.only.red.repr$coded_country[i],
                                                        paste0(" (n=",prettyNum(ctry.only.red.repr$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_sample = ctry.only.red.repr$coded_country
        
        choiceNames_psych = lapply(seq_along(ctry.red.repr$coded_country), 
                                   function(i) tagList(tags$img(src = ctry.red.repr$flag[i], width = 20, height = 15), 
                                                       ctry.red.repr$coded_country[i],
                                                       paste0(" (n=",prettyNum(ctry.red.repr$n[i], big.mark=",", scientific=FALSE),")")))
        choiceValues_psych = ctry.red.repr$coded_country
        
        
        # df <- rbind(ctry.scales.representative_s[ctry.scales.representative_s$coded_country %in% c("global"),], 
        #             ctry.scales_s[(ctry.scales_s$coded_country %in% ctry.scales.representative_s$coded_country[!ctry.scales.representative_s$coded_country %in% c("global")]) |
        #                           (ctry.scales_s$coded_country %in% select_ctry_psych[!select_ctry_psych %in% c("global")]),]
        # )
        df <- ctry.scales.representative_s

      }
      
      updateMultiInput_2(session = session, inputId = "sample_country_selection", choiceNames = choiceNames_sample, choiceValues = choiceValues_sample, 
                         selected = select_ctry_sample)
      updateMultiInput_2(session = session, inputId = "psych_country_selection", choiceNames = choiceNames_psych, choiceValues = choiceValues_psych, 
                         selected = select_ctry_psych)
      updateMultiInput_2(session = session, inputId = "cor_country_selection", choiceNames = choiceNames_sample, choiceValues = choiceValues_sample, 
                         selected = select_ctry_sample)
      
      return(df)
      
    }
  )
  
  output$eventTimeRemaining <- renderText({
    invalidateLater(1000, session)
    paste(seconds_to_period(interval(Sys.time(), "2021-02-15 23:59:59 CET", tz= "CET") %/% seconds(1)))
  })
  
  
  observeEvent(input$reset_input_ctry, {
    shinyjs::reset("country_controls")
  })
  
  
  observeEvent(input$sample_country_all, {
    updateMultiInput(
      session = session,
      inputId = "sample_country_selection",
      selected = select_ctry_sample
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
      selected = select_ctry_sample
    )
  })
  
  observeEvent(input$cor_country_none, {
    updateMultiInput(
      session = session,
      inputId = "cor_country_selection",
      selected = character(0)
    )
  })
  
  observeEvent(input$long_ctrs_none, {
    updateMultiInput(
      session = session,
      inputId = "long_ctrs_country_selection",
      selected = character(0)
    )
  })
  
  observeEvent(input$long_ctrs_all, {
    updateMultiInput(
      session = session,
      inputId = "long_ctrs_country_selection",
      selected = weeklyRegions$coded_country
    )
  })
  
  observeEvent(input$long_vars_none, {
    updateMultiInput(
      session = session,
      inputId = "long_vars_variables",
      selected = character(0)
    )
  })
  
  observeEvent(input$long_vars_all, {
    updateMultiInput(
      session = session,
      inputId = "long_vars_variables",
      selected = mvars
    )
  })
  
  
  
  shinyjs::onclick("menu",
                   shinyjs::toggle(id = "sideFooter", anim = F))
  
  shiny:::flushReact()

}

# Run the application 
shinyApp(ui = ui, server = server)

