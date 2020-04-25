library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stats)
library(shinydashboard)
library(metathis)
#library(dygraphs)
#library(RColorBrewer)
library(stringr)
#library(DT)
library(shinyjs)
library(shinyWidgets)
library(r2d3)
library(radarchart)
library(haven)
#library(leaflet)
library(highcharter)
library(rgeos)
library(scales)
library(grDevices)
library(shinyalert)
library(shinyBS)

# R Studio Clean-Up:
#cat("\014") # clear console
#rm(list=ls()) # clear workspace
#gc() # garbage collector
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # usually set by project

# load data:
load("data/shinyDataAggregated.RData")

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
      .attr('x',      150)
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
      .attr('x',      150)
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
      .attr('x', function(d) { return ((d.y * col_width()) + 150) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .style('font-family', 'sans-serif')
      .text(function(d) {return d.ylabel; });
  totals.transition()
      .duration(1000)
      .attr('x', function(d) { return ((d.y * col_width()) + 150) * 1.01; })
      .attr('y', function(d, i) { return i * col_heigth() + (col_heigth() / 2) + col_top(); })
      .attr('d', function(d) { return d.x; })
      .text(function(d) {return d.ylabel; });
  totals.exit().remove();
"
r2d3_file <- tempfile()
writeLines(r2d3_script, r2d3_file)


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
    sidebarMenu(
      menuItem("Our Sample", tabName = "sample", icon = icon("fas fa-users")),
      menuItem("Psychological Variables", tabName = "Variables", icon = icon("fas fa-pencil-ruler")),
      menuItem("Development", tabName = "development", icon = icon("fas fa-chart-line"), badgeLabel = "coming soon", badgeColor = "orange"),
      menuItem("Data", tabName = "data", icon = icon("fas fa-share-square")),
      menuItem("About", tabName = "about", icon = icon("info")),
      menuItem(HTML(paste0("Take the Suvey Now ", icon("external-link"))), icon=icon("fas fa-file-signature"), href = "https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", newtab = T)),
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
    #tags$head(tags$meta(name = "og:image", content = "width=1600")),
    tags$style(
      type = 'text/css',
      '.bg-aqua {background-color: #3c8dbe!important; }
      .bttn-simple.bttn-primary {background-color: #3c8dbe!important; }
      .btn.radiobtn.btn-primary {float: center!important;
                                 display: block;
                                 width: 150px}
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
        description = "social media cards for web sharing thingies",
        url = "https://psycorona.shinyapps.io/WebApp/",
        image = "https://garrickadenbuie.com/apple-touch-icon-114x114.png",
        image_alt = "An image for social meda cards",
        twitter_creator = "@JannisWrites",
        twitter_card_type = "summary",
        twitter_site = "@JannisWrites"
        ),
    
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "sample",
              useShinyalert(),
              h3("Our Sample"),
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
                    d3Output("d3.bar")
                    #textOutput("SampleTxt"), align = "center")
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
                                      highchartOutput("boxGov")
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
                                      highchartOutput("boxCom")
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
                                      highchartOutput("boxCog")
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
                                      htmlOutput("boxBeh")
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
                                                value = TRUE
                                              )
                                            ),
                                      chartJSRadarOutput('affect', height = "125", width = "400")
                             ),
                             tabPanel("Cross Domain Relationships",
                                      value = 6,
                                      highchartOutput("cor")
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
              fluidRow(
                box(title = "Data Protection",
                    width = 12, 
                    solidHeader = TRUE,
                    HTML("To protect the privacy and confidentiality of our participants, this application only uses aggregate, anonymized data (i.e., no individual person is identifiable).
                    <br><b>Anonymization:</b><br>  
                    <ol>
                    <li>We use <b>data aggregation</b> as the main method of data anonymization. This means that we never show data of a single person or small groups of people,
                    instead we combine data of multiple people to show country-level summary statistics. As an example, you can see the average level of hope in a country and 
                    how much variation there was in the responses but you cannot see the rating of any individual respondent.
                    <br>Importantly, the application only has access to the country-level summaries, which means that data cannot be linked or combined to single out individuals 
                    (e.g., you cannot see the level of hope for U.S. women, aged 25-34, who speak Farsi and have a doctorate degree).
                    <br><i>Note:</i> For aggregate data to effectively be anonymous we need to combine the responses of enough people, which is why we never display data of countries with less than 20 respondents.</li>
                    
                    <li>When we show summaries of categorical data (e.g., percentage of people identifying as female), we additionally apply <b>data perturbations</b> for small groups. This means that the counts and percentages 
                    for groups that comprise less than 20 people (less than 50 for political orientation) have been slightly changed (e.g., a random number between -2 and 2 or between -5 and 5 has been added; 
                    this process is also sometimes referred to as 'artificial noise addition'). 
                    <br>Please note that this also means that the numbers are not always 100% accurate. However, with this method, we can represent the full diversity of our sample while still protecting the identities of people in 
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
                    "One major aim of the PsyCorona initiative is to combine psychological reactions with local, regional, and national
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
                              part of the PsyCorona initiative, you contact us via our website: ",
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
                  
                  h4("The Initiative:"),
                  "Psychology and culture could affect the spread of the virus; human psychology could also change in response to the pandemic. 
                            We aim to mobilize behavioral and data scientists to identify targets for rapid intervention to slow the spread of the pandemic and minimize its social damage. 
                            All the scientists on the team are operating on a largely volunteer basis, relying on existing infrastructure and their own resources.
                            This is a global project based at New York University-Abu Dhabi and the University of Groningen (The Netherlands). 
                            We have evolved into a distributed team across the world, with autonomous work groups in numerous countries, each of whom understands the PsyCorona mission goals and needs. 
                            We aim to ensure global involvement, so we are translating the survey into more languages on a daily basis.
                            Currently more than 100 international social scientists are working together to collect immediate and longitudinal information on 
                            the key social science factors that might predict the spread of COVID-19. The project, is documented in detail on our",
                  tags$a(href="https://www.psycorona.org", 
                         target="_blank",
                         "PsyCorona website."),
                  " We pair social- and data scientists to connect data across multiple layers—individual survey reports of", prettyNum(sum(world.n$n), big.mark=" ", scientific=FALSE), 
                  "participants from more than",length(world.n$coded_country)-1,"countries, satellite data documenting social distancing, and World Health Organization data on county level spread 
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
                                      Here we also share information on our open-source code and connections to meta data repositories we are aiming to 
                                      connect to the psychological responses we measure during the PsyCorona initiative.")),
                  "The remaining three tabs offer tools to visualize the psychological data we collect in this project.",
                  tags$ul(
                    tags$li("The ",
                            a("Our Sample", onclick = "openTab('sample')", href="#"),
                            " tab offers an insight into the diversity of our participants. We share compound information on some demographic variables,
                                      as well as the number of participants we have reached in each country. Please note that to protect the privacy and anonymity
                                      of our participants data visualizations are only available for selections of more than 20 people."),
                    tags$li("The ",
                            a("Psychological Variables", onclick = "openTab('Variables')", href="#"),
                            " tab offers an interactive interface to explore the psychological variables we collect in the initiative's baseline survey. 
                                      This survey is open to anyone interested at",
                            tags$a(href="https://nyu.qualtrics.com/jfe/form/SV_6svo6J4NF7wE6tD", 
                                   target="_blank",
                                   "tiny.cc/corona_survey"),
                            "and currently includes over", prettyNum(sum(world.n$n), big.mark=" ", scientific=FALSE), 
                            "participants. You can explore psychological reactions to the coronavirus at five different levels: (1) Governmental Response, 
                                      (2) Community Response, (3) Cognitive Response, (4) Behavioral Response, as well as (5) Emotional Reponse. Additionally, we offer a
                                      tool to explore the mean level relationship between different variables for different countries. Please note that to protect the 
                                      privacy and anonymity of our participants we only provide country-level visualizations once we have data for more than 20 people from 
                                      any particular country."),
                    tags$li("The ",
                            a("Development", onclick = "openTab('development')", href="#"),
                            " tab gives you the possibility to interactively explore how different areas are evolving over time. This section is currently under
                                      construction, but will be available as soon as we finish data collection of developmental data that can be modeled over time.")
                  )
                ),
                box(width = 4,
                    HTML("<a class=\"twitter-timeline\" data-height=\"600\" href=\"https://twitter.com/FortuneMagazine/lists/coronavirus-updates?ref_src=twsrc%5Etfw\">A Twitter List by FortuneMagazine</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                )
              ),
              fluidRow(
                valueBox(prettyNum(sum(world.n$n), big.mark=" ", scientific=FALSE), "Participants", icon = icon("user-edit"), width = 4),
                valueBox(sum(str_count(names(ctry.scales), c("language")))-1, "Languages", icon = icon("language"), width = 4),
                valueBox("100+", "Researchers", icon = icon("user-graduate"), width = 4)#,
                #valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
              )
      )
    )
    )
  )


server <- function(input, output, session) {
  
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

  
  # shinyalert(title = "Mobile Version", 
  #            text = "This application is currently in development. 
  #            To protect the privacy and confidentiality of our participants this beta version relies on simulated data.",
  #            type = "info",
  #            animation = TRUE,
  #            confirmButtonCol = "#3b738f"
  #            )
  
  createAlert(session = session,
              anchorId = "dataAlert",
              #alertId="a1",
              title = paste(icon("warning"),"Data Notification"),
              content="To protect the privacy of everyone who took our survey, this application only uses aggregate, anonymized data (i.e., no individual person is identifiable). 
              For further information see our <a href='#' onclick=\"openTab('data')\">data description section</a>. Bear in mind that we display data collected over the past weeks. 
              This means the data might not be representative of how countries are doing right now.",
              style = "warning")
  
  output$sample.bar.NA <- renderText({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    
    test <- ctry.scales %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      select(starts_with(input$var)) %>%
      t() %>%
      as.data.frame()
    colnames(test) <- input$sample_country_selection
    test <- test %>%
      mutate(n = rowSums(., na.rm=TRUE),
             label = str_replace(rownames(.), ".*_", "")) %>%
      filter(n>0,
             label != "<NA>")
    
    ifelse(sum(test$n)<20, "Not enough data to display summary","")
  })
  
  output$SampleTxt <- renderText({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    
    explanation <- list(languages = "I have high hopes that the situation regarding coronavirus will improve. [Mean and 95%CI]", 
                        gender = "I think that this country is able to fight the Coronavirus. [Mean and 95%CI]",
                        age = "Mean Loneliness Scores [Mean and 95%CI]",
                        education = "Mean State Paranoia Scores [Mean and 95%CI]",
                        political = "Mean Conspiracy Scores [Mean and 95%CI]")
    explanation[[input$var]]
  })
  
  output$d3.bar <- renderD3({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    #input <- list(var = "gender", sample_country_selection = c("Poland", "Romania", "Albania"))
    
    dem <- ctry.scales %>%
      filter(coded_country %in% input$sample_country_selection) %>%
      select(starts_with(input$var)) %>%
      t() %>%
      as.data.frame()
    colnames(dem) <- input$sample_country_selection
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
    # input = list(psych_country_selection = c("global"))
    
    governmentRed <- ctry.scales %>%
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
    
    categories <- c("0" , "1<br>unclear", "2", "3", "4", "5", "6<br>clear")
    
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
      hc_yAxis(showFirstLabel = T,
               showLastLabel = T,
               min = 1,
               max = 6,
               #step = 1,
               #list(formatter = JS(gov.labs)), 
               #rotation = 0,
               categories = categories,
               #align = "center",
               tickmarkPlacement = seq(1,6,1)) %>%
      hc_tooltip(formatter = JS(tooltipJS))
    }
  })
  
  output$boxCom <- renderHighchart({
    # for testing:
    # input = list(ComVars = "comRule", psych_country_selection = c("Germany", "France"))
    
    communityRed <- ctry.scales %>%
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
    
    categories <- c("0" , "1<br>not at all", "2", "3", "4", "5", "6<br>very much")
    
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
      hc_yAxis(showFirstLabel = T,
               showLastLabel = T,
               min = 1,
               max = 6,
               #step = 1,
               #list(formatter = JS(gov.labs)), 
               #rotation = 0,
               categories = categories,
               #align = "center",
               tickmarkPlacement = seq(1,6,1)) %>%
      hc_tooltip(formatter = JS(tooltipJS))
    }
  })
  
  output$boxCog <- renderHighchart({
    # for testing:
    # input = list(CogVars = "covidHope", cog_country_selection = c("Germany", "France"))
    
    cognitiveRed <- ctry.scales %>%
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
    y.min <- list(covidHope = -3, 
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
  
  output$boxBeh <- renderUI({
    # for testing:
    # input = list(BehVars = "avoid", beh_country_selection = c("Germany", "France"))
    
    if (input$BehVars == "iso") {
      
      behaviorRedIso <- ctry.scales %>%
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
        hc_yAxis(min = 0,
                 max = 7,
                 categories = seq(0,7,1),
                 tickmarkPlacement = seq(0,7,1)) %>%
        hc_tooltip(formatter = JS(tooltipJSPers))
      
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
        hc_yAxis(min = 0,
                 max = 7,
                 categories = seq(0,7,1),
                 tickmarkPlacement = seq(0,7,1)) %>%
        hc_tooltip(formatter = JS(tooltipJSOnl))
      lst <- list(hcPers, hcOnli)  
      
      hw_grid(lst, ncol = 2, rowheight = "400")
      }
      
    } else {
      
      behaviorRed <- ctry.scales %>%
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
      y.max <- list(wash = 3, 
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
        hc_yAxis(showFirstLabel = T,
                 showLastLabel = T,
                 min = y.min[[input$BehVars]],
                 max = y.max[[input$BehVars]],
                 tickInterval = 1,
                 labels = list(formatter = JS(lab.ends.js)), 
                 categories = c("0"),
                 tickmarkPlacement = seq(0,7,1)) %>%
        hc_tooltip(formatter = JS(tooltipJS)) %>%
        hw_grid(ncol = 1, rowheight = "400")
      }
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
    chartJSRadar(radar, maxScale = 5, showToolTipLabel=TRUE, showLegend = T, responsive = T, 
                 labelSize = 12) 
  })
  
  output$cor <- renderHighchart({
    # for testing:
    # input = list(CorX = "covidHope", CorY = "covidEff")
    
    cor.dat <- ctry.scales %>%
      dplyr::select(coded_country, n,
                    xvar = one_of(input$CorX),
                    yvar = one_of(input$CorY)) %>%
      filter(coded_country != "global",
             coded_country %in% input$cor_country_selection)
    
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
        hc_tooltip(formatter = JS(tooltipJS))
    }
  })
  

  observeEvent(input$reset_input_ctry, {
    shinyjs::reset("country_controls")
  })
  
  observeEvent(input$sample_country_all, {
    updateMultiInput(
      session = session,
      inputId = "sample_country_selection",
      selected = ctry.only.red$coded_country
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
  
  shinyjs::onclick("menu",
                   shinyjs::toggle(id = "sideFooter", anim = F))
  
  shiny:::flushReact()
}

# Run the application 
shinyApp(ui = ui, server = server)
