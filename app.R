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
library(plotly)
#library(ggiraph)
library(gapminder)
#library(maps)
#library(rworldmap)
library(radarchart)
library(haven)
library(leaflet)
library(highcharter)
library(rgeos)

# R Studio Clean-Up
cat("\014") # clear console
rm(list=ls()) # clear workspace
gc # garbage collector
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# truncate data:
dt <- readRDS("data/reducedData.rds")

# load geo spatial data
library(rnaturalearth)
library(rnaturalearthdata)
world.data <- ne_countries(scale = "medium", returnclass = "sf")
unique(dt$coded_country)[!unique(dt$coded_country) %in% world.data$admin] # check whether all country names are spelled correctly

# all ISO-2 country code to dataframe and flags
dt <- merge(x = dt, y = world.data %>% select(admin, iso_a2), by.x = "coded_country", by.y = "admin", all.x = T)
dt$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(dt$iso_a2))

# add survey data to world map data
ctry.scales <- dt %>%
  filter(!is.na(coded_country)) %>%
  group_by(coded_country) %>%
  dplyr::summarize(
    n = n(),
    affHighPos = mean(affHighPos.m, na.rm = T),
    affHighNeg = mean(affHighNeg.m, na.rm = T),
    affLowPos = mean(affLowPos.m, na.rm = T),
    affLowNeg = mean(affLowNeg.m, na.rm = T),
    lone = mean(lone.m, na.rm = T),
    bor = mean(bor.m, na.rm = T),
    isoPers = mean(isoPers.m, na.rm = T),
    isoOnl = mean(isoOnl.m, na.rm = T),
    ext = mean(ext.m, na.rm = T),
    beh = mean(beh.m, na.rm = T),
    c19Hope = mean(c19Hope, na.rm = T),
    c19Eff = mean(c19Eff, na.rm = T),
    para = mean(para.m, na.rm = T),
    consp = mean(consp.m, na.rm = T),
    jobinsec = mean(jobinsec.m, na.rm = T),
    pfs = mean(pfs.m, na.rm = T)
  )

# radar <- vector(mode="list", length=nrow(ctry.scales))
# names(radar) <- ctry.scales$coded_country
# 
# for (i in 1:nrow(ctry.scales)) { 
#   radar[[i]] <- c(ctry.scales$affHighNeg[i], ctry.scales$affHighPos[i], ctry.scales$affLowNeg[i], ctry.scales$affLowPos[i]) 
# }
# 
# labs <- c("High Negative", "High Positive", "Low Negative", "Low Positive")
# chartJSRadar(scores = radar, labs = labs)

world.data <- merge(x=world.data, y=ctry.scales, by.x = "admin", by.y="coded_country", all.x=TRUE)
# world.data$n[is.na(world.data$n)] <- 0
world.n <- world.data %>% select(admin, iso_a2, n)


overview <- data.frame(language = dt$language, 
                       gender = as.character(as_factor(dt$gender)), 
                       age = as.character(as_factor(dt$age)), 
                       education = as.character(as_factor(dt$edu)),
                       political = as.character(dt$PolOrCat) %>% str_replace_all(c("Libertarian LeftLibertarian Right" = NA_character_)),
                       coded_country = dt$coded_country,
                       flag = dt$flag)

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
                menuItem("Data", tabName = "data", icon = icon("fas fa-share-square"), badgeLabel = "coming soon", badgeColor = "orange"),
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
                                    inputId = "var", label = "Participant characteristics:", 
                                    #choices = c("language", "gender", "age", "education", "political"), 
                                    selected = "language",
                                    justified = TRUE, 
                                    status = "primary",
                                    choiceNames = c("Survey language", "Gender", "Age", "Education", "Political orientation"),
                                    choiceValues = c("language", "gender", "age", "education", "political")
                                    #checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                ),
                                d3Output("d3.bar"),
                            )
                        ),
                        fluidRow(
                            box(
                                status = "primary",
                                width = 6,
                                tags$strong("World Map"),
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
                                choiceNames = lapply(seq_along(unique(overview$coded_country)), 
                                                     function(i) tagList(tags$img(src = unique(overview$flag)[i],
                                                                                  width = 20, 
                                                                                  height = 15), unique(overview$coded_country)[i])),
                                choiceValues = unique(overview$coded_country),
                                selected = unique(overview$coded_country)
                              ),
                              hr(),
                              
                              div(style="display:inline-block;width:100%;text-align: center;",
                                  actionBttn(
                                    inputId = "sample_country_all", 
                                    label = "All",
                                    style = "simple", 
                                    color = "primary",
                                    size = "sm"),
                                  actionBttn(
                                    inputId = "sample_country_none", 
                                    label = "None",
                                    style = "simple", 
                                    color = "primary",
                                    size = "sm"),
                              )
                            )
                        )
                ),
                tabItem(tabName = "Variables",
                        box(width = 12, solidHeader = TRUE,
                            navbarPage("Data Tool",
                                       tabPanel("Government Reponse",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    "Nice Controls here"
                                                    ),
                                                  mainPanel(
                                                    "Nice Plot here"
                                                  )
                                                )
                                                ),
                                       tabPanel("Community Response"),
                                       tabPanel("Behavioral Response"),
                                       tabPanel("Emotional Response",
                                                sidebarLayout(
                                                  sidebarPanel(
                                                    multiInput(
                                                      inputId = "sample_country_affect",
                                                      label = "Countries:", 
                                                      choices = NULL,
                                                      choiceNames = lapply(seq_along(unique(overview$coded_country)), 
                                                                           function(i) tagList(tags$img(src = unique(overview$flag)[i],
                                                                                                        width = 20, 
                                                                                                        height = 15), unique(overview$coded_country)[i])),
                                                      choiceValues = unique(overview$coded_country),
                                                      selected = NULL
                                                      )
                                                  ),
                                                  mainPanel(
                                                    chartJSRadarOutput('affect')
                                                  )
                                                )
                                                ),
                                       tabPanel("Cross Domain Relationships")
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
                                tags$br(),
                                tags$br(),
                                #plotlyOutput("gapminder")
                            )
                        )
                        ),
                tabItem(tabName = "data",
                        h2("Data Sharing"),
                        fluidRow(
                            box(#title = "Explore The Data", 
                                width = 12, 
                                heigth = "500px",
                                solidHeader = TRUE,
                                tags$br(),
                                tags$br(),
                                h4(HTML(paste("<center>","The aim of the PsyCorona initiative is to build a collaborative research effort.", "</center>"))),
                                h4(HTML(paste("<center>","We will soon start sharing some of our data on the Open Science Framework.", "</center>"))),
                                h4(HTML(paste("<center>","To protect the privacy and confidentiality of our participants, sensitive data will always only be available to members of the immediate research collaboration", "</center>"))),
                                tags$br(),
                                HTML("<center>Part of data sharing is also open availability of code. The code to this web applet is available at <a href='https://github.com/JannisCodes/PsyCorona-WebApp'>our git repository</a></center>"),
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
                            " More than 100 international social scientists are working together to collect immediate and longitudinal information on 
                                the key social science factors thatmight predict the spread of COVID-19. The project, known as",
                            tags$a(href="https://www.psycorona.org", 
                                   target="_blank",
                                   "PsyCorona"),
                            "will pair social and data scientists to connect data across multiple layers—individual survey reports from 10,000 participants 
                                from more than 9 countries, satellite data documenting social distancing, and World Health Organization data on county level spread 
                                of the disease.
                                ",
                            br(),
                            "You can find the PsyCorona Collective on: ",
                            tags$a(href="https://www.facebook.com/??/", 
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
                            " that enable visitors to directly interact with the data: ",
                            tags$ul(
                              tags$li("The ",
                                      a("A section", onclick = "openTab('data')", href="#"),
                                      ", which get's some A text")),
                            "The remaining three tabs offer tools to visualize the data according to ... .",
                            tags$ul(
                              tags$li("The ",
                                      a("Our Sample", onclick = "openTab('sample')", href="#"),
                                      " tab offers a ... ."),
                              tags$li("The ",
                                      a("B section", onclick = "openTab('Variables')", href="#"),
                                      " tab offers an interactive interface to explore .. ."),
                              tags$li("The ",
                                      a("Development", onclick = "openTab('development')", href="#"),
                                      " tab gives users the possibility to interactively explore data evolves over time ... .")
                            )
                            
                          ),
                          box(width = 4,
                              "We will put some live ticker element here."
                              #    HTML("<a class=\"twitter-timeline\" data-lang=\"en\" data-height=\"500\" href=\"https://twitter.com/ReMatriate?ref_src=twsrc%5Etfw\">Tweets by ReMatriate</a> <script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
                          )
                        ),
                        fluidRow(
                          valueBox(paste0(prettyNum(nrow(dt), big.mark=" ", scientific=FALSE), "+"), "Participants", icon = icon("user-edit"), width = 3),
                          valueBox(paste0(length(unique(dt$language)),"+"), "Languages", icon = icon("language"), width = 3),
                          valueBox("80+", "Researchesr", icon = icon("user-graduate"), width = 3),
                          valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
                        )
                )
            )
        )
    )


server <- function(input, output, session) {
    output$d3.bar <- renderD3({
        overview %>%
            filter(coded_country %in% input$sample_country_selection) %>%
            mutate(label = !!sym(input$var)) %>%
            group_by(label) %>%
            tally() %>%
            arrange(desc(n)) %>%
            na.omit() %>%
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
            data = world.n %>% filter(admin %in% input$sample_country_selection), value = "n",
            joinBy = c("iso-a2", "iso_a2"), name = "sample size",
            #dataLabels = list(enabled = TRUE, format = '{point.name}'),
            borderColor = "#FAFAFA", borderWidth = 0.1,
            tooltip = list(valueDecimals = 0, valuePrefix = "n = "))%>% 
        hc_mapNavigation(enabled = TRUE) %>%
        hc_colorAxis(minColor = "#c4e6c3", maxColor = "#1d4f60", type = "logarithmic") 
    })
    #Color schemes: https://carto.com/carto-colors/
    
    output$affect <- renderChartJSRadar({
      radar <- data.frame("label" = c("High Positive", "High Negative", "Low Positive", "Low Negative"), 
                          t(ctry.scales %>%
                              filter(coded_country %in% input$sample_country_affect) %>% 
                              select(starts_with("aff"))))
      names(radar) <- c("label", input$sample_country_affect)
      chartJSRadar(radar, maxScale = 5, showToolTipLabel=TRUE, showLegend = F) })
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)
