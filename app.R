library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stats)
library(shinydashboard)
library(dygraphs)
library(RColorBrewer)
library(dichromat)
library(zoo)
library(xts)
library(visNetwork)
#library(geomnet)
library(igraph)
library(stringr)
library(knitr)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(r2d3)
library(forcats)
library(rlang)
library(plotly)
library(ggiraph)
library(gapminder)
library(maps)
library(radarchart)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# truncate data:
dt <- readRDS("data/reducedData.rds")
countryList <- c("USA", "Germany", "Argentina", "Sweden", "Tanzania", "Italy", "Spain", "France", "Indonesia", "Turkey", "Algeria", "Chile", "Mexico")

dt$country_sim = sample(x = countryList, size = nrow(dt), replace = T)
dt$country_sim_iso2 <- iso.alpha(dt$country_sim, n=2)
dt$flag <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(dt$country_sim_iso2))

country_count <- dt %>%
  select(country_sim) %>%
  group_by(country_sim) %>%
  tally() %>%
  arrange(desc(n)) %>%
  na.omit()

#flags <- sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(unique(overview$country_sim_iso2))) #map.world$iso2

map.world = map_data("world")
map.world$iso2 <- iso.alpha(map.world$region, n=2) # add ISO-2 country codes
map.world$iso2[map.world$region == "Virgin Islands"] <- "VI" # add ISO-2 country code for VI
map.world <- map.world %>% 
  mutate(flag = sprintf("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/%s.svg", tolower(iso2))) # add flags
map.world <- merge(x=map.world, y=country_count, by.x = "region", by.y="country_sim", all=TRUE)
map.world$n[is.na(map.world$n)] <- 0

overview <- data.frame(language = dt$language, 
                       gender = as.character(as_factor(dt$gender)), 
                       age = as.character(as_factor(dt$age)), 
                       education = as.character(as_factor(dt$edu)),
                       political = as.character(dt$PolOrCat) %>% str_replace_all(c("Libertarian LeftLibertarian Right" = NA_character_)),
                       country_sim = dt$country_sim,
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
                menuItem(
                    "Info", tabName = "index", icon = icon("info")),
                menuItem("Our Sample", tabName = "sample", icon = icon("fas fa-users")),
                menuItem("Play Zone", tabName = "network", icon = icon("fas fa-pencil-ruler")),
                menuItem("Development", tabName = "development", icon = icon("fas fa-chart-line"), badgeLabel = "coming soon", badgeColor = "orange"),
                menuItem("Data", tabName = "data", icon = icon("fas fa-share-square"), badgeLabel = "coming soon", badgeColor = "orange")
            )
        ),
        
        dashboardBody(
            tags$script(HTML("$('body').addClass('sidebar-mini');")),
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
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
                tabItem(tabName = "index",
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
                                tags$a(href="https://github.com/JannisCodes", 
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
                                            a("B section", onclick = "openTab('network')", href="#"),
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
                            valueBox(9002, "Participants", icon = icon("user-edit"), width = 3),
                            valueBox(11, "Languages", icon = icon("language"), width = 3),
                            valueBox("75+", "Researchesr", icon = icon("user-graduate"), width = 3),
                            valueBox(404, "Something", icon = icon("project-diagram"), width = 3)
                        )
                        
                ),
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
                                
                                #selectInput("var", "Variable",
                                #            list("language", "gender", "age", "education", "political"),
                                #            selected = "language"),
                                d3Output("d3.bar"),
                            )
                        ),
                        fluidRow(
                            box(
                                width = 6,
                                "World Map",
                                #plotlyOutput("pp.map"),
                                girafeOutput("distPlot", width = "400px")
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
                              br(), 
                              
                              multiInput(
                                inputId = "sample_country_selection",
                                label = "Countries:", 
                                choices = NULL,
                                choiceNames = lapply(seq_along(unique(overview$country_sim)), 
                                                     function(i) tagList(tags$img(src = unique(overview$flag)[i],
                                                                                  width = 20, 
                                                                                  height = 15), unique(overview$country_sim)[i])),
                                choiceValues = unique(overview$country_sim),
                                selected = unique(overview$country_sim)
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
                tabItem(tabName = "network",
                        h2("Data Science"),
                        fluidRow(
                          box(#status = "primary",
                            width = 8,
                            #height = 500,
                            #visNetworkOutput('conceptnetwork', height = "600px")
                          ),
                          box(#status = "primary",
                            shinyjs::useShinyjs(),
                            id = "country_controls",
                            width = 4,
                            height = "600px",
                            title = "Controls",
                            solidHeader = T,
                            status = "primary",
                            "Use these controls to inspect the connectedness of the codes.",
                            br(), 
                            
                            multiInput(
                              inputId = "Id010",
                              label = "Countries :", 
                              choices = NULL,
                              choiceNames = lapply(seq_along(unique(overview$country_sim)), 
                                                   function(i) tagList(tags$img(src = unique(overview$flag)[i],
                                                                                width = 20, 
                                                                                height = 15), unique(overview$country_sim)[i])),
                              choiceValues = unique(overview$country_sim),
                              selected = NULL
                            ),
                            hr(),
                            
                            div(style="display:inline-block;width:100%;text-align: center;",
                                actionBttn(
                                  inputId = "reset_input_ctry",
                                  label = "Reset Inputs",
                                  style = "simple", 
                                  color = "primary",
                                  size = "sm"
                                ),
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
                                tags$br(),
                                tags$br(),
                                "For now you can play around with data tool by the ", tags$a(href="https://www.gapminder.org/", 
                                                                                            target="_blank",
                                                                                            "Gapminder Foundation"), ":",
                                plotlyOutput("gapminder")
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
                                HTML(paste("<center>","Part of data sharing is also open availability of code. The code to this web applet is available at: github-link-goes-here.", "</center>")),
                                tags$br(),
                                tags$br()
                            )
                        )
                        # tabsetPanel(type = "tab",
                        #             tabPanel("Twitter",
                        #                      HTML("<iframe src=\"https://drive.google.com/file/d/12Ix7DFUNXlj-t791K9_lP_l-Xp7nNeKB/preview\" width=\"100%\" height=\"725\"></iframe>")
                        #             ),
                        #             tabPanel("Interview",
                        #                      HTML("<iframe src=\"https://drive.google.com/file/d/12ADnePfV5ifgQsPg8nMpn8Ohj46XVEZT/preview\" width=\"100%\" height=\"725\"></iframe>")
                        #             )
                        # )
                )
            )
        )
    )


server <- function(input, output, session) {
    output$d3.bar <- renderD3({
        overview %>%
            filter(country_sim %in% input$sample_country_selection) %>%
            mutate(label = !!sym(input$var)) %>%
            group_by(label) %>%
            tally() %>%
            arrange(desc(n)) %>%
            na.omit() %>%
            mutate(
                y = n,
                ylabel = scales::percent(n/sum(n), accuracy = 0.01), #prettyNum(n/sum(n)*100, big.mark = ",", format = "f", digits = 2),
                fill = "#E69F00", #ifelse(label != input$val, "#E69F00", "red"),
                mouseover = "#0072B2"
            ) %>%
            r2d3(r2d3_file)
    })
    
    output$distPlot <- renderGirafe({
        ggiraph::ggiraph(
            ggobj = ggplot() + 
                geom_polygon_interactive(data = map.world, color = 'gray70', size = 0.1,
                                         aes(x = long, y = lat, group = group, fill = n,
                                             tooltip = sprintf("%s<br/>%s", region, n))) + #tooltip = sprintf("%s<br/>%s", region, Value))
                #scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
                scale_fill_gradientn(colours = c("white", "green", "red"), values = c(0,min(map.world$n[map.world$n>0]),max(map.world$n))) +
                labs(title = NULL, x = NULL, y = NULL) + #fill = data_type, color = data_type, caption = capt 
                scale_y_continuous(breaks = c()) + 
                scale_x_continuous(breaks = c()) + 
                theme_bw() + 
                theme(axis.text = element_text(size = 14),
                      axis.title = element_text(size = 14),
                      strip.text = element_text(size = 14),
                      panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(), 
                      legend.position = "bottom",
                      panel.border = element_blank(), 
                      strip.background = element_rect(fill = 'white', colour = 'white'))
            #code = print(worldMaps(df, world_data, input$data_type, input$period, input$indicator)))
        )
    })
    
    output$gapminder <- renderPlotly(
      gapminder %>%
        plot_ly(
          x = ~gdpPercap, 
          y = ~lifeExp, 
          size = ~pop, 
          color = ~continent, 
          #opacity = 0.75,
          frame = ~year, 
          text = ~country, 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          sizes = c(10, 70),
          marker = list(opacity = 0.5, sizemode = 'diameter')
        ) %>%
        layout(
          xaxis = list(
            title = "GPD per capita",
            type = "log"
          ),
          yaxis = list(
            title = "Lice expectency [years]"
          )
        )
    )
    
    observeEvent(input$reset_input_ctry, {
      shinyjs::reset("country_controls")
    })
    
    observeEvent(input$sample_country_all, {
      updateMultiInput(
        session = session,
        inputId = "sample_country_selection",
        selected = unique(overview$country_sim)
      )
    })
    
    observeEvent(input$sample_country_none, {
      updateMultiInput(
        session = session,
        inputId = "sample_country_selection",
        selected = character(0)
      )
    })
    
    # observeEvent(input$bar_clicked, {
    #     updateTextInput(session, "val", value = input$bar_clicked)
    # })
    
    # output$nodeinfo <- DT::renderDataTable(
    #     DT::datatable(nodeinfo %>% mutate(`Average Connection Density` = round(`Average Connection Density`,2)), 
    #                   filter = 'top', 
    #                   extensions = 'Buttons', 
    #                   options = list(
    #                       columnDefs = list(list(className = 'dt-center')),
    #                       autoWidth = TRUE,
    #                       dom = 'Bfrtlip',
    #                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    #         DT::formatRound('Average Connection Density', digits = 2)
    # )
    # 
    # output$conceptnetwork <- renderVisNetwork({
    #     
    #     nodes.fltr <- nodes %>% filter(value >= input$node.min, 
    #                                    value <= input$node.max)
    #     edges.fltr <- edges %>% filter(width2 >= input$cov.min, 
    #                                    width2 <= input$cov.max,
    #                                    as.character(edges$from) %in% as.character(nodes.fltr$label),
    #                                    as.character(edges$to) %in% as.character(nodes.fltr$label))
    #     
    #     visNetwork(nodes.fltr, edges.fltr, heigth = "100%", width = "100%") %>%
    #         visIgraphLayout(layout = "layout_in_circle") %>%
    #         visNodes(
    #             shape = "dot",
    #             color = list(
    #                 background = "#0085AF",
    #                 border = "#013848",
    #                 highlight = "#FF8000"
    #             )
    #         ) %>%
    #         visEdges(
    #             shadow = FALSE,
    #             color = list(color = "#0085AF", highlight = "#C62F4B"),
    #             scaling = list(min = 10, max = 30)
    #         ) %>%
    #         visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), 
    #                    nodesIdSelection = list(main = "Select variable"), 
    #                    selectedBy = list(variable = "sample", multiple = T, main="Select cluster")) %>% 
    #         visLayout(randomSeed = 11)
    # })
    # 
    # output$dygraph <- renderDygraph({
    #     
    #     tdata <- xts(x = tweet.code.dum %>% select(input$dev.vars),
    #                  order.by = tweet.code.dum$date)
    #     
    #     color.shade1 <- ifelse(input$rb.shade1 == TRUE, "lightgrey","white")
    #     color.shade2 <- ifelse(input$rb.shade2 == TRUE, "lightgrey","white")
    #     color.shade3 <- ifelse(input$rb.shade3 == TRUE, "lightgrey","white")
    #     
    #     dygraph(tdata) %>% 
    #         dyAxis("y", label = "Average Frequency per day") %>%
    #         dyRoller(rollPeriod = input$roll, showRoller=F) %>%
    #         dyShading(from = as.POSIXct("2015-4-14", format="%Y-%m-%d"), to = as.POSIXct("2015-11-1", format="%Y-%m-%d"), color = color.shade1) %>%
    #         dyShading(from = as.POSIXct("2015-12-1", format="%Y-%m-%d"), to = as.POSIXct("2016-5-31", format="%Y-%m-%d"), color = color.shade2) %>%
    #         dyShading(from = as.POSIXct("2016-8-1", format="%Y-%m-%d"), to = as.POSIXct("2018-6-20", format="%Y-%m-%d"), color = color.shade3) %>%
    #         dyEvent("2016-12-1", "NVA Report", labelLoc = "top") %>%
    #         dyEvent("2017-4-1", "NVA Report", labelLoc = "top") %>%
    #         dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1")) %>%
    #         dyHighlight()%>%
    #         dyLegend(labelsDiv = 'legend.div', labelsSeparateLines=T) %>%
    #         dyRangeSelector()
    # })
    # 
    # observeEvent(input$reset_input_net, {
    #     shinyjs::reset("network-controls")
    # })
    # 
    # observeEvent(input$reset_input_dev, {
    #     shinyjs::reset("development_controls")
    # })
    # 
    # #observeEvent(input$switch_Data, {
    # #    updateTabItems(session, "tabs", "data")
    # #})
    # 
}

# Run the application 
shinyApp(ui = ui, server = server)
