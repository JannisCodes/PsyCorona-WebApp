# UI
submenu_sampleUI <- function(id) {
  ns <- NS(id)
  
  # Our Sample
  tabItem(tabName = ns("sample"),
          useShinyalert(),
          h3("Our Sample"),
          bsAlert("dataAlert"),
          
          fluidRow(
            box(width = 12,
                div(style="display:inline-block;width:100%;text-align:center;",
                    radioGroupButtons(
                      inputId = ns("var"), 
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
                h3(textOutput(ns("sample.bar.NA")), align = "center"),
                d3Output(ns("d3.bar")),
                textOutput(ns("SampleTxt")), align = "center")
            #)
          ),
          fluidRow(
            box(
              status = "primary",
              width = 6,
              tags$strong("World Map (sample sizes)"),
              highchartOutput(ns("freqPlot"))
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
                inputId = ns("sample_country_selection"),
                label = "Please select the countries you are interested in (all countries n > 20):", 
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
                    inputId = ns("sample_country_none"), 
                    label = "None",
                    style = "simple", 
                    color = "primary",
                    size = "sm"),
                  HTML("&nbsp;&nbsp;"),
                  actionBttn(
                    inputId = ns("sample_country_all"), 
                    label = "All",
                    style = "simple", 
                    color = "primary",
                    size = "sm")
                  
              )
            )
          )
  )
  
}


# SERVER
submenu_sample <- function(input, output, session, data){
  select_ctry <- data$coded_country

  observeEvent(select_ctry, {
    updateMultiInput(
      session = session,
      inputId = "sample_country_selection",
      selected = select_ctry
    )
  })
  
  output$sample.bar.NA <- renderText({
    #input <- list(var = "language", sample_country_selection = c("France", "Germany"))
    
    test <- data %>%
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
    
    dem <- data %>%
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
          data = data %>% filter(coded_country %in% input$sample_country_selection),
          value = "n",
          joinBy = c("iso-a2", "iso_a2"), name = "sample size",
          #dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.1,
          tooltip = list(valueDecimals = 0, valuePrefix = "n = "))%>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(minColor = "#c4e6c3", maxColor = "#1d4f60", type = "logarithmic") 
  })
  #Color schemes: https://carto.com/carto-colors/
  
  
  
  observeEvent(input$sample_country_all, {
    updateMultiInput(
      session = session,
      inputId = "sample_country_selection",
      selected = select_ctry
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
