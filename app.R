#### Library and Data Imports ####
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(RCurl)

source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")
source("modules/ggplot_gen.R")
source("modules/leaflet_gen.R")
source("modules/gt_gen.R")
sourceDir("modules/shiny/R")

update_date <- "2021-03-14"

moving.avg.window <-
  7 # WARNING: Behavior for moving.avg.window > number of report dates for a region is undefined.
# (i.e. a 20 day window if Catskill /Region has 19 report dates.)
height <- "600px"# plot heights

# Leaving this in case we need it
# TODO: Implement other text as strings like this...
rpi_accessibility_link <-
  "<div class='center'><p><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a></p></div>"


footer_text <-
  "<p>COVID<b>MINDER</b> analysis and visualizations</b> are by students and staff
                                of <a href='http://idea.rpi.edu/'>The Rensselaer Institute for Data Exploration
                                and Applications</a> at <a href='http://rpi.edu/'>Rensselaer Polytechnic Institute</a>
                                with generous support from the United Health Foundation. COVID<b>MINDER</b> is an open
                                source project implemented on the <a href='https://shiny.rstudio.com/'>R Shiny platform</a>;
                                see the <a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER github</a>
                                for more information. COVID<b>MINDER</b> was directed by Kristin P. Bennett and John S. Erickson.<br><br>
                                <img src='comment.png' alt = 'Small text bubble icon' style='float:left;width:40px;margin-right:5px;' >
                                Thanks for using <b>COVIDMINDER!</b> Please take a few moments
                                to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a></p><br><br>
                                "
#<i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement
#of Accessibility</a></i></div>"

whatisit_text_abt <-
  "<p>COVID<b>MINDER</b> reveals the regional disparities
                                in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct
                                effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact
                                COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic.</p>"

whatisit_text <-
  "COVIDMINDER reveals the regional disparities in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic."


comments_link <-
  "<img src='comment.png' style='float:left;width:40px;padding-right:2px;' >
                                Thanks for using <b>COVIDMINDER!</b> Please take a few moments
                                to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a><br><br>
                                <i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement
                                of Accessibility</a></i>"

# For URL parameterization
url1 <- url2 <- ""

#### UI Code ####

### Footer ####
footer <- fluidRow(
  column(
    width = 12,
    fluidRow(
      column(
        width = 8,
        offset = 2,
        tags$hr()
            )
      ),
    fluidRow(
        column(
    width = 1,
    offset = 2,
    HTML("<a href='?tab=about'>About</a>&emsp;"),
         ),
  column(
    width = 3,
    HTML(
      "<a href='https://idea.rpi.edu/'>Institute for Data Exploration and Applications (IDEA)</a>&emsp;"
        )
    ),
  column(
    width = 1,
    HTML(
    "<a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER GitHub</a>&emsp;"
        )
    ),
  column(
    width = 1,
    HTML(
      "<a href='https://info.rpi.edu/statement-of-accessibility'>Accessibility</a>&emsp;"
    )
    ),
  column(
    width = 1,
    HTML(
      "<a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>
        <span title='Thanks for using COVIDMINDER! Please take a few moments to fill out our short comments form.'>Comments</span></a>&emsp;"
        )
    ),
  column(
    width = 1,
    tags$a(href = "#top", "Back to Navbar")
  )
 
    )
  
  )
 
 
  
)
# get rid of footer class code in css doc


header <- tags$div(
  class = "header",
  
  tags$div(
    class = "headerchild",
    HTML("<b>COVIDMINDER</b>"),
  )
)


ui <- function(request) {
  tagList(
    tags$html(lang = "en-us"),
    tags$head(includeHTML("www/analytics.html")),
    navbarPage(
      id = "tab",
      theme = "style.css",
      title = tags$a(
        class = "title-text",
        title = whatisit_text,
        name = "top",
        href = "/",
        img(class = "logo", src = "Rensselaer.png", 
            alt = "Rensselaer Polytechnic Institute Logo")
        # HTML("<span style='text-align: right;'> COVID<b>MINDER</b></span>")
      ),
      collapsible = TRUE, 
      windowTitle = "COVIDMINDER: Where you live matters",
      tabPanel(
        title = HTML("<b>NATIONAL REPORT CARD</b>"),
        value = "national_report_card",
        fluidRow(column(
          width = 2,
          offset = 10,
          tags$span(style = "float:right; padding: 5px 40px",
                    tags$b("Data last pulled:"),
                    HTML(update_date)),
          
        )),
        fluidRow(column(
          8,
          style = "text-align:center;font-weight:bold;background-color: #EBEBEB;",
          tags$h1("COVIDMINDER: Where you live matters"),
          offset = 2
        )),
        
        
        fluidRow(column(
          8,
          style = "text-align:center;",
          tags$h1("United States Overview"),
          offset = 2
        )),
        fluidRow(
          column(
            8,
            style = "text-align:center;",
            tags$h2("United States COVID-19 Case Curve"),
            tags$h3(
              "How have United States overall COVID-19 Cases changed over time?"
            ),
            offset = 2
          )
        ),
        fluidRow(column(
          8,
          plotOutput(
            outputId = "US.CoT",
            height = height,
            hover = hoverOpts(
              id = "US.CoT.hover",
              delay = 100,
              delayType = "throttle"
            )
          ),
          uiOutput("US.CoT.tooltip"),
          downloadButton("US.CoT.dl", label = "Download Case Barplot"),
          downloadButton("US.CoT.data.dl", label = "Download Data For This Plot"),
          offset = 2
        )),
        fluidRow(
          column(
            8,
            style = "text-align:center;",
            tags$h2("United States COVID-19 Mortality Curve"),
            tags$h3(
              "How have United States overall COVID-19 deaths changed over time?"
            ),
            offset = 2
            
          )
        ),
        fluidRow(
          column(
            8,
            plotOutput(
              outputId = "US.DoT",
              height = height,
              hover = hoverOpts(
                id = "US.DoT.hover",
                delay = 100,
                delayType = "throttle"
              )
            ),
            uiOutput("US.DoT.tooltip"),
            downloadButton("US.DoT.dl", label = "Download Mortality Barplot"),
            downloadButton("US.DoT.data.dl", label = "Download Data For This Plot"),
            offset = 2
          ),
          
          
          
        ),
        fluidRow(
          column(
            8,
            style = "text-align:center;",
            tags$h2("Flattening the Curve"),
            tags$p(
              "Nationwide, states have taken various approaches to mitigate the spread of coronavirus, such as social distancing interventions and encouraging mask use where social distancing is not possible. Studies by the CDC have shown these methods reduce new COVID-19 cases, hospitalizations, and deaths."
            ),
            tags$b("Data Source: "),
            tags$a("CDC", href = "https://wwwnc.cdc.gov/eid/article/26/8/20-1093_article"),
            offset = 2
          )
        ),
        fluidRow(column(8, gt_output("US.report"),
                        offset = 2)),
        fluidRow(column(
          8,
          style = "text-align:center;",
          tags$h1("State Level Breakdown"),
          offset = 2
        )),
        fluidRow(
          column(
            width = 8,
            offset = 2,
            style = "text-align:center;position:relative;",
            uiOutput("US.trends.title"),
          )
        ),
        fluidRow(
          column(
            width=4,
            offset=2,
            tags$div(
              style = "height:130px;",
              uiOutput("US.report.state.selector"),
                radioButtons(
                  inputId = "NRC.rate",
                  label = "Rate",
                  choices = c("Overall", "Per/100k"),
                  selected = "Per/100k"
              )
              
            ),
          )
        ),
        fluidRow(
          column(
            width=8,
            offset=2,
            tags$div(style="padding: 20px 0px;", "Use the above form to select 1 or more States."),
          )
        ),
        fluidRow(
          column(
            width=8,
            offset = 2,
            plotOutput(
              outputId = "US.trends",
              height = height,
              hover = hoverOpts(
                id = "US.trends.hover",
                delay = 100,
                delayType = "throttle"
              ),
              dblclick = "trends.dbl_click",
              brush = brushOpts(id = "trends.brush",
                                resetOnNew = TRUE)
            ),
            uiOutput("US.trends.tooltip"),
            downloadButton("US.trends.dl", label = "Download Case Trend Plot"),
            downloadButton("US.trends.data.dl", label = "Download Data For This Plot")
          ),
          
          
          
        ),
        tags$br(),
        fluidRow(column(
          8,
          offset = 2,
          style = "text-align:center;",
          tags$div(
            class = "info",
            HTML(
              "<h2>Disparity Color Legend</h2>
                               Colors on maps below represent:<br><br>
                                <div>
                               <div><span style='background: #BD0026; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #D73027; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #F46D43; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span> State rate is<strong> Higher</strong> than national average rate</div>
                               <div><span style='background: #f7f7f7; border:solid 1px; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span> State rate is<strong> About Equal</strong> to national average rate</div>
                               <div><span style='background: #253494; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #4575B4; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #74ADD1; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span> State rate is<strong> Lower</strong> than national average rate</div>
                               <i style='display:inline;'>Darker shades indicate greater disparity.</i><br><br>

                               </div>"
            )
          )
        )),
        fluidRow(
          column(
            8,
            tags$h2(style = "text-align:center;", "US COVID-19 Case Hotspots"),
            tags$h3(
              style = "text-align:center;",
              paste0(
                "What are the Nationwide disparities in Daily Case Rates? (",
                time.period,
                " day average)"
              )
            ),
            radioButtons(
              inputId = "NRC.case.time",
              label = "Time Frame",
              choices = c("Daily", "Overall"),
              selected = "Daily",
              inline = T
            ),
            leafletOutput("US.map.cases", height = height),
            
            downloadButton("US.map.cases.dl", label = "Download Case Map"),
            downloadButton("US.map.cases.data.dl", label = "Download Data For This Plot"),
            offset = 2
          ),
          column(
            8,
            tags$h2(style = "text-align:center;", "US COVID-19 Mortality Hotspots"),
            tags$h3(
              style = "text-align:center;",
              paste0(
                "What are the Nationwide disparities in Daily Mortality Rates? (",
                time.period,
                " day average)"
              )
            ),
            radioButtons(
              inputId = "NRC.deaths.time",
              label = "Time Frame",
              choices = c("Daily", "Overall"),
              selected = "Daily",
              inline = T
            ),
            leafletOutput("US.map.deaths", height = height),
            downloadButton("US.map.deaths.dl", label = "Download Mortality Map"),
            downloadButton("US.map.deaths.data.dl", label = "Download Data For This Plot"),
            offset = 2
          ),
          column(
            8,
            tags$h2(style = "text-align:center;", "US COVID-19 Testing Disparities"),
            tags$h3(
              style = "text-align:center;",
              "What are the Nationwide disparities in COVID-19 Testing?"
            ),
            leafletOutput("US.map.testing", height = height),
            
            downloadButton("US.maps.testing.dl", label = "Download Testing Map"),
            downloadButton("US.maps.testing.data.dl", label = "Download Data For This Plot"),
            offset = 2
          )
        ),
        tags$br(),
     
        tags$br(),
        tags$br(),
        fluidRow(
          column(width=8,
                 offset=2,
                 style = "text-align:center;",
                 tags$h1("Rankings")
                 )
        ),
        fluidRow(
          column(width=8,
                 offset=2,
              selectInput(
                inputId = "US.entries",
                label = "Entries",
                choices = c(
                  `Show 10` = 10,
                  `Show 25` = 25,
                  `Show 50` = 50
                ),
                selected = 10,
                width = "100px"
              ),
              radioButtons(
                inputId = "US.rank.order",
                label = "Order",
                choices = c("Ascending", "Descending"),
                selected = "Descending",
                width = "200px"
              )
                 )
        ),
        fluidRow(
          column(width=8, offset=2,
                 
            gt_output("US.ranking.table")
                 )
        ),
      footer  
      ),
      tabPanel(
        title = HTML("<div><b>STATE REPORT CARDS</b></div>"),
        value = "state_report_cards",
        fluidRow(column(
          width = 2,
          offset = 10,
          tags$span(style = "padding: 5px 40px;float:right;",
                    tags$b("Data last pulled:"),
                    HTML(update_date)),
          
        )),
        fluidRow(column(
          8,
          style = "text-align:center;font-weight:bold;background-color: #EBEBEB;",
          tags$h1("COVIDMINDER: Where you live matters"),
          offset = 2
        )),
        
        fluidRow(column(
          width=8,
          offset=2,
          selectInput(
            inputId = "state_name",
            label = "State Selector",
            choices = state.abr$name,
            selected = as.character(unlist(ranking[ranking$rank ==
                                                     50, "name"]))
          ),
        )),
        fluidRow(column(
          8,
          style = "text-align:center;",
          uiOutput("main_title"),
          offset = 2
        )),
        fluidRow(column(
          8,
          style = "text-align:center;",
          tags$b(
            tags$sup("*"),
            "States are ranked best to worst by their percentage change in COVID-19 cases over the past ",
            time.period,
            " days."
          ),
          offset = 2
        )),
        fluidRow(column(
          8,
          offset=2,
          style = "text-align:center;",
          uiOutput("state.CoT.title"),
        )),
        fluidRow(
          column(
            width = 8,
            offset = 2,
            plotOutput(
              outputId = "state.CoT",
              height = height,
              hover = hoverOpts(
                id = "state.CoT.hover",
                delay = 15,
                delayType = "debounce"
              )
            ),
            
            uiOutput("state.CoT.tooltip"),
            
            downloadButton("state.CoT.dl", label = "Download Case Barplot"),
            downloadButton("state.CoT.data.dl", label = "Download Data For This Plot")
          )
        ),
        
        fluidRow(column(
          8,
          offset = 2,
          style = "text-align:center;",
          uiOutput("state.DoT.title")
        )),
        fluidRow(column(
          8,
          plotOutput(
            outputId = "state.DoT",
            height = height,
            hover = hoverOpts(
              id = "state.DoT.hover",
              delay = 15,
              delayType = "debounce"
            )
          ),
          uiOutput("state.DoT.tooltip"),
          downloadButton("state.DoT.dl", label = "Download Mortality Barplot"),
          downloadButton("state.DoT.data.dl", label = "Download Data For This Plot"),
          offset = 2
        )),
        
        
        
        
        
        
        fluidRow(
          column(
            8,
            style = "text-align:center;",
            tags$h2("Flattening the Curve"),
            tags$p(
              "Nationwide, states have taken various approaches to mitigate the spread of coronavirus, such as social distancing interventions and encouraging mask use where social distancing is not possible. Studies by the CDC have shown these methods reduce new COVID-19 cases, hospitalizations, and deaths."
            ),
            tags$b("Data Source: "),
            tags$a("CDC", href = "https://wwwnc.cdc.gov/eid/article/26/8/20-1093_article"),
            offset = 2
          )
        ),
        fluidRow(column(8, gt_output("state.report"),
                        offset = 2)),
        fluidRow(column(
          8,
          style = "text-align:center;",
          tags$h1("County Level Breakdown"),
          offset = 2
        )),
        fluidRow(column(
          8,
          offset = 2,
          style = "text-align:center;",
          
          uiOutput("state.trends.title"),
        )),
        fluidRow(
          column(
            width=8,offset=2,
            tags$div(
              style = "height:130px;width:100%;text-align:left;",
              uiOutput("state.report.county.selector"),
              radioButtons(
                inputId = "SRC.rate",
                label = "Rate",
                choices = c("Overall", "Per/100k"),
                selected = "Per/100k"
              )
            )
          )
        ),
        fluidRow(
          column(width=8,offset=2,
            tags$div(style="padding: 20px 0px;", "Use the above form to select 1 or more Counties."),
                 )
        ),
        fluidRow(
          column(width=8,offset=2,
                 plotOutput(
              outputId = "state.trends",
              height = height,
              hover = hoverOpts(
                id = "state.trends.hover",
                delay = 15,
                delayType = "debounce"
              ),
              dblclick = "trends.dbl_click",
              brush = brushOpts(id = "trends.brush",
                                resetOnNew = TRUE)
            ),
            uiOutput("state.trends.tooltip"),
            downloadButton("state.trends.dl", label = "Download Case Trends Plot"),
            downloadButton("state.trends.data.dl", label = "Download Data For This Plot"),
                 )
        ),
        fluidRow(column(
          8,
          style = "text-align:center;",
          tags$div(
            class = "info",
            HTML(
              "<h2>Disparity Color Legend</h2>
                               Colors on maps below represent:<br><br>
                                <div>
                               <div><span style='background: #BD0026; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #D73027; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #F46D43; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span> County rate is <strong>Higher</strong> than national average rate</div>
                               <div><span style='background: #f7f7f7; border:solid 1px; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span> County rate is <strong>About Equal</strong> to national average rate</div>
                               <div><span style='background: #253494; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #4575B4; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span>
                                    <span style='background: #74ADD1; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span> County rate is <strong>Lower</strong> than national average rate</div>
                               <i style='display:inline;'>Darker shades indicate greater disparity.</i><br><br>
                               </div>"
            )
          ),
          offset = 2
        )),
        fluidRow(
          column(
            8,
            tags$div(style = "text-align:center;", uiOutput("state.county.cases")),
            radioButtons(
              inputId = "SRC.case.time",
              label = "Time Frame",
              choices = c("Daily", "Overall"),
              selected = "Daily",
              inline = T
            ),
            leafletOutput("map.cases", height = height),
            downloadButton("map.cases.dl", label = "Download Case Map"),
            downloadButton("map.cases.data.dl", label = "Download Data For This Plot"),
            offset = 2
          ),
          column(
            8,
            tags$div(style = "text-align:center;", uiOutput("state.county.deaths")),
            radioButtons(
              inputId = "SRC.death.time",
              label = "Time Frame",
              choices = c("Daily", "Overall"),
              selected = "Daily",
              inline = T
            ),
            leafletOutput("map.deaths", height = height),
            downloadButton("map.deaths.dl", label = "Download Mortality Map"),
            downloadButton("map.deaths.data.dl", label = "Download Data For This Plot"),
            offset = 2
          )
        ),
        tags$br(),
        fluidRow(column(
          8,
          style = "text-align:center;",
          tags$h1("Comorbidities and Other Determinants"),
          offset = 2
        )),
        fluidRow(
          column(
            8,
            style = "text-align:center;",
            uiOutput("determinant.title"),
            offset = 2
          ),
          column(
            8,
            selectInput(
              inputId = "state.determinant",
              label = "Determinant",
              choices = c("Diabetes", "Obesity", "CRD Mortality"),
              selected = "Diabetes"
              # selected = "GOP Vote"
            ),
            leafletOutput("maps.determinant", height = height),
            downloadButton("map.determinant.dl", label = "Download Determinant Map"),
            downloadButton("map.determinant.data.dl", label = "Download Data For This Plot"),
            offset = 2
          ),
          column(
            8,
            style = "text-align:center;",
            tags$p(
              textOutput("determinant.text"),
              tags$br(),
              tags$b("Data Source: "),
              tags$a("CDC", href = "www.cdc.gov/diabetes/data"),
              ", ",
              tags$a("CHR", href =
                       "https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/health-behaviors/diet-exercise/adult-obesity")
            ),
            offset = 2
          )
        ),
        tags$br(),
        tags$br()
        ,
        fluidRow(
          column(8, offset = 2,style = "text-align:center;",
                 tags$h1("Rankings"),
                 
          tags$a(name = "ranking"),
          )), 
            fluidRow(
              column(
                width=8,
                offset=2,
              selectInput(
                inputId = "entries",
                label = "Entries",
                choices = c(
                  `Show 10` = 10,
                  `Show 25` = 25,
                  `Show 50` = 50
                ),
                selected = 10,
                width = "100px"
              ),
              radioButtons(
                inputId = "rank.order",
                label = "Order",
                choices = c("Ascending", "Descending"),
                selected = "Descending",
                width = "200px"
              ),
              
              )
              
            ),
        fluidRow(
          column(
            width=8,offset=2,
            gt_output("ranking.table")
          )
        ),
        footer 
          ),
        
      #### State Determinants UI ####
      navbarMenu(
        menuName = "determinant_menu",
        HTML("<div><b>DETERMINANT ANALYSIS</b></div>"),
        tabPanel(
          tags$div(
            class = "tab-title",
            style = "text-align:center;",
            HTML("<div><b>State Analysis</b></div>")
          ),
          value = "state_social_det",
          
          fluidRow(column(
            8,
            style = "text-align:center;font-weight:bold;background-color: #EBEBEB;",
            tags$h1("COVIDMINDER: Where you live matters"),
            offset = 2
          )),
        
          fluidRow(column(
            8,
            selectInput("social_det_picker", "Select a state to display", state.name, selected = NULL, multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            plotOutput("social_det_graph", width = "100%", height = "1600px"),
            
            offset = 2
          ))
        ),
        tabPanel(
          tags$div(
            class = "tab-title",
            style = "text-align:center;",
            HTML("<div><b>Determinant Descriptions</b></div>")
          ),
          value = "state_social_det_descriptions",
          
          fluidRow(column(
            8,
            style = "text-align:center;font-weight:bold;background-color: #EBEBEB;",
            tags$h1("COVIDMINDER: Where you live matters"),
            offset = 2
          )),
        
          fluidRow(column(
            8,
            gt_output("determinant_descriptions"),
            
            offset = 2
          ))
        ),
        tabPanel(
          tags$div(
            class = "tab-title",
            style = "text-align:center;",
            HTML("<div><b>DISCLAIMER</b></div>")
          ),
          value = "determinant_disclaimer",
          
          fluidRow(column(
            8,
            style = "text-align:center;font-weight:bold;background-color: #EBEBEB;",
            tags$h1("COVIDMINDER: Where you live matters"),
            offset = 2
          )),
        
          fluidRow(column(
            8,
            class = "about",
            tags$h1("Disclaimer"),
            tags$p(
              "Determinant tabs are experimental and expected to change substantially, current displayed data may not be accurate."
            ),
            offset = 2
          )),
          footer
        )
        
        
      ),
      
      tabPanel(
        HTML("<div><b>ABOUT</b></div>"),
        value = "about",
        
        fluidRow(column(
          8,
          style = "text-align:center;font-weight:bold;background-color: #EBEBEB;",
          tags$h1("COVIDMINDER: Where you live matters"),
          offset = 2
        )),
        
        fluidRow(column(
          8,
          offset = 2,
          class = "about",
          tags$h1("About the Project"),
          HTML(whatisit_text_abt),
          HTML(footer_text)
        ),),
        footer
      )
    
      
      ),
    )
    
  
}

#### Server Code ####
server <- function(input, output, session) {
  # Leaflet plot colors
  colors <-
    c(
      "#253494",
      "#4575B4",
      "#74ADD1",
      "#ABD9E9",
      "#f7f7f7",
      "#FDAE61",
      "#F46D43",
      "#D73027",
      "#BD0026"
    )
  bins <- c(5, 3, 2, 1, .2,-.2,-1,-2,-3,-5)
  
  
  #### State Report Cards Code ####
  
  output$state.report.county.selector <- renderUI ({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    
    # Grab state subset for dataframe
    state.df <- covid_TS_counties_long.cases %>%
      select(-c(countyFIPS, StateFIPS)) %>%
      filter(State == state_initial)
    
    if (state_initial == "NY") {
      nyc.population <- state.df %>%
        filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        group_by(County) %>%
        top_n(n = 1, wt = date) %>%
        select(population)
      nyc.population <- sum(nyc.population$population)
      
      NYC <- state.df %>%
        filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        group_by(State, date) %>%
        summarise(
          County = "New York City",
          cases = sum(cases),
          deaths = sum(deaths),
          population = nyc.population,
          p_cases = sum(cases) * 100000 / nyc.population,
          p_deaths = sum(deaths) * 100000 / nyc.population,
          diff = sum(diff),
          p_diff = sum(diff) * 100000 / nyc.population,
          d_diff = sum(d_diff),
          p.d_diff = sum(p.d_diff) * 100000 / nyc.population
        )
      state.df <- state.df %>%
        filter(!County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        rbind.data.frame(NYC)
    }
    
    counties <- state.df %>%
      select(County) %>%
      unlist() %>%
      unique()
    selected <- state.df %>%
      group_by(County) %>%
      top_n(1, cases) %>%
      arrange(desc(cases)) %>%
      select(County) %>%
      unlist() %>%
      unique()
    
    if (length(selected) > 5) {
      selected <- selected[1:5]
    }
    selectInput(
      inputId = "SRC.county",
      label = "County Selector",
      choices = sort(counties),
      selected = selected,
      multiple = TRUE,
      selectize = FALSE,
      size = min(5, length(counties))
    )
  })
  
  output$US.report.state.selector <- renderUI({
    state.names <- states %>%
      filter(NAME != "District of Columbia") %>%
      select(NAME) %>%
      unlist() %>%
      unique() %>%
      sort()
    
    selected <- states %>%
      arrange(desc(`Daily Case_rate`)) %>%
      select(NAME) %>%
      unlist() %>%
      unique()
    
    if (length(selected > 10)) {
      selected <- selected[1:10]
    }
    selectInput(
      inputId = "NRC.state",
      label = "State Selector",
      choices = state.names,
      selected = selected,
      multiple = TRUE,
      selectize = FALSE,
      size = min(5, length(state.names))
    )
    
  })
  
  output$main_title <- renderUI({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    worst <-
      as.character(unlist(ranking[ranking$rank == 50, "name"]))
    if (!state_name == worst) {
      tagList(tags$h1(paste0(state_name, " Overview")),
              tags$h2(
                tags$a(
                  paste0("State rank: ", ranking[ranking$State == state_initial, "rank"]),
                  tags$sup("*"),
                  href = "#ranking",
                  style = "color:black;"
                )
              ))
    } else{
      tagList(tags$h1(
        paste0(state_name, " Overview (Current Highest Case Rate)")
      ),
      tags$h2(
        tags$a(
          paste0("State rank: ", ranking[ranking$State == state_initial, "rank"]),
          tags$sup("*"),
          href = "#ranking",
          style = "color:black;"
        )
      ))
    }
    
  })
  
  output$state.CoT.title <- renderUI({
    state_name <- input$state_name
    tagList(tags$h2(paste0(state_name, " COVID-19 Case Curve")),
            tags$h3(
              paste0(
                "How have ",
                state_name,
                " COVID-19 Cases Over Time per 100k changed compared to US?"
              )
            ))
  })
  
  
  output$state.DoT.title <- renderUI({
    state_name <- input$state_name
    tagList(tags$h2(paste0(
      state_name, " COVID-19 Mortality Curve"
    )),
    tags$h3(
      paste0(
        "How has ",
        state_name,
        " COVID-19 Mortality Over Time per 100k changed compared to US?"
      )
    ))
  })
  
  output$state.trends.title <- renderUI({
    state_name <- input$state_name
    rate <- input$SRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else {
      #if per/100k
      y.value = "p_diff"
    }
    tagList(tags$h2(paste0(state_name, " Daily Case Trends")),
            tags$h3(
              paste0(
                "How have ",
                get_y_label(y.value),
                " changed in ",
                state_name,
                " counties? (7 day average)"
              )
            ))
  })
  
  output$state.county.cases <- renderUI({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    time <- input$SRC.case.time
    if (time == "Daily") {
      m.a.w <- paste0(" (", time.period, " day average)")
    }
    else {
      m.a.w <- ""
    }
    tagList(tags$h2(paste0(state_name, " COVID-19 Case Hotspots")),
            tags$h3(
              paste0(
                "What are the ",
                state_initial,
                " Countywide disparities in ",
                time,
                " Case Rates?",
                m.a.w
              )
            ))
  })
  
  output$state.county.deaths <- renderUI({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    time <- input$SRC.death.time
    if (time == "Daily") {
      m.a.w <- paste0(" (", time.period, " day average)")
    }
    else {
      m.a.w <- ""
    }
    
    tagList(tags$h2(paste0(
      state_name, " COVID-19 Mortality Hotspots"
    )),
    tags$h3(
      paste0(
        "What are the ",
        state_initial,
        " Countywide disparities in ",
        time,
        " Mortality Rates?",
        m.a.w
      )
    ))
  })
  
  output$determinant.title <- renderUI({
    state_name <- input$state_name
    det <- input$state.determinant
    if (det ==  "CRD Mortality") {
      det <- "Cronic Respiratory Disease (CRD) Mortality"
    } 
    
    if (det ==  "GOP Vote") {
      det <- "2016 Presidential Election Voting Tendancies: GOP Voting Rate"
      
      tagList(tags$h2(paste0(state_name, " ", det)),
              tags$h3(
                paste0(
                  "How do ",
                  state_name, 
                  " GOP voting rates in the 2016 presidential election compare to the US Average?"
                )
              ))
      
    } else {
     tagList(tags$h2(paste0(state_name, " ", det, " Disparities")),
            tags$h3(
              paste0(
                "What are the ",
                state_name,
                " disparities in ",
                det,
                " Rates compared to US Average?"
              )
            ))
    }
  })
  
  output$determinant.text <- renderText({
    det <- input$state.determinant
    if (det ==  "GOP Vote") {
      paste0("2016 voting tendancies may be an indicator of a county's acceptance of state and national COVID-19 guidelines.")
    } else {
    paste0("Nationwide, ",
           det,
           " has been observed as a leading comorbidity of COVID-19.")
    }
  })
  
  output$state.report <- render_gt({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    stats.table(state_initial)
  })
  
  output$map.cases <- renderLeaflet({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    time <- input$SRC.case.time
    if (time == "Daily") {
      param <- "Daily Case"
    }
    else {
      param <- "Case"
    }
    
    geo.plot(state_initial, param)
  })
  
  output$map.cases.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_cases.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      title <-
        tags$h1(paste0(state_name, " COVID-19 Case Hotspots"))
      time <- input$SRC.case.time
      if (time == "Daily") {
        param <- "Daily Case"
      }
      else {
        param <- "Case"
      }
      
      mapshot(
        x = geo.plot(state_initial,
                     param,
                     title = tags$div(title)),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    },
    contentType = 'image/png'
  )
  output$map.cases.data.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_cases_data.csv"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      dataset <- todays.case.data %>%
      filter(State == state_initial)
      write_csv(dataset, file)
    },
    contentType = 'text/csv'
  )
  
  output$map.deaths <- renderLeaflet({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    time <- input$SRC.death.time
    if (time == "Daily") {
      param <- "Daily Mortality"
    }
    else {
      param <- "Mortality"
    }
    
    geo.plot(state_initial, param)
  })
  
  
  output$map.deaths.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_mortality.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      title <-
        tags$h1(paste0(state_name, " COVID-19 Case Hotspots"))
      time <- input$SRC.death.time
      if (time == "Daily") {
        param <- "Daily Mortality"
      }
      else {
        param <- "Mortality"
      }
      mapshot(
        x = geo.plot(state_initial,
                     param,
                     title = tags$div(title)),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    },
    contentType = 'image/png'
  )
  output$map.deaths.data.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_mortality_data.csv"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
      state.abr[state.abr$name == state_name, "abr"]
      dataset <- todays.case.data %>%
      filter(State == state_initial)
      write_csv(dataset, file)
    },
    contentType = 'text/csv'
  )
  
  output$maps.determinant <- renderLeaflet({
    state_name <- input$state_name
    det <- input$state.determinant
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    geo.plot(state_initial, det)
  })
  
  
  output$map.determinant.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      det <- input$state.determinant
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_", det, ".png"))
    },
    content = function(file) {
      state_name <- input$state_name
      det <- input$state.determinant
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      det_title <- det
      if (det ==  "CRD Mortality") {
        det_title <- "Cronic Respiratory Disease (CRD) Mortality"
      }
      title <-
        tags$h1(paste0(
          state_name,
          " ",
          det_title,
          " Rate Disparities Compared to US Average"
        ))
      mapshot(
        x = geo.plot(
          state_initial,
          det,
          title = tags$div(title, class = "leaflet-map-title")
        ),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    },
    contentType = 'image/png'
  )
   output$map.determinant.data.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      det <- input$state.determinant
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_", det, "_data.csv"))
    },
    content = function(file) {
      state_name <- input$state_name
      det <- input$state.determinant
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
   dataset <- todays.case.data %>%
     filter(State == state_initial) 
   write_csv(dataset, file)
    },
    contentType = 'text/csv'
  )
  
  barplot.tooltip <- function(hover,
                              state_initial,
                              y.value = "p_cases",
                              moving.avg.window = 14) {
    #print(session$clientData)
    pixelratio <- session$clientData$pixelratio
    left.offset <- 0
    top.offset <- -200
    
    if (is.null(hover)) {
      return(NULL)
    }
    my_diff <- get_dif(y.value)
    category <- get_y_label(y.value)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    
    if ((hover$range$right - hover$range$left) * (1 - left_pct) < 300) {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left) - 301 *
        pixelratio
    }
    else {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0(
      "position:absolute;
                    z-index:100;
                    width:300px;",
      "left:",
      (left_px) / pixelratio + left.offset,
      "px;
                    top:",
      (top_px) / pixelratio + top.offset,
      "px;"
    )
    
    state_cases <- covid_TS_state_long.cases %>%
      filter(State == state_initial) %>%
      rename(Values = all_of(y.value)) %>%
      rename(Value_diff = all_of(my_diff)) %>%
      mutate(diff.ma =  c(Value_diff[1:7 - 1], zoo::rollmean(Value_diff, 7, align =
                                                               "right"))) %>%
      #mutate(pct_increase =diff.ma/Values*100) %>%
      mutate(pct_increase = Value_diff / Values * 100) %>%
      mutate(ma = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(Values, moving.avg.window, align = "right")
      ))
    #state_cases[state_cases$diff.ma > 0 & state_cases$pct_increase > 5, "pct_increase"] <- 5
    #state_cases[state_cases$Value_diff > 0 & state_cases$pct_increase > 5, "pct_increase"] <- 5
    state_cases[is.na(state_cases$pct_increase) |
                  state_cases$pct_increase < 0, "pct_increase"] <- 0
    state_cases <- state_cases %>%
      filter(date == as.Date(as.POSIXct(hover$x, origin = "1970-01-01"), tz =
                               "EST"))
    # actual tooltip created as wellPanel
    
    five.plus <- ""
    #if (length(state_cases$pct_increase) > 0) {
    #  if(state_cases$pct_increase >= 5) {
    #    five.plus <- "+"
    #  }
    #}
    wellPanel(style = style,
              class = "gg_tooltip",
              p(HTML(
                paste0(
                  "<b> Date: </b>",
                  as.Date(as.POSIXct(hover$x, origin = "1970-01-01"), tz = "EST"),
                  "<br/>",
                  "<b>",
                  category,
                  ": </b>",
                  format(round(state_cases$Values, 2), big.mark = ","),
                  "<br/>",
                  "<b> Change in ",
                  category,
                  ": </b>+",
                  format(round(state_cases$Value_diff, 2), big.mark = ","),
                  "<br/>",
                  "<b> Daily Percentage Increase: </b>",
                  format(round(state_cases$pct_increase, 2), big.mark = ","),
                  "%",
                  five.plus,
                  "<br/>"
                )
              )))
  }
  
  output$state.CoT.tooltip <- renderUI({
    hover <- input$state.CoT.hover
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    
    barplot.tooltip(hover, state_initial, "p_cases")
  })
  
  output$state.DoT.tooltip <- renderUI({
    hover <- input$state.DoT.hover
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    
    barplot.tooltip(hover, state_initial, "p_deaths")
  })
  
  trends.tooltip <- function(hover,
                             state_initial = "NY",
                             y.value = "p_cases",
                             counties = c("Rensselaer"),
                             moving.avg.window = 7) {
    #print(session$clientData)
    pixelratio <- session$clientData$pixelratio
    left.offset <- 0
    top.offset <- -200
    
    #if(is.null()) {return(NULL)}
    y_label <- get_y_label(y.value)
    state.name <- state.abr[state.abr$abr == state_initial, "name"]
    covid_TS_counties.cases.plot <- covid_TS_counties_long.cases %>%
      select(-c(countyFIPS, StateFIPS)) %>%
      filter(State == state_initial) %>%
      group_by(County) %>%
      filter(n() >= moving.avg.window) %>%
      mutate(diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(diff, moving.avg.window, align = "right")
      )) %>%
      mutate(p_diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(p_diff, moving.avg.window, align = "right")
      )) %>%
      ungroup()
    
    if (state_initial == "NY") {
      nyc.population <- covid_TS_counties.cases.plot %>%
        filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        group_by(County) %>%
        top_n(n = 1, wt = date) %>%
        select(population)
      nyc.population <- sum(nyc.population$population)
      
      NYC <- covid_TS_counties.cases.plot %>%
        filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        group_by(State, date) %>%
        summarise(
          County = "New York City",
          cases = sum(cases),
          deaths = sum(deaths),
          population = nyc.population,
          p_cases = sum(cases) * 100000 / nyc.population,
          p_deaths = sum(deaths) * 100000 / nyc.population,
          diff = sum(diff),
          p_diff = sum(diff) * 100000 / nyc.population,
          d_diff = sum(d_diff),
          p.d_diff = sum(p.d_diff) * 100000 / nyc.population
        )
      covid_TS_counties.cases.plot <-
        covid_TS_counties.cases.plot %>%
        filter(!County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        rbind.data.frame(NYC)
      
    }
    
    state <- covid_TS_state_long.cases %>%
      filter(State == state_initial) %>%
      filter(n() >= moving.avg.window) %>%
      mutate(diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(diff, moving.avg.window, align = "right")
      )) %>%
      mutate(p_diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(p_diff, moving.avg.window, align = "right")
      ))
    
    state$County = state_initial
    
    covid_TS_counties.cases.plot <-
      covid_TS_counties.cases.plot %>%
      filter(County %in% counties) %>%
      rbind.data.frame(state) %>%
      filter(get(y.value) > 0) %>%
      group_by(County)
    
    point <-
      nearPoints(
        covid_TS_counties.cases.plot,
        hover,
        threshold = 5,
        maxpoints = 1,
        addDist = TRUE
      ) %>%
      rename(Values = all_of(y.value))
    
    if (nrow(point) == 0)
      return(NULL)
    point <- point[1, ]
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - log(hover$y, 10)) / (hover$domain$top - hover$domain$bottom)
    # Log10 is needed to account for log y axis
    
    # calculate distance from left and bottom side of the picture in pixels
    
    if ((hover$range$right - hover$range$left) * (1 - left_pct) < 200) {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left) - 205 *
        pixelratio
    }
    else {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    
    #left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0(
      "position:absolute;
                    z-index:100;
                    width: 200px;",
      "left:",
      (left_px) / pixelratio + left.offset,
      "px;
                    top:",
      (top_px) / pixelratio + top.offset,
      "px;",
      "pointer-events: none;"
    )
    
    state.or.county = "County: "
    if (state_initial %in% point$County) {
      state.or.county = "State: "
      point <- point %>%
        left_join(state.abr[c("name", "abr")],
                  by = c("State" = "abr"))
    }
    else {
      point$name <- point$County
    }
    wellPanel(style = style,
              class = "gg_tooltip",
              p(HTML(
                paste0(
                  "<b>",
                  state.or.county,
                  "</b>",
                  point$name,
                  "<br/>",
                  "<b> Date: </b>",
                  point$date,
                  "<br/>",
                  "<b>",
                  y_label,
                  ": </b>",
                  format(round(point$Values), big.mark = ","),
                  "<br/>"
                )
              )))
    
  }
  
  output$state.trends.tooltip <- renderUI({
    hover <- input$state.trends.hover
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    counties <- input$SRC.county
    rate <- input$SRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else {
      #if per/100k
      y.value = "p_diff"
    }
    
    trends.tooltip(hover, state_initial, y.value, counties)
  })
  
  nation.trends.tooltip <- function(hover,
                                    y.value = "p_cases",
                                    selected.states = c(),
                                    moving.avg.window = 7) {
    pixelratio <- session$clientData$pixelratio
    left.offset <- 0
    top.offset <- -200
    
    y_label <- get_y_label(y.value)
    covid_TS_state.cases.plot <- covid_TS_state_long.cases %>%
      select(-c(population)) %>%
      filter(State %in% selected.states$abr) %>%
      group_by(State) %>%
      filter(n() >= moving.avg.window) %>%
      mutate(diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(diff, moving.avg.window, align = "right")
      )) %>%
      mutate(p_diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(p_diff, moving.avg.window, align = "right")
      )) %>%
      ungroup()
    
    US <- covid_TS_US_long.cases %>%
      mutate(diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(diff, moving.avg.window, align = "right")
      )) %>%
      mutate(p_diff = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(p_diff, moving.avg.window, align = "right")
      ))
    
    US$State = "US"
    
    covid_TS_state.cases.plot <-  covid_TS_state.cases.plot %>%
      group_by(State) %>%
      ungroup() %>%
      rbind.data.frame(US) %>%
      filter(get(y.value) > 1)
    
    point <-
      nearPoints(
        covid_TS_state.cases.plot,
        hover,
        threshold = 5,
        maxpoints = 1,
        addDist = TRUE
      ) %>%
      rename(Values = all_of(y.value)) %>%
      left_join(selected.states,
                by = c("State" = "abr"))
    
    if (nrow(point) == 0)
      return(NULL)
    point <- point[1, ]
    
    state.or.national <- "State: "
    if (is.na(point$name)) {
      point$name <- ""
      state.or.national <- "United States"
    }
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - log(hover$y, 10)) / (hover$domain$top - hover$domain$bottom)
    # Log10 is needed to account for log y axis
    
    # calculate distance from left and bottom side of the picture in pixels
    if ((hover$range$right - hover$range$left) * (1 - left_pct) < 200) {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left) - 201 *
        pixelratio
    }
    else {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    #left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0(
      "position:absolute;
                    z-index:100;
                    width:200px;",
      "left:",
      (left_px) / pixelratio + left.offset,
      "px;
                    top:",
      (top_px) / pixelratio + top.offset,
      "px;"
    )
    
    wellPanel(style = style,
              class = "gg_tooltip",
              p(HTML(
                paste0(
                  "<b>",
                  state.or.national,
                  "</b>",
                  point$name,
                  "<br/>",
                  "<b> Date: </b>",
                  point$date,
                  "<br/>",
                  "<b>",
                  y_label,
                  ": </b>",
                  format(round(point$Values), big.mark = ","),
                  "<br/>"
                )
              )))
  }
  
  output$US.trends.tooltip <- renderUI({
    hover <- input$US.trends.hover
    selected.states <- data.frame(name = input$NRC.state)
    selected.states <- selected.states %>%
      left_join(state.abr[c("name", "abr")],
                by = c("name" = "name"))
    rate <- input$NRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else {
      #if per/100k
      y.value = "p_diff"
    }
    
    nation.trends.tooltip(hover, y.value, selected.states)
  })
  
  output$state.CoT <- renderPlot({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    ggbar.overall(state_initial,
                  y.value = "p_cases",
                  remove.title = T) +
      #geom_vline(xintercept=reactive.line$x, color= "black", linetype="solid", size = 1, show.legend = F) +
      NULL
  })
  
  output$state.CoT.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_CoT_plot.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      ggsave(
        filename = file,
        plot = ggbar.overall(
          state_initial,
          y.value = "p_cases",
          remove.title = F,
          date = update_date
        ) + NULL,
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
    },
    contentType = 'image/png'
  )
    output$state.CoT.data.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_CoT_data.csv"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      state <- covid_TS_state_long.cases %>%
    filter(State == state_initial)
  write_csv(state, file)
    },
    contentType = 'image/png'
  )
  
  output$state.DoT.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_DoT_plot.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      ggsave(
        filename = file,
        plot = ggbar.overall(
          state_initial,
          y.value = "p_deaths",
          remove.title = F,
          date = update_date
        ) + NULL,
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
    },
    contentType = 'image/png'
  )
    output$state.DoT.data.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_DoT_data.csv"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      state <- covid_TS_state_long.cases %>%
    filter(State == state_initial)
  write_csv(state, file)
    },
    contentType = 'image/png'
  )
  
  output$state.DoT <- renderPlot({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    ggbar.overall(state_initial,
                  y.value = "p_deaths",
                  remove.title = T)
  })
  
  Tr.ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$trends.dbl_click, {
    brush <- input$trends.brush
    if (!is.null(brush)) {
      Tr.ranges$x <-
        as.POSIXct(c(brush$xmin, brush$xmax), origin = "1970-01-01")
      Tr.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      Tr.ranges$x <- NULL
      Tr.ranges$y <- NULL
    }
  })
  
  output$state.trends <- renderPlot({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    counties <- input$SRC.county
    rate <- input$SRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else {
      #if per/100k
      y.value = "p_diff"
    }
    
    ggplot.state(
      state_initial,
      y.value = y.value,
      counties = counties,
      remove.title = T
    ) +
      coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
      NULL
  })
  
  output$state.trends.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_trends_plot.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      counties <- input$SRC.county
      rate <- input$SRC.rate
      if (rate == "Overall") {
        y.value = "diff"
      }
      else {
        #if per/100k
        y.value = "p_diff"
      }
      ggsave(
        filename = file,
        plot = ggplot.state(
          state_initial,
          y.value = y.value,
          counties = counties,
          remove.title = F,
          date = update_date
        ) +
          coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
          NULL,
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
    },
    contentType = 'image/png'
  )
    output$state.trends.data.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_trends_data.csv"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <-
        state.abr[state.abr$name == state_name, "abr"]
      counties <- input$SRC.county
      rate <- input$SRC.rate
      if (rate == "Overall") {
        y.value = "diff"
      }
      else {
        #if per/100k
        y.value = "p_diff"
      }
    covid_TS_counties.cases.plot <- covid_TS_counties_long.cases %>%
    select(-c(countyFIPS, StateFIPS)) %>%
    filter(State == state_initial) %>%
    group_by(County) %>% 
    filter(n() >= moving.avg.window) %>%
    mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
    mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
    ungroup()
    write_csv(covid_TS_counties.cases.plot, file)
    },
    contentType = 'text/csv'
  )
  
  output$ranking.table <- render_gt({
    entries <- input$entries
    if ("Ascending" %in% input$rank.order) {
      order <- function(x) {
        x
      }
    }
    if ("Descending" %in% input$rank.order) {
      order <- dplyr::desc
    }
    
    # reconstruct the front of the url from the session
    front_url = paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, session$clientData$url_pathname)
    gt.ranking(as.numeric(entries), order, front_url)
  })
  
  ### National Overview Code ###
  
  # TODO: Remove second version of ranking.table
  output$US.ranking.table <- render_gt({
    entries <- input$US.entries
    if ("Ascending" %in% input$US.rank.order) {
      order <- function(x) {
        x
      }
    }
    if ("Descending" %in% input$US.rank.order) {
      order <- dplyr::desc
    }
    # reconstruct the front of the url from the session
    front_url = paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, session$clientData$url_pathname)
    gt.ranking(as.numeric(entries), order, front_url)
  })
  
  output$US.trends.title <- renderUI({
    rate <- input$NRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else {
      #if per/100k
      y.value = "p_diff"
    }
    tagList(tags$h2("United States Daily Case Trends"),
            tags$h3(
              paste0(
                "How have ",
                get_y_label(y.value),
                " changed in US states? (7 day average)"
              )
            ))
  })
  
  output$US.determinant.title <- renderUI({
    tagList(tags$h2(paste0(
      "Significant United States Determinants"
    )),
    tags$h3(
      paste0(
        "What are the significant socioeconomic and medical determinants that impact COVID mortality rates?"
      )
    ))
  })
  
  output$US.determinant.text <- renderText({
    det <- input$US.determinant
    paste0("Nationwide, ",
           det,
           " has been observed as a leading comorbidity of COVID-19.")
  })
  
  output$US.map.cases <- renderLeaflet({
    time <- input$NRC.case.time
    if (time == "Daily") {
      param <- "Daily Case"
    }
    else {
      param <- "Case"
    }
    geo.plot("US", param)
  })
  
  output$US.map.cases.dl <- downloadHandler(
    filename = function() {
      return("US_cases.png")
    },
    content = function(file) {
      title <-
        tags$h1(style = "text-align:center;", "US COVID-19 Case Hotspots")
      time <- input$NRC.case.time
      if (time == "Daily") {
        param <- "Daily Case"
      }
      else {
        param <- "Case"
      }
      mapshot(
        x = geo.plot("US",
                     param,
                     title = tags$div(title)),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    },
    contentType = 'image/png'
  )
  output$US.map.cases.data.dl <- downloadHandler(
    filename = function() {
      return("US_cases_data.csv")
    },
    content = function(file) {
   write_csv(states, file) 
    },
    contentType = 'text/csv'
  )
  
  output$US.determinants.dl <- downloadHandler(
    filename = function() {
      return("US_determinants_analysis.png")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = ggplot.natDet(remove.title = F),
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
      #file.copy("www/national_sd.png", file)
    },
    contentType = 'image/png'
  )
    output$US.determinants.data.dl <- downloadHandler(
    filename = function() {
      return("US_determinants_analysis_data.csv")
    },
    content = function(file) {
      write_csv(GWAS_data, file)
    },
    contentType = 'image/png'
  )
  
  output$US.report <- render_gt({
    US.stats.table()
  })
  
  output$US.map.testing <- renderLeaflet({
    geo.plot("US", "Daily Testing", reverse = T)
  })
  
  output$US.maps.testing.dl <- downloadHandler(
    filename = function() {
      return("US_mortality.png")
    },
    content = function(file) {
      title <-
        tags$h2(style = "text-align:center;", "US COVID-19 Testing Disparities")
      #time <- input$NRC.deaths.time
      #if (time == "Daily") {
      #  param <- "Daily Mortality"
      #}
      #else {
      #  param <- "Mortality"
      #}
      mapshot(
        x = geo.plot(
          "US",
          "Daily Testing",
          title = tags$div(title),
          reverse = T
        ),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    },
    contentType = 'image/png'
  )
  output$US.maps.testing.data.dl <- downloadHandler(
    filename = function() {
      return("US_testing_data.csv")
    },
    content = function(file) {
      write_csv(states, file)
    },
    contentType = 'text/csv'
  )
  
  output$US.map.deaths <- renderLeaflet({
    time <- input$NRC.deaths.time
    if (time == "Daily") {
      param <- "Daily Mortality"
    }
    else {
      param <- "Mortality"
    }
    geo.plot("US", param)
  })
  
  output$US.map.deaths.dl <- downloadHandler(
    # filename = function() {
    #   return("US_mortality.png")
    # },
    filename = "US_mortality.png",
    content = function(file) {
      title <-
        tags$h2(style = "text-align:center;", "US COVID-19 Mortality Hotspots")
      time <- input$NRC.deaths.time
      if (time == "Daily") {
        param <- "Daily Mortality"
      }
      else {
        param <- "Mortality"
      }
      mapshot(
        x = geo.plot("US",
                     param,
                     title = tags$div(title)),
        file = file,
        cliprect = "viewport",
        selfcontained = T
      )
    },
    contentType = 'image/png'
  )
    output$US.map.deaths.data.dl <- downloadHandler(
    filename = "US_mortality_data.csv",
    content = function(file) {
      write_csv(states, file)
    },
    contentType = 'text/csv'
  )
  
  US.barplot.tooltip <- function(hover,
                                 y.value = "cases",
                                 moving.avg.window = 14) {
    pixelratio <- session$clientData$pixelratio
    left.offset <- 0
    top.offset <- -200
    
    if (is.null(hover)) {
      return(NULL)
    }
    my_diff <- get_dif(y.value)
    category <- get_y_label(y.value)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <-
      (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <-
      (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    if ((hover$range$right - hover$range$left) * (1 - left_pct) < 250) {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left) - 251 *
        pixelratio
    }
    else {
      left_px <-
        hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    top_px <-
      hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0(
      "position:absolute;
                    z-index:100;
                    width:250px;",
      "left:",
      (left_px) / pixelratio + left.offset,
      "px;
                    top:",
      (top_px) / pixelratio + top.offset,
      "px;"
    )
    
    US.ma <- covid_TS_US_long.cases %>%
      rename(Values = all_of(y.value)) %>%
      rename(my_diff = all_of(my_diff)) %>%
      mutate(diff.ma =  c(my_diff[1:7 - 1], zoo::rollmean(my_diff, 7, align =
                                                            "right"))) %>%
      #mutate(pct_increase =diff.ma/Values*100) %>%
      mutate(pct_increase = my_diff / Values * 100) %>%
      mutate(ma = c(
        numeric(moving.avg.window - 1),
        zoo::rollmean(Values, moving.avg.window, align = "right")
      )) %>%
      filter(ma > 0)
    #US.ma[US.ma$diff.ma > 0 & US.ma$pct_increase > 5, "pct_increase"] <- 5
    #US.ma[US.ma$my_diff > 0 & US.ma$pct_increase > 5, "pct_increase"] <- 5
    US.ma[is.na(US.ma$pct_increase) |
            US.ma$pct_increase <= 0, "pct_increase"] <- 0
    US.ma <- US.ma %>%
      filter(date == as.Date(as.POSIXct(hover$x, origin = "1970-01-01"), tz =
                               "EST"))
    
    five.plus <- ""
    #if (length(US.ma$pct_increase) > 0) {
    #  if(US.ma$pct_increase >= 5) {
    #    five.plus <- "+"
    #  }
    #}
    
    wellPanel(style = style,
              class = "gg_tooltip",
              p(HTML(
                paste0(
                  "<b> Date: </b>",
                  as.Date(as.POSIXct(hover$x, origin = "1970-01-01"), tz = "EST"),
                  "<br/>",
                  "<b>",
                  category,
                  ": </b>",
                  format(round(US.ma$Values, 2), big.mark = ","),
                  "<br/>",
                  "<b> Change in ",
                  category,
                  ": </b>+",
                  format(round(US.ma$my_diff, 2), big.mark = ","),
                  "<br/>",
                  "<b> Daily Percentage Increase: </b>",
                  format(round(US.ma$pct_increase, 2), big.mark = ","),
                  "%",
                  five.plus,
                  "<br/>"
                )
              )))
    
  }
  
  output$US.CoT.tooltip <- renderUI({
    hover <- input$US.CoT.hover
    US.barplot.tooltip(hover, "cases")
  })
  
  output$US.DoT.tooltip <- renderUI({
    hover <- input$US.DoT.hover
    US.barplot.tooltip(hover, "deaths")
  })
  
  output$US.determinants <- renderPlot({
    ggplot.natDet(remove.title = T)
  })
  
  output$US.CoT <- renderPlot({
    ggbar.US(y.value = "cases", remove.title = T)
  })
  
  output$US.CoT.dl <- downloadHandler(
    filename = function() {
      return("US_CoT_plot.png")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = ggbar.US(
          y.value = "cases",
          remove.title = F,
          date = update_date
        ) + NULL,
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
    },
    contentType = 'image/png'
  )
  output$US.CoT.data.dl <- downloadHandler(
    filename = function() {
      return("US_CoT_data.csv")
    },
    content = function(file) {
      y.value="cases"
      my_diff <- get_dif(y.value)
      US.ma <- covid_TS_US_long.cases %>%
      rename(Values = all_of(y.value)) %>%
      rename(my_diff = all_of(my_diff)) %>%
      mutate(diff.ma =  c(my_diff[1:7-1], zoo::rollmean(my_diff, 7, align="right"))) %>%
      mutate(pct_increase =my_diff/Values*100) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
      names(US.ma)[names(US.ma) == "Values"] <- "cases"
      names(US.ma)[names(US.ma) == "ma"] <- "cases_7day_moving_average"
      write.csv(select(US.ma, "date", "cases", "cases_7day_moving_average"), file);
    },
    contentType = 'text/csv'
  )
  output$US.DoT <- renderPlot({
    ggbar.US(y.value = "deaths", remove.title = T)
  })
  
  output$US.DoT.dl <- downloadHandler(
    filename = function() {
      return("US_DoT_plot.png")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = ggbar.US(
          y.value = "deaths",
          remove.title = F,
          date = update_date
        ) + NULL,
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
    },
    contentType = 'image/png'
  )
    output$US.DoT.data.dl <- downloadHandler(
    filename = function() {
      return("US_DoT_data.csv")
    },
    content = function(file) {
      y.value="deaths"
      my_diff <- get_dif(y.value)
      US.ma <- covid_TS_US_long.cases %>%
      rename(Values = all_of(y.value)) %>%
      rename(my_diff = all_of(my_diff)) %>%
      mutate(diff.ma =  c(my_diff[1:7-1], zoo::rollmean(my_diff, 7, align="right"))) %>%
      mutate(pct_increase =my_diff/Values*100) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
      names(US.ma)[names(US.ma) == "Values"] <- "deaths"
      names(US.ma)[names(US.ma) == "ma"] <- "deaths_7day_moving_average"
      write.csv(select(US.ma, "date", "deaths", "deaths_7day_moving_average"), file);
    },
    contentType = 'text/csv'
  )
  
  output$US.trends <- renderPlot({
    selected.states <- data.frame(name = input$NRC.state)
    selected.states <- selected.states %>%
      left_join(state.abr[c("name", "abr")],
                by = c("name" = "name"))
    rate <- input$NRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else {
      #if per/100k
      y.value = "p_diff"
    }
    
    ggplot.US(
      y.value = y.value,
      moving.avg.window = 7,
      selected.states = selected.states$abr,
      remove.title = T
    ) +
      coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
      NULL
  })
  
  output$US.trends.dl <- downloadHandler(
    filename = function() {
      return("US_trends_plot.png")
    },
    content = function(file) {
      selected.states <- data.frame(name = input$NRC.state)
      selected.states <- selected.states %>%
        left_join(state.abr[c("name", "abr")],
                  by = c("name" = "name"))
      rate <- input$NRC.rate
      if (rate == "Overall") {
        y.value = "diff"
      }
      else {
        #if per/100k
        y.value = "p_diff"
      }
      ggsave(
        filename = file,
        plot = ggplot.US(
          y.value = y.value,
          moving.avg.window = 7,
          selected.states = selected.states$abr,
          remove.title = F,
          date = update_date
        ) +
          coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
          NULL,
        device = "png",
        width = 12,
        height = 8,
        units = "in"
      )
    },
    contentType = 'image/png'
  )
    output$US.trends.data.dl <- downloadHandler(
    filename = function() {
      return("US_trends_data.csv")
    },
    content = function(file) {
  write_csv(covid_TS_state_long.cases, file)
    },
    contentType = 'text/csv'
  )
    
  #### state determinants output ####
    # import the function for making the plots
    source("./data/social_det_gen/getplotdata.R")
    ## this output just puts the image on the page
    # output$social_det_graph <- renderImage({
    #   # When input$n is 1, filename is ./images/image1.jpeg
    #   filename <- paste0("./data/social_det_gen/covid_determinants_output/images/kendall", input$social_det_picker,".png")
    #   filename <- normalizePath(file.path(filename))
    #   # Return a list containing the filename
    #   list(src = filename)
    # }, deleteFile = FALSE)
    
    ## instead let's build the ggplot object
    output$social_det_graph <- renderPlot(
      make_state_det_image(input$social_det_picker)
    )
    # for the determinant descriptions table
    output$determinant_descriptions <- render_gt(
      gt(read_csv("./data/social_det_gen/covid_determinants_data/SocialDeterminants.csv")[,c("Name", "Definitions")])
    )
    
    
  ### The following code deals with setting or responding to parameterized URLs
  observe({
    # trigger observer on these values
    input$state_name
    input$tab
    # update the list of reactive variables we are excluding from our bookmarked url and exclude them by passing said list to setbookmarkexclude
    # isolate so we don't trigger observe on literally every input change
    reactvals <- names(isolate(reactiveValuesToList(input)))
    toparameterize <- c("state_name", "tab")
    toexclude = reactvals[!(reactvals %in% toparameterize)]
    setBookmarkExclude(toexclude)
    # bookmark application state
    session$doBookmark()
  })
  # every time we bookmark application state set the ada modal flag to true
  onBookmark(function(state) {
    state$values$adamodal <- TRUE
  })
  # every time we bookmark application state we update the url
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  
  # Content of modal dialog
  query_modal <- modalDialog(
    title = "Welcome to COVIDMINDER",
    "PLEASE NOTE: This application is the result of the efforts of students at 
    Rensselaer’s Data INCITE Lab. It is presented here to showcase the talents 
    of our students.  The application may not meet all of the standards one might 
    expect of a production commercial product.",
    easyClose = F,
    footer = tagList(actionButton("run", "Continue with COVIDMINDER"))
  )
  
  # the following is a hack to prevent the ADA disclaimer from popping up on page load if the user
  # has clicked on a state link from the US.ranking.table
  # the links in the table have a tag in the url telling the app not to display the disclaimer
  # the following code checks if we are loading the page with such a tagged url and removes the tag if so
  
  onRestore(function(state) {
  # if the tag is in the url, remove it and don't show the modal
    
  if(parseQueryString(session$clientData$url_search)$adamodal == "true"){
    # Creates modal dialog
    showModal(query_modal)
  }
  })
  
  # Removes modal
  observeEvent(input$run, {
    removeModal()
  })
  
  
  
  
  
 
  
  
  
  
  
  
}
#### Set up Shiny App ####
shinyApp(ui = ui,
         server = server,
         enableBookmarking = "url")
