#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")
source("modules/ggplot_gen.R")
source("modules/leaflet_gen.R")
source("modules/gt_gen.R")
sourceDir("modules/shiny/R")


update_date <- "8-12-2020" # makes it easy to change all occurances when we update


moving.avg.window <- 7 # WARNING: Behavior for moving.avg.window > number of report dates for a region is undefined.
                       # (i.e. a 20 day window if Catskill Region has 19 report dates.)
height <- "600px"# plot heights

# Leaving this in case we need it
# TODO: Implement other text as strings like this...
rpi_accessibility_link <- "<div class='center'><p><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a></p></div>"


footer_text <- "<p>COVID<b>MINDER</b> analysis and visualizations</b> are by students and staff
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

whatisit_text_abt <-"<p>COVID<b>MINDER</b> reveals the regional disparities 
                                in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct 
                                effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact 
                                COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic.</p>"

whatisit_text <- "COVIDMINDER reveals the regional disparities in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic."


comments_link <-"<img src='comment.png' style='float:left;width:40px;padding-right:2px;' >
                                Thanks for using <b>COVIDMINDER!</b> Please take a few moments 
                                to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a><br><br>
                                <i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement 
                                of Accessibility</a></i>"

# For URL parameterization
url1 <- url2 <- ""

#### UI Code ####
ui <- 
  tagList(
    tags$html(lang = "en-us"),
    tags$head(includeHTML("www/analytics.html")),
    navbarPage(
      id="tab",
      theme="style.css",
      title=tags$a(class="title-text",
                   title = whatisit_text,
                   name = "top",
                   href = "/",
                   img(class="logo", src="Rensselaer_round.png", alt="Small Rensselaer Polytechnic Institute Logo"),
                   HTML("COVID<b>MINDER</b>")),
      windowTitle = "COVIDMINDER: Where you live matters",
      tabPanel(title = HTML("<div><b>STATE REPORT CARDS</b></div>"),
               value = "state_report_cards",
                  fluidPage(
                    fluidRow(column(12,
                                    selectInput(inputId = "state_name",
                                                label = "State Selector",
                                                title = "State selecting form tool.",
                                                choices = state.abr$name,
                                                selected = as.character(unlist(ranking[ranking$rank==50, "name"]))),
                                    tags$div(style = "float:right;",
                                             HTML(paste("<b>Date:</b>", update_date))))),
                    fluidRow(column(12, style="text-align:center;",uiOutput("main_title"))),
                    tags$br(),
                    fluidRow(column(8, style="text-align:center;",
                                    tags$b(tags$sup("*"),"States are ranked best to worst by their percentage change in COVID-19 cases over the past ",time.period," days."),
                                    offset=2)),
                    fluidRow(column(12, style="text-align:center;position:relative;",uiOutput("state.CoT.title"),
                                    plotOutput(outputId = "state.CoT", 
                                               height = height, 
                                               hover = hoverOpts(id = "state.CoT.hover",
                                                                 delay = 100,
                                                                 delayType = "throttle")),
                                    uiOutput("state.CoT.tooltip")),
                             column(1, downloadButton("state.CoT.dl", label="Download Case Barplot"),offset = 9),
                             column(12, style="text-align:center;position:relative;",uiOutput("state.DoT.title"),
                                    plotOutput(outputId = "state.DoT", 
                                               height = height, 
                                               hover = hoverOpts(id = "state.DoT.hover",
                                                                 delay = 100,
                                                                 delayType = "throttle")),
                                    uiOutput("state.DoT.tooltip"))),
                             column(1, downloadButton("state.DoT.dl", label="Download Mortality Barplot"), offset = 9),
                    fluidRow(column(8, style="text-align:center;",
                                    tags$h2("Flattening the Curve"),
                                    tags$p("Nationwide, states have taken various approaches to mitigate the spread of coronavirus, such as social distancing interventions and encouraging mask use where social distancing is not possible. Studies by the CDC have shown these methods reduce new COVID-19 cases, hospitalizations, and deaths."),
                                    tags$b("Data Source: "), tags$a("CDC", href="https://wwwnc.cdc.gov/eid/article/26/8/20-1093_article"), offset=2)),
                    tags$br(),
                    tags$br(),
                    fluidRow(column(8,gt_output("state.report"),
                                    offset = 2)),
                    fluidRow(column(12, style="text-align:center;",
                                    tags$h1("County Level Breakdown"))),
                    fluidRow(column(12, style="text-align:center;position:relative;",uiOutput("state.trends.title"),
                                    tags$div(style = "height:130px;width:100%;text-align:left;padding-left:4%;",
                                    uiOutput("state.report.county.selector"),
                                             radioButtons(inputId = "SRC.rate",
                                                 label = "Rate",
                                                 choices = c("Overall", "Per/100k"),
                                                 selected = "Per/100k")),
                                    tags$div(style="text-align:left;padding-left:4%", "Use the above form to select 1 or multiple Counties."),
                                    plotOutput(outputId = "state.trends", 
                                               height=height,
                                               hover = hoverOpts(id = "state.trends.hover",
                                                                 delay = 100,
                                                                 delayType = "throttle"),
                                               dblclick = "trends.dbl_click",
                                               brush = brushOpts(
                                                 id = "trends.brush",
                                                 resetOnNew = TRUE)),
                                    uiOutput("state.trends.tooltip"))),
                             column(1, downloadButton("state.trends.dl", label="Download Case Trends Plot"), offset = 9),
                    tags$br(),
                    fluidRow(column(6, style="text-align:center;",
                                    tags$div(class = "info",
                                             HTML("<h2>Disparity Color Legend</h2>
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
                               </div>")), offset=3)),
                    fluidRow(column(6,
                                    tags$div(style = "text-align:center;",uiOutput("state.county.cases")),
                                    radioButtons(inputId = "SRC.case.time",
                                                 label = "Time Frame",
                                                 choices = c("Daily", "Overall"),
                                                 selected = "Daily",
                                                 inline = T),
                                    leafletOutput("map.cases", height = height),
                                    column(2, downloadButton("map.cases.dl", label="Download Case Map"), offset=6)),
                             column(6,
                                    tags$div(style = "text-align:center;",uiOutput("state.county.deaths")),
                                    radioButtons(inputId = "SRC.death.time",
                                                 label = "Time Frame",
                                                 choices = c("Daily", "Overall"),
                                                 selected = "Daily",
                                                 inline = T),
                                    leafletOutput("map.deaths", height = height),
                                    column(2, downloadButton("map.deaths.dl", label="Download Mortality Map"), offset = 6))),
                    tags$br(),
                    fluidRow(column(12, style="text-align:center;",
                                    tags$h1("Comorbidities"))),
                    fluidRow(column(12, style="text-align:center;",
                                    uiOutput("determinant.title")),
                             column(6,
                                    selectInput(inputId = "state.determinant",
                                                label = "Determinant",
                                                title = "Determinant selecting state tool.",
                                                choices = c("Diabetes", "Obesity", "CRD Mortality"),
                                                selected = "Diabetes"),
                                    leafletOutput("maps.determinant", height = height),
                                    column(2, downloadButton("map.determinant.dl", label="Download Determinant Map"), offset = 6), offset = 3),
                             column(8, style="text-align:center;",
                                    tags$p(textOutput("determinant.text"),
                                           tags$br(),
                                           tags$b("Data Source: "), tags$a("CDC", href="www.cdc.gov/diabetes/data"), ", ",
                                           tags$a("CHR", href = 
                                                    "https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/health-behaviors/diet-exercise/adult-obesity")), 
                                    offset = 2
                             )),
                    tags$br(),
                    tags$br()
                    ,fluidRow(column(12, style="text-align:center;",
                                     tags$h1("Rankings")),
                              tags$a(name = "ranking"),
                              column(8,
                                     fluidRow(style="float:right;width:250px;",
                                              selectInput(inputId = "entries",
                                                          label = "Entries",
                                                          title = "Entry amount selecting form tool.",
                                                          choices = c(`Show 10` = 10, 
                                                                      `Show 25` = 25,
                                                                      `Show 50` = 50),
                                                          selected = 10,
                                                          width = "50%"),
                                              radioButtons(inputId = "rank.order",
                                                           label = "Order",
                                                           choices = c("Ascending", "Descending"),
                                                           selected = "Descending",
                                                           width="50%")),
                                     gt_output("ranking.table"), offset = 2))
                 )
      ),
      tabPanel(title = HTML("<b>NATIONAL REPORT CARD</b>"),
               value = "national_report_card",
               tags$div(style = "float:right;",
                        HTML(paste("<b>Date:</b>", update_date))),
               fluidRow(column(12, style="text-align:center;",tags$h1("United States Overview"))),
               tags$br(),
               fluidRow(column(10, style="text-align:center;position:relative;",
                               tags$h2("United States COVID-19 Case Curve"),
                               tags$h3("How have United States overall COVID-19 Cases changed over time?"),
                               plotOutput(outputId = "US.CoT", 
                                          height = height,
                                          hover = hoverOpts(id = "US.CoT.hover",
                                                           delay = 100,
                                                           delayType = "throttle")),
                               uiOutput("US.CoT.tooltip"), offset = 1),
                        column(1, downloadButton("US.CoT.dl", label="Download Case Barplot"),offset = 9),
                        column(10, style="text-align:center;position:relative;",
                               tags$h2("United States COVID-19 Mortality Curve"),
                               tags$h3("How have United States overall COVID-19 deaths changed over time?"),
                               plotOutput(outputId = "US.DoT", 
                                          height = height,
                                          hover = hoverOpts(id = "US.DoT.hover",
                                                            delay = 100,
                                                            delayType = "throttle")),
                               uiOutput("US.DoT.tooltip"), offset = 1),
                        column(1, downloadButton("US.DoT.dl", label="Download Mortality Barplot"),offset = 9)),
               fluidRow(column(8, style="text-align:center;",
                               tags$h2("Flattening the Curve"),
                               tags$p("Nationwide, states have taken various approaches to mitigate the spread of coronavirus, such as social distancing interventions and encouraging mask use where social distancing is not possible. Studies by the CDC have shown these methods reduce new COVID-19 cases, hospitalizations, and deaths."),
                               tags$b("Data Source: "), tags$a("CDC", href="https://wwwnc.cdc.gov/eid/article/26/8/20-1093_article"), offset=2)),
               tags$br(),
               tags$br(),
               fluidRow(column(8,gt_output("US.report"),
                               offset = 2)),
               fluidRow(column(12, style="text-align:center;",
                               tags$h1("State Level Breakdown"))),
               fluidRow(column(10, style="text-align:center;position:relative;",uiOutput("US.trends.title"),
                               tags$div(style = "height:130px;text-align:left;padding-left:4%;",
                                        uiOutput("US.report.state.selector"),
                                        radioButtons(inputId = "NRC.rate",
                                                     label = "Rate",
                                                     choices = c("Overall", "Per/100k"),
                                                     selected = "Per/100k")),
                               tags$div(style="text-align:left;padding-left:4%", "Use the above form to select 1 or multiple States."),
                               plotOutput(outputId = "US.trends", 
                                          height=height,
                                          hover = hoverOpts(id = "US.trends.hover",
                                                            delay = 100,
                                                            delayType = "throttle"),
                                          dblclick = "trends.dbl_click",
                                          brush = brushOpts(
                                            id = "trends.brush",
                                            resetOnNew = TRUE)),
                               uiOutput("US.trends.tooltip"), offset = 1),
                        column(1, downloadButton("US.trends.dl", label="Download Case Trend Plot"), offset = 9)),
               tags$br(),
               fluidRow(column(4, style="text-align:center;",
                               tags$div(class = "info",
                               HTML("<h2>Disparity Color Legend</h2>
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
                               </div>")), offset=4)),
               fluidRow(column(6,
                               tags$h2(style="text-align:center;", "US COVID-19 Case Hotspots"),
                               tags$h3(style="text-align:center;", paste0("What are the Nationwide disparities in Daily Case Rates? (",time.period, " day average)")),
                               radioButtons(inputId = "NRC.case.time",
                                            label = "Time Frame",
                                            choices = c("Daily", "Overall"),
                                            selected = "Daily",
                                            inline = T),
                               leafletOutput("US.map.cases", height = height),
                               column(2, downloadButton("US.map.cases.dl", label="Download Case Map"), offset=6)),
                        column(6,
                               tags$h2(style="text-align:center;", "US COVID-19 Mortality Hotspots"),
                               tags$h3(style="text-align:center;", paste0("What are the Nationwide disparities in Daily Mortality Rates? (",time.period, " day average)")),
                               radioButtons(inputId = "NRC.deaths.time",
                                            label = "Time Frame",
                                            choices = c("Daily", "Overall"),
                                            selected = "Daily",
                                            inline = T),
                               leafletOutput("US.map.deaths", height = height),
                               column(2, downloadButton("US.map.deaths.dl", label="Download Mortality Map"), offset = 6)),
                        column(6,
                               tags$h2(style="text-align:center;", "US COVID-19 Testing Disparities"),
                               tags$h3(style="text-align:center;", "What are the Nationwide disparities in COVID-19 Testing?"),
                               leafletOutput("US.map.testing", height = height), 
                               column(2, downloadButton("US.maps.testing.dl", label="Download Testing Map"), offset = 6), offset = 3
                               )),
               tags$br(),
               fluidRow(column(12, style="text-align:center;",
                               uiOutput("US.determinant.title")),
                        column(6,
                               selectInput(inputId = "US.determinant",
                                           label = "Determinant",
                                           title = "Determinant selecting form tool",
                                           choices = c("Diabetes", "Obesity", "CRD Mortality", "Heart Disease Mortality"),
                                           selected = "Diabetes"),
                               leafletOutput("US.maps.determinant", height = height),
                               column(2, downloadButton("US.maps.determinant.dl", label="Download Determinant Map"), offset = 6), offset = 3),
                        column(8, style="text-align:center;",
                               tags$p(textOutput("US.determinant.text"),
                                      tags$br(),
                                      tags$b("Data Source: "), tags$a("CDC", href="www.cdc.gov/diabetes/data"), ", ",
                                      tags$a("CHR", href = 
                                               "https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/health-behaviors/diet-exercise/adult-obesity")), 
                               offset = 2
                        )),
               tags$br(),
               tags$br(),
               fluidRow(column(12, style="text-align:center;",
                               tags$h1("Rankings")),
                        tags$a(name = "ranking"),
                        column(8,
                               fluidRow(style="float:right;width:250px;",
                                        selectInput(inputId = "US.entries",
                                                    label = "Entries",
                                                    title = "Entry amount selecting form tool.",
                                                    choices = c(`Show 10` = 10, 
                                                                `Show 25` = 25,
                                                                `Show 50` = 50),
                                                    selected = 10,
                                                    width = "50%"),
                                        radioButtons(inputId = "US.rank.order",
                                                     label = "Order",
                                                     choices = c("Ascending", "Descending"),
                                                     selected = "Descending",
                                                     width="50%")),
                               gt_output("US.ranking.table"), offset = 2))
      ),
      # navbarMenu(menuName = "outcome_plots_menu",
      #            #HTML("<div style='font-size:90%;line-height:1.3;'><b>OUTCOME (GRAPHS)</b><br>Select a state outcome</div>"),
      #            HTML("<div><b>OUTCOME (GRAPHS)</b></div>"),
      #            
      #            tabPanel(title=tags$div(class="tab-title",style="text-align:center;",
      #                                    HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Trends in new Cases (Region)</div>")),
      #                     value="outcome_ny_new_cases",
      #                     fluidPage( 
      #                       fluidRow(class="page_title", tags$h1("OUTCOME: New York trends of new COVID-19 Cases")),
      #                       fluidRow(class="page_title", tags$h2("How have new COVID-19 Cases been mitigated in New York State over time?")),
      #                       fluidRow(class = "map-container",
      #                                column(8, style=paste0("height:",height,";"), id = "mainpanel_ny_new_case",
      #                                       tags$div(class = "page_title",
      #                                                selectInput(inputId = "NYRegion3",
      #                                                            label = "NY Regions",
      #                                                            choices = c("All Regions", sort(unique(covid_NY_TS_plot.cases$Region))),
      #                                                            selected = "All Regions"),
      #                                                dateRangeInput(inputId = "NYDate.ma",
      #                                                               label = "Date Range",
      #                                                               min = min(covid_NY_TS_plot.cases$date),
      #                                                               max = max(covid_NY_TS_plot.cases$date),
      #                                                               start = as.Date(max(covid_NY_TS_plot.cases$date)) - 31,
      #                                                               end = max(covid_NY_TS_plot.cases$date)),
      #                                                radioButtons(inputId = "rate.ma",
      #                                                             label = "",
      #                                                             choices = c("Overall", "Per/100k"),
      #                                                             selected = "Per/100k")),
      #                                       tags$div(class = "NY_case_plots",
      #                                                plotOutput(outputId = "NY.cases.ma", height="100%", 
      #                                                           click = clickOpts(id ="NY.cases.TS_click_ma"),
      #                                                           dblclick = "NY.cases.TS_dblclick",
      #                                                           brush = brushOpts(
      #                                                             id = "NY.cases.TS_brush",
      #                                                             resetOnNew = TRUE))
      #                                       ),
      #                                       HTML("<div style='position:absolute;bottom:0;'>
      #                           <br>To zoom plot, click and drag, then double-click in select box<br>
      #                           To un-zoom, double-click in plot<br>
      #                           For region details, single-click on line<br>
      #                           </div>")
      #                                ),
      #                                column(4, id = "sidebar_ny_new_case",
      #                                       tags$h2("New York Regions Map"),
      #                                       img(src="New-York-Regional-Map.png",style="width: 90%;"),
      #                                       HTML(paste0("<div>
      #                          <strong>Date: </strong>",update_date,"<br>
      #                          <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a>
      #                          </div>")),
      #                                       HTML("<h2>Phase One: Capital Region, Central New York, Finger Lakes, Long Island, Mid-Hudson, Mohawk Valley, 
      #                                            North Country, Southern Tier and Western New York are allowed to partially reopen </h2>"),
      #                                       HTML("<b>Data Source:</b> <a href='https://forward.ny.gov/industries-reopening-phase'>NY Gov</a>"),
      #                                       uiOutput("click_info_ma")
      #                                ))
      #                     )
      #            ),
      #            tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                              HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Trends in Mortality (Region)</div>")),
      #                     value="outcome_ny_cases_time",
      #                     fluidPage(
      #                       fluidRow(class="page_title", tags$h1("OUTCOME: New York Counties trends of new COVID-19 Deaths")),
      #                       fluidRow(class="page_title", tags$h2("How have new COVID-19 Deaths been mitigated in New York State over time?")),
      #                       fluidRow(class = "map-container",
      #                                
      #                                column(8, style=paste0("height:",height,";"),id = "mainpanel_ny_CoT",
      #                                       tags$div(
      #                                         selectInput(inputId = "NYRegion",
      #                                                     label = "NY Regions",
      #                                                     choices = c("All Regions", sort(unique(covid_NY_TS_plot.deaths$Region))),
      #                                                     selected = "All Regions"),
      #                                       #   selectInput(inputId = "NYCounty",
      #                                       #               label = "NY Counties",
      #                                       #               choices = c("All Counties", sort(unique(covid_NY_TS_plot.cases$County))),
      #                                       #               selected = 1)
      #                                       # ),
      #                                       dateRangeInput(inputId = "NYDoTDate",
      #                                                      label = "Date Range",
      #                                                      min = min(covid_NY_TS_plot.deaths$date),
      #                                                      max = max(covid_NY_TS_plot.deaths$date),
      #                                                      start = as.Date(max(covid_NY_TS_plot.deaths$date)) - 31,
      #                                                      end = max(covid_NY_TS_plot.deaths$date)),
      #                                       radioButtons(inputId = "rate.DoT",
      #                                                    label = "",
      #                                                    choices = c("Overall", "Per/100k"),
      #                                                    selected = "Per/100k")),
      #                                       tags$div(class = "NY_case_plots",
      #                                                plotOutput(outputId = "NY.deaths.ma", height="100%", 
      #                                                           click = clickOpts(id ="NY.cases.TS_click"),
      #                                                           dblclick = "NY.cases.TS_dblclick",
      #                                                           brush = brushOpts(
      #                                                             id = "NY.cases.TS_brush",
      #                                                             resetOnNew = TRUE))
      #                                       ),
      #                                       HTML("<div style='position:absolute;bottom:0;'>
      #                           <br>To zoom plot, click and drag, then double-click in select box<br>
      #                           To un-zoom, double-click in plot<br>
      #                           For county details, single-click on line<br>
      #                           </div>")),
      #                                column(4,
      #                                       id = "sidebar_ny_CoT",
      #                                       tags$h2("New York Regions Map"),
      #                                       img(src="New-York-Regional-Map.png",style="width: 90%;"),
      #                                       HTML(paste0("<div>
      #                          <strong>Date: </strong>",update_date,"<br>
      #                          <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a> and 
      #                          <a href='https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/'>USA Facts</a>
      #                          </div>")),
      #                                       HTML("<h2>Phase One: Capital Region, Central New York, Finger Lakes, Long Island, Mid-Hudson, Mohawk Valley, 
      #                                            North Country, Southern Tier and Western New York are allowed to partially reopen </h2>"),
      #                                       HTML("<b>Data Source:</b> <a href='https://forward.ny.gov/industries-reopening-phase'>NY Gov</a><br>
      #                          "),
      #                                       uiOutput("click_info")
      #                                )
      #                       )
      #                     )
      #            ),
      #            tabPanel(title=tags$div(class="tab-title",style="text-align:center;",
      #                                    HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Mortality over Time (County)</div>")),
      #                     value="outcome_ny_cases_time_region",
      #                     fluidPage(
      #                       fluidRow(class="page_title", tags$h1("OUTCOME: New York County COVID-19 Deaths over time")),
      #                       fluidRow(class="page_title", tags$h2("How have COVID-19 Deaths increased across New York State over time?")),
      #                       fluidRow(class = "map-container",
      #                                column(8,style=paste0("height:",height,";"), id = "mainpanel_ny_CoT_region",
      #                                       selectInput(inputId = "NYCounty",
      #                                                   label = "NY Counties",
      #                                                   choices = c("All Counties", sort(covid_NY_TS_plot.deaths %>%
      #                                                                                      filter(Region != "New York City") %>%
      #                                                                                      select(County) %>%
      #                                                                                      unlist() %>%
      #                                                                                      unique() %>%
      #                                                                                      c("New York City"))),
      #                                                   selected = "All Counties"),
      #                                       dateRangeInput(inputId = "NYcDoTDate",
      #                                                      label = "Date Range",
      #                                                      min = min(covid_NY_TS_plot.deaths$date),
      #                                                      max = max(covid_NY_TS_plot.deaths$date),
      #                                                      start = as.Date(max(covid_NY_TS_plot.deaths$date)) - 62,
      #                                                      end = max(covid_NY_TS_plot.deaths$date)),
      #                                       radioButtons(inputId = "rate.cDoT",
      #                                                    label = "",
      #                                                    choices = c("Overall", "Per/100k"),
      #                                                    selected = "Per/100k"),
      #                                       tags$div(class = "NY_case_plots",
      #                                                plotOutput(outputId = "NY.deaths.TS", height="100%", 
      #                                                           click = clickOpts(id ="NY.cases.TS_click_reg"),
      #                                                           dblclick = "NY.cases.TS_dblclick",
      #                                                           brush = brushOpts(
      #                                                             id = "NY.cases.TS_brush",
      #                                                             resetOnNew = TRUE))
      #                                       ),
      #                                       HTML("<div style='position:absolute;bottom:0;'>
      #                           <br>To zoom plot, click and drag, then double-click in select box<br>
      #                           To un-zoom, double-click in plot<br>
      #                           For county details, single-click on line<br>
      #                           </div>")),
      #                                column(4, id = "sidebar_ny_CoT_region",
      #                                       tags$h2("New York Regions Map"),
      #                                       img(src="New-York-Regional-Map.png",style="width: 90%;"),
      #                                       HTML(paste0("<div>
      #                          <strong>Date: </strong>",update_date,"<br>
      #                          <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a> and 
      #                                            <a href='https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/'>USA Facts</a>
      #                          </div>")),
      #                                       HTML("<h2>Phase One: Capital Region, Central New York, Finger Lakes, Long Island, Mid-Hudson, Mohawk Valley, 
      #                                            North Country, Southern Tier and Western New York are allowed to partially reopen </h2>"),
      #                                       HTML("<b>Data Source:</b> <a href='https://forward.ny.gov/industries-reopening-phase'>NY Gov</a><br>"),
      #                                       uiOutput("click_info_reg"))
      #                       )
      #                     )
      #            ),
      #            tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                              HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Racial Disparity</div>")),
      #                     value="outcome_ny_racial_disparity",
      #                     fluidPage(
      #                       fluidRow(class="page_title", tags$h1("OUTCOME: New York Racial Disparities of COVID-19 mortality")),
      #                       fluidRow(class="page_title", tags$h2("Do minorities make up a higher percentage of COVID-19 deaths when compared to 
      #                   their population percentage? Do New York City and the rest of New York State have 
      #                   different disparities in minority COVID-19 deaths?")),
      #                       fluidRow(class = "map-container",
      #                                column(8,style=paste0("height:",height,";"), id = "mainpanel_ny_race", 
      #                                       plotOutput(outputId = "NY.race.nys", height="50%"), 
      #                                       plotOutput(outputId = "NY.race.nyc", height="50%")),
      #                                column(4,
      #                                       id = "sidebar_ny_race",
      #                                       #HTML(whatisit_text),
      #                                       HTML("
      #                   <div>
      #                   <a href='https://bit.ly/2Krl5RG'>Evidence suggests</a> that COVID-19 deaths may be higher for certain racial/ethnic groups.<br><br>
      #                   If the percentage  of COVID-19 deaths experienced by a racial/ethnic group is higher than that 
      #                   group’s population percentage for a region, this suggests that COVID-19 may have a disparate 
      #                   impact on that group in that region. Social and economic determinants may contribute to this disparity. <br><br>"),
      #                                       HTML("For each racial/ethnic group, the proportion of COVID-19 deaths for that group is:<br>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than population percentage for disparity index &gt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to the population percentage for -0.2 &lt;disparity index &lt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than population percentage for disparity index &lt; -0.2</div>
      #                          <i>Darker shades indicate greater disparity.</i><br><br>
      #                          
      #                          <strong>Group COVID-19 Death Percentage</strong> = number of COVID-19 deaths for group/total COVID-19 deaths<br>
      #                          <strong>Population Percentage</strong> = number of residents from that group/ total number of residents<br>
      #                          <strong>Death Rate Disparity Index</strong> = log(Group COVID-19 Death Percentage/Population Percentage)
      #                          <br>
      #                   </div>"
      #                                       ),
      #                                       HTML(paste0("<div>
      #                          <br><br>
      #                          <strong>Date: </strong>",update_date,"<br><br>
      #                          <b>DATA SOURCE:</b> <a href='https://on.ny.gov/2VehafT'>New York State Dept. of Health COVIDTracker (daily)</a><br>
      #                          </div>")))
      #                       )
      #                     )
      #            ),
      #            tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                              HTML("<div><b>OUTCOME (CT)</b></br>COVID-19 Racial Disparity</div>")),
      #                     value="outcome_ct_racial_disparity",
      #                     fluidPage(
      #                       fluidRow(class="page_title", tags$h1("OUTCOME: Connecticut Racial Disparities of COVID-19 mortality")),
      #                       fluidRow(class="page_title", tags$h2("Do minorities in Connecticut make up a higher percentage of COVID-19 deaths when compared to 
      #                   their population percentage?")),
      #                       fluidRow(class = "map-container",
      #                                column(8, id = "mainpanel_ct_race", 
      #                                       plotOutput(outputId = "NY.race.ct", height=height)),
      #                                column(4, 
      #                                       id = "sidebar_ct_race",
      #                                       #HTML(whatisit_text),
      #                                       HTML("
      #                   <div>
      #                   <a href='https://bit.ly/2Krl5RG'>Evidence suggests</a> that COVID-19 deaths may be higher for certain racial/ethnic groups.<br><br>
      #                   If the percentage  of COVID-19 deaths experienced by a racial/ethnic group is higher than that 
      #                   group’s population percentage for a region, this suggests that COVID-19 may have a disparate 
      #                   impact on that group in that region. Social and economic determinants may contribute to this disparity. <br><br>"),
      #                                       HTML("For each racial/ethnic group, the proportion of COVID-19 deaths for that group is:<br>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than population percentage for disparity index &gt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to the population percentage for -0.2 &lt;disparity index &lt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than population percentage for disparity index &lt; -0.2</div>
      #                          <i>Darker shades indicate greater disparity.</i><br><br>
      #                          
      #                          <strong>Group COVID-19 Death Percentage</strong> = number of COVID-19 deaths for group/total COVID-19 deaths<br>
      #                          <strong>Population Percentage</strong> = number of residents from that group/ total number of residents<br>
      #                          <strong>Death Rate Disparity Index</strong> = log(Group COVID-19 Death Percentage/Population Percentage)
      #                          <br>
      #                   </div>"
      #                                       ),
      #                                       HTML(paste0("<div>
      #                          <br><br>
      #                          <strong>Date: </strong>",update_date,"<br><br>
      #                          <b>DATA SOURCE:</b> <a href='https://bit.ly/3bJ77GZ'>ct.gov</a><br>
      #                          </div>")))
      #                       )
      #                     )
      #            )),
      # navbarMenu(menuName = "outcome_maps_menu",
      #            HTML("<div><b>OUTCOME (MAPS)</b></div>"),
      # tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
      #                   HTML("<div><b>OUTCOME (USA)</b></br>Mortality Rate</div>")),
      #          value="outcome_usa_mortality",
      #          fluidPage(
      #            fluidRow(class = "page_title",tags$h1("OUTCOME: USA COVID-19 Mortality Rates Disparities")),
      #            fluidRow(class = "page_title",tags$h2("What are the disparities between states  in  rates of COVID-19 deaths per 100k population 
      #              when compared to the average USA rate?")),
      #           fluidRow(class = "map-container",
      #          column(8,
      #                   id = "mainpanel_us_mort",
      #                   tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by State Compared to Average US Rate"),
      #                   leafletOutput(outputId = "map.covid_deaths", height=height))
      #          ,
      #          column(4,
      #                 id = "sidebar_us_mort",
      #                 #HTML(whatisit_text),
      #                 HTML(paste0("<div>The rate of COVID-19 deaths per 100k in a state is: <br>
      #               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
      #               <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt; disparity index &lt; 0.2</div>
      #               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2 </div>
      #               <i>Darker shades indicate greater disparity.</i><br><br>
      #               
      #               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
      #               <strong>Death Rate Disparity Index</strong> = log(Mortality Rate  in state/mean Mortality Rate of US)<br>
      #               <strong>Date: </strong>",update_date,"<br><br>
      # 
      #               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
      #               </div>
      #               ")),
      #                 #HTML(footer_text),
      #          ))), 
      #          #tags$script(src = "style.js")
      # ), 
      # tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
      #                   HTML("<div><b>OUTCOME (USA)</b></br>Racial/Ethnic Disparity</div>")),
      #          value="outcome_usa_racial_disparity",
      #          fluidPage(
      #            fluidRow(class="page_title", tags$h1("OUTCOME: Racial/Ethnic Disparities of COVID-19 Mortality")),
      #            fluidRow(class="page_title", tags$h2("Do minorities make up a higher percentage of COVID-19 deaths across the United States when compared to 
      #                     their population percentage?")),
      #            fluidRow(class="map-container",
      #              column(8,
      #                      id = "mainpanel_us_mort_race",
      #                      tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by State by Race/Ethnicity"),
      #                      #HTML("<br><br>"),
      #                      tags$div(class = "select-bar",
      #                               selectInput(inputId = "race",
      #                                           label = NULL,
      #                                           choices =  c("Non-hispanic White"="nhw",
      #                                                        "Non-hispanic American Indian/Alaska Native"="nhaian",
      #                                                        "Non-hispanic Asian Pacific Islander"="nhapi",
      #                                                        "Hispanic/Latino (total)"="hlt",
      #                                                        "Non-hispanic Black/African American"="nhbaa"),
      #                                           selected = "nhbaa")),
      #                      leafletOutput(outputId = "map.covid_deaths.race", height=height)),
      #                     column(4,
      #                            id = "sidebar_us_mort_race",
      #                            #HTML(whatisit_text),
      #                            HTML(paste0("
      #                     <div>
      #                     <a href='https://bit.ly/2Krl5RG'>Evidence suggests</a> that COVID-19 deaths may be higher for certain racial/ethnic groups.<br><br>
      #                     If the percentage of COVID-19 deaths experienced by a racial/ethnic group is higher than that 
      #                     group’s population percentage for a region, this suggests that COVID-19 may have a disparate 
      #                     impact on that group in that region. Social and economic determinants may contribute to this disparity.
      #                     <br><br>",
      #                                        
      #                                        "For each racial/ethnic group, the proportion of COVID-19 deaths for that group is:<br>
      #                     <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than population percentage for disparity index &gt; 0.2</div>
      #                     <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to the population percentage for -0.2 &lt;disparity index &lt; 0.2</div>
      #                     <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than population percentage for disparity index &lt; -0.2</div>
      #                     <i>Darker shades indicate greater disparity.</i><br><br>
      #                          
      #                     <strong>Group COVID-19 Death Percentage</strong> = number of COVID-19 deaths for group/total COVID-19 deaths<br>
      #                     <strong>Population Percentage</strong> = number of residents from that group/ total number of residents<br>
      #                     <strong>Death Rate Disparity Index (DI)</strong> = log(Group COVID-19 Death Percentage/Population Percentage)
      #                     <br><br>
      #                     <strong>Date: </strong>",update_date,"<br><br>
      #                     <b>DATA SOURCE:</b> <a href='https://data.cdc.gov/resource/pj7m-y5uh.csv'>data.cdc.gov</a><br>
      #                     </div>"))
      #                            #HTML(footer_text),
      #                     )
      #            
      #            )
      #          ), 
      # ), 
      # tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                   HTML("<div><b>OUTCOME (STATE)</b></br>Mortality Rate</div>")),
      #          value="outcome_state_mortality",
      #          fluidPage(
      #              fluidRow(class="page_title", uiOutput("state_mort_heading")),
      #              fluidRow(class="map-container",
      #              column(8,id = "mainpanel_ny_mort",
      #                     uiOutput("state_mort_map_title"),
      #                     tags$div(class = "select-bar",
      #                              selectInput(
      #                                inputId = "state_mort",
      #                                label = NULL,
      #                                choices = state.abr$abr,
      #                                selected = "NY"    
      #                              )),
      #                      leafletOutput(outputId = "map.NY.deaths", height = height)),
      #              column(4,
      #                     id = "sidebar_ny_mort",
      #                     #HTML(whatisit_text),
      #                     HTML(paste0("<div>
      #                          The rate of COVID-19 deaths per 100k in a county is<br>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
      #                          <i>Darker shades indicate greater disparity.</i><br><br>
      #                          
      #                          <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
      #                          <strong>Death Rate Disparity Index</strong> = log(Mortality Rate in state/mean Mortality Rate in US)<br>
      #                          <strong>Date: </strong>",update_date,"<br><br>
      #                          
      #                          <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a> and 
      #                          <a href='https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/'>USA Facts</a><br>
      #                          
      #                          </div>"))
      #                     #HTML(footer_text),
      #              ))
      #            
      #            
      #            
      #            )),
      # tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                   HTML("<div><b>OUTCOME (STATE)</b></br>COVID-19 Cases</div>")),
      #          value="outcome_state_cases",
      #          fluidPage(
      #            fluidRow(class="page_title", uiOutput("state_case_heading")),
      #            fluidRow(class = "map-container",
      #            column(8, id = "mainpanel_ny_cases",
      #                   uiOutput("state_case_map_title"),
      #                   tags$div(class = "select-bar",
      #                            selectInput(
      #                              inputId = "state_case",
      #                              label = NULL,
      #                              choices = state.abr$abr,
      #                              selected = "NY"    
      #                            )), 
      #                   leafletOutput(outputId = "map.NY.cases", height=height)),
      #            column(4,
      #              id = "sidebar_ny_cases",
      #              #HTML(whatisit_text),
      #              HTML(paste0("<div>
      #                          
      #                          The rate of COVID-19 deaths per 100k in a county is<br>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
      #                          <i>Darker shades indicate greater disparity.</i><br><br>
      #                          
      #                          <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
      #                          <strong>Date: </strong>",update_date,"<br><br>
      #                          
      #                          <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
      #                          </div>")),
      #              )
      #            )
      #              )
      #            )),
      # navbarMenu(menuName = "mediation_menu",
      #            #HTML("<div style='font-size:90%;line-height:1.3;'><b>MEDIATION</b><br>Select a USA mediation</div>"),
      #            HTML("<div><b>MEDIATION</b></div>"),
      #            tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                   HTML("<div><b>MEDIATION (USA)</b></br>COVID-19 Testing</div>")),
      #          value="mediation_usa_testing",
      #          fluidPage(
      #            fluidRow(class="page_title", tags$h1("MEDIATION: Nationwide testing disparities compared to top testing countries")),
      #            fluidRow(class="page_title", tags$h2("What are the disparities between US states  in  their rates of COVID-19 testing per 1k population 
      #             when compared to the average rates from other countries? When compared with the current average
      #             US rate?")),
      #            fluidRow(class = "map-container",
      #            column(8, id = "mainpanel_us_test",
      #                   tags$h3(class="map-title", paste0("COVID-19 Testing Rate Disparities by State Compared to Selected Country")),
      #                   HTML("<br><br>"),
      #                   tags$div(class="select-bar",
      #                            selectInput(inputId = "country",
      #                                        label = NULL,
      #                                        choices = country_testing_choices,
      #                                        selected = "de")),
      #                   leafletOutput(outputId = "map.testing", height=height)),
      #            column(4,
      #              id = "sidebar_us_test",
      #              #HTML(whatisit_text),
      #              HTML(paste0("
      #             <div>
      #             Several countries significantly effected by COVID-19 can be used as testing reference rates. 
      #             Some of these countries are regarded as having successfully  used testing to “flatten the curve”,
      #             while others are still in the midst of dealing with the crisis.<br><br>
      #              The rate of testing per 1k in a state is: <br>
      #                <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than selected country testing rate for disparity index &gt; 0.2</div>
      #                <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to selected country testing rate for -0.2 &lt; disparity index &lt; 0.2</div>
      #                <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than selected country testing rate for disparity index &lt; -0.2</div>
      #              <i>Darker shades indicate greater disparity.</i><br><br>
      #              
      #              <strong>Testing Rate</strong> = number of COVID-19 tests per 1K population <br>
      #              <strong>Testing Rate Disparity Index</strong> = log(Testing Rate in state/Testing Rate in selected country) <br>
      #   <strong>Date: </strong>",update_date,"<br><br>
      #              
      #              <b>DATA SOURCES:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a>, 
      #              <a href='https://ourworldindata.org/coronavirus'>Our World in Data</a>
      #              </div>")))
      #            )
      #          )
      # ),
      # tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                   HTML("<div><b>MEDIATION (USA)</b></br>Hospital Beds</div>")),
      #          value="mediation_usa_hospital_beds",
      #          fluidPage(
      #            fluidRow(class="page_title", tags$h1("MEDIATION: Nationwide Hospital Bed Disparities vs Italy")),
      #            fluidRow(class="page_title", tags$h2("What are the disparities between states  in  the rate of hospital beds 
      #                           per 100k population when compared to the rate in Italy?")),
      #            fluidRow(class = "map-container",
      #            column(8,id = "mainpanel_us_hosp",
      #                  tags$h3(class="map-title", "COVID-19 Hospital Bed Rate Disparities by State Compared to Average Italian Rate"),
      #                  leafletOutput(outputId = "map.hospital", height=height)),
      #            column(4,
      #              id = "sidebar_us_hosp",
      #              #HTML(whatisit_text),
      #              HTML(paste0("
      #                           <div>
      #                           Italy has a higher hospital 
      #                           bed rate than the US, yet still faced challenges meeting peak COVID bed needs. Thus we use 
      #                           Italy’s rate as a minimum target rate.<br><br>
      #                           The rate of hospital beds per 100k in a state is<br>
      #                            <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than Italian rate for disparity index &gt; 0.2</div>
      #                            <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to Italian rate for -0.2 &lt;disparity index &lt; 0.2</div>
      #                            <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than Italian rate for disparity index &lt; -0.2</div>
      #                          <i>Darker shades indicate greater disparity.</i><br><br>
      #                          
      #                          <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
      #                          <strong>Testing Rate Disparity Index</strong> = log(Testing Rate  in state/Testing Rate in Italy) <br>
      #                          <strong>Date: </strong>",update_date,"<br><br>
      #                          
      #                          <b>DATA SOURCE:</b> <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a><br>
      # 
      #                          </div>")))
      #            )
      #          )
      # )),
      navbarMenu(menuName ="determinant_menu",
                 HTML("<div><b>DETERMINANT ANALYSIS</b></div>"),
               tabPanel(tags$div(class="tab-title",style="text-align:center;",
                                 HTML("<div><b>DISCLAIMER</b></div>")),
                        value = "determinant_disclaimer",
                        fluidRow(column(8, class = "about",
                                        tags$h1("Disclaimer"),
                                        tags$p("Determinant tabs are experimental and expected to change substantially, current displayed data may not be accurate."), 
                                        offset = 2)))
               #,
      #          tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                HTML("<div><b>DETERMINANT</b></br>USA</div>")),
      #          value="determinant_usa",
      #          fluidPage(
      #            fluidRow(class="page_title", uiOutput("us_det_title")),
      #            fluidRow(class="page_title", uiOutput("us_det_subtitle")),
      #            fluidRow(class = "map-container",
      #           column(8, id = "mainpanel_us_db",
      #                  uiOutput("us_det_map_title"),
      #                  tags$br(),tags$br(),
      #                  tags$div(class = "select-bar",
      #                           selectInput(inputId = "determinant",
      #                                       label = NULL,
      #                                       choices = c("Diabetes", "Obesity", "CRD Mortality", "Heart Disease Mortality"),
      #                                       selected = "Diabetes"
      #                           )),
      #                  leafletOutput(outputId = "map.determinant", height=height)),
      #            column(4,
      #              id = "sidebar_us_db",
      #                          uiOutput("sb_us_det_output"),
      #                          HTML(
      #                          "<div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
      #                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
      #                          <i>Darker shades indicate greater disparity.</i><br><br></div>"),
      #                          uiOutput("sb_us_det_footer")))
      #          )
      # ),
      # tabPanel(tags$div(class="tab-title",style="text-align:center;",
      #                   HTML("<div><b>DETERMINANT</b></br>NY</div>")),
      #          value="determinant_ny",
      #          fluidPage(
      #            fluidRow(class="page_title", uiOutput("ny_det_title")),
      #            fluidRow(class="page_title", uiOutput("ny_det_subtitle")),
      #            fluidRow(class = "map-container",
      #            column(8, id = "mainpanel_ny_det",
      #              uiOutput("ny_det_map_title"),
      #              tags$br(),tags$br(),
      #              tags$div(class = "select-bar",
      #                       selectInput(inputId = "determinant_NY",
      #                                   label = NULL,
      #                                   choices = c("Diabetes", "Obesity"), # , "Obesity", "Heart Disease"
      #                                   selected = "Diabetes"
      #                       )),
      #              leafletOutput(outputId = "map.NY.determinant", height=height)),
      #            column(4,
      #              id = "sidebar_ny_det",
      #              uiOutput("sb_ny_det_output"),
      #              HTML("
      #                   <div>
      #                  <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
      #                  <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt; disparity index &lt; 0.2</div>
      #                  <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
      #                  <i>Darker shades indicate greater disparity.</i><br><br>
      #                  </div>"),
      #              uiOutput("sb_ny_det_footer")))
      #          )
      # )
      ),
      tabPanel(HTML("<div><b>ABOUT</b></div>"),
               value="about",
               fluidRow(
                 column(8,offset=2,class="about",
                        tags$h1("About the Project"),
                        HTML(whatisit_text_abt),
                        HTML(footer_text))
               )
               )
    ), 
    ### Footer
    fluidRow(
      column(12, class = "footer",
             hr(),
             HTML("<a href='?tab=about'>About</a>&emsp;"),
             HTML("<a href='https://idea.rpi.edu/'>Institute for Data Exploration and Applications (IDEA)</a>&emsp;"),
             HTML("<a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER GitHub</a>&emsp;"),
             HTML("<a href='https://info.rpi.edu/statement-of-accessibility'>Accessibility</a>&emsp;"),
             HTML("<a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>
                  <span title='Thanks for using COVIDMINDER! Please take a few moments to fill out our short comments form.'>Comments</span></a>&emsp;"),
             tags$a(href="#top", "Back to Navbar")
             )
    )
    #,tags$script(src = "style.js")
  )

#### Server Code ####
server <- function(input, output, session) {
  # Leaflet plot colors
  colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
  bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
  
  # Join NY shape and data, may move to processing 
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    # browser()
    country <- input$country # selected country
    
    # modify states to have selected columns for our plot
    tests_ldi <- states %>% 
      select(starts_with("tests_ldi")) %>%
      select(ends_with(country))
    
    states <- data.frame(states, "tests_ldi"=unlist(tests_ldi)) # Append to states
    
    
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=TRUE)
#    browser()
    labels2 <- sprintf(
      paste0("<strong>%s</strong> State<br/>
      Testing Rate vs ", toupper(country)," DI: %.2g<br>
      Testing Rate: %.1f /1000"),
      states$NAME, states$tests_ldi, states$tests_per_1000
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes, width="100%", height="100%") %>%
      setView(-96, 37.8, 4) %>% # TODO: Doesn't seem to do anything
      addPolygons(
        fillColor = ~pal2(states$tests_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend(pal = pal2, 
                values = ~states$tests_ldi, 
                opacity = 0.7, 
                title = paste0("Disparity Index<br/>US Total Tests vs. ",toupper(country)),
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                   cuts[n] = paste0(cuts[n]," lower") 
                   # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                   for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                   cuts[2] = paste0(cuts[2]," higher") 
                   paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
                ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
  })
  
  output$map.determinant <- renderLeaflet({
    det <- input$determinant
    geo.plot("US", det)
  })
  
  output$us_det_map_title <- renderUI ({
    select.det <- input$determinant
    if (select.det == "CRD Mortality") {
      select.det <- "Cronic Respiratory Disease (CRD) Mortality"
    }
    tags$h3(class = "map-title", paste0("US ",select.det," Rate Disparities by State Compared to Average US Rate"))
  })
  
  output$ny_det_map_title <- renderUI ({
    select.det <- input$determinant_NY
    tags$h3(class="map-title", paste0("NY ",select.det," Rate Disparities by County Compared to Average US Rate"))
  })
  
  output$state_mort_heading <- renderUI({
    state <- input$state_mort
    name <- state.abr[state.abr$abr==state,"name"]
    tagList(
    tags$h1(paste0("OUTCOME: ",name," rates of COVID-19 Mortality")),
    tags$h2(paste0("What are the disparities between counties of ",name," in rates of COVID-19 deaths 
                   per 100k population when compared to the average USA rate?"))
    )
  })
  output$state_mort_map_title <- renderUI({
    state <- input$state_mort
    name <- state.abr[state.abr$abr==state,"name"]
    tags$h3(class="map-title", paste0("COVID-19 Mortality Rate Disparities by County in ",name," 
                                      Compared to Average US Rate"))
  })
  
  output$state_case_heading <- renderUI({
    state <- input$state_case
    name <- state.abr[state.abr$abr==state,"name"]
    tagList(
      tags$h1(paste0("OUTCOME: ",name," rates of COVID-19 Cases")),
      tags$h2(paste0("What are the disparities between counties of ",name," in rates of COVID-19 cases 
                   per 100k population when compared to the average USA rate?"))
    )
  })
  output$state_case_map_title <- renderUI({
    state <- input$state_case
    name <- state.abr[state.abr$abr==state,"name"]
    tags$h3(class="map-title", paste0("COVID-19 Case Rate Disparities by County in ",name," 
                                      Compared to Average US Rate"))
  })
  
  output$us_det_title <- renderUI ({
    select.det <- input$determinant
    tags$h1(paste0("DETERMINANT: Nationwide ",select.det," Disparities"))
  })
  
  output$ny_det_title <- renderUI ({
    select.det <- input$determinant_NY
    tags$h1(paste0("DETERMINANT: New York ",select.det," Disparities"))
  })
  
  output$us_det_subtitle <- renderUI ({
    select.det <- input$determinant
    if ( select.det == "Diabetes") {
      tags$h2("What are the disparities between states in rate of diabetes patients 
              per 100k population per state when compared to the average United States rate?")
    }
    else if ( select.det == "Obesity") {
      tags$h2("What are the disparities between states in percent of obese patients 
              per state when compared to the average United States rate?")
    }
    else if ( select.det == "Heart Disease") {
      tags$h2("What are the disparities between states in rate of deaths (black non-hispanic) due to heart disease
               per 100k population per state when compared to the average United States rate?")
    }
  })
  
  output$ny_det_subtitle <- renderUI ({
    select.det <- input$determinant_NY
    if ( select.det == "Diabetes") {
      tags$h2("What are the disparities between New York counties in the rate 
                                of diabetes patients per 100k population when compared to the average United 
                                States rate?")
    }
    else if ( select.det == "Obesity") {
      tags$h2("What are the disparities between New York counties in the rate 
                                of people with Obesitys per 100k population when compared to the average United 
                                States rate?")
    }
    else if ( select.det == "Heart Disease") {
      
    }
  })
  
  output$sb_us_det_output <- renderUI ({
    select.det <- input$determinant
    if ( select.det == "Diabetes") {
      tagList(
        tags$p("Diabetes puts patients at increased risk of contracting and dying from COVID-19, 
      so areas with higher diabetes rates may face increased COVID-19 burdens."),
        tags$p("The  rate of diabetes deaths per 100k in a state is:")
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$p("Obesity puts patients at increased risk of contracting and dying from COVID-19, so areas with higher rates of obesity may face increased COVID-19 burdens."),
        tags$p("The  rate of obesity in a state is:")
      )
    }
    else if ( select.det == "Heart Disease") {
      tagList(
        tags$p("Heart disease patients at increased risk of contracting and dying from COVID-19,
                                 so areas with a history of higher heart disease mortality may face increased COVID-19 burdens.
                                 Furthermore, some ethnic groups have higher mortality rates due to heart disease than other groups."),
        tags$p("The rate of deaths due to heart disease (black non-hispanic) per 100k in a state is:")
      )
    }
  })
  
  output$sb_ny_det_output <- renderUI ({
    select.det <- input$determinant_NY
    if ( select.det == "Diabetes") {
      tagList(
        tags$p("Diabetes puts patients at increased risk of contracting and dying from COVID-19, 
                so areas with higher diabetes rates may face increased COVID-19 burdens."),
        tags$p("The rate of diabetes patients per 100k in a county is:")
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$p("Obesity puts patients at increased risk of contracting and dying from COVID-19, so areas with higher rates of obesity may face increased COVID-19 burdens."),
        tags$p("The  rate of obesity in a county is:")
      )
    }
    else if ( select.det == "Heart Disease") {
      #tagList(
      #  tags$p("Heart disease patients at increased risk of contracting and dying from COVID-19,
      #                           so areas with a history of higher heart disease mortality may face increased COVID-19 burdens.
      #                           Furthermore, some ethnic groups have higher mortality rates due to heart disease than other groups."),
      #  tags$p("The rate of deaths due to heart disease (black non-hispanic) per 100k in a state is:")
      #)
    }
  })
  
  output$sb_us_det_footer <- renderUI ({
    select.det <- input$determinant
    if ( select.det == "Diabetes") {
      tagList(
        tags$div(HTML(paste0(
          "<strong>Diabetes Rate</strong> = number of diabetic patients per 100K population <br>
          <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
          <strong>Date: </strong>","2020"," <br><br>
          <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
          <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>")))
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$div(HTML(paste0(
          "<strong>Obesity Rate</strong> = number of obese patients per 100K population <br>
                                <strong>Obesity Disparity Index</strong> = log(Obesity Rate in state/average Obesity Rate in US)<br>
                                <strong>Date: </strong>","2016","<br><br>
                                
                                <b>DATA SOURCE:</b> <a href='https://stateofchildhoodobesity.org/adult-obesity/'>State of Childhood Obesity</a>"
        )))
      )
    }
    else if ( select.det == "Heart Disease") {
      tags$div(HTML(
        "<strong>Heart Disease Death Rate (BNH)</strong> = number of heart disease deaths (black non-hispanic) per 100K population <br>
                                <strong>Heart Disease Death Disparity Index (BNH)</strong> = log(Heart Disease Death Rate (BNH) in state/average Heart Disease Death Rate in US)<br>
                                <strong>Date: </strong> 2015<br><br>
       
                                <b>DATA SOURCE:</b> <a href='https://sortablestats.cdc.gov/#/indicator'>CDC</a><br>"
      ))
    }
  })
  output$sb_ny_det_footer <- renderUI ({
    select.det <- input$determinant_NY
    if ( select.det == "Diabetes") {
      tagList(
        tags$div(HTML(paste0(
          "  <strong>Diabetes Rate</strong> = number of diabetic patients  per 100K population <br>
             <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
             <strong>Date: </strong> 2020<br><br>
             <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
             <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>")))
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$div(HTML(paste0(
          "  <strong>Obesity Rate</strong> = number of self reported Obese people per 100K population <br>
             <strong>Obesity Disparity Index</strong> = log(Obesity Rate in state/average Obesity Rate in US)<br>
             <strong>Date: </strong> 2016<br><br>
             <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
             <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>")))
      )
    }
    else if ( select.det == "Heart Disease") {
    }
  })

  output$map.hospital <- renderLeaflet({
    
    pal2 <- leaflet::colorBin(colors, domain = states$hosp_beds_ldi, bins = bins, reverse=TRUE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Hospital Beds vs Italy DI: %.2g",
      states$NAME, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$hosp_beds_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend(pal = pal2, 
                values = ~states$hosp_beds_ldi, 
                opacity = 0.7, 
                title = "Disparity Index<br/>US Hospital Beds vs Italy",
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
                ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.covid_deaths <- renderLeaflet({
    geo.plot("US", "Mortality")
  })

  output$map.covid_deaths.race <- renderLeaflet({
    
    race <- input$race # selected race

    # modify states to have selected columns for our plot
    death_rate_ldi_race <- states %>% 
      select(starts_with("death")) %>%
      select(ends_with(race))

    race_deaths_pct <- states %>% 
      select(starts_with(race)) %>%
      select(ends_with("deaths_pct"))
    
    race_wd_pop_pct <- states %>% 
      select(starts_with(race)) %>%
      select(ends_with("wd_pop_pct"))
    
    states <- data.frame(states, "death_rate_ldi_race"=unlist(death_rate_ldi_race)) # Append to states
    states <- data.frame(states, "race_deaths_pct"=unlist(race_deaths_pct)) # Append to states
    states <- data.frame(states, "race_wd_pop_pct"=unlist(race_wd_pop_pct)) # Append to states

    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi_race, bins = bins, reverse=FALSE)
    
    labels2 <- sprintf(
      paste0("<strong>%s</strong> (",toupper(race),")<br/>
      COVID-19 Mortality Pct DI: %.2g<br>
      COVID-19 Mortality Pct: %.1f<br>
      Percentage of population (weighted): %.1f<br><br>"),
      # Total COVID-19-related Executive Orders: %.0f<br>
      # Total COVID-19-related Bills: %.0f"),
      states$NAME, states$death_rate_ldi_race, 
      states$race_deaths_pct, 
      states$race_wd_pop_pct
      # states$covid_eo,states$covid_bills
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$death_rate_ldi_race),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels2,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend(pal = pal2, 
                values = ~states$death_rate_ldi_race, 
                opacity = 0.7, 
                title = "Disparity Index<br/>US COVID-19 Mortality Rates",
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
      ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.NY.deaths <- renderLeaflet({
    selected_state <- input$state_mort
    selected_feature <- "Mortality"
    geo.plot(selected_state, selected_feature)
  })
  
  output$map.NY.cases <- renderLeaflet({
    selected_state <- input$state_case
    selected_feature <- "Case"
    geo.plot(selected_state, selected_feature)
  })
  
  output$map.NY.determinant <- renderLeaflet({
    selected_state <- "NY"
    selected_feature <- input$determinant_NY
    geo.plot(selected_state, selected_feature)
  })

  # This sets the range for zooming the following plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$NY.cases.ma <- renderPlot({
    # browser()
    selected.region <- input$NYRegion3
    select.rate <- input$rate.ma
    select.date <- input$NYDate.ma
    #range <- as.numeric(select.date[2]) - as.numeric(select.date[1])
    
    #width <- input$width
    select.size <- 3
    
    if (selected.region == "All Regions") {
      selected.region <- sort(unique(covid_NY_TS_plot.cases$Region))
      select.size <- 1.5
    }
    
    if (select.rate=="Overall") {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(diff = sum(diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
      y_lab <- paste0("New Cases (",moving.avg.window," day Average)")
      gg_title <- paste0("New York State New COVID-19 Case Trends (",moving.avg.window," day Average)")
    }
    else {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(p_diff = sum(p_diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
      y_lab <- paste0("New Cases (",moving.avg.window," day Average) per 100k")
      gg_title <- paste0("New York State New COVID-19 Case Trends per 100k (",moving.avg.window," day Average)")
    }
    
    region.num <- unique(covid_NY_TS_plot.ma$Region) %>% as.data.frame()
    colnames(region.num) <- c("Region")
    region.num <- region.num %>%
      mutate(num = row_number()) %>%
      mutate(Region = as.character(Region))
    
    n_region <- nrow(region.num)
    
    highlight_points <- covid_NY_TS_plot.ma %>%
      ungroup() %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      left_join(region.num, by=c("Region" = "Region")) %>%
      group_by(Region) %>%
      mutate(range = as.numeric(as.Date(max(date))) - as.numeric(as.Date(min(date))))  %>% 
      mutate(max_date = as.Date(max(date))) %>%
      mutate(max_date = max_date - ((num*((range%/%n_region) + 1))%%range))  %>%
      filter(as.Date(date) == max_date) %>%
      top_n(1, wt=date)
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    

    
    covid_NY_TS_plot.ma %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      ggplot(aes(date, 
                 ma, 
                 color = Region)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,25,100,250,500,1000,2500,5000,10000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab(y_lab) + 
      xlab("Date") +
      ggtitle(gg_title)  +  
      theme(legend.key.size = unit(1.5, "lines")) + 
      gghighlight(Region %in% selected.region, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      geom_label_repel(
        data=highlight_points,  
        aes(label=Region,fill=Region), 
        box.padding = unit(1.75, 'lines'),
        color = "black",
        segment.color = "black",
        size = 5,
        show.legend = FALSE
        ) +
      scale_color_manual(values=NY_region_palette, aesthetics = c("fill", "box.padding")) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      geom_vline(aes(xintercept=as_datetime("2020-05-15"), linetype="Gov. Cuomo issues Phase 1 Reopening (5 Regions)"), color = "blue") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2,2), 
                            guide = guide_legend(override.aes = list(color = c("blue","black")))) +
      NULL
    
  })
  
  output$NY.deaths.ma <- renderPlot({
    # browser()
    selected.region <- input$NYRegion
    #selected.county <- input$NYCounty
    select.rate <- input$rate.DoT
    select.date <- input$NYDoTDate
    #range <- as.numeric(select.date[2]) - as.numeric(select.date[1])
    
    #width <- input$width
    select.size <- 3
    
    if (selected.region == "All Regions") {
      selected.region <- sort(unique(covid_NY_TS_plot.deaths$Region))
      select.size <- 1.5
    }
    
    if (select.rate=="Overall") {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.deaths %>%
        group_by(Region, date) %>%
        summarise(diff = sum(diff)) %>%
        mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
        filter(ma > 0)
      y_lab <- paste0("New Deaths (",moving.avg.window," day Average)")
      gg_title <- paste0("New York State New COVID-19 Death Trends (",moving.avg.window," day Average)")
      scale_y <- scale_y_continuous(
          trans = "log10",
          breaks = c(10,25,100,250,500,1000,2500,5000,10000)
        )
    }
    else {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.deaths %>%
        group_by(Region, date) %>%
        summarise(p_diff = sum(p_diff)) %>%
        mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
        filter(ma > 0)
      y_lab <- paste0("New Deaths (",moving.avg.window," day Average) per 100k")
      gg_title <- paste0("New York State New COVID-19 Death Trends per 100k (",moving.avg.window," day Average)")
      scale_y <- scale_y_continuous(
          breaks = c(10,25,100,250,500,1000,2500,5000,10000)
        )
    }
    
    region.num <- unique(covid_NY_TS_plot.ma$Region) %>% as.data.frame()
    colnames(region.num) <- c("Region")
    region.num <- region.num %>%
      mutate(num = row_number()) %>%
      mutate(Region = as.character(Region))
    
    n_region <- nrow(region.num)
    
    highlight_points <- covid_NY_TS_plot.ma %>%
      ungroup() %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      left_join(region.num, by=c("Region" = "Region")) %>%
      group_by(Region) %>%
      mutate(range = as.numeric(as.Date(max(date))) - as.numeric(as.Date(min(date))))  %>% 
      mutate(max_date = as.Date(max(date))) %>%
      mutate(max_date = max_date - ((num*((range%/%n_region) + 1))%%range))  %>%
      filter(as.Date(date) == max_date) %>%
      top_n(1, wt=date)
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
    
    
    covid_NY_TS_plot.ma %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      ggplot(aes(date, 
                 ma, 
                 color = Region)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab(y_lab) + 
      xlab("Date") +
      ggtitle(gg_title)  +  
      theme(legend.key.size = unit(1.5, "lines")) + 
      gghighlight(Region %in% selected.region, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      geom_label_repel(
        data=highlight_points,  
        aes(label=Region,fill=Region), 
        box.padding = unit(1.75, 'lines'),
        color = "black",
        segment.color = "black",
        size = 5,
        show.legend = FALSE
      ) +
      scale_color_manual(values=NY_region_palette, aesthetics = c("fill", "box.padding")) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      geom_vline(aes(xintercept=as_datetime("2020-05-15"), linetype="Gov. Cuomo issues Phase 1 Reopening (5 Regions)"), color = "blue") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2,2), 
                            guide = guide_legend(override.aes = list(color = c("blue","black")))) +
      NULL
    
  })
  
  output$NY.deaths.TS <- renderPlot({
    # browser()
    #selected.region <- input$NYRegion
    selected.county <- input$NYCounty
    select.rate <- input$rate.cDoT
    select.date <- input$NYcDoTDate
    
    #range <- as.numeric(select.date[2]) - as.numeric(select.date[1])
    #print(range)
    
    NYC <- covid_NY_TS_plot.deaths %>%
      filter(Region == "New York City") %>%
      group_by(Region, Color, date) %>%
      summarise(deaths = sum(deaths),
                Population = sum(Population),
                p_deaths = mean(p_deaths),
                diff = sum(diff),
                p_diff = mean(p_diff),
                log_deaths = sum(log_deaths),
                log_p_deaths = mean(log_p_deaths)) %>%
      mutate(County = "New York City")
    
    covid_NY_TS <- covid_NY_TS_plot.deaths %>%
      filter(Region != "New York City") %>%
      rbind.data.frame(NYC)
    
    select.size <- 3
    if (selected.county == "All Counties") {
      selected.county <- sort(unique(covid_NY_TS$County))
      select.size <- 1.5
    }
    
    if (select.rate=="Overall") {
      covid_NY_TS <- covid_NY_TS %>%
        mutate(y = deaths) %>%
        filter(y >= 20)
      y_lab <- "Cumulative Number of Deaths"
      title <- "New York State COVID-19 Deaths per County (Mar-June 2020)"
    }
    else {
      covid_NY_TS <- covid_NY_TS %>%
        mutate(y = p_deaths) %>%
        filter(y >= 5)
      y_lab <- "Deaths per 100K Population"
      title <- "New York State COVID-19 Deaths per 100K Population by County (Mar-June 2020)"
    }
    
    county.num <- unique(covid_NY_TS$County) %>% as.data.frame()
    colnames(county.num) <- c("County")
    county.num <- county.num %>%
      mutate(num = row_number()) %>%
      mutate(County = as.character(County))
    
    n_county <- nrow(county.num)
    
    highlight_points <- covid_NY_TS %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      left_join(county.num, by=c("County" = "County")) %>%
      group_by(County) %>%
      mutate(range = as.numeric(as.Date(max(date))) - as.numeric(as.Date(min(date))))  %>% 
      mutate(max_date = as.Date(max(date))) %>%
      mutate(max_date = max_date - ((num*((range%/%n_county) + 1))%%range))  %>%
      filter(as.Date(date) == max_date) %>%
      top_n(1, wt=date)
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
    covid_NY_TS %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      ggplot(aes(date, 
                 y, 
                 color = Region,
                 group = County)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,100,500,1000,5000,10000, 50000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab(y_lab) + 
      xlab("Date") +
      ggtitle(title)  +  
      theme(legend.key.size = unit(1.5, "lines")) + 
      gghighlight(County %in% selected.county, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      geom_label_repel(
        data=highlight_points,  
        aes(label=County,fill=Region), 
        box.padding = unit(1.75, 'lines'),
        color = "black",
        segment.color = "black",
        size = 5,
        show.legend = FALSE
      ) +
      scale_color_manual(values=NY_region_palette, aesthetics = c("fill", "box.padding")) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      geom_vline(aes(xintercept=as_datetime("2020-05-15"), linetype="Gov. Cuomo issues Phase 1 Reopening (5 Regions)"), color = "blue") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2,2), 
                            guide = guide_legend(override.aes = list(color = c("blue","black")))) +
      NULL
      })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$click_info <- renderPrint({
    hover <- input$NY.cases.TS_click
    selected.region <- input$NYRegion
    select.rate <- input$rate.DoT
    select.date <- input$NYDoTDate
    
    if (select.rate=="Overall") {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.deaths %>%
        group_by(Region, date) %>%
        summarise(diff = sum(diff)) %>%
        mutate(y = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
        filter(y > 0)
      per <- ": "
    }
    else {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.deaths %>%
        group_by(Region, date) %>%
        summarise(p_diff = sum(p_diff)) %>%
        mutate(y = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
        filter(y > 0)
      per <- "/100k: "
    }
    
    point <- nearPoints(covid_NY_TS_plot.ma, hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="y")
    # browser()
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); height:15%;",
                    "right:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    if (nrow(point) != 0) {
      avg_window <- as.Date(point$date, format = "%m-%d-%Y") - moving.avg.window + 1
      if (point$Region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region,"</b><br>Average New Deaths",per,format(round(point$y),big.mark = ","),"<br>Window: ",avg_window, " to ", point$date)))
        )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region," Region</b><br>Average New Deaths",per,format(round(point$y),big.mark = ","),"<br>Window: ",avg_window, " to ", point$date)))
        )
        
      }
    }
    else if(selected.region != "All Regions") {
      yesterday <- select.date[2]
      avg_window <- as.Date(yesterday, format = "%m-%d-%Y") - moving.avg.window + 1
      point <- covid_NY_TS_plot.ma %>%
        filter(Region == selected.region & date == yesterday)
      if (selected.region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.region,"</b><br>Average New Deaths",per,format(round(point[1,]$y), big.mark = ","),"<br>Window: ",avg_window, " to ", yesterday)))
        )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.region," Region</b><br>Average New Deaths",per,format(round(point[1,]$y), big.mark = ","),"<br>Window: ",avg_window, " to ", yesterday)))
        )
      }
    }
  })
  
  output$click_info_ma <- renderPrint({
    hover <- input$NY.cases.TS_click_ma
    selected.region <- input$NYRegion3
    select.rate <- input$rate.ma
    select.date <- input$NYDate.ma
    
    if (select.rate=="Overall") {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(diff = sum(diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)# %>%
      #filter(date >= date[1] & date <= date[2])
      per <- ": "
    }
    else {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(p_diff = sum(p_diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)# %>%
      #filter(date >= date[1] & date <= date[2])
      per <- "/100k: "
    }
    
    point <- covid_NY_TS_plot.ma %>%
      nearPoints(hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="ma")
    # browser()
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); 
                    width:30%; padding: 0; margin: 0;",
                    "left:", 0, "px;")
    
    # actual tooltip created as wellPanel
    if (nrow(point) != 0) {
      avg_window <- as.Date(point$date, format = "%m-%d-%Y") - moving.avg.window + 1
      if (point$Region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region,"</b><br>Average New Cases",per,format(round(point$ma),big.mark = ","),"<br>Window: ",avg_window, " to ", point$date)))
        )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region," Region</b><br>Average New Cases",per,format(round(point$ma),big.mark = ","),"<br>Window: ",avg_window, " to ", point$date)))
        )
        
      }
    }
    else if(selected.region != "All Regions") {
      yesterday <- select.date[2]
      avg_window <- as.Date(yesterday, format = "%m-%d-%Y") - moving.avg.window + 1
      point <- covid_NY_TS_plot.ma %>%
        filter(Region == selected.region & date == yesterday)
      if (selected.region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.region,"</b><br>Average New Cases",per,format(round(point[1,]$ma), big.mark = ","),"<br>Window: ",avg_window, " to ", yesterday)))
        )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.region," Region</b><br>Average New Cases",per,format(round(point[1,]$ma), big.mark = ","),"<br>Window: ",avg_window, " to ", yesterday)))
        )
      }
    }
  })
  
  output$click_info_reg <- renderPrint({
    hover <- input$NY.cases.TS_click_reg
    #selected.region <- input$NYRegion2
    #select.region <- input$rate.CoT.reg
    #select.date <- input$NYRegionDate
    
    selected.county <- input$NYCounty
    select.rate <- input$rate.cDoT
    select.date <- input$NYcDoTDate
    
    NYC <- covid_NY_TS_plot.deaths %>%
      filter(Region == "New York City") %>%
      group_by(Region, Color, date) %>%
      summarise(deaths = sum(deaths),
                Population = sum(Population),
                p_deaths = mean(p_deaths),
                diff = sum(diff),
                p_diff = mean(p_diff),
                log_deaths = sum(log_deaths),
                log_p_deaths = mean(log_p_deaths)) %>%
      mutate(County = "New York City")
    
    covid_NY_TS <- covid_NY_TS_plot.deaths %>%
      filter(Region != "New York City") %>%
      rbind.data.frame(NYC)
    
    
    if (select.rate=="Overall") {
      covid_NY_TS <- covid_NY_TS %>%
        mutate(y = deaths) %>%
        filter(y >= 20)
      per <- ": "
    }
    else {
      covid_NY_TS <- covid_NY_TS %>%
        mutate(y = p_deaths) %>%
        filter(y >= 5)
      per <- "/100k: "
    }
    
    point <- covid_NY_TS %>%
      nearPoints(hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="y")
    # browser()
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    if (nrow(point) != 0) {
      if (point$Region == "New York State"){
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$County,"</b><br>Total Deaths",per,format(round(point$y),big.mark = ","),"<br>Date: ",point$date)))
          )
      } else {
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$County," County</b><br>Total Deaths",per,format(round(point$y),big.mark = ","),"<br>Date: ",point$date)))
        )
        
      }
    }
    else if(selected.county != "All Counties") {
      yesterday <- select.date[2]
      point <- covid_NY_TS %>%
        filter(County == selected.county & date == yesterday)
      if (selected.county == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$County,"</b><br>Total Deaths",per,format(round(point[1,]$y),big.mark = ","),"<br>Date: ",yesterday)))
        )
      } else {
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$County," County</b><br>Total Deaths",per,format(round(point[1,]$y),big.mark = ","),"<br>Date: ",yesterday)))
          )
      }
    }
  })
  
  
  
  observeEvent(input$NY.cases.TS_dblclick, {
    brush <- input$NY.cases.TS_brush
    if (!is.null(brush)) {
      #browser()
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin="1970-01-01")
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  }) 

  observeEvent(input$NY.cases.TS.rates_dblclick, {
    brush2 <- input$NY.cases.TS.rates_brush
    if (!is.null(brush2)) {
      #browser()
      ranges2$x <- as.POSIXct(c(brush2$xmin, brush2$xmax), origin="1970-01-01")
      ranges2$y <- c(brush2$ymin, brush2$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  }) 
  
  output$NY.race.nys <- renderPlot({
    
    # Creating dataframe for death disparity data from health.ny.gov's COVID-19 tracker
    # See NYS COVIDTracker https://on.ny.gov/2VehafT for current numbers; the following are current as of 04/19
    NYS_Dis.df <- data.frame("Race.Ethnicity" =        c("Hispanic", "Black", "White", "Asian"), 
                             "Percent.of.Pop" =        c(12,  9, 74, 4), 
                             "Percent.of.Fatalities" = c(14, 18, 60, 4))
    
    
    # Creating columns to measure disparity between state pop percent and fatality percent
    NYS_Dis_m.df <- NYS_Dis.df %>%
      mutate(Dis = -log(Percent.of.Pop/Percent.of.Fatalities)) %>%
      arrange(desc(Race.Ethnicity))
    
    # Setup: COVIDMINDER Colors and DI bins
    di_pal <- leaflet::colorBin(colors, domain = NYS_Dis_m.df$Dis, bins = bins, reverse=FALSE)
    
    # Create color column with correct mapping
    NYS_Dis_m.df <- NYS_Dis_m.df %>% 
      mutate(Race.Ethnicity = ordered(Race.Ethnicity, levels=Race.Ethnicity)) %>% 
      mutate(color = di_pal(Dis))
    
    # Plotting NYS dispairity between NYS pop percent and fatality percent
    NYS_Dis_m.df %>% 
      ggplot(aes(x = Race.Ethnicity, 
                 y = Dis,
                 fill = NYS_Dis_m.df$Race.Ethnicity
      )) + guides(fill = FALSE) + coord_flip() +
      geom_bar(stat = "Identity", colour="black") +
      scale_fill_manual(values=NYS_Dis_m.df$color, name = "Race/Ethnicity") +
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      xlab("Race/Ethnicity") + 
      theme(axis.text.y = element_text(face="bold", size=14)) +
      ylab("Disparity") + 
      scale_y_continuous(
        breaks = c(-5,-3,-2,-1,-.2,.2,1,2,3,5),
        limits = c(-1.5,1.5)
      ) +
      labs(title = "Racial/ethnic disparities in % of COVID-19 deaths as compared to population %",
           subtitle = "For  New York State (excluding New York City)",
           caption = "Source: covid19tracker.health.ny.gov") +
      theme(
                legend.position = "none",
        plot.title = element_text(vjust = 0, face="bold",size=18)) + 
      geom_hline(aes(yintercept=-0.2, linetype="Lower Bound"), color = "#253494") +
      geom_hline(aes(yintercept= 0.2, linetype="Upper Bound"), color = "#BD0026") +
      scale_linetype_manual(name = "Target Levels",
                            values = c(2,
                                       2),
                            guide = guide_legend(override.aes = list(color = c("#253494",
                                                                               "#BD0026")))) +
      annotate(geom="text", y=-1, x="Hispanic", label="Lower", color="#253494", size=10) +
      annotate(geom="text", y=1, x="Hispanic", label="Higher", color="#BD0026", size=10) +
      NULL
  })

  output$NY.race.nyc <- renderPlot({
    
    # Creating dataframe for death disparity data from health.ny.gov's COVID-19 tracker
    # See NYS COVIDTracker https://on.ny.gov/2VehafT for current numbers; the following are current as of 04/19

    NYC_Dis.df <- data.frame("Race.Ethnicity" =        c("Hispanic", "Black", "White", "Asian"), 
                             "Percent.of.Pop" =        c(29, 22, 32, 14), 
                             "Percent.of.Fatalities" = c(34, 28, 27,  7))
    
    
    # Creating columns to measure disparity between city pop percent and fatality percent
    NYC_Dis_m.df <- NYC_Dis.df %>%
      mutate(Dis = -log(Percent.of.Pop/Percent.of.Fatalities)) %>%
      arrange(desc(Race.Ethnicity))
    
    # Setup: COVIDMINDER Colors and DI bins
    di_pal <- leaflet::colorBin(colors, domain = NYC_Dis_m.df$Dis, bins = bins, reverse=FALSE)
    
    # Create color column with correct mapping
    NYC_Dis_m.df <- NYC_Dis_m.df %>% 
      mutate(Race.Ethnicity = ordered(Race.Ethnicity, levels=Race.Ethnicity)) %>% 
      mutate(color = di_pal(Dis)) 
    
    # Plotting NYS dispairity between NYS pop percent and fatality percent
    NYC_Dis.p <- NYC_Dis_m.df %>% 
      ggplot(aes(x = Race.Ethnicity, 
                 y = Dis,
                 fill = NYC_Dis_m.df$Race.Ethnicity
      )) + guides(fill = FALSE) + coord_flip() +
      geom_bar(stat = "Identity", colour="black")
    
    NYC_Dis.p + 
      scale_fill_manual(values=NYC_Dis_m.df$color, name = "Race/Ethnicity") + 
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      xlab("Race/Ethnicity") +
      theme(axis.text.y = element_text(face="bold", size=14)) +
      ylab("Disparity") + 
      scale_y_continuous(
        breaks = c(-5,-3,-2,-1,-.2,.2,1,2,3,5),
        limits = c(-1.5,1.5)
      ) +
      labs(title = "Racial/ethnic disparities in % of COVID-19 deaths as compared to population %",
           subtitle = "For New York City",
           caption = "Source: covid19tracker.health.ny.gov") +
      theme(
        legend.position = "none",
        plot.title = element_text(vjust = 0, face="bold",size=18)) + 
      geom_hline(aes(yintercept=-0.2, linetype="Lower Bound"), color = "#253494") +
      geom_hline(aes(yintercept= 0.2, linetype="Upper Bound"), color = "#BD0026") +
      scale_linetype_manual(name = "Target Levels",
                            values = c(2,
                                       2),
                            guide = guide_legend(override.aes = list(color = c("#253494",
                                                                               "#BD0026")))) +
      annotate(geom="text", y=-1, x="Hispanic", label="Lower", color="#253494", size=10) +
      annotate(geom="text", y=1, x="Hispanic", label="Higher", color="#BD0026", size=10) +
      NULL
  
    })
  
  output$NY.race.ct <- renderPlot({
    
    # Data source: ct.gov
    CT_Dis.df <- data.frame("Race.Ethnicity" =        c("Hispanic", "Black", "White", "Asian"), 
                            "Percent.of.Pop" =        c(16.5,  12, 66.5, 4.9), 
                            "Percent.of.Fatalities" = c(8.9, 14.8, 67.9, 1.3))
    
    # Creating columns to measure disparity between state pop percent and fatality percent
    CT_Dis_m.df <- CT_Dis.df %>%
      mutate(Dis = -log(Percent.of.Pop/Percent.of.Fatalities))
    
    # Setup: COVIDMINDER Colors and DI bins
    di_pal <- leaflet::colorBin(colors, domain = CT_Dis_m.df$Dis, bins = bins, reverse=FALSE)
    
    
    # Create color column with correct mapping
    CT_Dis_m.df <- CT_Dis_m.df %>% 
      mutate(Race.Ethnicity = ordered(Race.Ethnicity, levels=Race.Ethnicity)) %>% 
      mutate(color = di_pal(Dis))
    
    # Plotting dispairity between CT pop percent and fatality percent
    CT_Dis_m.df %>% 
      ggplot(aes(x = Race.Ethnicity, 
                 y = Dis,
                 fill = CT_Dis_m.df$Race.Ethnicity
      )) + guides(fill = FALSE) + coord_flip() +
      geom_bar(stat = "Identity", colour="black") +
      scale_fill_manual(values=CT_Dis_m.df$color, name = "Race/Ethnicity") +
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      xlab("Race/Ethnicity") + 
      theme(axis.text.y = element_text(face="bold", size=14)) +
      ylab("Disparity") + 
      scale_y_continuous(
        breaks = c(-5,-3,-2,-1,-.2,.2,1,2,3,5),
        limits = c(-1.5,1.5)
      ) +
      labs(title = "Racial/ethnic disparities in % of COVID-19 deaths compared to population %",
           subtitle = "For Connecticut",
           caption = "Source: ct.gov") +
      theme(
        legend.position = "none",
        plot.title = element_text(vjust = 0, face="bold",size=18)) + 
      geom_hline(aes(yintercept=-0.2, linetype="Lower Bound"), color = "#253494") +
      geom_hline(aes(yintercept= 0.2, linetype="Upper Bound"), color = "#BD0026") +
      scale_linetype_manual(name = "Target Levels",
                            values = c(2,
                                       2),
                            guide = guide_legend(override.aes = list(color = c("#253494",
                                                                               "#BD0026")))) +
      annotate(geom="text", y=-1, x="White", label="Lower", color="#253494", size=12) +
      annotate(geom="text", y=1, x="White", label="Higher", color="#BD0026", size=12) +
      NULL
    
  })
  
  ### State Report Cards Code ###
  
  output$state.report.county.selector <- renderUI ({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    
    # Grab state subset for dataframe
    state.df <- covid_TS_counties_long.cases %>%
      select(-c(countyFIPS, stateFIPS))%>%
      filter(State == state_initial)
    
    if (state_initial == "NY") {
      nyc.population <- state.df %>%
        filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        group_by(County) %>%
        top_n(n=1, wt=date) %>%
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
          p_cases = sum(cases)*100000/nyc.population,
          p_deaths = sum(deaths)*100000/nyc.population,
          diff = sum(diff),
          p_diff = sum(diff)*100000/nyc.population,
          d_diff = sum(d_diff),
          p.d_diff = sum(p.d_diff)*100000/nyc.population
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
    
    if (length(selected) > 10) {
      selected <- selected[1:10]
    }
    selectInput(inputId = "SRC.county",
                label = "County Selector",
                title = "County selecting form tool.",
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
    selectInput(inputId = "NRC.state",
                label = "State Selector",
                title = "State selecting form tool.",
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
    tagList(
      tags$h1(paste0(state_name, " Overview")),
      tags$h2(tags$a(paste0("State rank: ", ranking[ranking$State == state_initial, "rank"]), tags$sup("*"), href = "#ranking", style="color:black;"))
    )
  })
  
  output$state.CoT.title <- renderUI({
    state_name <- input$state_name
    tagList(
      tags$h2(paste0(state_name, " COVID-19 Case Curve")),
      tags$h3(paste0("How have ", state_name, " COVID-19 Cases Over Time per 100k changed compared to US?"))
    )
  })
  
  
  output$state.DoT.title <- renderUI({
    state_name <- input$state_name
    tagList(
      tags$h2(paste0(state_name, " COVID-19 Mortality Curve")),
      tags$h3(paste0("How has ", state_name, " COVID-19 Mortality Over Time per 100k changed compared to US?"))
    )
  })
  
  output$state.trends.title <- renderUI({
    state_name <- input$state_name
    rate <- input$SRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else { #if per/100k
      y.value = "p_diff"
    }
    tagList(
      tags$h2(paste0(state_name, " Daily Case Trends")),
      tags$h3(paste0("How have ", get_y_label(y.value), " changed in ", state_name," counties? (7 day average)"))
    )
  })
  
  output$state.county.cases <- renderUI({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    time <- input$SRC.case.time
    if (time == "Daily") {
      m.a.w <- paste0(" (",time.period," day average)")
    }
    else {
      m.a.w <- ""
    }
    tagList(
      tags$h2(paste0(state_name, " COVID-19 Case Hotspots")),
      tags$h3(paste0("What are the ", state_initial, " Countywide disparities in ",time," Case Rates?",m.a.w))
    )
  })
  
  output$state.county.deaths <- renderUI({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    time <- input$SRC.death.time
    if (time == "Daily") {
      m.a.w <- paste0(" (",time.period," day average)")
    }
    else {
      m.a.w <- ""
    }
    
    tagList(
      tags$h2(paste0(state_name, " COVID-19 Mortality Hotspots")),
      tags$h3(paste0("What are the ", state_initial, " Countywide disparities in ",time," Mortality Rates?", m.a.w))
    )
  })
  
  output$determinant.title <- renderUI({
    state_name <- input$state_name
    det <- input$state.determinant
    if(det ==  "CRD Mortality") {
      det <- "Cronic Respiratory Disease (CRD) Mortality"
    }
    tagList(
      tags$h2(paste0(state_name, " ", det, " Disparities")),
      tags$h3(paste0("What are the ", state_name, " disparities in ", det, " Rates compared to US Average?"))
    )
  })
  
  output$determinant.text <- renderText({
    det <- input$state.determinant
    paste0("Nationwide, ",det," has been observed as a leading comorbidity of COVID-19.")
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
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_cases.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      title <- tags$h1(paste0(state_name, " COVID-19 Case Hotspots"))
      time <- input$SRC.case.time
      if (time == "Daily") {
        param <- "Daily Case"
      }
      else {
        param <- "Case"
      }
      
      mapshot(x = geo.plot(state_initial, 
                           param, 
                           title = tags$div(title)
                           ),
              file = file,
              cliprect = "viewport",
              selfcontained = F)
    }
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
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_mortality.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      title <- tags$h1(paste0(state_name, " COVID-19 Case Hotspots"))
      time <- input$SRC.death.time
      if (time == "Daily") {
        param <- "Daily Mortality"
      }
      else {
        param <- "Mortality"
      }
      mapshot(x = geo.plot(state_initial, 
                           param,
                           title = tags$div(title)),
              file = file,
              cliprect = "viewport",
              selfcontained = F)
    }
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
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_",det,".png"))
    },
    content = function(file) {
      state_name <- input$state_name
      det <- input$state.determinant
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      det_title <- det
      if(det ==  "CRD Mortality") {
        det_title <- "Cronic Respiratory Disease (CRD) Mortality"
      }
      title <- tags$h1(paste0(state_name, " ", det_title, " Rate Disparities Compared to US Average"))
      mapshot(x = geo.plot(state_initial, 
                           det,
                           title = tags$div(title, class = "leaflet-map-title")),
              file = file,
              cliprect = "viewport",
              selfcontained = F)
    }
  )
  
  barplot.tooltip <- function(hover, 
                              state_initial,
                              y.value="p_cases", 
                              moving.avg.window=14) {
    #print(session$clientData)
    pixelratio <- session$clientData$pixelratio
    left.offset <- 17
    top.offset <- 82
    
    if(is.null(hover)) {return(NULL)}
    my_diff <- get_dif(y.value)
    category <- get_y_label(y.value)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    
    if ((hover$range$right - hover$range$left)*(1-left_pct) < 300 ) {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) - 301*pixelratio
    }
    else {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0("position:absolute; 
                    z-index:100;
                    width:300px;",
                    "left:", (left_px)/pixelratio + left.offset, "px; 
                    top:", (top_px)/pixelratio + top.offset, "px;")
    
    state_cases <- covid_TS_state_long.cases %>%
      filter(State == state_initial) %>%
      rename(Values = all_of(y.value)) %>%
      rename(Value_diff = all_of(my_diff)) %>%
      mutate(diff.ma =  c(Value_diff[1:7-1], zoo::rollmean(Value_diff, 7, align="right"))) %>%
      #mutate(pct_increase =diff.ma/Values*100) %>%
      mutate(pct_increase =Value_diff/Values*100) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right")))
    #state_cases[state_cases$diff.ma > 0 & state_cases$pct_increase > 5, "pct_increase"] <- 5
    #state_cases[state_cases$Value_diff > 0 & state_cases$pct_increase > 5, "pct_increase"] <- 5
    state_cases[is.na(state_cases$pct_increase) | state_cases$pct_increase < 0, "pct_increase"] <- 0
    state_cases <- state_cases %>%
      filter(date == as.Date(as.POSIXct(hover$x, origin="1970-01-01"), tz="EST"))
    # actual tooltip created as wellPanel
    
    five.plus <- ""
    #if (length(state_cases$pct_increase) > 0) {
    #  if(state_cases$pct_increase >= 5) {
    #    five.plus <- "+"
    #  }
    #}
    wellPanel(
      style = style,
      class = "gg_tooltip",
      p(HTML(paste0("<b> Date: </b>", as.Date(as.POSIXct(hover$x, origin="1970-01-01"), tz="EST"), "<br/>",
                    "<b>", category, ": </b>", format(round(state_cases$Values,2), big.mark = ","), "<br/>",
                    "<b> Change in ", category, ": </b>+",  format(round(state_cases$Value_diff,2),big.mark = ","), "<br/>",
                    "<b> Daily Percentage Increase: </b>",  format(round(state_cases$pct_increase,2),big.mark = ","), "%",five.plus,"<br/>"
    ))))
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
                             y.value="p_cases", 
                             counties = c("Rensselaer"), 
                             moving.avg.window=7) {
    #print(session$clientData)
    pixelratio <- session$clientData$pixelratio
    left.offset <- 17
    top.offset <- 226
    
    #if(is.null()) {return(NULL)}
    y_label <- get_y_label(y.value)
    state.name <- state.abr[state.abr$abr==state_initial,"name"]
    covid_TS_counties.cases.plot <- covid_TS_counties_long.cases %>%
      select(-c(countyFIPS, stateFIPS)) %>%
      filter(State == state_initial) %>%
      group_by(County) %>% 
      filter(n() >= moving.avg.window) %>%
      mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
      ungroup()
    
    if (state_initial == "NY") {
      nyc.population <- covid_TS_counties.cases.plot %>%
        filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        group_by(County) %>%
        top_n(n=1, wt=date) %>%
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
          p_cases = sum(cases)*100000/nyc.population,
          p_deaths = sum(deaths)*100000/nyc.population,
          diff = sum(diff),
          p_diff = sum(diff)*100000/nyc.population,
          d_diff = sum(d_diff),
          p.d_diff = sum(p.d_diff)*100000/nyc.population
        ) 
      covid_TS_counties.cases.plot <- covid_TS_counties.cases.plot %>%
        filter(!County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
        rbind.data.frame(NYC)
      
    }
    
    state <- covid_TS_state_long.cases %>%
      filter(State == state_initial) %>%
      filter(n() >= moving.avg.window) %>%
      mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right")))
    
    state$County = state_initial
    
    covid_TS_counties.cases.plot <-  covid_TS_counties.cases.plot %>%
      filter(County %in% counties) %>%
      rbind.data.frame(state) %>%
      filter(get(y.value) > 0) %>%
      group_by(County)
    
    point <- nearPoints(covid_TS_counties.cases.plot, hover, threshold = 5, maxpoints = 1, addDist = TRUE) %>%
      rename(Values = all_of(y.value))
    
    if (nrow(point) == 0) return(NULL)
    point <- point[1,]
    
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - log(hover$y, 10)) / (hover$domain$top - hover$domain$bottom)
    # Log10 is needed to account for log y axis
    
    # calculate distance from left and bottom side of the picture in pixels
    
    if ((hover$range$right - hover$range$left)*(1-left_pct) < 200 ) {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) - 205*pixelratio
    }
    else {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    
    #left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; 
                    z-index:100;
                    width: 200px;",
                    "left:", (left_px)/pixelratio + left.offset, "px; 
                    top:", (top_px)/pixelratio + top.offset, "px;")
    
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
    wellPanel(
      style = style,
      class = "gg_tooltip",
      p(HTML(paste0("<b>",state.or.county, "</b>", point$name, "<br/>",
                    "<b> Date: </b>", point$date, "<br/>",
                    "<b>", y_label, ": </b>",  format(round(point$Values),big.mark = ","), "<br/>"
      ))))
    
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
    else { #if per/100k
      y.value = "p_diff"
    }
    
    trends.tooltip(hover, state_initial, y.value, counties)
  })
  
  nation.trends.tooltip <- function(hover, 
                                    y.value="p_cases", 
                                    selected.states = c(), 
                                    moving.avg.window=7) {
    pixelratio <- session$clientData$pixelratio
    left.offset <- 17
    top.offset <- 226
    
    y_label <- get_y_label(y.value)
    covid_TS_state.cases.plot <- covid_TS_state_long.cases %>%
      select(-c(population)) %>%
      filter(State %in% selected.states$abr) %>%
      group_by(State) %>% 
      filter(n() >= moving.avg.window) %>%
      mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
      ungroup()
    
    US <- covid_TS_US_long.cases %>%
      mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right")))
    
    US$State = "US"
    
    covid_TS_state.cases.plot <-  covid_TS_state.cases.plot %>%
      group_by(State) %>%
      ungroup() %>%
      rbind.data.frame(US) %>%
      filter(get(y.value) > 1)
    
    point <- nearPoints(covid_TS_state.cases.plot, hover, threshold = 5, maxpoints = 1, addDist = TRUE) %>%
      rename(Values = all_of(y.value)) %>%
      left_join(selected.states,
                by = c("State" = "abr"))
    
    if (nrow(point) == 0) return(NULL)
    point <- point[1,]
    
    state.or.national <- "State: "
    if(is.na(point$name)) {
      point$name <- ""
      state.or.national <- "United States"
    }
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - log(hover$y, 10)) / (hover$domain$top - hover$domain$bottom)
    # Log10 is needed to account for log y axis
    
    # calculate distance from left and bottom side of the picture in pixels
    if ((hover$range$right - hover$range$left)*(1-left_pct) < 200 ) {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) - 201*pixelratio
    }
    else {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    #left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; 
                    z-index:100;
                    width:200px;",
                    "left:", (left_px)/pixelratio + left.offset, "px; 
                    top:", (top_px)/pixelratio + top.offset, "px;")
    
    wellPanel(
      style = style,
      class = "gg_tooltip",
      p(HTML(paste0("<b>",state.or.national,"</b>", point$name, "<br/>",
                    "<b> Date: </b>", point$date, "<br/>",
                    "<b>", y_label, ": </b>",  format(round(point$Values),big.mark = ","), "<br/>"
      ))))
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
    else { #if per/100k
      y.value = "p_diff"
    }
    
    nation.trends.tooltip(hover, y.value, selected.states)
  })
  
  output$state.CoT <- renderPlot({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    ggbar.overall(state_initial, y.value = "p_cases", remove.title = T) + 
      #geom_vline(xintercept=reactive.line$x, color= "black", linetype="solid", size = 1, show.legend = F) +
      NULL
  }, alt = "A time series bar plot representing the states COVID-19 cases over time, per 100k.")
  
  output$state.CoT.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_CoT_plot.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      ggsave(filename = file, 
             plot = ggbar.overall(state_initial, y.value = "p_cases", remove.title = F, date = update_date) + NULL,
             device = "png",
             width = 12,
             height = 8,
             units = "in")
    }
  )
  
  output$state.DoT.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_DoT_plot.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      ggsave(filename = file, 
             plot = ggbar.overall(state_initial, y.value = "p_deaths", remove.title = F, date = update_date) + NULL,
             device = "png",
             width = 12,
             height = 8,
             units = "in")
    }
  )
  
  output$state.DoT <- renderPlot({
    state_name <- input$state_name
    state_initial <- state.abr[state.abr$name == state_name, "abr"]
    ggbar.overall(state_initial, y.value = "p_deaths", remove.title = T)
  }, alt = "A time series bar plot representing the states COVID-19 deaths over time, per 100k.")
  
  Tr.ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$trends.dbl_click, {
    brush <- input$trends.brush
    if (!is.null(brush)) {
      #browser()
      Tr.ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin="1970-01-01")
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
    else { #if per/100k
      y.value = "p_diff"
    }
    
    ggplot.state(state_initial, y.value = y.value, counties = counties,remove.title = T) +
      coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
      NULL
  }, alt = "A time series bar plot representing the states new COVID-19 cases over time")
  
  output$state.trends.dl <- downloadHandler(
    filename = function() {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      return(paste0(state_initial, "_trends_plot.png"))
    },
    content = function(file) {
      state_name <- input$state_name
      state_initial <- state.abr[state.abr$name == state_name, "abr"]
      counties <- input$SRC.county
      rate <- input$SRC.rate
      if (rate == "Overall") {
        y.value = "diff"
      }
      else { #if per/100k
        y.value = "p_diff"
      }
      ggsave(filename = file, 
             plot = ggplot.state(state_initial, 
                                 y.value = y.value, 
                                 counties = counties,
                                 remove.title = F, 
                                 date = update_date) + 
               coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
               NULL,
             device = "png",
             width = 12,
             height = 8,
             units = "in")
    }
  )
  
  output$ranking.table <- render_gt({
    entries <- input$entries
    if ("Ascending" %in% input$rank.order) {
      order <- function(x){x}
    }
    if ("Descending" %in% input$rank.order) {
      order <- dplyr::desc
    }
    gt.ranking(as.numeric(entries), order)
  })
  
  ### National Overview Code ###
  
  # TODO: Remove second version of ranking.table
  output$US.ranking.table <- render_gt({
    entries <- input$US.entries
    if ("Ascending" %in% input$US.rank.order) {
      order <- function(x){x}
    }
    if ("Descending" %in% input$US.rank.order) {
      order <- dplyr::desc
    }
    gt.ranking(as.numeric(entries), order)
  })
  
  output$US.trends.title <- renderUI({
    rate <- input$NRC.rate
    if (rate == "Overall") {
      y.value = "diff"
    }
    else { #if per/100k
      y.value = "p_diff"
    }
    tagList(
      tags$h2("United States Daily Case Trends"),
      tags$h3(paste0("How have ", get_y_label(y.value), " changed in US states? (7 day average)"))
    )
  })
  
  output$US.determinant.title <- renderUI({
    det <- input$US.determinant
    if (det == "CRD Mortality") {
      det <- "Cronic Respiratory Disease (CRD) Mortality"
    }
    tagList(
      tags$h2(paste0("United States ", det, " Disparities")),
      tags$h3(paste0("What are the nationwide disparities in ", det, " Rates compared to the US Average?"))
    )
  })
  
  output$US.determinant.text <- renderText({
    det <- input$US.determinant
    paste0("Nationwide, ",det," has been observed as a leading comorbidity of COVID-19.")
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
      title <- tags$h1(style="text-align:center;", "US COVID-19 Case Hotspots")
      time <- input$NRC.case.time
      if (time == "Daily") {
        param <- "Daily Case"
      }
      else {
        param <- "Case"
      }
      mapshot(x = geo.plot("US", 
                           param, 
                           title = tags$div(title)
      ),
      file = file,
      cliprect = "viewport",
      selfcontained = F)
    }
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
      title <- tags$h2(style="text-align:center;", "US COVID-19 Testing Disparities")
      #time <- input$NRC.deaths.time
      #if (time == "Daily") {
      #  param <- "Daily Mortality"
      #}
      #else {
      #  param <- "Mortality"
      #}
      mapshot(x = geo.plot("US", 
                           "Daily Testing", 
                           title = tags$div(title),
                           reverse = T
      ),
      file = file,
      cliprect = "viewport",
      selfcontained = F)
    }
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
    filename = function() {
      return("US_mortality.png")
    },
    content = function(file) {
      title <- tags$h2(style="text-align:center;", "US COVID-19 Mortality Hotspots")
      time <- input$NRC.deaths.time
      if (time == "Daily") {
        param <- "Daily Mortality"
      }
      else {
        param <- "Mortality"
      }
      mapshot(x = geo.plot("US", 
                           param, 
                           title = tags$div(title)
      ),
      file = file,
      cliprect = "viewport",
      selfcontained = F)
    }
  )
  
  US.barplot.tooltip <- function(hover,
                                 y.value="cases", 
                                 moving.avg.window=14) {
    pixelratio <- session$clientData$pixelratio
    left.offset <- 17
    top.offset <- 82
    
    if(is.null(hover)) {return(NULL)}
    my_diff <- get_dif(y.value)
    category <- get_y_label(y.value)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    if ((hover$range$right - hover$range$left)*(1-left_pct) < 250 ) {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) - 251*pixelratio
    }
    else {
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    }
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    style <- paste0("position:absolute; 
                    z-index:100;
                    width:250px;",
                    "left:", (left_px)/pixelratio + left.offset, "px; 
                    top:", (top_px)/pixelratio + top.offset, "px;")
    
    US.ma <- covid_TS_US_long.cases %>%
      rename(Values = all_of(y.value)) %>%
      rename(my_diff = all_of(my_diff)) %>%
      mutate(diff.ma =  c(my_diff[1:7-1], zoo::rollmean(my_diff, 7, align="right"))) %>%
      #mutate(pct_increase =diff.ma/Values*100) %>%
      mutate(pct_increase = my_diff/Values*100) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
    #US.ma[US.ma$diff.ma > 0 & US.ma$pct_increase > 5, "pct_increase"] <- 5
    #US.ma[US.ma$my_diff > 0 & US.ma$pct_increase > 5, "pct_increase"] <- 5
    US.ma[is.na(US.ma$pct_increase) | US.ma$pct_increase <= 0, "pct_increase"] <- 0
    US.ma <- US.ma %>%
      filter(date == as.Date(as.POSIXct(hover$x, origin="1970-01-01"), tz="EST"))
    
    five.plus <- ""
    #if (length(US.ma$pct_increase) > 0) {
    #  if(US.ma$pct_increase >= 5) {
    #    five.plus <- "+"
    #  }
    #}
    
    wellPanel(
      style = style,
      class = "gg_tooltip",
      p(HTML(paste0("<b> Date: </b>", as.Date(as.POSIXct(hover$x, origin="1970-01-01"), tz="EST"), "<br/>",
                    "<b>", category, ": </b>", format(round(US.ma$Values,2), big.mark = ","), "<br/>",
                    "<b> Change in ", category, ": </b>+",  format(round(US.ma$my_diff,2),big.mark = ","), "<br/>",
                    "<b> Daily Percentage Increase: </b>",  format(round(US.ma$pct_increase,2),big.mark = ","), "%",five.plus,"<br/>"
      ))))
    
  }
  
  output$US.CoT.tooltip <- renderUI({
    hover <- input$US.CoT.hover
    US.barplot.tooltip(hover, "cases")
  })
  
  output$US.DoT.tooltip <- renderUI({
    hover <- input$US.DoT.hover
    US.barplot.tooltip(hover, "deaths")
  })
  
  output$US.CoT <- renderPlot({
    ggbar.US(y.value = "cases", remove.title = T)
  }, alt = "A time series bar plot representing United States cumilative COVID-19 cases over time.")
  
  output$US.CoT.dl <- downloadHandler(
    filename = function() {
      return("US_CoT_plot.png")
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = ggbar.US(y.value = "cases", remove.title = F, date = update_date) + NULL,
             device = "png",
             width = 12,
             height = 8,
             units = "in")
    }
  )
  
  output$US.DoT <- renderPlot({
    ggbar.US(y.value = "deaths", remove.title = T)
  }, alt = "A time series bar plot representing United States cumilative COVID-19 deaths over time.")
  
  output$US.DoT.dl <- downloadHandler(
    filename = function() {
      return("US_DoT_plot.png")
    },
    content = function(file) {
      ggsave(filename = file, 
             plot = ggbar.US(y.value = "deaths", remove.title = F, date = update_date) + NULL,
             device = "png",
             width = 12,
             height = 8,
             units = "in")
    }
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
    else { #if per/100k
      y.value = "p_diff"
    }
    
    ggplot.US(y.value=y.value, moving.avg.window=7, selected.states=selected.states$abr, remove.title=T) +
      coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
      NULL
  }, alt = "A time series plot representing United States new COVID-19 cases over time, broken down by state.")
  
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
      else { #if per/100k
        y.value = "p_diff"
      }
      ggsave(filename = file, 
             plot = ggplot.US(y.value=y.value, 
                              moving.avg.window=7, 
                              selected.states=selected.states$abr, 
                              remove.title=F,
                              date = update_date) +
               coord_cartesian(xlim = Tr.ranges$x, ylim = Tr.ranges$y) +
               NULL,
             device = "png",
             width = 12,
             height = 8,
             units = "in")
    }
  )
  
  # TODO: This is written twice
  output$US.maps.determinant <- renderLeaflet({
    det <- input$US.determinant
    geo.plot("US", det)
  })
  
  output$US.maps.determinant.dl <- downloadHandler(
    filename = function() {
      det <- input$US.determinant
      return(paste0("US_", det, ".png"))
    },
    content = function(file) {
      det <- input$US.determinant
      det_title <- det
      if (det == "CRD Mortality") {
        det_title <- "Cronic Respiratory Disease (CRD) Mortality"
      }
      title <- tags$h1(paste0("US ", det_title, " Rate Disparities Compared to the US Average"))
      mapshot(x = geo.plot("US", 
                           det, 
                           title = tags$div(title)
      ),
      file = file,
      cliprect = "viewport",
      selfcontained = F)
    }
  )
  
  
  ### The following code deals with setting or responding to parameterized URLs
  observe(print(input$tab))
  
  observe({
    # This "does the right thing" for an incoming URL
    # suppose url is http://127.0.0.1:5682/?tab=tab3c/plot
    query <- parseQueryString(session$clientData$url_search)
    
    if(!is.null(query$tab)) {
      url <- strsplit(query$tab,"/")[[1]]
      url1 <<- url[1]
      url2 <<- url[2]
      updateTabsetPanel(session, 'tab', url1)
    }
  })
  
  
  observe({
    # Trigger this observer every time an input changes
    params <- reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    # Construct the replacement URL:
    url.new <- paste0(
      session$clientData$url_protocol,"//",
      session$clientData$url_hostname,
      session$clientData$url_pathname,
      "?tab=",
      session$clientData$url_port,
      session$input$tab
    )
    #TODO: Special handling for tabs with selectors!
    # browser()
    updateQueryString(url.new)
  })
  
  observe({ # this observer executes once, when the page loads " 
    
    data <- parseQueryString(session$clientData$url_search)
    
    # browser()
    # the navbar tab and tabpanel variables are two variables 
    # we have to pass to the client for the update to take place
    # if nav is defined, send a message to the client to set the nav tab
    if (! is.null(data$page)) {
      session$sendCustomMessage(type='setNavbar', data)
    }
    
    # if the tab variable is defined, send a message to client to update the tab
    if (any(sapply(data[c("state_report_cards",
                          "national_report_card",
                          'determinant_disclaimer',
                          'about'
    )], 
    Negate(is.null)))) {
      # browser()
      session$sendCustomMessage(type='setTab', data)
    }
    
  })
}

#### Set up Shiny App ####
shinyApp(ui = ui, server = server, enableBookmarking = "url")
