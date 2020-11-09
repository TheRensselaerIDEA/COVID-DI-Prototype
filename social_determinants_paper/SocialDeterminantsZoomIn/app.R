#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("Source.R")

state.list <- state.abb
names(state.list) <- state.name
# state.list <- append(state.list, "United States", after = 0)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MortalityMinder Visualization Attempt"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$div(
                class = "input",
                tags$span(id = "input_state", "State:"),
                pickerInput(
                    inputId = "state_choice",
                    label = h4("State"), 
                    choices = state.list,
                    selected = "OH",
                    options = list(
                        `live-search` = TRUE,
                        "dropup-auto" = FALSE
                    )      
                )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("textDeathTrends"),
            plotOutput("trendPlot"),
            plotOutput("kendallPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$textDeathTrends <- renderUI({
        tagList(
            tags$h3(
                title="This plot represents the COVID-19 death trends for each risk group. The blue line represents the national average. ",
                "Deaths of COVID-19 Trends for Risk Groups across ", names(which(state.list == input$state_choice))
            ),
            tags$h4("The COVID-19 death trends for each risk group compared with the national average (in blue)."),
            NULL
        )
    })

    output$trendPlot <- renderPlot({
        
        covid.data <- subset(covid_us_hist, period %in% date_of_all)
        covid.data <- covid.data %>% drop_na(death_num,population)
        
        nation <- aggregate(cbind(covid.data$death_num, covid.data$population), by=list(period=covid.data$period), FUN=sum)
        nation <- rename(nation,c(death_num = V1,population=V2))
        nation$death_rate <- nation$death_num/nation$population*100000
        
        nation$cluster <- "National"
        nation$count <- NA
        nation <- subset(nation, select = c(period,cluster,death_rate,count))
        
        cluster.raw <- covid.data %>%
            dplyr::filter(state_abbr == input$state_choice) %>%
            tidyr::drop_na(county_fips) %>%
            dplyr::select(county_fips, death_rate, period) %>%
            dplyr::group_by(period) %>% 
            dplyr::mutate(id=1:n()) %>%
            tidyr::spread(key = period, value = death_rate) %>%
            dplyr::arrange(county_fips) %>%
            km.func(3)
        
        avg.cluster.raw <- covid.data %>%
                               dplyr::filter(state_abbr == input$state_choice) %>%
                               dplyr::right_join(cluster.raw, by = "county_fips") %>%
                               dplyr::group_by(period, cluster) %>%
                               dplyr::summarise(
                                   death_rate = sum(death_num) / sum(population) * 10^5,
                                   count = n()
                               ) %>%
                               dplyr::ungroup()
        
        cluster.map <- avg.cluster.raw %>%
                           dplyr::filter(period == tail(date_of_all, n=1)) %>%
                           dplyr::arrange(death_rate) %>%
                           dplyr::mutate(ord = as.character(1:n())) %>%
                           dplyr::select(-c(period, death_rate)) %>%
                           textshape::column_to_rownames("cluster")
        
        avg.cluster.ord <- dplyr::mutate(avg.cluster.raw, cluster = cluster.map[cluster, "ord"])
        
        nclusters <- max(cluster.raw$cluster)
        total.data <- rbind(avg.cluster.ord,nation)
        
        # TODO: 
        # AK: invalid first argument
        # ND: invalid first argument
        # OR: invalid first argument
        # UT: invalid first argument
        # KS: invalid first argument
        # DE: Insufficient values in manual scale. 3 needed but only 1 provided.
        # HI
        # RI: STRANGE OUTPUT
        # ID: number of cluster centres must lie between 1 and nrow(x)
        # MA: number of cluster centres must lie between 1 and nrow(x)
        # IA: cannot take a sample larger than the population when 'replace = FALSE'
        # MI: cannot take a sample larger than the population when 'replace = FALSE'
        # MT: cannot take a sample larger than the population when 'replace = FALSE'
        # NE: cannot take a sample larger than the population when 'replace = FALSE'
        # NY: cannot take a sample larger than the population when 'replace = FALSE'
        # SD: cannot take a sample larger than the population when 'replace = FALSE'
        
        total.data$cluster[total.data$cluster == 1] <- "1: Low"
        total.data$cluster[total.data$cluster == 2] <- "2: Medium"
        total.data$cluster[total.data$cluster == 3] <- "3: High"
        line_plot <- ggplot(total.data,        
                            aes(x = period, 
                                y = death_rate, 
                                color = cluster,
                                group = cluster) ) + 
            geom_line(size = 1.5) + 
            geom_point(color = "#565254", shape = 21, fill = "#f7f7f7", size = 2) + 
            scale_color_manual(values = theme.categorical.colors.accent(nclusters)) +
            theme.line.mort() + 
            theme(legend.position = "left") + 
            guides(color = guide_legend(reverse = T,
                                        title = "Risk Group:")) +
            labs(fill = "Cluster", 
                 color = "Cluster",
                 title = paste("Deaths of COVID-19 Trends for Risk Groups across", names(which(state.list == input$state_choice))),
                 caption = "Mortality Data: JHU CSSE COVID-19 Data\nFeature Data: County Health Rankings\nAnalysis: The Rensselaer IDEA") + 
            ylab("COVID-19 Deaths per 100,000")
        line_plot
        # ggsave(paste(input$state_choice, "_trend.png", sep=""), width = 8, height = 6)
    })
    
    output$kendallPlot <- renderPlot({
        
        covid.data <- subset(covid_us_hist, period %in% date_of_all)
        covid.data <- covid.data %>% drop_na(death_num,population)
        
        mort.rate <- covid.data %>% dplyr::filter(
                         state_abbr == input$state_choice,
                         period == tail(date_of_all, n=1)
                     ) %>%
                         dplyr::mutate(
                             # death_rate = death_num / population * 10^5
                             #death_rate = cut(death_rate, bin.geo.mort("Despair"))
                         ) %>%
                         dplyr::select(county_fips, death_rate)
        
        kendall.cor <- mort.rate %>% 
                           dplyr::mutate(VAR = death_rate) %>%
                           kendall.func(chr.data.2019) %>%
                           dplyr::mutate(
                               DIR = dplyr::if_else(
                               kendall_cor <= 0,
                               "Protective",
                               "Destructive"
                               ),
                               chr_code = chr.namemap.2019[chr_code, 1]
                           ) %>% na.omit()
        
        # Sort by kendall.cor
        kendall.cor.new <- kendall.cor %>% 
                               dplyr::filter(kendall_p < 0.1) %>% 
                               dplyr::arrange(desc(kendall_cor)) %>% 
                               dplyr::top_n(15, abs(kendall_cor)) %>% 
                               dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
        
        # # Set currently selected determinant to most correlated determinant
        # max.cor.ind = which.max(abs(kendall.cor.new$kendall_cor))
        # input$determinant_choice = kendall.cor.new[max.cor.ind, "chr_code"]
        
        #Only display the social determinants graph if there is any significant social determinant
        #Ex: New Hampshire, Delaware doesn't have any significant social determinant with p < 0.05
        if(nrow(kendall.cor.new) > 0) {
            # updatePickerInput(session, "determinant_choice", selected = kendall.cor.new$chr_code[[1]])
            kendall.cor.new %>% 
                ggplot(
                    aes(
                        #x = reorder(chr_code, kendall_cor), 
                        x = chr_code, 
                        y = kendall_cor, 
                        color = DIR, 
                        fill = DIR)
                ) + 
                
                # Lolipop chart
                geom_point(stat = 'identity', size = 12) + 
                geom_segment(
                    size = 1,
                    aes(
                        y = 0, 
                        #x = reorder(chr_code, kendall_cor), 
                        x = chr_code, 
                        yend = kendall_cor, 
                        #xend = reorder(chr_code, kendall_cor), 
                        xend = chr_code, 
                        color = DIR
                    )
                ) +
                geom_text(
                    aes(
                        label = chr_code, 
                        y = ifelse(DIR == "Protective", 0.1, -0.1),
                        hjust = ifelse(DIR == "Protective", 0, 1)
                    ), 
                    color = "#565254", 
                    size = 4
                ) +
                geom_text(
                    aes(label = round(kendall_cor, 2)), 
                    color = "#565254", 
                    size = 3
                ) +
                
                # Coordinates
                coord_flip() + 
                scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
                
                # Themes
                geom_hline(yintercept = .0, linetype = "dashed") + 
                labs(
                    y = "Correlation",
                    x = NULL,
                    title = paste("Factors Associated with COVID-19 Deaths for", names(which(state.list == input$state_choice))),
                    fill = "Relationship",
                    color = "Relationship"
                ) +
                theme_minimal() +
                theme.text() + 
                theme.background() + 
                theme(
                    axis.text.y = element_blank(),
                    axis.text.x = element_text(size = 12),
                    axis.title.x = element_text(size = 12),
                    panel.grid.major.y = element_blank()
                ) + 
                theme(legend.position="top")
        }
        #Display something else when there are no significant SD
        else {
            
            # empty plot, then put text on it ?
            ggplot() + theme_void() +
                geom_text(aes(x = 0, y = 0, label="There are no significant social determinants."))
            
        }
        # ggsave(paste(input$state_choice, "_kendall.png", sep=""), width = 6, height = 10)
    }, bg = "transparent")
}

# Run the application 
shinyApp(ui = ui, server = server)
