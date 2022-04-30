
#load packages
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MetBrewer)
library(RColorBrewer)
library(gganimate)
library(gifski)
library(ggiraph)
library(glue)
library(ggstream)
library(plotly)
library(tm)
library(quanteda)
library(tidytext)
data("stop_words")
library(textreadr)
library(textstem)
library(wordcloud)
library(reshape2)

#NLP annotate overrides ggplot2 annotate function so have to call ggplot2::annotate

#####LOAD DATA######
#load ranked data
world_rank <- read.csv("CO2_ranked_sectors_percentage.csv", header = T)
#remove first column with X
world_rank <- world_rank[,-1]
#load EU data
EU_story <- read.csv("EU_percentchange_pop.csv", header = T)
#remove first column with X
EU_story <- EU_story[, -1]

#NLP
#load word frequency data
words <- read.csv("df_words_frequency.csv", header = T, fileEncoding="latin1")

###########


#make SHINY APP
# Define UI
ui <- navbarPage(title = "Carbon Emissions",
                 tabPanel("Overview",

    #create a grid on website
    fixedRow(
      column(width = 5, offset = 2,
             #slider for Year
             chooseSliderSkin("Round", "#6495ED"),
             setSliderColor("#6495ED", c(1)), #set how many sliders with this specific color
             sliderInput("Year_slider", label = h4("Year"),
                         min = 1850, max = 2018, value = 2018,
                         step = 1, sep = "", width = 400, animate = F)
             )
        ),
    fixedRow(
        column(width = 6,
             plotOutput("rank", height = 500, width = 1000,
                        #brush = brushOpts("rank_brush", resetOnNew = T),
                        #hover = hoverOpts("rank_hover", nullOutside = T, delay = 0)
                        #click = clickOpts("rank_click", clip = T)
                        )
             ),

        column(width = 4, offset = 2,
               h3("Sectors"),
               br(),
               p("\nHover over a country for carbon emissions breakdown by sector"),
               girafeOutput("sector")
               )
        ),
                 ),

    tabPanel("Story",
             fluidRow(column(width = 5, offset = 1, style = "padding-bottom:100px",
                             imageOutput("eu_rank")
                             ),
                      column(width = 6, style = "padding-bottom:100px",
                             mainPanel(br(),
                               h3("The Story in Europe"),
                               p("Europe has always been at the forefront of policymaking on environmental issues, in particular, resource sustainability. From the plot on the left we can see that even though European countries have not been one of the top 3 countries with the greatest carbon emissions since 1975, countries in Europe have maintained their spots on the top 10 list throughout the past decade and a half. Notably, prior to 1872 the United Kingdom has #1 in total carbon emissions for 21 years far outstripping the country that came in second (United States). In recent years the US, Russia, and China have taken the top 3 spots on total carbon emissions, we propose a question of whether Europe's diminished emissions have to do with policy making such as the Paris Agreement (2015) and the European Green Deal (2018).",
                                         style = "font-family: 'Helvetica'; fontsi16pt")))
                      ),

             fluidRow(column(width = 12)
                      ),

             fluidRow(column(width = 11, offset = 1, style = "padding:100px",
                             tabsetPanel(
                               tabPanel("Emissions Stream",
                                        br(),
                                        plotOutput("EU_emissions_stream", width = 1000, height = 500)
                                        ),
                               tabPanel("EU Emissions Overtime",
                                        br(),
                                        plotlyOutput("EU_emissions", width = 1000, height = 500)
                                        ),
                               tabPanel("EU Emissions Percentage Change",
                                        br(),
                                        plotOutput("EU_percent_change", width = 1000, height = 500)
                                        )
                               )
                             )
             )
    ),
    tabPanel("IPCC Reports Analysis",
             fluidRow(column(width = 10, offset = 2,
                             plotOutput("wordcloud", height = 1000, width = 1000)
                             )
                      ),

             fluidRow(column(width = 11, offset = 1,
                             plotOutput("top10_words_all")
                             )
                      ),

             fluidRow(column(width = 11, offset = 1,
                             plotOutput("top10_words_reports", 1000, height = 500)
                             )
                      ),

             fluidRow(column(width = 11, offset = 1,
                             plotOutput("words_overtime")
                             )
                      )

    )

)



# Define server logic
server <- function(input, output, session) {

  #updateSelectizeInput(session, "Year_slider", choices = world_rank, server = TRUE)

  #create bar chart of world ranking carbon emissions
  output$rank <- renderPlot({

    #filter for YEAR
    Filtered <- world_rank %>%
      filter(Year == input$Year_slider)

    #put plot code HERE
    Filtered %>%
      filter(Sector == "Total excluding LULUCF") %>%
      ggplot(aes(rank, Emissions, group = reorder(Country, Emissions), fill = as.factor(rank))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(y = 0, label = paste(Country, "")), vjust = 0.2, hjust = 1) +
      scale_fill_manual(values = met.brewer("OKeeffe1", 10)) +
      scale_x_reverse() +
      coord_flip(clip = "off", expand = FALSE) +
      theme(legend.position = "none",
            legend.text = element_blank(),
            legend.title = element_blank(),
            panel.grid.major.x = element_line(color = "#D3D3D3", size = 0.2),
            panel.grid.minor.x = element_line(color = "#D3D3D3", size = 0.1),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
            axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 12),
            axis.text.x = element_text(family = "Helvetica", color = "#36454F"),
            axis.title.y = element_text(family = "Helvetica", color = "#36454F"),
            axis.text.y = element_blank(),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 12),
            plot.margin = margin(2, 2, 2, 4, "cm")) +
      labs(title = "Top 10 Countries: Total Carbon Emissions",
           caption = "\nData from the Postdam Institute for Climate Impact Research",
           x = "",
           y = "Carbon Emissions per Metric Ton")

  })

  #render the bar chart with the labelled sectors
  output$sector <- renderGirafe({

    country_select <- nearPoints(world_rank, input$rank_hover, threshold = 10, maxpoints = 1)

    #ggplot code
    sector_percentage <- world_rank %>%
      filter(Year == input$Year_slider) %>% #filtering which year is selected
      #filter(Country == country_select) %>%
      filter(Sector != "Total excluding LULUCF") %>%
      ggplot(aes(rank, Percentage)) +
      geom_bar_interactive(aes(y = Percentage, fill = reorder(Sector, Percentage),
                               tooltip = glue("{Country}\nSector: {Sector}\nPercentage: {Percentage}%\nRaw Emissions (Metric Tons): {Emissions}")),
                           stat = "identity", position = "stack", show.legend = T) +
      scale_fill_manual(labels = c("Energy", "Industrial Processes \nand Product Use", "Waste", "Agriculture", "No Data"),
                        values = c("Energy" = "#CC7722", "Industrial Processes and Product Use" = "#dea414",
                                   "Waste" = "#749222", "Agriculture" = "#076244", "No Data" = "#A9A9A9")) +
      geom_text(aes(y = 0, label = paste(Country, "")), vjust = 0.2, hjust = 1,
                family = "Helvetica", color = "#36454F", check_overlap = TRUE) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_x_reverse() +
      theme(legend.position = "bottom",
            legend.key.size = unit(6, "mm"),
            legend.text = element_text(family = "Helvetica", color = "#36454F", size = 9),
            legend.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA), #make bg transparent
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            plot.background = element_rect(fill = "transparent", colour = NA),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 8),
            plot.margin = margin(1, 1, 1, 3, "cm"))

    #tooltip code (ggiraph)
    girafe(ggobj = sector_percentage,
           bg = "transparent",
           options = list(
             opts_tooltip(
               opacity = 0.8, use_fill = T, use_stroke = F,
               css = "padding:5pt;font-family: Helvetica;color:white"
               ),
             opts_hover_inv(css = "opacity:0.5"),
             opts_hover(css = "fill:#EDEADE")
           )
    )

  })

  output$eu_rank <- renderImage({

    outfile <- tempfile(fileext = ".gif")

      eu_story <- world_rank %>%
        ggplot(aes(rank, group = Country, fill = as.factor(EU))) +
        geom_tile(aes(y = Emissions_tot / 2, height = Emissions_tot, width = 0.9), alpha = 0.8) +
        geom_text(aes(y = 0, label = paste(Country, "")), vjust = 0.2, hjust = 1,
                  family = "Helvetica", color = "#36454F", check_overlap = TRUE) +
        geom_text(aes(y = Emissions_tot, label = round(Emissions_tot), hjust = -1),
                  family = "Helvetica", color = "#36454F", check_overlap = TRUE) +
        scale_fill_manual(values = c("1" = "#1F51FF", "0" = "#B2BEB5")) +
        scale_x_reverse() +
        coord_flip(clip = "off", expand = FALSE) +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.position = "none",
              legend.text = element_blank(),
              legend.title = element_blank(),
              panel.grid.major.x = element_line(color = "#D3D3D3", size = 0.2),
              panel.grid.minor.x = element_line(color = "#D3D3D3", size = 0.1),
              panel.grid.major.y = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold"),
              axis.title.x = element_text(family = "Helvetica", color = "#36454F"),
              axis.text.x = element_blank(),
              axis.title.y = element_text(family = "Helvetica", color = "#36454F"),
              axis.text.y = element_blank(),
              plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 30, face = "bold"),
              plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10),
              plot.margin = margin(2, 2, 2, 4, "cm")) +
        #animating
        labs(title = "Top 10 Countries: Total Carbon Emissions",
             subtitle = "Year: {closest_state}",
             caption = "\nData from the Postdam Institute for Climate Impact Research",
             x = "",
             y = "") +
        transition_states(Year, transition_length = 1, state_length = 1) +
        view_follow(fixed_x = T)

      #animate the plot with gganimate
      anim_save("outfile.gif",
                animate(eu_story, fps = 15, end_pause = 7, nframe = 2.5 * length(unique(world_rank$Year)),
                        width = 500, height = 600)
                )

      # Return a list containing the filename
      list(src = "outfile.gif", contentType = "image/gif")


  }, deleteFile = TRUE)


  output$EU_emissions_stream <- renderPlot({

    #order country by average emissions
    country_emi_rank <- unique(EU_story$Country)

    #stream graph
    EU_story %>%
      transform(Country = factor(Country, levels = country_emi_rank)) %>%
      transform(Year = as.numeric(Year)) %>%
      ggplot(aes(Year, Emissions, color = Country, fill = Country)) +
      geom_stream(geom = "contour", color = "white", size = 1.25, bw = 0.1) +
      geom_stream(geom = "polygon", bw = 0.1, size = 0) +
      scale_fill_manual(values = rev(met.brewer("Renoir", n = length(unique(EU_story$Country)))), name = "Country") +
      guides(fill = guide_legend(nrow = 2, ncol = 14)) +
      theme(legend.position = "bottom",
            legend.key.height = unit(0.5, "mm"),
            legend.key.width = unit(1.5, "mm"),
            legend.key.size = unit(2, "mm"),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.title = element_text(size = 15, color = "#36454F"),
            legend.text = element_text(size = 10, color = "#36454F"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(family = "Helvetica", color = "#36454F",  face = "bold", size = 15),
            axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
            axis.title.y = element_text(family = "Helvetica", color = "#36454F"),
            axis.text.y = element_blank(),
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 25),
            plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 10),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10)) +
      labs(title = "Carbon Emissions Overtime in Europe",
           subtitle = "Annual Emissions in Metric Tons",
           caption = "\nData from the Postdam Institute for Climate Impact Research",
           x = "Year",
           y = "") +
      geom_segment(aes(x = 1850 , y = 0, xend = 2018, yend = 0),
                   color = "#E5E4E2", size = 0.1, lty = "dotted") +
      ggplot2::annotate(geom = "text", x = 2022, y = 0, label = "0", size = 3, family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1950 , y = 1000, xend = 2018, yend = 1000),
                   color = "#E5E4E2", size = 0.1, lty = "dotted") +
      ggplot2::annotate(geom = "text", x = 2022, y = 1000, label = "1000", size = 3, family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1969 , y = 2000, xend = 2018, yend = 2000),
                   color = "#E5E4E2", size = 0.1, lty = "dotted") +
      ggplot2::annotate(geom = "text", x = 2022, y = 2000, label = "2000", size = 3, family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1950 , y = -1000, xend = 2018, yend = -1000),
                   color = "#E5E4E2", size = 0.1, lty = "dotted") +
      ggplot2::annotate(geom = "text", x = 2022, y = -1000, label = "1000", size = 3, family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1969 , y = -2000, xend = 2018, yend = -2000),
                   color = "#E5E4E2", size = 0.1, lty = "dotted") +
      ggplot2::annotate(geom = "text", x = 2022, y = -2000, label = "2000", size = 3, family = "Helvetica", color = "#36454F")

  })


  output$EU_emissions <- renderPlotly({

    #order country by average emissions
    country_emi_rank <- unique(EU_story$Country)

    #interactive line chart of emissions over time
    eu_emissions <- EU_story %>%
      transform(Country = factor(Country, levels = country_emi_rank)) %>%
      transform(Year = as.numeric(Year)) %>%
      ggplot(aes(Year, Emissions, color = Country)) +
      geom_line() +
      scale_color_manual(values = rev(met.brewer("Renoir", n = length(unique(EU_story$Country)))), name = "Country") +
      scale_x_continuous(breaks = c(1850, 1875, 1900, 1925, 1950, 1975, 2000, 2018)) +
      scale_y_continuous(breaks = c(0, 300, 600, 900, 1200)) +
      guides(col = guide_legend(nrow = length(unique(EU_story$Country)))) +
      theme(legend.position = "right", #c(0.25, 0.7),
            legend.key.height = unit(0.5, "mm"),
            legend.key.width = unit(1.5, "mm"),
            legend.key.size = unit(2, "mm"),
            legend.key = element_rect(fill = "transparent", color = NA),
            legend.title = element_text(size = 10, color = "#36454F"),
            legend.text = element_text(size = 8, color = "#36454F"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#D3D3D3", size = 0.3),
            panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.1),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(family = "Helvetica", color = "#36454F"),
            axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
            axis.title.y = element_text(family = "Helvetica", color = "#36454F"),
            axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10)) +
      labs(title = "Carbon Emissions Overtime in Europe\n ",
           caption = "\nData from the Postdam Institute for Climate Impact Research",
           x = "Year",
           y = "Annual Carbon Emissions (Metric Tons)") +
      geom_segment(aes(x = 2015 , y = 0, xend = 2015, yend = 1190), color = "#50C878") +
      ggplot2::annotate(geom = "text", x = 2015, y = 1260, label = "Paris \nAgreement", size = 2.5,
               family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1987, y = 0, xend = 1987, yend = 1190), color = "#50C878") +
      ggplot2::annotate(geom = "text", x = 1987, y = 1260, label = "Montreal \nProtocol", size = 2.5,
               family = "Helvetica", color = "#36454F")

    #create interactivity
    ggplotly(eu_emissions, dynamicTicks = F, tooltip = c("Country", "Year", "Emissions"))

  })


  output$EU_percent_change <- renderPlot({

    #order by population
    EU_story2 <- EU_story %>%
      group_by(Country) %>%
      arrange(desc(avg_pop))

    country_order <- unique(EU_story2$Country)

    #percentage change graph
    EU_story2 %>%
      transform(Country = factor(Country, levels = country_order)) %>%
      ggplot(aes(Year, percent_change, fill = percent_change > 0)) +
      geom_bar(stat = "identity") +
      facet_wrap(~as.factor(Country), nrow = 4) +
      scale_fill_manual(name = "percent_change > 0",
                        values = setNames(c("#C41E3A", "#0096FF"), c(T, F)), guide = "none") +
      theme(strip.text = element_text(family = "Helvetica", color = "#36454F", size = 12, face = "bold.italic"),
            #strip.background = element_rect(fill = "#F0FFFF", color = "#F0FFFF"),
            strip.background = element_rect(fill = "transparent", color = NA),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "#D3D3D3", size = 0.1),
            panel.border = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_line(color = "#D3D3D3", size = 0.1),
            axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 15),
            axis.text.x = element_blank(),
            axis.title.y = element_text(family = "Helvetica", color = "#36454F", size = 15),
            axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 25),
            plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 18),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10)) +
      labs(title = "Percentage Change in Carbon Emissions",
           subtitle = "in European Countries from 1850 to 2018\n ",
           caption = "*each bar represents 1 year \nData from the Postdam Institute for Climate Impact Research",
           x = "Years",
           y = "Percentage Change")

  })

  output$wordcloud <- renderPlot({

    #for reproducibility
    set.seed(1234)

    #word cloud
    wordcloud(words = words$word, freq = words$total, min.freq = 1,
              max.words=80, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))

  })


  output$top10_words_all <- renderPlot({

    #top 10 words across all reports
    words %>%
      arrange(desc(total)) %>%
      dplyr::slice(1:10) %>%
      ggplot(aes(x = reorder(word, total), total)) +
      geom_col(fill = "#1FA187") +
      labs(x = NULL, y = "Word Frequency") +
      ggtitle("Word Frequency in All IPCC Reports (1990 - 2014)") +
      coord_flip() +
      theme(panel.grid.major.x = element_line(color = "#D3D3D3", size = 0.3),
            panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.1),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 15),
            axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
            axis.title.y = element_text(family = "Helvetica", color = "#36454F", size = 15),
            axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10))

  })


output$top10_words_reports <- renderPlot({

  #reorganize dataframe
  melted_d <- melt(words, id = "word", measure.vars = 2:ncol(words))

  #changing the labels names
  melted_d$variable <- factor(melted_d$variable, levels = c("ipcc_90_92.pdf", "ipcc_95.pdf", "ipcc_01.pdf", "ipcc_07.pdf", "ipcc_14.pdf","total"),
                              labels = c("IPCC 1992 Report", "IPCC 1995 Report", "IPCC 2001 Report", "IPCC 2007 Report", "IPCC 2014 Report","All Reports")
  )

  #need to fist filter for top 10 of all the reports
  # Plot Data Frame
  pd <- melted_d %>%
    group_by(variable) %>%
    top_n(10, value) %>%
    # 1. Remove grouping
    ungroup() %>%
    # 2. Arrange by
    #   i.  facet group
    #   ii. bar height
    arrange(variable, word) %>%
    # 3. Add order column of row numbers
    mutate(order = row_number())

  #top 10 words
  top_10 <- words %>%
    arrange(desc(total)) %>%
    dplyr::slice(1:10)

  top10_words <- unique(top_10$word)

  #plot top 10 words faceted by report
  pd %>%
    mutate(color = as.factor(case_when(word %in% top10_words ~ 1,
                                       T ~0))) %>%
    ggplot(aes(x = reorder_within(word, value, variable), value, fill = color)) +
    scale_x_reordered() +
    geom_col(width = 0.8, aes(fill = color)) +
    scale_fill_manual(values = c("1" = "#1FA187", "0" = "#FF7518")) +
    #geom_bar(stat = "identity", show.legend = FALSE, fill = "#1FA187") +
    facet_wrap(~ variable, scales = "free") +
    xlab("Words") +
    ylab("Frequency") +
    theme_bw() +
    coord_flip() +
    theme(legend.position = "none",
          strip.text = element_text(family = "Helvetica", color = "#36454F", size = 12, face = "bold.italic"),
          strip.background = element_rect(fill = "transparent", color = NA),
          panel.spacing = unit(30, "pt"),
          panel.grid.major.x = element_line(color = "#D3D3D3", size = 0.3),
          panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.1),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 15),
          axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
          axis.title.y = element_text(family = "Helvetica", color = "#36454F", size = 15),
          axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
          plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
          plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 18),
          plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10)) +
    labs(title = "Word Frequency in IPCC Reports",
         subtitle = "from 1992 to 2014\n ")

})


output$words_overtime <- renderPlot({

  #get top ten
  key_words = c("change", "climate", "increase", "emission", "much", "impact", "can", "level", "scenario", "use")
  melted_time <- subset(melted_d, word %in% key_words) #filter from melted_d

  #recode variables
  melted_time$variable <- recode_factor(melted_time$variable,"IPCC 2001 Report" = "2001", "IPCC 2007 Report" = "2007",
                                        "IPCC 2014 Report" = "2014","IPCC 1992 Report" = "1992","IPCC 1995 Report" = "1995")

  melted_time$variable <- as.numeric(as.character(melted_time$variable))

  #order the words from all reports (to fix legend ordering)
  word_order <- melted_time %>%
    filter(is.na(variable)) %>%
    arrange(desc(value))

  word_order_legend <- unique(word_order$word)

  #plot words overtime
  ggplot(melted_time, aes(x =  variable, y = value, color = word)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = met.brewer("Peru1", 10),
                       breaks = (word_order_legend)) +
    ylim(c(0, 3500)) +
    #scale_x_continuous(breaks = c(1992, 1995, 2001, 2007, 2014)) +
    labs(title = "Evolution of Keyword Frequencies from 1992 to 2014",
         x = "Year of the Published Report",
         y = "Word Frequency",
         color = "Keywords") +
    theme(legend.position = "bottom", #c(0.25, 0.7),
          legend.key.height = unit(0.5, "mm"),
          legend.key.width = unit(1.5, "mm"),
          legend.key.size = unit(2, "mm"),
          legend.key = element_rect(fill = "transparent", color = NA),
          legend.title = element_text(size = 10, color = "#36454F"),
          legend.text = element_text(size = 8, color = "#36454F"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#D3D3D3", size = 0.3),
          panel.grid.minor.y = element_line(colour = "#D3D3D3", size = 0.1),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 15),
          axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
          axis.title.y = element_text(family = "Helvetica", color = "#36454F", size = 15),
          axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
          plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
          plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 18),
          plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10))

  #I want to make this interactive
})

  #output$info <- renderPrint({
    #hover_year <- world_rank %>%
      #filter(Year == input$Year_slider)

    #nearPoints(hover_year, input$rank_hover, threshold = 10, maxpoints = 1) })

}

# Run the application
shinyApp(ui = ui, server = server)
