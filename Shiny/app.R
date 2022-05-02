
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
library(textreuse)
library(DT)
library(leaflet)
library(ggrepel)
library(ggspatial)
library(lwgeom)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(htmlwidgets)
library(htmltools)
library(leafletCN)

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
#load document comparison data
comparisons <- read.csv("comparisons.csv", sep = ",")

###########

#####ORGANIZE MAP DATA######
#read in csv
world <- read.csv("historical_emissions_world.csv", header = T, check.names = F)
#remove source column
world <- world[, -2]

#make long
world_long <- world %>%
  pivot_longer(!(c(Country, Sector, Gas, Unit)),
               names_to = "Year", values_to = "Emissions")

#filter out variables that are not needed
world_CO2 <- world_long %>%
  filter(Gas == "CO2") %>%
  filter(Sector == "Total excluding LULUCF") %>%
  filter(!((Country == "BASIC countries (Brazil, South Africa, India and China)") |
             (Country == "Alliance of Small Island States (AOSIS)") |
             (Country == "Annex-I Parties to the Convention") |
             (Country == "Least Developed Countries") |
             (Country == "Non-Annex-I Parties to the Convention") |
             (Country == "European Union (27)") |
             (Country == "Umbrella Group") |
             (Country == "World"))) %>%
  select(-(c(Sector, Gas, Unit)))


#load map
map <- ne_countries(scale = "medium", returnclass = "sf")
class(map)

#select variables from map
map_c <- map %>%
  select(c(name_long, geometry, continent))

#change Russian Federation to Russia
map_c$name_long[map_c$name_long == "Russian Federation"] <- "Russia"


#merge dataset and map
world_map <- merge(map_c, world_CO2, by.x = "name_long", by.y = "Country", all.y = T)

#remove countries that do not have geometries
world_map_c <- world_map %>%
  rename(Country = name_long) %>%
  filter(!((Country == "Brunei") |
             (Country == "Gambia") |
             (Country == "Hong Kong, Special Administrative Region of China") |
             (Country == "Macao, Special Administrative Region of China") |
             (Country == "Netherlands Antilles") |
             (Country == "South Korea") |
             (Country == "Tuvalu") |
             (Country == "Eswatini") |
             (Country == "Holy See (Vatican City State)") |
             (Country == "Laos") |
             (Country == "Micronesia") |
             (Country == "North Korea") |
             (Country == "Sao Tome and Principe") |
             (Country == "Taiwan, Republic of China"))) %>%
  na.omit()
#############



#make SHINY APP
# Define UI
ui <- navbarPage(title = "Carbon Emissions",
                 tabPanel("Overview",

    #create a grid on website
    fluidRow(column(width = 12,
                    leafletOutput("map", width = "100%", height = 680)
                    )
             ),

    fluidRow(
      column(width = 5, offset = 4,
             #slider for Year
             chooseSliderSkin("Round", "#6495ED"),
             setSliderColor("#6495ED", c(1)), #set how many sliders with this specific color
             sliderInput("Year_slider", label = h4("Year"),
                         min = 1850, max = 2018, value = 2018,
                         step = 1, sep = "", width = 400, animate = F)
             )
        ),
    fluidRow(
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
             fluidRow(column(width = 5, offset = 1, style = "padding-bottom:50px",
                             imageOutput("eu_rank")
                             ),
                      column(width = 6, style = "padding-bottom:50px",
                             mainPanel(br(),
                               h3("The Story in Europe"),
                               p("Europe has always been at the forefront of policymaking on environmental issues, in particular, resource sustainability. We thought it would be interesting to look into why this is the case. Going back in time to the 1850s, we find ourselves in the midst of the industrial revolution with the United Kingdom and other European countries leading the way in advancements in machinery and technology. However, this advancement came at the cost of the environment, leading to an exponential rise in greenhouse gas emissions. During that period, the United Kingdom held the #1 position in total carbon emissions for 21 years far outstripping the country that came in second (United States). At the same time, other European countries occupied the top 10 spots with the greatest carbon emissions.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                               br(),
                               p("From the plot on the left we can see that while the total amount of carbon emissions emitted per country increases as time passes, European countries have not been one of the top 3 countries with the greatest carbon emissions since 1975. In recent years the US, Russia, and China have taken the top 3 spots on total carbon emissions, which leads us to propose the question of whether Europe's diminished emissions have to do with policy making, such as the Montreal Protocol (1987), the Kyoto Protocol (1997), and more recently the Paris Agreement (2015) and the European Green Deal (2018).",
                                 style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                               )
                             )
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
             ),

             fluidRow(column(width = 11, offset = 1,
                             mainPanel(p("Looking at the line plot of European Union emissions overtime, we can trace a general increasing trend for over a century before a slight decline begins to happen in the 1980s. According to the European Environmental Agency, the 80s was a decade where political and social events shaped policy making on environmental sustainability. On the political front the Green Party that had made some headway in the 70s became more prominent in the European Parliment and in 1989 they won 26 seats on the Parliment. However, the 80s was also the decade when an environmental disaster struck in the form of a nuclear accident at the Chernobyl power plant in 1986. Following the catastrophe, the European Community devoted a whole section to environmental policy-making and all the members of in UN signed the Montreal Protocol promising the phase out the use of CFCs (Chlorofluorocarbons), substances that were identified to be depleting the ozone layer.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("A decade onward (1990s), various governing bodies in Europe have organized panels for discussing the issue of climate change, introduced various policies, as well as began to bridge the gap between the scientific community and policy-makers. The European Environmental Agency was established in 1994 to provide both the government and the public on information regarding the climate change. In 1997, the Kyoto protocol was signed by all members of the EU with the goal of reducing carbon emissions. 2 years later, the Amsterdam Treaty (1999) was adopted by the EU, requiring environmental protection to be part of any policy enacted by member communities.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("Overall, between 1980 - 2000 Europen countries have made various policy changes that appear to coincide with the decline in carbon emissions. This brief snapshot of historical events may provide part of the answer to whether policy making has impacted carbon emissions. In part the answer may be that policy making in its aggregate has been impactful in decreasing harmful gases. However, it would be too simplistic to say that policies have been the most important aspect that has affected change, since it is difficult to disentangle the effect of policy-making and other social changes in Europe that may have affected carbon emissions as well.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       tags$a(href = "https://www.eea.europa.eu/", "Citation", target="_blank"),
                                       br()
                                       )
                             )
                      )
    ),

    tabPanel("IPCC Reports Analysis",
             fluidRow(column(width = 12,
                             mainPanel(h1("Intergovernmental Panel on Climate Change (IPCC) Reports"))
                             )
                      ),

             fluidRow(column(width = 6, offset = 1, style = "padding-bottom:100px",
                             mainPanel(br(),
                                       h3("Introduction"),
                                       p("The fact that emissions of greenhouse gas are contributing to global warming has been advocated by many individuals throughout history, starting with renowned scientists such as Svante Arrhenius, who received a Nobel Prize for his work on CO2 emissions, to the celebrity advocate and almost-a-president Al Gore. In particular, carbon dioxide emissions is the most potent of all the greenhouse gases, accounting for more than 70% of emissions.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("We analyzed six Assessment Reports published by the Intergovernmental Panel on Climate Change (IPCC) from 1992 to 2014 with the aim to “assess the impacts of climate change on ecosystems and human communities”. The organization was established in 1988 to inspect scientific research on climate change and produce reports to provide governments with a direction when it comes to policy-making on climate issues. Over 270 authors from 67 countries contribute to these reports. Our text analysis below describes the frequency of most common words, the evolution of word frequencies through the years, as well the similarity between the 5 reports.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                                       )
                             ),
                      column(width = 5, style = "padding-bottom:100px;padding-left:0",
                             plotOutput("wordcloud")
                             )
                      ),

             #word frequencies
             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:30px",
                             mainPanel(h2("Frequently Repeated Words in the Reports")
                                       )
                             )
                      ),

             #fluidRow(column(width = 11, offset = 1, style = "paddding-bottom:50px",
                             #plotOutput("top10_words_all")
                             #)
                      #),

             fluidRow(column(width = 11, offset = 1,
                             plotOutput("top10_words_reports", 1000, height = 500)
                             )
                      ),

             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:100px",
                             mainPanel(br(),
                                       p("Unsurprisingly, the most frequent words are “climate” and “change”. When focusing on the individual reports, we can see that the most frequently appearing words in all reports are shared by the individual reports. However, there are also some report-specific words that are used frequently in each of of the different reports. In 1995, the city of Chicago was hit by a deadly heatwave that took the lives of more than 700 citizens. The word “usa” made it to the top three most used words in that year’s report. Similarly, the 2014 report is very specific with its focus on solutions rather than problems as the terms “mitigation” and “adaptation” appear in the list of top ten words.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"))
                             )
                      ),

             #evolution through time
             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:30px",
                             mainPanel(h2("Evolution of Keywords used throughout the Years")
                                       )
                             )
                      ),

             fluidRow(column(width = 12, style = "padding-bottom:50px",
                             girafeOutput("words_overtime")
                             )
                      ),

             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:100px",
                             mainPanel(br(),
                                       p("Overall the reports stress the global aspect of climate change, the increase in greenhouse gas emissions, as well as the risks and costs it poses to our environment and society. Looking at the evolution of keyword usage through time shows that the words ”use” and “level” are gaining traction in the lastest report. Possibly due to the increasing urgency to decrease emissions in order to avoid catastrophic climate change.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"))
                             )
                      ),

             #Jaccard similarity score
             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:30px",
                              mainPanel(h2("Are All the Reports the Same?")
                                        )
                       ),

             fluidRow(column(width = 6, offset = 1,
                             dataTableOutput("comparisons_table")
                             ),

                      column(width = 4, style = "padding-left:50px",
                             plotOutput("comparisons_heatmap", height = 350, width = 350)
                             )
                      ),

             fluidRow(column(width = 11, offset = 1,
                             mainPanel(br(),
                                       p("The similarity between the most frequent keywords used in the reports and the same evolution curve the words follow through the years raises the question of whether the reports are simply rephrasing the same message every single year. Calculated Jaccard similarity of documents, however, shows the opposite - while the frequent keywords might be the same for the reports, the content is not. In other words, they are worth the read!",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       tags$a(href = "https://www.ipcc.ch/reports/", "Citation", target="_blank"),
                                       br())
                             )
                      )
             )

    )


)



# Define server logic
server <- function(input, output, session) {

  #updateSelectizeInput(session, "Year_slider", choices = world_rank, server = TRUE)

  output$map <- renderLeaflet({

    #filter for Year
    world_map_filter <- world_map_c %>%
      filter(Year == input$Year_slider)

    Year_lab <- unique(world_map_filter$Year)

    #make map
    bins = c(0, 50, 100, 200, 500, 1000, 2500, 5000, 10000, 11000)

    pal = colorBin("YlOrRd", domain = world_map_filter$Emissions, bins = bins)

    labels = sprintf(
      "<strong>%s</strong>, %s<br/>
  <strong>%g MtCO₂e</strong>",
      world_map_filter$Country, world_map_filter$continent, world_map_filter$Emissions) %>%
      lapply(htmltools::HTML)


    my_title = tags$p(tags$style("p {color: dark grey; font-size:20px}"),
                      tags$b("CO₂ Emission by Country per Metric Tons in", Year_lab))

    map = leaflet(world_map_filter) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(
        fillColor = ~pal(Emissions),
        weight = 0.5,
        opacity = 1,
        color = "white",
        dashArray = "0",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2, color = "#0818A8", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      setView(lat = 40.318001, lng = -11.400387, zoom = 2)

    map %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = "CO₂ Emission per Metric Ton",
                position = "bottomright")%>%
      addControl(my_title, position = "topright")

  })


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
      labs(title = "Top 10 Countries: Total Carbon Emissions\n ",
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
    cloud <- wordcloud(words = words$word, freq = words$total, min.freq = 1,
              max.words=80, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))

    #create png
    #png("clouds.png", width = 10, height = 10, units='in', res = 300)

    #remove the margins
    par(mar = rep(0, 4))

  })


  #output$top10_words_all <- renderPlot({

    #top 10 words across all reports
    #words %>%
      #arrange(desc(total)) %>%
      #dplyr::slice(1:10) %>%
      #ggplot(aes(x = reorder(word, total), total)) +
      #geom_col(fill = "#1FA187") +
      #labs(x = NULL, y = "Word Frequency") +
      #ggtitle("Word Frequency in All IPCC Reports (1990 - 2014)") +
      #coord_flip() +
      #theme(panel.grid.major.x = element_line(color = "#D3D3D3", size = 0.3),
            #panel.grid.minor.x = element_line(colour = "#D3D3D3", size = 0.1),
            #panel.grid.major.y = element_blank(),
            #panel.border = element_blank(),
            #panel.background = element_blank(),
            #axis.ticks.x = element_blank(),
            #axis.ticks.y = element_blank(),
            #axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 15),
            #axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
            #axis.title.y = element_text(family = "Helvetica", color = "#36454F", size = 15),
            #axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
            #plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
            #plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10))

  #})


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
    scale_fill_manual(values = c("1" = "#1FA187", "0" = "#FF7518"),
                      name = "Words Between Documents", labels = c("Shared", "Unique")) +
    #geom_bar(stat = "identity", show.legend = FALSE, fill = "#1FA187") +
    facet_wrap(~ variable, scales = "free") +
    xlab("Words") +
    ylab("Frequency") +
    theme_bw() +
    coord_flip() +
    theme(legend.position = "bottom",
          legend.key.height = unit(1, "mm"),
          legend.key.width = unit(2, "mm"),
          legend.key.size = unit(2, "mm"),
          legend.key = element_rect(fill = "transparent", color = NA),
          legend.title = element_text(size = 10, color = "#36454F"),
          legend.text = element_text(size = 8, color = "#36454F"),
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


output$words_overtime <- renderGirafe({

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
  words_time <- ggplot(melted_time, aes(x =  variable, y = value, color = word)) +
    geom_line() +
    geom_point_interactive(size = 2,
                           aes(tooltip = glue("{word}\nYear: {variable}\nFrequency: {value}"))) +
    scale_color_manual(values = met.brewer("Peru1", 10), breaks = (word_order_legend)) +
    scale_x_continuous(breaks = c(1992, 1995, 2001, 2007, 2014)) +
    ylim(c(0, 3500)) +
    labs(title = "Evolution of Keyword Frequencies from 1992 to 2014",
         subtitle = "Hover over a point for more detailed information",
         x = "Year of the Published Report",
         y = "Word Frequency",
         color = "Keywords") +
    #guides(col = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
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
          plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 10),
          plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10))

  #make interactive with ggiraph
  girafe(ggobj = words_time,
         width_svg = 9, height_svg = 5,
         bg = "transparent",
         options = list(
           opts_tooltip(
             opacity = 0.8, use_fill = T, use_stroke = F,
             css = "padding:5pt;fontfamily: Helvetica;color:white"
           ),
           opts_hover_inv(css = "opacity:0.5;"),
           opts_hover(css = "fill:#EDEADE")
           )
    )

})


output$comparisons_table <- renderDataTable({

  #round the numbers
  comparisons <- comparisons %>%
    mutate_if(is.numeric, round, digits = 5)

  #make matrix
  comparisons_m <- as.matrix(comparisons)

  #code for creating alternating rows
  #code from Stéphane Laurent
  rowCallback <- c(
    "function(row, data, num, index){",
    "  var $row = $(row);",
    "  if($row.hasClass('even')){",
    "    $row.css('background-color', '#F0FFFF');",
    "    $row.hover(function(){",
    "      $(this).css('background-color', '#96DED1');",
    "     }, function(){",
    "      $(this).css('background-color', '#F0FFFF');",
    "     }",
    "    );",
    "  }else{",
    "    $row.css('background-color', 'white');",
    "    $row.hover(function(){",
    "      $(this).css('background-color', '#96DED1');",
    "     }, function(){",
    "      $(this).css('background-color', 'white');",
    "     }",
    "    );",
    "  }",
    "}"
  )

  #datatable
  datatable(comparisons_m,
            options = list(initComplete = JS("function(settings, json) {",
                                             "$('body').css({'font-family': 'Arial'});",
                                             "$(this.api().table().header()).css({'background-color': '#1FA187', 'color': 'white'});",
                                             "$('table.dataTable tr.odd').css('background-color', 'white');",
                                             "$('table.dataTable tr.even').css('background-color', '#F0FFFF');",
                                             "}"),
                           rowCallback = JS(rowCallback),
                           dom = 't'
            ),
            colnames = c('IPCC 1992', 'IPCC 1995', 'IPCC 2001', 'IPCC 2007', 'IPCC 2014'),
            rownames = c('IPCC 1992', 'IPCC 1995', 'IPCC 2001', 'IPCC 2007', 'IPCC 2014'),
            caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                              htmltools::HTML("Table 1: <b><i>Jaccard Similarity Score</i></b>")
              )
            ) %>%
    formatStyle(" ", color = "white", backgroundColor = "#1FA187", textAlign = "center", fontWeight = "bold")

})


output$comparisons_heatmap <- renderPlot({

  #restructuring data
  comparisons_df <- as.data.frame(comparisons)
  comparisons_df <- tibble::rownames_to_column(comparisons_df, "report1")
  comparisons_long <- gather(comparisons_df, report, jaccard, ipcc_1992:ipcc_2014, factor_key = TRUE)

  #rename rows
  comparisons_long <- comparisons_long %>%
    mutate(report1 = case_when(report1 == "1" ~ "1992",
                               report1 == "2" ~ "1995",
                               report1 == "3" ~ "2001",
                               report1 == "4" ~ "2007",
                               report1 == "5" ~ "2014"))

  #coerce NA to 0
  comparisons_long[is.na(comparisons_long)] <- 0

  #heatmap
  ggplot(comparisons_long, aes(report, report1)) +
    coord_flip() +
    geom_tile(aes(fill = jaccard), color = "white") +
    scale_fill_gradient(low = "white", high = "#E35335", space = "Lab") +
    scale_x_discrete(labels = c("1992", "1995", "2001", "2007", "2014")) +
    theme(legend.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 10),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          #panel.grid.major.y = element_line(color = "#D3D3D3", size = 0.3),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 15),
          axis.text.x = element_text(family = "Helvetica", color = "#36454F", size = 10),
          axis.title.y = element_text(family = "Helvetica", color = "#36454F", size = 15),
          axis.text.y = element_text(family = "Helvetica", color = "#36454F", size = 10),
          plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 20),
          plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 15),
          plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10)) +
  labs(title = "Jaccard Similarity Score",
       subtitle = "for all IPCC Reports",
       fill = "Jaccard \nScore",
       x = "",
       y = "")


})


}

# Run the application
shinyApp(ui = ui, server = server)
