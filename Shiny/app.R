
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
library(rsconnect)

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
words <- read.csv("df_words_frequency.csv", header = T, fileEncoding ="latin1")
#load document comparison data
comparisons <- read.csv("comparisons.csv", sep = ",")

###data organizing###
#create dataframe long: melted_d
melted_d <- melt(words, id = "word", measure.vars = 2:ncol(words))

#changing the labels names
melted_d$variable <- factor(melted_d$variable,
                            levels = c("ipcc_90_92.pdf", "ipcc_95.pdf", "ipcc_01.pdf", "ipcc_07.pdf", "ipcc_14.pdf","total"),
                            labels = c("IPCC 1992 Report", "IPCC 1995 Report", "IPCC 2001 Report", "IPCC 2007 Report", "IPCC 2014 Report","All Reports"))

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
ui <- navbarPage(
  #CSS
  tags$head(
    tags$style(htmltools::HTML("
    h1 { font-family: 'Tahoma'; color: #0047AB; font-weight: 'bold'; }
    h2 { font-family: 'Tahoma'; color: #4682B4; font-weight: 'bold'; }
    h3 { font-family: 'Tahoma'; color: #4682B4; font-weight: 'bold'; }
    body { background-color: 'white'; }
    .navbar { background-color: #96DED1 }
    .navbar-default .navbar-brand { font-family: 'Tahoma'; color: #0047AB; }
    .navbar-dropdown { font-family: Arial; font-size: 13px; color: #4682B4; }
    .filters { margin: 0px auto; background-color: #96DED1; }
    .shiny-input-container { width:75% !important; background-color: #96DED1; padding-top: 0px; padding-left: 10px;padding-right: 10px; padding-bottom: 10px; border-radius: 20px; }
    .js-irs-0 .irs-grid-text { color: #0047AB }
    .irs-grid-pol:nth-of-type(odd) { background: #0047AB }
    .irs-grid-pol:nth-of-type(even) { background: #0047AB }
                               ")
               )
  ),

  title = "Carbon Emissions", position = "fixed-top",

  tabPanel("Intro", style = "padding-top:75px",
           fluidRow(column(width = 11, offset = 1,
                           h1("Examining Carbon Emissions and Environmental Policy")
                           )
                    ),

           fluidRow(column(width = 11, offset = 1, style = "padding-top:30px",
                           mainPanel(h2("Introduction"),
                                     br(),
                                     p("Climate chnage has been a hot topic for many years, generating much debate and governmental policy, not least because it is an issue that affects everyone that inhabits this world. In 2021, the clock in Union Square, New York, no longer counted down the hours left in a day, it became a doomsday countdown. Specifically, the clock shows the current percentage of global energy generated by renewable resources, using data retrived from Our World in Data project.",
                                       style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                     br(),
                                     p("Taking inspiration from the use of data to raise awareness about climate change in a public space, we decided to use data to investigate the question of whether policy-making has had an impact on the environment, in particular, carbon emissions. The fact that emissions of greenhouse gas are contributing to global warming has been advocated by many individuals throughout history, starting with renowned scientists such as Svante Arrhenius, who received a Nobel Prize for his work on CO2 emissions, to the celebrity advocate and almost-a-president Al Gore. In particular, carbon dioxide emissions is the most potent of all the greenhouse gases, accounting for more than 70% of emissions.",
                                       style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                     tags$a(href = "https://www.timeout.com/newyork/news/that-giant-clock-in-union-square-now-has-a-fascinating-new-feature-042021", "Citation", target ="_blank"),
                                     br(),
                                     p("*This website was meant to be viewed in fullscreen. If you would like to view the story as we have intended, you can click through the tabs on the top in its current order. Otherwise explore the website as you please.",
                                       style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify; font-style: italic; padding-top:40px; color: #1434A4")
                                     )
                           )
                    ),

           fluidRow(column(width = 11, offset = 1, style = "padding-top:75px",
                           h1("About the Creators"),
                           p("Feel free to reach out to us if you have any questions!",
                             style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                           )
                    ),

           fluidRow(column(width = 4, style = "padding-bottom:50px",
                           h2("Cho Laam Yuen",
                              style = "text-align:center"),
                           img(src = "Cly.png", align = "center", height = "320px", width = "325px", style = "display: inline-block; margin-left: 15%; margin-right: auto; padding-bottom:20px"),
                           br(),
                           p("Contact: cy2617@columbia.edu",
                             style = "font-family: 'Helvetica'; fontsi16pt; text-align:center; color: #1434A4")
                           ),
                    column(width = 4, style = "padding-bottom:50px",
                           h2("Ludmila Filipova",
                              style = "text-align:center"),
                           img(src = "Ludmila.png", align = "left", height = "320px", width = "325px", style = "display: inline-block; margin-left: 15%; margin-right: 15%; padding-bottom:20px"),
                           br(),
                           p("Contact: lf2719@columbia.edu",
                             style = "font-family: 'Helvetica'; fontsi16pt; text-align:center; color: #1434A4")
                           ),
                    column(width = 4, style = "padding-bottom:50px",
                           h2("David Liang",
                              style = "text-align:center"),
                           br(),
                           p("Contact: Zl3076@columbia.edu",
                             style = "font-family: 'Helvetica'; fontsi16pt; text-align:center; color: #1434A4; padding-top:20px"))
                    )
  ),

                 tabPanel("Overview",

    #create a grid on website
    fluidRow(column(width = 12, style = "padding-top:50px",
                    leafletOutput("map", width = "100%", height = 670)
                    )
             ),

    fluidRow(
      column(width = 5, offset = 4,
             h3("Year"),
             #slider for Year
             chooseSliderSkin("Round", "#0047AB"),
             setSliderColor("#0047AB", c(1)), #set how many sliders with this specific color
             sliderInput("Year_slider", label = h4(""),
                         min = 1850, max = 2018, value = 2018,
                         step = 1, sep = "", width = 400, animate = F),
             p("This slider controls the map and 2 other graphs below",
               style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify; font-style: italic")
             )
        ),

    fluidRow(column(width = 10, offset = 1, style = "padding-right:100px; padding-top:30px; padding-bottom:10px",
                    h2("State of the World in Carbon Emissions"),
                    p("Overall, CO2 emissions have exponentially increased in the past century. Initially, the European Nations had the highest CO2 emission, this was expected as the European nations were the first to enter the Industrial Revolution, however, as time moved closer to the present, countries like the United States, China and Russia became the countries with the highest CO2 emission. This is due to the mass population as well as the strict environmental policies in more developed nations, forcing firms to move their manufacturing activities to less developed countries such as China. Surprisingly, despite the United States having relatively strict envrionmental policies, it is still one of the highest CO2 emitter in the world.",
                      style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                      br()
                    )
             ),

    fluidRow(
        column(width = 4, style = "padding-bottom:0px;padding-right:150px",
             plotOutput("rank", height = 500, width = 900,
                        #brush = brushOpts("rank_brush", resetOnNew = T),
                        #hover = hoverOpts("rank_hover", nullOutside = T, delay = 0)
                        #click = clickOpts("rank_click", clip = T)
                        )
             ),

        column(width = 4, offset = 4, style = "padding-bottom:0px;padding-left:50px",
               h4("Sectors",
                  style = "font-family:'Helvetica'; font-weight: bold, font-size:30px"),
               p("\nHover over a country for carbon emissions breakdown by sector",
                 style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
               br(),
               girafeOutput("sector")
               )
        ),

                 ),


    tabPanel("Story",
             fluidRow(column(width = 5, offset = 1, style = "padding-top:75px; padding-bottom:50px",
                             img(src = "EU_rank.gif", align = "left")
                             ),
                      column(width = 6, style = "padding-top:75px; padding-bottom:50px",
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
                             mainPanel(h2("Environmental Policies in Europe between 1980-1999"),
                                       p("Looking at the line plot of European Union emissions overtime, we can trace a general increasing trend for over a century before a slight decline begins to happen in the 1980s. According to the European Environmental Agency, the 80s was a decade where political and social events shaped policy making on environmental sustainability. On the political front the Green Party that had made some headway in the 70s became more prominent in the European Parliment and in 1989 they won 26 seats on the Parliment. However, the 80s was also the decade when an environmental disaster struck in the form of a nuclear accident at the Chernobyl power plant in 1986. Following the catastrophe, the European Community devoted a whole section to environmental policy-making and all members of the UN signed the Montreal Protocol promising the phase out the use of CFCs (Chlorofluorocarbons), substances that were identified to be depleting the ozone layer.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("A decade onward (1990s), various governing bodies in Europe organized panels for discussing the issue of climate change, introduced various policies, as well as began to bridge the gap between the scientific community and policy-makers. The European Environmental Agency was established in 1994 to provide both the government and the public with information regarding climate change. In 1997, the Kyoto protocol was signed by all members of the EU with the goal of reducing carbon emissions. 2 years later, the Amsterdam Treaty (1999) was adopted by the EU, requiring environmental protection to be part of any policy enacted by member communities.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("Overall, between 1980-1999 Europen countries have made various policy changes that appear to coincide with the decline in carbon emissions. This brief snapshot of historical events may provide part of the answer to whether policy making has impacted carbon emissions. In part the answer may be that policy making in its aggregate has been impactful in decreasing harmful gases. However, it would be too simplistic to say that policies have been the most important aspect that has affected change, since it is difficult to disentangle the effect of policy-making and other social changes in Europe that may have affected carbon emissions as well.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       tags$a(href = "https://www.eea.europa.eu/", "Citation", target="_blank"),
                                       br()
                                       )
                             )
                      )
    ),

    tabPanel("IPCC Reports Analysis", style = "padding-top:75px",
             fluidRow(column(width = 12,
                             mainPanel(h1("Intergovernmental Panel on Climate Change (IPCC) Reports"))
                             )
                      ),

             fluidRow(column(width = 6, offset = 1, style = "padding-bottom:100px",
                             mainPanel(br(),
                                       h3("Introduction"),
                                       p("In addition to tracking carbon emissions throughout time and highlighting certain policy changes in Europe, we were also interested in exploring whether scientific reports that influenced governmental policy-making changed throughout time as well. We hypothesized that the big topics regarding climate change such as the goal to reduce global temperatures and to decrease greenhouse gas emissions would remain largely the same. However, we were curious about whether more pressing or specific issues would differ year by year.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("To answer these questions we analyzed six Assessment Reports published by the Intergovernmental Panel on Climate Change (IPCC) from 1992 to 2014 with the aim to “assess the impacts of climate change on ecosystems and human communities”. The organization was established in 1988 to inspect scientific research on climate change and produce reports to provide governments with a direction when it comes to policy-making on climate issues. Over 270 authors from 67 countries contribute to these reports. Our text analysis below describes the frequency of most common words, the evolution of word frequencies through the years, as well the similarity between the 5 reports.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                                       )
                             ),
                      column(width = 5, style = "padding-bottom:100px;padding-left:0",
                             img(src = "wordcloud.png", align = "center")
                             )
                      ),

             #word frequencies
             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:30px",
                             mainPanel(h2("'Climate Change' Dominates the Reports"),
                                       p("Unsurprisingly, the most frequent words are “climate” and “change”. When focusing on the individual reports, we can see that the most frequently appearing words in all reports are shared by the individual reports. However, there are also some report-specific words that are used frequently in each of of the different reports. In 1995, the city of Chicago was hit by a deadly heatwave that took the lives of more than 700 citizens. The word “usa” made it to the top three most used words in that year’s report. Similarly, the 2014 report is very specific with its focus on solutions rather than problems as the terms “mitigation” and “adaptation” appear in the list of top ten words.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                                       )
                             )
                      ),

             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:100px",
                             plotOutput("top10_words_reports", 1000, height = 500)
                             )
                      ),


             #Evolution through time
             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:30px",
                             mainPanel(h2("No New Trends in use of Keywords"),
                                       p("Overall the reports stress the global aspect of climate change, the increase in greenhouse gas emissions, as well as the risks and costs it poses to our environment and society. Looking at the evolution of keyword usage through time shows that the words ”use” and “level” are gaining traction in the lastest report. Possibly due to the increasing urgency to decrease emissions in order to avoid catastrophic climate change.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                                       )
                             )
                      ),

             fluidRow(column(width = 12, style = "padding-bottom:100px",
                             girafeOutput("words_overtime")
                             )
                      ),


             #Jaccard similarity score
             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:15px",
                              mainPanel(h2("Are All the Reports the Same?"),
                                        p("The similarity between the most frequent keywords used in the reports and the same evolution curve the words follow through the years raises the question of whether the reports are simply rephrasing the same message every single year. Calculated Jaccard similarity of documents, however, shows the opposite - while the frequent keywords might be the same for the reports, the content is not. In other words, they are worth the read!",
                                          style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                        tags$a(href = "https://www.ipcc.ch/reports/", "Citation", target="_blank"),
                                        br())
                                        )
                       ),

             fluidRow(column(width = 11, offset = 1, style = "padding-bottom:20px",
                             mainPanel(h3("About the Jaccard Similarity Index"),
                                       p("The Jaccard similarity index compares corpus documents, measuring the similarity between them. It can be deployed on numeric data but it is also a popular natural language processing method, its ability to detect context and common words can help determine the level of document similarity. In reporting and publishing, reusing text is quite common - topics and phrases overlap and sometimes this overlap is almost too precise. As such, there is value in using the Jaccard score to check for similarity.",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify"),
                                       br(),
                                       p("Calculated Jaccard similarity score identified the highest similarity between the 2014 and 2007 documents, as well as the 2007 and 2001 documents. This could mean that whole sentences were reused or the documents referred to the same topics, making it impossible to avoid overlaps. The score, however, remains very low, 0.01578 means that an approximately 1.6% similarity was detected - a very small number given the size ofs the documents!",
                                         style = "font-family: 'Helvetica'; fontsi16pt; text-align:justify")
                                       )
                             )
             ),

             fluidRow(column(width = 6, offset = 1,
                             dataTableOutput("comparisons_table")
                             ),

             column(width = 4, style = "padding-left:50px; padding-bottom:20px",
                    plotOutput("comparisons_heatmap", height = 350, width = 350)
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

    #set labels and details
    bins = c(0, 50, 100, 200, 500, 1000, 2500, 5000, 10000, 11000)

    pal = colorBin("YlOrRd", domain = world_map_filter$Emissions, bins = bins)

    labels = sprintf(
      "<strong>%s</strong>, %s<br/>
  <strong>%g MtCO₂e</strong>",
      world_map_filter$Country, world_map_filter$continent, world_map_filter$Emissions) %>%
      lapply(htmltools::HTML)


    my_title = tags$p(tags$b("Total CO₂ Emission per Metric Tons by Country in", Year_lab),
                      style = "color: dark grey; font-size:20px")

    #make map
    map = leaflet(world_map_filter, options = leafletOptions(minZoom = 2)) %>%
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
      setView(lat = 40.318001, lng = -11.400387, zoom = 2) %>%
      setMaxBounds(lng1 = -180, lat1 = -180, lng2 = 180, lat2 = 180)

    map %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = "CO₂ Emission per Metric Ton",
                position = "bottomleft") %>%
      addControl(my_title, position = "topright")

  }) %>%
    bindCache(input$Year_slider)


  #create bar chart of world ranking carbon emissions
  output$rank <- renderPlot({

    #filter for YEAR
    Filtered <- world_rank %>%
      filter(Year == input$Year_slider)

    Year_emi <- unique(Filtered$Year)

    #create a title
    title <- glue("Top 10 Countries: Total Carbon Emissions in ", Year_emi, "\n ")

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
            plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 25),
            axis.title.x = element_text(family = "Helvetica", color = "#36454F", size = 12),
            axis.text.x = element_text(family = "Helvetica", color = "#36454F"),
            axis.title.y = element_text(family = "Helvetica", color = "#36454F"),
            axis.text.y = element_blank(),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 12),
            plot.margin = margin(2, 2, 2, 4, "cm")) +
      labs(title = title,
           caption = "\nData from the Postdam Institute for Climate Impact Research",
           x = "",
           y = "Carbon Emissions per Metric Ton")

  }) %>%
    bindCache(input$Year_slider)


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
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            #plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 25),
            #plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 10),
            plot.background = element_rect(fill = "transparent", colour = NA),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 8),
            plot.margin = margin(1, 1, 1, 3, "cm"))
      #labs(title = "Sectors",
           #subtitle = "Hover over a country for carbon emissions breakdown by sector\n ")

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

  }) %>%
    bindCache(input$Year_slider)





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
            plot.subtitle = element_text(family = "Helvetica", color = "#36454F", size = 15),
            plot.caption = element_text(family = "Helvetica", color = "#36454F", face = "italic", size = 10)) +
      labs(title = "Carbon Emissions Overtime in Europe\n ",
           subtitle = "Hover over the lines for more specific information",
           caption = "\nData from the Postdam Institute for Climate Impact Research",
           x = "Year",
           y = "Annual Carbon Emissions (Metric Tons)") +
      geom_segment(aes(x = 2015 , y = 0, xend = 2015, yend = 1190), color = "#50C878") +
      ggplot2::annotate(geom = "text", x = 2015, y = 1260, label = "Paris \nAgreement", size = 2.5,
               family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1987, y = 0, xend = 1987, yend = 1190), color = "#50C878") +
      ggplot2::annotate(geom = "text", x = 1987, y = 1260, label = "Montreal \nProtocol", size = 2.5,
               family = "Helvetica", color = "#36454F") +
      geom_segment(aes(x = 1997, y = 0, xend = 1997, yend = 1190),  color = "#50C878") +
      ggplot2::annotate(geom = "text", x = 1997, y = 1260, label = "Kyoto \nProtocol", size = 2.5,
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



output$top10_words_reports <- renderPlot({

  #need to fist filter for top 10 of all the reports
  # Plot Data Frame
  pd <- melted_d %>%
    group_by(variable) %>%
    top_n(10, value) %>%
    ungroup() %>%
    arrange(variable, word) %>%
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
          plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 25),
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
          legend.title = element_text(family = "Helvetica", color = "#36454F", size = 10),
          legend.text = element_text(family = "Helvetica", color = "#36454F", size = 8),
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
          plot.title = element_text(family = "Helvetica", color = "#36454F", face = "bold", size = 25),
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

