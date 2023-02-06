"
Shiny app v0.2

Author: Pedro Alencar

14.01.2023
"

#%% Load packages --------------------------------------------------------------
library(shiny) # comment out when pushing to shinyapps.io
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(shinythemes)
library(bslib)
library(plotly)
library(leaflet)
library(RColorBrewer)
library(fst)
library(tibble)
library(tibbletime)
library(lubridate)
library(sf)


#%% Load datasets --------------------------------------------------------------
# df <- fst::read_fst("./pre-processing/crop_bb_complete.bin")
df <- data.table::fread("crop_bb_complete.csv")


borders <- sf::read_sf("./GIS/bbr.shp")
borders$GEN <-
  c(
    "Berlin",
    "Brandenburg an der Havel (KfS)",
    "Cottbus (KfS)",
    "Frankfurt (Oder) (KfS)",
    "Potsdam (KfS)",
    "Barnim",
    "Dahme-Spreewald",
    "Elbe-Elster",
    "Havelland",
    "Maerkisch-Oderland",
    "Oberhavel",
    "Oberspreewald-Lausitz",
    "Oder-Spree",
    "Ostprignitz-Ruppin",
    "Potsdam-Mittelmark",
    "Prignitz",
    "Spree-Neiße",
    "Teltow-Flaeming",
    "Uckermark"
  )

names(borders)[names(borders) == "GEN"] <- "Landkreis"

# menu choices
landkreise <- unique(df$Landkreis)
crops <- unique(df$Crop)
varieties <- unique(filter(df, Crop == "Getreide")$Variety)

#palette
pal_1 <- brewer.pal(n = 7, name = 'YlOrRd')[c(7,4,2)]


# if (interactive()) {
#%% User interface -------------------------------------------------------------
ui <- fluidPage(

  theme = bs_theme(version = 4, bootswatch = "materia"),



  title = "Crops in Berlin-Brandenburg",

  titlePanel("Crops in Berlin-Brandenburg"),

  # inputs and simple map ----
  fluidRow(
    column(
      4,
      h4("Select map parameters:"),
      # add menus for selections ----
      selectInput(
        "crop_",
        "Choose crop type",
        choices = crops,
        selected = "Getreide"
      ),
      selectInput(
        "variety_",
        "Choose crop variety",
        choices = varieties,
        selected = "Getreide insgesamt"
      ),
      selectInput(
        "landkreis_",
        "Choose Landkreis",
        choices = landkreise,
        selected = "Barnim"
      ),
      sliderInput(
        "year_",
        "Choose year",
        min = 2006,
        max = 2021,
        value = c(2016)
      ),
    ),
    # show map with states and production values ----
    column(8, offset = 0, #
           leafletOutput("Map1")),
  ),

  # production time series ----
  fluidRow(column(12,
                  # br(),
                  hr(), # horizontal line
                  # br(),
                  h4("Time series"),
                  br(),
                  plotlyOutput("graph1"),
                  plotlyOutput("graph2"),
                  ),
  ),

  # notes ----

  wellPanel(fluidRow(column(12,
                  h4("About this app"),
                  h5("All data used was provided by the Amt für Statistik Berlin-Brandenburg (2022)
                     under CC-BY license, allowing sharing and editing of its contents. This tool aims to facilitate crop data communication, visualization, and
                     interpretation. It was elaborated in the context of the project CliWac (Climate and
                     Water under Change)",
                     tags$br(),
                     tags$br(),
                     "Author:", tags$a(href="https://www.tu.berlin/oekohydro/team/pedro-alencar/", "Pedro Alencar"),
                     tags$a(href="https://orcid.org/0000-0001-6221-8580", "(0000-0001-6221-8580)"),
                     tags$br(),
                     "Berlin, 14.01.2023"
                     ),
                  br(),
                  h4("Download the data!"),
                  downloadButton("downloadData", "Download"),
                  ),
           )
  )
)

#%% The server: processing -----------------------------------------------------
server <- function(input, output, session) {

  # waits for changes in crop selection to update variety list
  observeEvent(input$crop_, label = "update_variety_list",{
    crop <- input$crop_

    varieties <- df %>%
      dplyr::filter(Crop == crop) %>%
      select(Variety) %>%
      unique() %>% unlist() %>%
      data.frame() %>%
      magrittr::set_names("var")

    updateSelectInput(
      session,
      "variety_",
      label = paste0("Choose crop variety - ", crop),
      choices = varieties,
      selected = "Getreide insgesamt"
    )
  })

  # grabs input and creates map
  observe(label = "map_desing",{
    crop <- input$crop_
    variety <- input$variety_
    year <- input$year_
    landkreis <- input$landkreis_

    # get selection production
    prod <- df %>% filter(Crop == input$crop_,
                          Year == input$year_,
                          Variety == input$variety_)

    borders_plot <-
      suppressMessages(left_join(borders, prod)) #update borders, keep quiet
    border_select <-
      borders_plot %>% filter(Landkreis == landkreis)


    # get leaflet scale
    bins <-
      seq(0, ceiling(max(c(
        borders_plot$P_per_A, 0
      ), na.rm = T)), length.out = 5)
    pal <-
      colorBin("YlOrRd", domain = borders_plot$P_per_A, bins = bins)

    # define label features for leaflet map
    output$Map1 <- renderLeaflet({
      labels <- sprintf(
        "<strong>%s</strong><br/>%g Mg.ha<sup> -1</sup><br/>%g ha<br/>%g Mg",
        borders_plot$Landkreis,
        borders_plot$P_per_A,
        borders_plot$A_ha,
        borders_plot$P_t
      ) %>% lapply(htmltools::HTML)

      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = border_select,
          weight = 3,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          dashArray = "1"
        ) %>%
        addPolygons(
          data = borders_plot,
          fillColor = ~ pal(P_per_A), # show production per area
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          label = labels, # in labels, show all production fetures
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    })

  })

  observe(label = "plots", {

    crop <- input$crop_
    variety <- input$variety_
    year <- input$year_
    landkreis <- input$landkreis_

    # get selection production
    prod_series <- df %>% filter(Crop == crop,
                          Variety == variety,
                          Landkreis == landkreis)

    output$graph1 <- renderPlotly({#production - total and relative

      plot_ly(prod_series, x = ~as.factor(Year)) %>%
        add_lines(
          y = ~P_per_A,
          name = "Prod. per Area",
          yaxis = "y1",
          line = list(color = pal_1[1]),
          marker = list(color = pal_1[1])
        )  %>%
        add_lines(
          y = ~P_t,
          name = "Total prod.",
          yaxis = "y2",
          line = list(color = pal_1[2]),
          marker = list(color = pal_1[2])
        )  %>%
        layout(
          title = paste0("Crop yield of ",crop, " (", variety, ") in ", landkreis),
          xaxis = list(
            title = "Year",
            domain = c(0, 0.95),
            # type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "Produciton (Mg/ha)",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            nticks = 6,
            rangemode="tozero",
            scaleanchor='y', scaleratio=1, constraintoward='bottom', secondary_y=T,
            # range = c(-10, 35),
            dtick = 10
          ),
          yaxis2 = list(
            title = "Production (Mg)",
            side = "right",
            color = "black",
            overlaying = "y",
            anchor = "free",
            position = 0.95,
            nticks =6,
            rangemode="tozero",
            scaleanchor='y2',scaleratio=1, constraintoward='bottom', secondary_y=F,
            dtick = 20000
          ),
          showlegend = T
        )
    })

    output$graph2 <- renderPlotly({#production and area

      plot_ly(prod_series, x = ~as.factor(Year)) %>%
        add_lines(
          y = ~P_per_A,
          name = "Prod. per Area",
          yaxis = "y1",
          line = list(color = pal_1[1]),
          marker = list(color = pal_1[1])
        )  %>%
        add_lines(
          y = ~A_ha,
          name = "Total area",
          yaxis = "y2",
          line = list(color = pal_1[3]),
          marker = list(color = pal_1[3])
        )  %>%
        layout(
          # title = paste0("Crop yield of ",crop, " (", variety, ") in ", landkreis),
          xaxis = list(
            title = "Year",
            domain = c(0, 0.95),
            # type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "Produciton (Mg/ha)",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            nticks = 6,
            rangemode="tozero",
            scaleanchor='y', scaleratio=1, constraintoward='bottom', secondary_y=T,
            # range = c(-10, 35),
            dtick = 10
          ),
          yaxis2 = list(
            title = "Area (ha)",
            side = "right",
            color = "black",
            overlaying = "y",
            anchor = "free",
            position = 0.95,
            nticks =6,
            rangemode="tozero",
            scaleanchor='y2',scaleratio=1, constraintoward='bottom', secondary_y=F,
            dtick = 10000
          ),
          showlegend = T
        )
    })

    # Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(landkreis,crop,variety, '.csv', sep = "_")
      },
      content = function(file) {
        write.csv(prod_series, file)
      }
    )

  })

}

#%% Build app ------------------------------------------------------------------
shinyApp(ui, server)
# }
