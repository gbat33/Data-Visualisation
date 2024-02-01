#install.packages("randomForest")
install.packages("shinythemes")
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(ggplot2)
library(corrplot)
library(randomForest)
library(shinythemes)
library(xgboost)
library(readxl)
library(reshape2)
Dataviz <- read_excel("C:/Users/grace.batibuka/OneDrive - ADDACTIS GROUP/Documents/Data-Visualisation/Dataviz.xlsx")


models <- list()

for (ville in unique(Dataviz$town)) {
  data_ville <- Dataviz %>% filter(town == ville)
  
  model <- randomForest(realSum ~ room_type + person_capacity + metro_dist + guest_satisfaction_overall, 
                        data = data_ville)
  models[[ville]] <- model
}

saveRDS(models, "models_rf.rds")
# UI
ui <- dashboardPage(
  dashboardHeader(title = NULL),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .centered-image {
          text-align: center;
          display: block;
          margin: 0 auto
          
          width: 50%;
        }
        .custom-box-title .box-header > h3 {font-size: 10px; font-weight: normal; }
        .tab-content > .tab-pane[presentation] { background-color: #e74c3c; }
        .tab-content > .tab-pane[prix] { background-color: #3498db; }
        .tab-content > .tab-pane[analyse] { background-color: #2ecc71; }
        .tab-content > .tab-pane[cartographie] { background-color: #f1c40f; }
        .tab-content > .tab-pane[prediction] { background-color: #9b59b6; }
      "))
    ),
    navbarPage(theme = shinytheme("flatly"),
              tabPanel("Présentation et analyse des résultats", value = "presentation",
                        fluidRow(
                          box(
                            title = "Recommandations d'appartement Airbnb",
                            width = 12,
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE, 
                            div(class = "custom-box-title", 
                             )
                          )
                        )
                        
               ),
              
              tabPanel("Présentation et analyse des résultats", value = "presentation",
                       box(
                         width = 12,
                         div(class = "centered-image",
                             img(src = "https://th.bing.com/th/id/OIP.b0e_MskjChPtU5qTZdxF-gHaE6?rs=1&pid=ImgDetMain", style = "width: 200px; height: auto;")
                         ),
                             br(),
                         br(),
                       p("Cette application Shiny est dédiée à l'analyse des données d'appartements Airbnb. Elle vise à fournir des insights pertinents pour les utilisateurs cherchant à louer des appartements, en prenant en compte divers paramètres tels que les prix, la localisation, les évaluations des clients, et plus encore."),
                        p("Naviguez à travers les différentes pages pour explorer des analyses détaillées, des visualisations interactives et des recommandations spécifiques d'appartements."),
                       br(),
                       h3("Présentation des données"),
                       
                       p(HTML("Nos données sont issues de <a href='https://www.kaggle.com/datasets/thedevastator/airbnb-prices-in-european-cities' target='_blank'>Kaggle Dataset</a> et présentent les prix et caractéristiques des appartements loués sur Airbnb dans 10 grandes villes européennes. Les prix sont calculés sur base de 2 nuits dans les appartements. Un travail de concaténation et de restructuration des données a été effectué avant les analyses. En l'occurrence, on tient compte uniquement des prix inférieurs à 1000€ puisqu'on considère que des prix au-delà fausserait nos représentation et plus tard notre modèle.")
                       ),
                         br(),
                       h3("Analyse des prix"),
                       p("Sur cette page, au choix de l'utilisateur l'histogrammes des prix de chaque ville est représenté. On remarquera après analyse de ces histogrammes, que le fait de louer un appartement pendant les weekend n'a globalement pas d'effet sur le prix sauf à Paris et à Londres. Le graphique en toile d'arraignée a permit d'étudier la relation entre le prix et la variable explicative type d'appartement. Il en ressort que les prix augmentent pour une location d'un appartement ou une maison en entier, baisse relativement lorsqu'il s'agit d'une location de chambre privée, et baisse encore plus pour les chambres partagés."),
                       br(),
                       h3("Analyse avancée"),
                       p("Le graphique en 3 dimensions nous permet d'identifier les relations entre le prix, la capacité d'accueil, et la distance au métro. On conclut un relation globalement positive entre le prix et la capacité d'accueil, dans le sens où, lorsque cette dernière variable augmente , le prix augmente et une relation négative entre le prix et la distance au métro. Une carte des corrélation a permit ensuite de mieux visualiser ces relations même si les corrélations ne semblent pas très fortes."),
                       br(),
                       h3("Cartographie"),
                       p("Après les choix de l'utilisateur, apparaissent les appartements qui correspondent à ces choix suivis de quelques détails : la note de satisfaction, le prix et la distance au métro (il faut cliquer sur les points pour visualiser ces informations). Les points en couleur verte représentent les appartements dont la note de satisfaction est supérieure à 90 et les autres sont en rouge. On a choisi  de faire la recherche sur une seule ville à la fois pour que la visualisation soit bien détaillée et précise."),
                       br(),
                       h3("Prédiction"),
                       p("Toutes les visualisations précédentes concernent les données actuelles disponibles. On a donc souhaiter que l'utilisation puisse avoir une estimation rapide du prix de son choix d'appartement sans passer par la cartographie. Pour cela on implémente un modèle de régression de forêt aléatoire qui nous permet d'estimer le prix en fonction du choix de la ville, de la capacité d'accueil, de la distance au métro et de la note de satisfaction.")
                       
                       )
              ),
      
      # Deuxième page : Analyse des Prix
      tabPanel("Analyse des prix", value = "prix",
              fluidRow(
                box(
                  title = "Histogramme des Prix des Appartements",
                  width = 12,
                  selectInput("town", "Choisir une ville", 
                              choices = unique(Dataviz$town)),
                  plotlyOutput("histogram_prix")
                ),
                box(
                  title = "Graphique en toile d'arraignée",
                  width = 12,
                  plotlyOutput("scarterplot")
              )
              )
      ),
      
      # Troisième page : Analyse Avancée
      tabPanel("Analyse avancée", value = "analyse",
              fluidRow(
                box(
                  title = "Graphique 3D - Prix, Distance au Métro, Capacité d'Accueil",
                  width = 12,
                  plotlyOutput("graph3d")
                ),
                box(
                  title = "Carte de Corrélation",
                  selectInput("selectedTown", "Choisir une ville", choices = unique(Dataviz$town)),
                  width = 12,
                  plotlyOutput("corrplot")
                )
              )
      ),
      
      # Quatrième page : Cartographie Améliorée
      tabPanel("Cartographie", value = "cartographie",
              fluidRow(
                box( 
                  title = "Cartographie Dynamique des Appartements",
                  width = 12,
                  selectInput("town2", "Choisir une ville", choices = unique(Dataviz$town)),
                  sliderInput("realSum2", "Plage de prix", min = 0, max = 1500, value = c(50, 150)),
                  sliderInput("guest_satisfaction_overall2", "Note de satisfaction minimale", min = 0, max = 100, value = 8),
                  sliderInput("metro_dist2", "Distance au métro maximale (km)", min = 0, max = 10, value = 2),
                    
                  #actionButton("recommander", "Recommander"),
                  leafletOutput("map", height = "500px")
                )
              )
      ),
      
      tabPanel("Prédiction des Prix", value = "prediction",
              fluidRow(
                box(
                  width = 12,
                selectInput("town3", "Choisir une ville", choices = unique(Dataviz$town)),
                selectInput("room_type3", "Type d'appartement", choices = unique(Dataviz$room_type)),
                numericInput("person_capacity3", "Capacité d'accueil", min=1, value = 2),
                numericInput("metro_dist3", "Distance au métro",min = 0, max = 10, value = 1),
                numericInput("guest_satisfaction_overall3", "Note de satisfaction",min = 0, max = 100, value = 20),
                actionButton("predict", "Prédire le Prix"),
                verbatimTextOutput("pricePrediction")
              )
      )
      
    )
)
)
)


server <- function(input, output, session) {
  
  output$histogram_prix <- renderPlotly({
    data_f <- data_filtered <- subset(Dataviz, realSum < 1000 & town == input$town)
    histogram <- ggplot(data_f, aes(x = realSum, fill = factor(weekend))) +
      geom_histogram(binwidth = 50, position = "dodge") +
      facet_grid(~town) +
      labs(title = "Distribution des prix des appartements par ville et weekend",
           x = "Prix", y = "Fréquence") +
      theme_minimal()
    ggplotly(histogram)
  })
  
  output$scarterplot <- renderPlotly({
    data_f <- subset(Dataviz, realSum < 1000)
    

    colors <- c("Entire home/apt" = "#1f77b4", "Private room" = "#ff7f0e", "Shared room" = "#2ca02c")
    
    maxSize <- 15  
    
    fig <- plot_ly(data_f, type = 'scatterpolar', mode = 'markers',
                   r = ~realSum, theta = ~room_type, 
                   size = ~person_capacity, sizes = c(5, maxSize),
                   color = ~room_type, colors = colors,
                   marker = list(sizemode = 'diameter', opacity = 0.7, line = list(color = 'rgba(0, 0, 0, 0.5)', width = 1))
    )
    fig <- fig %>% layout(
      polar = list(
        radialaxis = list(visible = TRUE, tickangle = 45, tickfont = list(size = 10), gridcolor = 'lightgray'),
        angularaxis = list(tickfont = list(size = 12))
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.1, y = 1.1)  # Améliorer la position de la légende
    )
    fig
  })
  
 
  output$graph3d <- renderPlotly({
    data_f <- subset(Dataviz, realSum < 1000)
    fig2 <- plot_ly(data_f, x = ~metro_dist, y = ~person_capacity, z = ~realSum, color = ~town,
                    size = ~guest_satisfaction_overall, text = ~paste("Prix : $", realSum))
    fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = "Distance au métro"),
                                         yaxis = list(title = "Capacité d'accueil"),
                                         zaxis = list(title = "Prix de l'appartement")))
    fig2
  })
  

  output$corrplot <- renderPlotly({
    req(input$selectedTown)  
    
    data_f <- Dataviz %>%
      filter(town == input$selectedTown, realSum < 1000) %>%
      select(realSum, metro_dist, guest_satisfaction_overall, bedrooms, person_capacity, cleanliness_rating)
    
    correlation_matrix <- cor(data_f)
    
    correlation_data <- melt(as.matrix(correlation_matrix), na.rm = TRUE)

    p <- ggplot(correlation_data, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            axis.text.y = element_text(size = 12)) +
      labs(x = '', y = '', title = 'Correlation Matrix') +
      coord_fixed()

    ggplotly(p, tooltip = "text")
  })
  
    
  
  output$map <- renderLeaflet({
      filtered_data <- reactive({
      Dataviz[ Dataviz$town == input$town2 &
                Dataviz$realSum >= input$realSum2[1] & 
                Dataviz$realSum <= input$realSum2[2] &
                Dataviz$guest_satisfaction_overall >= input$guest_satisfaction_overall2 &
                Dataviz$metro_dist <= input$metro_dist2, ]
    })
    
    #req(input$town)
    # filtered_data <-  Dataviz %>%  Dataviz$realSum %in% seq(input$realSum[1],input$realSum[2]) %>% 
    #             Dataviz$guest_satisfaction_overall %in% seq(0,input$guest_satisfaction_overall )%>% 
    #             Dataviz$town  %in% input$town%>% 
    #             Dataviz$metro_dist %in% seq (0,input$metro_dist )
    
    data <- filtered_data()  
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        radius = ~sqrt(realSum)/10,
        color = ~ifelse(guest_satisfaction_overall > 90, "green", "red"),
        popup = ~paste("Prix : $", realSum, "<br>",
                       "Note : ", guest_satisfaction_overall, "<br>",
                       "Distance au métro : ", metro_dist, "km"),
        opacity = 0.8,
        fillOpacity = 0.8
      )
  })
  
  
  models <- readRDS("models_rf.rds")
  
  
  output$pricePrediction <- renderText({
    #req(input$town, input$person_capacity, input$metro_dist, input$guest_satisfaction_overall)
    
    #req(input$predict)  
    
    model <- models[[input$town3]]
    
    new_data <- data.frame(
      room_type = input$room_type3,
      person_capacity = input$person_capacity3,
      metro_dist = input$metro_dist3,
      guest_satisfaction_overall = input$guest_satisfaction_overall3
    )
    
   prediction<- predict(model, newdata = new_data)
    
    paste("Prix estimé : €", round(prediction, 2))
  })


}

# Lancement de l'application
shinyApp(ui, server)

