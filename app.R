library(shiny)
library(readr)
library(DT) # librairie pour affichage tableau : https://rstudio.github.io/DT
library(DBI) #librairie pour utiliser SQL
library(shinydashboard)
library(ggplot2)
library(scales)
library(forcats)
library(dplyr)
library(dbConnect)
library(plotly)

#nba <- read.csv2("nbaNew.csv", sep=",")

#Importation données
smp = read_csv(file = "nbaNew.csv")

#Enlever les 5 dernieres lignes
nbligne=nrow(smp)
nblignebis=nbligne-5
nba=smp[-c(nblignebis:nbligne),]

#Création des colonnes moyennes (Points, Rebonds, Passes décisives)
nba <- mutate(nba, moypts = round(nba$PTS / nba$G, 1), moyreb = round(nba$TRB / nba$G, 1), moypad = round(nba$AST / nba$G, 1))
#Et classement en fonction de l'année
nba <- nba %>% arrange(desc(SeasonStart))

#Recodage de la variable année en facteur




ui = dashboardPage(
  dashboardHeader(
    title = "NBA",
    titleWidth =  300
  ),
  dashboardSidebar(
    #Premier onglet
    sidebarMenu(
      menuItem("Qui sont les meilleurs joueurs ?",
               tabName = "meilleurs"
      ),#Deuxième onglet
      menuItem("Statistiques pour chaque joueur", tabName="stat_joueur")
    )
  ),
  dashboardBody(
    tabItems(         #Forme du première onglet
      tabItem("meilleurs",
              box(
                width = 3,
                selectInput("saison",
                            "Choix de la saison",
                            choices = c(
                              "Toutes les saisons",
                              unique(nba$SeasonStart)
                            ))
              ),
              tabBox(title = "Classement",
                     width = 4,
                     tabPanel(title = "Points",
                              tableOutput("classement_points"),
                              img(src='kobe.png',style="margin:-475px -350px", height = 600, width = 350)
                     ),
                     tabPanel(title = "Passes decisives",
                              tableOutput("classement_pad"),
                              img(src='irving.png',style="margin:-550px -350px", height = 600, width = 400)
                    ),
                    tabPanel(title = "Rebonds",
                             tableOutput("classement_reb"),
                            img(src='shaq.png',style="margin:-475px -400px", height = 600, width = 450)
                            )
              ),
              infoBox( 
                title ="Anecdote",
                width = 5,
                value = tags$p("Mr. Triple Double", style = "font-size: 150%;"), 
                subtitle = tags$p(textOutput("texte_td"), style = "font-size: 100%;"),
                icon=icon("info"),
                color = "red"
              ),
              
              tabBox(title = "Performances Historiques", 
                     width = 5,
                     tabPanel(title = "Triple Double",
                              tableOutput("triple_double")
                     ),
                     img(src='russwest.gif',style="display: block; margin-left: auto; margin-right: auto; width: 100%;",height = 250, width = 350)
                     
              )
              
              
              
      ), #Forme du deuxième onglet
      tabItem("stat_joueur",
              box(
                width = 4,
                selectInput("joueur",
                            "Choix du joueur",
                            choices = c(
                              "Choisir un joueur",
                              unique(nba$PlayerName)
                            ))
                  ),
              infoBox(
                title = "Meilleur saison en points par match",
                value = textOutput("point_joueur"),
                subtitle = "Moyenne Pts/Match",
                icon = icon("line-chart"),
                fill = TRUE,
                color = "blue",
                width = 5
              ),
              box(
                title = "Evolution de la moyenne des points par match",
                plotOutput("player_points"),
                width = 7
              )
              
              
      )
  
    ) 
  ),
  title = "titre dans le navigateur",
  skin = "blue"
)

server <- function(input, output) {
  donnees = reactive({
    if (input$saison == "Toutes les saisons") {
      onlyseason = nba
    } else {
      onlyseason = nba %>% filter(nba$SeasonStart == input$saison)
    }
    onlyseason
  })
  
  #Affichage des 10 meilleurs scoreurs par saison
  output$classement_points = renderTable({
    only1=donnees()
    nba %>% 
      filter(SeasonStart==only1$SeasonStart) %>% 
      select(PlayerName, moypts) %>% 
      arrange(desc(moypts)) %>% 
      slice(1:10) %>% 
      rename("Joueur" = PlayerName,
             "Points par match" = moypts)
  })
  
  #Affichage des 10 meilleurs passeurs par saison
  output$classement_pad = renderTable({
    only1=donnees()
    nba %>% 
      filter(SeasonStart==only1$SeasonStart) %>% 
      select(PlayerName, moypad) %>% 
      arrange(desc(moypad)) %>% 
      slice(1:10) %>% 
      rename("Joueur" = PlayerName,
             "Passes decisives par match" = moypad)
  })
  
  #Affichage des 10 meilleurs rebondeurs par saison
  output$classement_reb = renderTable({
    only1=donnees()
    nba %>% 
      filter(SeasonStart==only1$SeasonStart) %>% 
      select(PlayerName, moyreb) %>% 
      arrange(desc(moyreb)) %>% 
      slice(1:10) %>% 
      rename("Joueur" = PlayerName,
             "Rebonds par match" = moyreb)
  })
  
  # Recupération du joueur choisi par l'utilisateur
  joueur_choisi = reactive({
    if (input$joueur == "Choisir un joueur") {
      player = nba %>% filter(nba$PlayerName %in% "Russell Westbrook")
    } else {
      player = nba %>% filter(nba$PlayerName %in% input$joueur)
    }
    player
  })
  
  # Affichage graphique du nombre de points par match pour le joueur choisi
  output$player_points = renderPlot({
    joueur_choisi1=joueur_choisi()
    NBA_joueur = nba %>% filter(PlayerName %in% joueur_choisi1$PlayerName) %>% arrange(SeasonStart)
    plot(NBA_joueur$SeasonStart, NBA_joueur$moypts, main="Moyenne des points par match", xlab="Saison", ylab="Moy points")
  })
  
#  output$player_points = renderText({
  #    joueur_choisi1=joueur_choisi()
  #    meilleur_moyenne = nba %>% filter(PlayerName %in% joueur_choisi1$PlayerName) %>% arrange(desc(moypts)) %>% slice(1:2)
  #    paste(meilleur_moyenne$moypts, "est la meilleur moyenne de points par match de", meilleur_moyenne$PlayerName)
    
  #  })
  output$triple_double = renderTable({ 
    only1=donnees()
    nba %>% 
      filter(moypts>=10 & moyreb>=10 & moypad>=10) %>% 
      select(SeasonStart, PlayerName, moypts, moypad, moyreb) %>% 
      arrange(desc(SeasonStart)) %>% 
      rename("Saison" = SeasonStart,
             "Joueur" = PlayerName,
             "Points par match" = moypts,
             "Passes decisives par match" = moypad,
             "Rebonds par match" = moyreb)
  })
  output$texte_td = renderText({ 
    paste("55 ans après Roberson, Westbrook devient le deuxième joueur de 
          l'histoire a réaliser un triple double de moyenne sur une saison.
          Performance qu'il reproduira l'année suivante, faisant de lui le seul joueur de l'histoire 
          à réaliser un
          'back-to-back triple double'")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

