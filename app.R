library(shiny)
library(readr)
library(DT) # librairie pour affichage tableau : https://rstudio.github.io/DT
library(DBI)
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
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("meilleurs",
              box(
                width = 4,
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
                              tableOutput("classement_points")
                     ),
                     tabPanel(title = "Passes decisives",
                              tableOutput("classement_pad")
                    ),
                    tabPanel(title = "Rebonds",
                             tableOutput("classement_reb"))
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
  

}

# Run the application 
shinyApp(ui = ui, server = server)

