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
library(fmsb)
library(questionr)

#nba <- read.csv2("nbaNew.csv", sep=",")

#Importation données
smp = read_csv(file = "nbaNew.csv")

#Enlever les 5 dernieres lignes : contenants beaucoup de virgules mais sans données
nbligne=nrow(smp)
nblignebis=nbligne-5
nba=smp[-c(nblignebis:nbligne),]

#Palette de couleurs pour le radar plot
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )


#Création des colonnes moyennes (Points, Rebonds, Passes décisives)
nba <- mutate(nba, moypts = round(nba$PTS / nba$G, 1), moyreb = round(nba$TRB / nba$G, 1), moypad = round(nba$AST / nba$G, 1))

#Et classement en fonction de l'année
nba <- nba %>% arrange(desc(SeasonStart))
nba

#Renommer variables
nba <- rename.variable(nba, "FG%", "XFG")
nba <- rename.variable(nba, "eFG%", "XeFG")
nba <- rename.variable(nba, "3P%", "X3P")
nba <- rename.variable(nba, "2P%", "X2P")
nba <- rename.variable(nba, "FT%", "XFT")

#Recodage des variables : caractère -> numérique
nba$XFG<-as.numeric(gsub("%","",as.character(nba$XFG)))
nba$XeFG<-as.numeric(gsub("%","",as.character(nba$XeFG)))
nba$X3P<-as.numeric(gsub("%","",as.character(nba$X3P)))
nba$X2P<-as.numeric(gsub("%","",as.character(nba$X2P)))
nba$XFT<-as.numeric(gsub("%","",as.character(nba$XFT)))
nba$X3P=100*nba$X3P
nba$SeasonStart=as.character(nba$SeasonStart)



ui = dashboardPage(
  
  dashboardHeader(
    title = div(img(src="nba.png",style="display: block; margin-left: auto; margin-right: auto; width: 60%;",height = 50))
  ),
  dashboardSidebar(
    #Premier onglet
    sidebarMenu(
      menuItem("Statistiques descriptives",
               tabName = "joueurs",
               icon = icon("futbol")
      ),#Deuxi�me onglet
      menuItem("Qui sont les meilleurs joueurs ?",
               tabName = "meilleurs"
      ),#Troisi�me onglet
      menuItem("Statistiques pour chaque joueur", tabName="stat_joueur")
    )
  ),
  dashboardBody(
    tabItems(         #Forme du premier onglet
      tabItem("meilleurs",
              fluidRow(
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
                     
              )),
              fluidRow(
                infoBox( 
                  title =" ",
                  width = 13,
                  value = tags$p("Qui est le meilleur tireur ? A vous de comparer !", style = "font-size: 150%;"), 
                  icon=icon("percent"),
                  color = "blue"
                ),
              box(
                width = 4,
                selectInput("joueur1",
                            "Choisissez un premier joueur",
                            choices = c(
                              "Tous les joueurs",
                              unique(nba$PlayerName)
                            ))
              ),
              box(
                title = div(img(src="nba.png",style="display: block; margin-left: auto; margin-right: auto; width: 40%; height: 40%")),
                plotOutput("spiderweb"),
                width = 4
              ),
              box(
                width = 4,
                selectInput("joueur2",
                            "Choisissez un second joueur",
                            choices = c(
                              "Tous les joueurs",
                              unique(nba$PlayerName)
                            )))
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
  #Choix de la saison
  choix_saison = reactive({
    if (input$saison == "Toutes les saisons") {
      onlyseason = nba
    } else {
      onlyseason = nba %>% filter(nba$SeasonStart == input$saison)
    }
    onlyseason
  })
  #1er choix de joueur pour le radar plot
  choix_joueur1 = reactive({
    if (input$joueur1 == "Tous les joueurs") {
      onlyplayers = nba %>% filter(nba$PlayerName=="LeBron James")
    } else {
      onlyplayers = nba %>% filter(nba$PlayerName == input$joueur1)
    }
    onlyplayers
  })
  #2ème choix de joueur pour le radarplot
  choix_joueur2 = reactive({
    if (input$joueur2 == "Tous les joueurs") {
      onlyplayers2 = nba %>% filter(nba$PlayerName=="Kobe Bryant")
    } else {
      onlyplayers2 = nba %>% filter(nba$PlayerName == input$joueur2)
    }
    onlyplayers2
  })
  
  #Affichage des 10 meilleurs scoreurs par saison
  output$classement_points = renderTable({
    only1=choix_saison()
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
    only1=choix_saison()
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
    only1=choix_saison()
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
  
  #Représentation des triple double de moyenne
  output$triple_double = renderTable({ 
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
  #Case anecdote sur le Triple Double
  output$texte_td = renderText({ 
    paste("55 ans après Robertson, Westbrook devient le deuxième joueur de 
          l'histoire a réaliser un triple double de moyenne sur une saison.
          Performance qu'il reproduira l'année suivante, faisant de lui le seul joueur de l'histoire 
          à réaliser un
          'back-to-back triple double'.")
  })
  #Radar plot 
  output$spiderweb = renderPlot({
        only_player1=choix_joueur1()
        only_player2=choix_joueur2()
        newbdd=nba %>% 
            filter(PlayerName==only_player1$PlayerName | PlayerName==only_player2$PlayerName) %>% 
            select(PlayerName, XFG, XeFG, X3P, X2P, XFT) 
        newbdd=aggregate(newbdd,list(newbdd$PlayerName), mean)
        newbdd=newbdd[,-c(1:2)]
        newbdd <- rbind(rep(100,5) , rep(0,5) , newbdd)
        radarchart( newbdd  , axistype=1 , 
                #custom polygon
                pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,25), cglwd=0.8,
                #custom labels
                vlcex=0.8 
    )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

