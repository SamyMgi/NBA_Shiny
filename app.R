library(shiny)
library(readr)
library(DT) # librairie pour affichage tableau : https://rstudio.github.io/DT
library(DBI) #librairie pour utiliser SQL
library(shinydashboard)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(dplyr)
#library(dbConnect)
library(plotly)
library(fmsb)
library(questionr)
library(magrittr)
library(gghighlight)

#nba <- read.csv2("nbaNew.csv", sep=",")

#Importation donnÃ©es
smp = read_csv(file = "nbaNew.csv")

#Enlever les 5 dernieres lignes : contenants beaucoup de virgules mais sans données
nbligne=nrow(smp)
nblignebis=nbligne-5
nba=smp[-c(nblignebis:nbligne),]

#Palette de couleurs pour le radar plot
colors_border=c( rgb(0.2,0.5,0.8,0.9), rgb(1,0,0,0.6) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.8,0.4), rgb(1,0,0,0.3) , rgb(0.7,0.5,0.1,0.4) )


#CrÃ©ation des colonnes moyennes (Points, Rebonds, Passes dÃÂ©cisives)
nba <- mutate(nba, moypts = round(nba$PTS / nba$G, 1), moyreb = round(nba$TRB / nba$G, 1), moypad = round(nba$AST / nba$G, 1))

#Création bdd avec uniquement les 32 équipes actuelles pour l'onglet stat par équipe
nba_teamactuel = nba  %>% filter(Tm %in% c("ATL", "BRK", "BOS", "CHA","CHI","CLE","DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"))

#Et classement en fonction de l'annÃ©e
nba <- nba %>% arrange(desc(SeasonStart))
nba

#Renommer variables
nba <- rename.variable(nba, "FG%", "XFG")
nba <- rename.variable(nba, "eFG%", "XeFG")
nba <- rename.variable(nba, "3P%", "X3P")
nba <- rename.variable(nba, "2P%", "X2P")
nba <- rename.variable(nba, "FT%", "XFT")

#Recodage des variables : caractÃ¨re -> numÃ©rique
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
    sidebarMenu(
      menuItem("Description",#Premier onglet
               tabName = "description",
               icon = icon("info")),
      
      menuItem("Statistiques descriptives",#DeuxiÃ¨me onglet
               tabName = "joueurs",
               icon = icon("table")
      ),
      menuItem("Qui sont les meilleurs joueurs ?",#TroisÃ¨me onglet
               tabName = "meilleurs",
               icon = icon("medal")
      ),
      menuItem("Comparez-les !", #QuatriÃ¨me onglet
               tabName = "comp_joueurs",
               icon = icon("not-equal")
      ),
      menuItem("Statistiques pour chaque joueur", #CinquiÃ¨me onglet
               tabName="stat_joueur",
               icon = icon("chart-line")
               ),
      menuItem("Statistiques pour chaque équipe", #CinquiÃ¨me onglet
               tabName="stat_equipe",
               icon = icon("chart-line")
      )
              )
  ),
  dashboardBody(
    tabItems(    
      tabItem("joueurs",
              fluidRow(
                tags$h2("Application SHINY sur la NBA !"),
                tags$div(
                  HTML(paste(tags$blockquote("Ici, on retrouve toutes les donnees des joueurs de la NBA depuis XXXX. Cette application est destinee pour les fan ou encore pour les curieux qui voudraient en apprendre davantage sur les caracteristiques et performance des", tags$span(style="color:red", "joueurs"), sep = "")))
                ),
                
                valueBox(
                  value = textOutput("nb_joueur"),
                  subtitle = "Nombre de joueurs professionnels de la NBA",
                  #icon = icon("usd"),
                  color = "green",
                  width = 4
                ),
                
                valueBox(
                  value = textOutput("nb_variable"),
                  subtitle = "Nombre de variable affecte a chaque joueur professionnel de la NBA",
                  #icon = icon("usd"),
                  color = "green",
                  width = 4
                ),
                tabBox(title = "Informations",
                       width = 4,
                       tabPanel(title = "Age moyen des joueurs de la NBA",
                                tableOutput("info_age")
                       ),
                       tabPanel(title = "Salaire median des joueurs de la NBA",
                                tableOutput("info_salaire")
                       )
                ),
                box(
                  selectInput("club",
                              "Choisir un club",
                              choices = sort(unique(nba$Tm))
                  )
                ),
                
                box(
                  selectInput("saison_desc",
                              "Choisir une saison",
                              choices = sort(unique(nba$SeasonStart))
                  )
                ),
                tabBox(
                  tabPanel(title = "Statistiques des joueurs",
                           dataTableOutput("joueurs")
                  ),
                  width = 11
                )
                
              ),
              tags$div(
                HTML(paste(tags$blockquote(tags$a(href="www.nba.com","SITE OFFICIEL DE LA", tags$span(style="color:red", "NBA"), sep = ""))))
              )
      ),
      
      #Forme du premier onglet
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
                color = "red",
                fill = TRUE
              ),
              
              tabBox(title = "Performances Historiques", 
                     width = 5,
                     tabPanel(title = "Triple Double",
                              tableOutput("triple_double")
                     ),
                     img(src='russwest.gif',style="display: block; margin-left: auto; margin-right: auto; width: 100%;",height = 250, width = 350)
                     
              ))
              
              
              
              
      ), #Forme du deuxiÃ¨me onglet
      tabItem("comp_joueurs",
               fluidRow(
                 infoBox( 
                   title =" ",
                   width = 12,
                   value = tags$p("Qui est le meilleur tireur ? A vous de comparer !", style = "font-size: 150%;"), 
                   icon=icon("percent"),
                   color = "green",
                   fill = TRUE
                 ),
                 box(
                   width = 4,
                   selectInput("joueur1",
                               tags$p("Choisissez un premier joueur", style = "color:blue; font-size: 120%;"),
                               choices = c(
                                 "Tous les joueurs",
                                 unique(nba$PlayerName)
                               )),
                   img(src='james.png',style="margin:0px 0px", height = 600, width = 450)
                 ),
                 box(
                   title = div(img(src="nba.png",style="display: block; margin-left: auto; margin-right: auto; width: 40%; height: 40%")),
                   plotOutput("spiderweb"),
                   width = 4
                 ),
                 box(
                   width = 4,
                   selectInput("joueur2",
                               tags$p("Choisissez un second joueur", style = "color:red; font-size: 120%;"),
                               choices = c(
                                 "Tous les joueurs",
                                 unique(nba$PlayerName)
                               )),
                   img(src='curry.png',style="margin:0px 0px", height = 600, width = 450)
                   )
                 
               )
               ),
      #Forme du troisiÃ¨me onglet
      tabItem("stat_joueur",
            fluidRow(
              column(width = 6, 
              box(
                width = NULL,
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
                icon = icon("basketball-ball"),
                fill = TRUE,
                color = "blue",
                width = NULL
              ),
              infoBox(
                title = "Meilleur saison en assists par match",
                value = textOutput("assist_joueur"),
                subtitle = "Moyenne Pad/Match",
                icon = icon("hands-helping"),
                fill = TRUE,
                color = "red",
                width = NULL
              ), infoBox(
                title = "Meilleur saison en rebonds par match",
                value = textOutput("rebond_joueur"),
                subtitle = "Moyenne Rbs/Match",
                icon = icon("arrows-alt-v"),
                fill = TRUE,
                color = "green",
                width = NULL
              ),
              infoBox( 
                title ="Wilt Chamberlain, jamais égalé ?",
                width = NULL,
                value = tags$p("L'histoire du meilleur joueur des années 1970", style = "font-size: 150%;"), 
                subtitle = tags$p(htmlOutput("wilt"), style = "font-size: 100%;"),
                color = "red",
                icon=icon("info"),
                fill = TRUE
              ))
              ,column(width = 6,
              box(
                title = "Performance en moyenne par match",
               plotOutput("player_points"),
                width = NULL
              ),
              box(
                img(src='chamberlain.gif',style="display: block; margin-left: auto; margin-right: auto; width: 100%;")))
              
            )     
      ),
      tabItem("stat_equipe",
              fluidRow(
                column(width = 6, 
                       box(
                         width = NULL,
                         selectInput("Equipe",
                                     "Choix de l'équipe",
                                     choices = c(
                                       "Choisir une équipe",
                                       sort(unique(nba_teamactuel$Tm))
                                     ))
                       ),
                       infoBox(
                         title = "Meilleur PF de l'histoire de l'équipe",
                         value = textOutput("best_PF"),
                         
                         
                         fill = TRUE,
                         color = "blue",
                         width = NULL
                       ),
                       infoBox(
                         title = "Meilleur PG de l'histoire de l'équipe",
                         value = textOutput("best_PG"),
                         
                         
                         fill = TRUE,
                         color = "blue",
                         width = NULL
                       ), infoBox(
                         title = "Meilleur C de l'histoire de l'équipe",
                         value = textOutput("best_C"),
                         
                         
                         fill = TRUE,
                         color = "blue",
                         width = NULL
                       ),infoBox(
                         title = "Meilleur SG de l'histoire de l'équipe",
                         value = textOutput("best_SG"),
                         
                         
                         fill = TRUE,
                         color = "blue",
                         width = NULL
                       ),
                       infoBox(
                         title = "Meilleur SF de l'histoire de l'équipe",
                         value = textOutput("best_SF"),
                         
                         fill = TRUE,
                         color = "blue",
                         width = NULL
                       ),infoBox( 
                         title ="Explication",
                         width = NULL,
                         value = tags$p("Les postes", style = "font-size: 150%;"), 
                         subtitle = tags$p(htmlOutput("poste_td"), style = "font-size: 100%;"),
                         color = "red",
                         icon=icon("info"),
                         fill = TRUE
                       )),
                column(width = 6,
                        box(
                          title = "Poste qui a le plus marqué l'histoire de l'équipe",
                          plotOutput("poste_points"),
                          width = NULL
                        ),
                box(
                    img(src='postes.gif',style="display: block; margin-left: auto; margin-right: auto; width: 100%;",height = 200, width = 50)),
                box(
                  uiOutput(outputId = "logo"),
                  width = 4
                )
                
              ) )    
      )
  
    ) 
  ),
  title = "titre dans le navigateur",
  skin = "blue"
)

server <- function(input, output) {
  
  output$joueurs = renderDataTable({
    table = nba %>% filter(Tm == input$club & SeasonStart == input$saison_desc)
    datatable(
      data.frame(
        table
      ),
      rownames = FALSE,
      extensions = 'FixedColumns',
      options = list(
        dom = 'tip',
        scrollX = TRUE,
        fixedColumns = list(leftColumn = 1, rightColumns = 1)
      )
    ) %>%
      formatStyle('PlayerName',
                  color = 'white',
                  backgroundColor = 'steelblue',
                  fontWeight = 'bold')
  })
  
  output$nb_joueur = renderText({
    length(unique(nba$PlayerName))
  })
  
  output$nb_variable = renderText({
    ncol(nba)-1
  })
  
  output$info_age = renderTable({
    data.frame(
      Statistique = c("Minimum", "Moyenne", "Mediane", "Maximum"),
      Valeur = c(
        min(nba$Age, na.rm = T),
        mean(nba$Age, na.rm = T),
        median(nba$Age, na.rm = T),
        max(nba$Age, na.rm = T)
      )
    )
  })
  
  output$info_salaire = renderTable({
    data.frame(
      Statistique = c("Minimum", "Moyenne", "Médiane", "Maximum"),
      Valeur = c(
        min(nba$PlayerSalary, na.rm = T),
        mean(nba$PlayerSalary, na.rm = T),
        median(nba$PlayerSalary, na.rm = T),
        max(nba$PlayerSalary, na.rm = T)
      )
    )
  })
  
  
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
  #2Ã¨me choix de joueur pour le radarplot
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
    #Creation table avec le joueur choisi
    joueur_choisi1=joueur_choisi()
    NBA_joueur = nba %>% filter(PlayerName %in% joueur_choisi1$PlayerName) %>%  select(SeasonStart, moypts, moyreb, moypad) %>%  gather(key = "variable", value= "value", -SeasonStart)
    ggplot(NBA_joueur, aes(SeasonStart, value, group=variable))+
        geom_line(aes(color=variable, linetype=variable))+
        labs(x = "Année", y="Moyenne de la variable choisie par match (points, assists ou rebonds)") +
        theme(legend.position ="right")+
      geom_point(aes(group = seq_along(SeasonStart)))+
      scale_linetype_manual(values=c("longdash", "solid", "twodash"))+
      scale_color_manual(values=c('red','black', 'blue'))
    })
    
  
  #CrÃ©ation de deux infobox qui indiquent les meilleurs statistiques pour le joueur choisi
  output$point_joueur = renderText({
    joueur_choisi1=joueur_choisi()
    NBA_joueur = nba %>% filter(PlayerName %in% joueur_choisi1$PlayerName) %>%  select(SeasonStart, moypts, moyreb, moypad) %>% arrange(desc(moypts)) %>% slice(1:1)
    paste(NBA_joueur$moypts, "Points par match pour la saison", NBA_joueur$SeasonStart)
  })
  
  output$assist_joueur = renderText({
    joueur_choisi1=joueur_choisi()
    NBA_joueur = nba %>% filter(PlayerName %in% joueur_choisi1$PlayerName) %>%  select(SeasonStart, moypts, moyreb, moypad) %>% arrange(desc(moypad)) %>% slice(1:1)
    paste(NBA_joueur$moypad, "Points par match pour la saison", NBA_joueur$SeasonStart)
  })
  
  output$rebond_joueur = renderText({
    joueur_choisi1=joueur_choisi()
    NBA_joueur = nba %>% filter(PlayerName %in% joueur_choisi1$PlayerName) %>%  select(SeasonStart, moypts, moyreb, moypad) %>% arrange(desc(moyreb)) %>% slice(1:1)
    paste(NBA_joueur$moyreb, "Points par match pour la saison", NBA_joueur$SeasonStart)
  })
  
  
  
  
  #ReprÃ©sentation des triple double de moyenne
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
    paste("55 ans aprÃ¨s Robertson, Westbrook devient le deuxiÃ¨me joueur de 
          l'histoire a rÃ©aliser un triple double de moyenne sur une saison.
          Performance qu'il reproduira l'annÃ©e suivante, faisant de lui le seul joueur de l'histoire 
          Ã  rÃ©aliser un
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
  
  
  #Partie 5 __ Choix équipe, plot des points par poste:
  #Récuperation Equipe choisie
  equipe_choisi = reactive({
    if (input$Equipe == "Choisir un joueur") {
      team = ""
    } else {
      team = nba %>% filter(nba$Tm %in% input$Equipe)
    }
    team
  })
  
  #Graphique des points par poste:
  output$poste_points = renderPlot({
    Team1=equipe_choisi()
    NBA_equipe = nba %>% filter(Tm %in% Team1$Tm & Pos %in% c("PF", "PG", "SF", "C", "SG"))
    ggplot(NBA_equipe, aes(Pos, moypts, fill=Pos))+
      geom_bar(stat="identity", width=1)+
      coord_polar()+
      labs(x = "Position", y="Points par match") +
      theme(legend.position ="right")
    
  })
  
  output$best_C = renderText({
    Team1=equipe_choisi()
    NBA_equipe = nba %>% filter(Tm %in% Team1$Tm & Pos =="C") %>%  group_by(PlayerName) %>% summarise(n=n(), TotalP= sum(PTS)) %>% arrange(desc(TotalP)) %>% slice(1:1)
    paste(NBA_equipe$TotalP, "Points marqués par ", NBA_equipe$PlayerName, " en ", NBA_equipe$n, "saisons")
  })
  output$best_PF = renderText({
    Team1=equipe_choisi()
    NBA_equipe = nba %>% filter(Tm %in% Team1$Tm & Pos =="PF") %>%  group_by(PlayerName) %>% summarise(n=n(), TotalP= sum(PTS)) %>% arrange(desc(TotalP)) %>% slice(1:1)
    paste(NBA_equipe$TotalP, "Points marqués par ", NBA_equipe$PlayerName, " en ", NBA_equipe$n, "saisons")
  })
  output$best_PG = renderText({
    Team1=equipe_choisi()
    NBA_equipe = nba %>% filter(Tm %in% Team1$Tm & Pos =="PG") %>%  group_by(PlayerName) %>% summarise(n=n(), TotalP= sum(PTS)) %>% arrange(desc(TotalP)) %>% slice(1:1)
    paste(NBA_equipe$TotalP, "Points marqués par ", NBA_equipe$PlayerName, " en ", NBA_equipe$n, "saisons")
  })
  output$best_SG = renderText({
    Team1=equipe_choisi()
    NBA_equipe = nba %>% filter(Tm %in% Team1$Tm & Pos =="SG") %>%  group_by(PlayerName) %>% summarise(n=n(), TotalP= sum(PTS)) %>% arrange(desc(TotalP)) %>% slice(1:1)
    paste(NBA_equipe$TotalP, "Points marqués par ", NBA_equipe$PlayerName, " en ", NBA_equipe$n, "saisons")
  })
  output$best_SF = renderText({
    Team1=equipe_choisi()
    NBA_equipe = nba %>% filter(Tm %in% Team1$Tm & Pos =="SF") %>%  group_by(PlayerName) %>% summarise(n=n(), TotalP= sum(PTS)) %>% arrange(desc(TotalP)) %>% slice(1:1)
    paste(NBA_equipe$TotalP, "Points marqués par ", NBA_equipe$PlayerName, " en ", NBA_equipe$n, "saisons")
  })
  
  #Box explication des postes
  output$poste_td = renderUI({ 
    p1 = paste("Au basket, il existe 5 postes :")
    p2= paste("-Meneur ou PG : Il guide le jeu, annonce les systèmes et impose le rythme")
    p3= paste("-Arrière ou SG : Son rôle est de tirer à l'exterieur de la raquette, notamment à trois points")
    p4= paste("-Ailier ou SF : Il s'agit d'un joueur polyvalent qui peut aussi bien tirer que pénétrer dans la raquette")
    p5= paste("-Ailier Fort ou PF : Il a principalement un jeu défensif, mais peut assister le pivot en attaque")
    p6= paste("-Pivot ou C :  C'est le joueur le plus grand, il joue dans la raquette et récupère des rebonds")
    
    HTML(paste(p1,p2,p3,p4,p5,p6, sep="<br/>"))
  })
  
  #Box explication Perf de Wilt
  output$wilt = renderUI({
    p0=paste("")
    p1= paste("Wilt Chamberlain a joué en NBA des années 1960 jusqu'en 1973. En 15 saisons, il a marqué la NBA pour des décénnies.")
    p2= paste("En effet, il s'agit du seul joueur à atteindre 50 points par match en moyenne. Il détient par ailleurs les 3 premières places au classement des meilleurs saisons en points par match.")
    p3= paste("Mais au-delà des points, il est également le seul à obtenir plus de 25 rebonds par match en moyenne. Pour comparer, en 2019, le meilleur rebondeur avait 15 rebonds par match.")
    p4=paste("Précision: Dans les années 60, il était possible d'arrêter un tir même durant sa phase descendante, augmentant le nombre de rebonds défensifs. Désormais, cela est interdit.")
    p5= paste("Ses records seront-t-ils menacé un jour ?")
    HTML(paste(p1,p0, p2,p0, p3,p0, p4,p5, sep="<br/>"))
    
  })
  
  #Un logo par équipe
  
  output$logo = renderUI({
    Team1=equipe_choisi()
    if (Team1 == "") {
      img(src = "NBA_white.jpg", height="200px", width = "200px")
    } else{
      Choix_equipe = unique(Team1$Tm)
      photo = paste(Choix_equipe, ".png", sep="")
      img(src = photo, height="200px", width = "200px")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

