library(shiny)
library(shinythemes)
library(stringi)
library(ggplot2)
library(plotly)
library(devtools)
library(Rcpp)
library(RCurl)
library(RJSONIO)
library(DT)
library(RgoogleMaps)
library(ggmap)
library(rjson)
require(devtools)
require(rCharts)
library(treemap)
library(d3treeR)
library(writexl)
library(shinydashboard)
library(shinyWidgets)
library(knob)
library(gridExtra)
library(stringr)
library(leaflet)
library(sf)
library(data.table)
library(dplyr)
library(leaflet.extras)
library(highcharter)
library(urltools)



load("MyData.RData")




### Fonctions ###

paste_number <- function(x){
  
  for(i in 1:nrow(x)){
    
    x[i,1] <- paste0(x[i,1]," (",x[i,2],")")
    
  }
  
  return(x)
  
}

get_number <- function(x){
  
  if(length(grep(";",x)) == 0){
    
    r = 0
    if(x!="Non"){
      
      r <- str_split(x,pattern = "\\(")[[1]][2]
      r <- str_split(r,"\\)")[[1]][1]
    }
    
    r <- round(as.numeric(r)/10000,0)
    r <- paste(r,"(ha)")
    return(r)
    
  } else{
    
    r = 0
    if(x!="Non"){
      
      r <- str_split(x,pattern = "\\(")[[1]][2]
      r <- str_split(r,"\\)")[[1]][1]
      r <- paste(r,"(ha)")
      
      return(r)
    }
    
    
    
  }
}


get_number2 <- function(x){
  
  if(length(grep(";",x)) == 0){
    
    r = 0
    if(x!="Non"){
      
      r <- str_split(x,pattern = "\\(")[[1]][2]
      r <- str_split(r,"\\)")[[1]][1]
    }
    
    r <- round(as.numeric(r)/10000,0)
    return(r)
    
  } else{
    
    r = 0
    if(x!="Non"){
      
      r <- str_split(x,pattern = "\\(")[[1]][2]
      r <- str_split(r,"\\)")[[1]][1]
      
      
      return(r)
    }
    
    
    
  }
}
















########################################################################################################
### UI ###
########################################################################################################





ui <- shinyUI( fluidPage(theme = "bootstrap.css",
                         includeCSS("styles.css"),
                         navbarPage(em("Évaluation nationale des sites humides emblématiques (2010 - 2020)"),selected="",windowTitle="ENSHE 2010-2020",
                                    
                                    
                                    header = tagList(
                                      useShinydashboard()
                                    ),
                                    
                                    setBackgroundColor(
                                      color = rgb(0.96,0.96,1),
                                    ),      
                                    
                                    
                                    tags$style("@import url(https://use.fontawesome.com/releases/v5.15.1/css/all.css);"),
                                    
                                    
                                    # mise en page des tableaux de données #
                                    
                                    tags$head(tags$style("#DataTables_Table_0_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    
                                    tags$head(tags$style("#DataTables_Table_1_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    tags$head(tags$style("#DataTables_Table_2_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    tags$head(tags$style("#DataTables_Table_3_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    tags$head(tags$style("#DataTables_Table_4_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    tags$head(tags$style("#DataTables_Table_5_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    tags$head(tags$style("#DataTables_Table_6_length{
                                 text-align:left;
                                 
                                 }"
                                    )
                                    ),
                                    
                                    tags$head(tags$style(
                                      type="text/css",
                                      "#image2 img {height:80px;width:90px}"
                                    )),
                                    
                                    ### INTRO ###    
                                    
                                    tabPanel("",icon=icon("home"),
                                             fluidPage(
                                               
                                               
                                               
                                               column(1,style="border-radius: 10px",
                                                      imageOutput("image2",height="80px")
                                               ),
                                               column(10,align ="center", style="background-color:#d1dad8;border-radius: 10px;padding-left:0px;padding-right:0px;margin-top:10px",
                                                      
                                                      
                                                      strong(em(p("Bienvenue sur le site de visualisation des données de l'Évaluation nationale des sites humides emblématiques (2010-2020)",style="color:black;font-size:22px;padding-top:15px"))),
                                                      
                                               ),
                                               column(1,style="border-radius: 10px;padding-left:20px", align="center",
                                                      imageOutput("image1",height="80px")
                                               ),
                                               
                                               
                                               tags$head(tags$style(
                                                 type="text/css",
                                                 "#image1 img {height:90px; width:80px}"
                                               )),
                                               
                                               
                                               
                                               
                                               fluidRow(
                                                 
                                                 column(12,style="border-radius: 10px;padding-left:0px;padding-right:0px;height:100px",
                                                        imageOutput("image3")
                                                 ),
                                               ),
                                               
                                               tags$head(tags$style("#image3 img{
                                 max-width: 100%; width: 100%;
                                 height:100px;
                                 }
                                 }"
                                               )
                                               ),
                                               
                                               
                                               
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px",
                                                        
                                                        
                                                        strong(p("Présentation du dispositif de l'évaluation",style="text-align:justify;color:black;padding:10px;font-size:20px")),
                                                        
                                                        p("Depuis 1990, tous les 10 ans, le service statistique du ministère en charge de l’environnement procède à une évaluation de l’état et de l’évolution d’un échantillon de sites humides emblématiques métropolitains et ultramarins. Cette évaluation décennale permet ainsi de caractériser les pressions exercées sur ces espaces menacés de disparition, de dresser un panorama de l’état et de l’évolution des sites humides emblématiques et de mettre en lumière les défis à relever pour les sauvegarder." ,style="text-align:justify;color:black;padding-left:15px"), 
                                                        
                                                        
                                                        p("Portée par le" ,span("Service des données et études statistiques (SDES) et l'Office français pour la biodiversité (OFB)",style="font-weight:bold"),  ", la cinquième évaluation nationale couvrant la période 2010-2020 s’est déroulée en 2019. Elle portait sur 174 sites de métropole et 49 sites ultramarins. Entre août et décembre 2019, plus de 1 200 référents du monde associatif (Ligue pour la protection des oiseaux, Conservatoire du littoral, etc.), des collectivités territoriales (syndicats de rivière, mairies...) et de services déconcentrés de l'État (Direction régionale de l’environnement de l’aménagement et du logement, Direction de l’environnement de l’aménagement et du logement, Direction du service de l'agriculture, de la forêt et de l'environnement, Direction départementale des territoires) ont été mobilisés pour contribuer à l'évaluation des sites retenus. Pour ce faire, il leur a été demandé de renseigner un questionnaire en ligne abordant de nombreuses thématiques en lien avec les principaux enjeux rencontrés sur ces espaces :",span("activités humaines",style="font-weight:bold")," et leurs pressions, ",span("étendue et état des milieux",style="font-weight:bold")," humides présents sur le site, ",span("état des espèces",style="font-weight:bold")," communes et à forts enjeux, problématiques liées à ",span("l'hydrologie et l'hydraulique,",style="font-weight:bold")," problématiques liées à la ",span("faune et la flore envahissantes et indigènes à fort développement, effets du changement climatique, services rendus à la société.",style="font-weight:bold"),style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                        
                                                        
                                                        
                                                        p("Cet espace numérique présente les nombreux ",span("jeux d'indicateurs d'état et d'évolution",style="font-weight:bold")," issus de l'exploitation des informations produites par les référents. Un commentaire synthétique est adossé à chaque indicateur. Les données et les indicateurs sont en libre accès et téléchargeables. Cet espace comprend également une rubrique intitulée ",span("« Portraits de territoire » dont la vocation est d'enrichir les informations disponibles concernant chaque site. Il met notamment en exergue certains résultats de l'évaluation 2010-2020 en les comparant à la moyenne ",style="font-weight:bold"),"des sites relevant de sa typologie.",style="text-align:justify;color:black;padding-left:15px")
                                                 ),
                                               ),
                                               
                                               br(),
                                               
                                               fluidRow(
                                                 
                                                 column(12,align = "center",style="background-color:#d1dad8;border-radius: 10px",
                                                        strong(p("Méthodologie",style="text-align:justify;color:black;padding:10px;font-size:20px")),
                                                        
                                                        
                                                        
                                                        strong(p("Les sites évalués",style="text-align:justify;color:black;padding-left:15px;text-decoration:underline;")),
                                                        
                                                        p("Ces sites ont été sélectionnés pour leur richesse faunistique, floristique, culturelle et patrimoniale. Certains peuvent faire l’objet d’un statut de protection ou de gestion spécifiques (parc national, réserve naturelle, Natura 2000, labellisation Ramsar, etc.). L’évaluation 2010-2020 a porté sur 223 sites",span("(dont 189 ont pu être étudiés)"),"appartenant à six grands types, représentatifs de la diversité des écosystèmes humides français et de leur degré de résilience face aux diverses menaces : vallées alluviales, littoral atlantique, Manche et mer du Nord, littoral méditerranéen, outre-mer, plaines intérieures et massif à tourbières.",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                        
                                                        p("Afin de garantir une continuité entre les différents millésimes de cette évaluation nationale, 206 sites évalués durant la campagne 2000-2010 ont été conservés. Pour la campagne 2010-2020, le choix a été fait d’élargir cette liste avec les sites Ramsar qui n'avaient pas été intégrés lors de la campagne 2000-2010, ceux ayant été labellisés depuis 2010 et ceux en cours de labellisation en 2019 qui seront probablement labellisés Ramsar lors de la prochaine évaluation 2020-2030. D'autres sites présentant de forts enjeux locaux, ont également été ajoutés au panel.",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                        strong(p("Les référents mobilisés",style="text-align:justify;color:black;padding-left:15px;text-decoration:underline;")),
                                                        
                                                        p("L'annuaire des référents mobilisés pour conduire la campagne 2010-2020 a été élaboré à partir de celui de la précédente évaluation 2000-2010. Ces référents ",span("sont issus de structures assurant le suivi administratif",style="font-weight:bold"), "(Direction régionale de l’environnement de l’aménagement et du logement, Direction de l’environnement de l’aménagement et du logement, Direction du service de l'agriculture, de la forêt et de l'environnement, Direction départementale des territoires, Agence de l’eau, collectivités...)",span("et la gestion technique/opérationnelle",style="font-weight:bold")," (association, fédération...). Pour pouvoir être désigné  référent d'un site, deux critères devaient être remplis : ",span("connaître le site de préférence depuis plus de 10 ans et avoir une connaissance ou participer à la gestion du site sur sa majeure partie.",style="font-weight:bold")," Afin de limiter les biais dans les évaluations, plusieurs référents d'une même structure ont pu être sollicités et plusieurs structures ont pu être invitées à compléter le questionnaire pour un même site. De même, les référents assurant la gestion de plusieurs sites ont pu évaluer plusieurs sites. Cette situation a amené les pilotes de l'enquête à adresser ",span("plus de 2 500 questionnaires en ligne.",style="font-weight:bold"),style="text-align:justify;color:black;padding-left:15px")
                                                        
                                                 )
                                                 
                                               ),
                                               br(),
                                             )
                                             
                                             
                                    ),
                                    
                                    navbarMenu("Thématiques",
                                               
                                               ### Renseignements généraux ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Renseignements généraux"), icon=icon("atlas"),
                                                        
                                                        tabsetPanel(type="tabs",tabPanel(strong("RENSEIGNEMENTS GÉNÉRAUX"), br(),
                                                                                         
                                                                                         tabsetPanel(type="pills",
                                                                                                     
                                                                                                     tabPanel(h5(strong("Sites humides de l'évaluation")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(6, align="center",
                                                                                                                         fluidRow(
                                                                                                                           h3("Liste des sites humides de l'évaluation",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           dataTableOutput("table_P1")
                                                                                                                         ),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(12, 
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                           
                                                                                                                         ),
                                                                                                                         fluidRow(
                                                                                                                           column(12,  align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text1_P),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), br(),
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  column(6, align = "center",
                                                                                                                         fluidRow(
                                                                                                                           
                                                                                                                           h3("Répartition des 189 sites humides en fonction de la typologie",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_P1",height=580),height=581,width=12
                                                                                                                               
                                                                                                                           )
                                                                                                                         ),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                           
                                                                                                                         ),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text2_P),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), br(),
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     
                                                                                                     tabPanel(h5(strong("Référents")),
                                                                                                              
                                                                                                              fluidPage(  
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(3),column(6,align="center",selectInput("P30",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                c( "France entière" = "1",
                                                                                                                                                                   "France métropolitaine" = "2",
                                                                                                                                                                   "Territoires d'Outre-mer" = "3")
                                                                                                                                                                
                                                                                                                  )
                                                                                                                  
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(6, align = "center",
                                                                                                                         
                                                                                                                         h3("Ancienneté des référents sur les sites humides",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           
                                                                                                                           
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_P3",height=580),height=581,width=12),
                                                                                                                           
                                                                                                                         ),
                                                                                                                         fluidRow(
                                                                                                                           column(10,offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           ),
                                                                                                                           
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text3_P),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), br(),
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6, align = "center",
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           
                                                                                                                           h3("Fréquentations professionnelle et personnelle des sites humides",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_P4",height=580),height=581,width=12),
                                                                                                                           
                                                                                                                         ),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )   ,
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text4_P),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), br(),
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                  )
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                                br(),
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(6, align="center",
                                                                                                                         
                                                                                                                         h3("Organismes auxquels appartiennent les référents",style="color:rgb(70,105,100)"),
                                                                                                                         selectInput("P60",width=200,"Type d'organismes : ",
                                                                                                                                     c( "Établissements publics" = "1",
                                                                                                                                        "Associations" = "2",
                                                                                                                                        "Autres" = "3")),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           uiOutput("P6_ui")
                                                                                                                         ),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10,offset=1,  align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text5_P),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ),
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  column(6, align = "center", 
                                                                                                                         
                                                                                                                         h3("Participation aux précédentes évaluations",style="color:rgb(70,105,100)"), 
                                                                                                                         br(),br(),br(),br(),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_P5",height=581),height=580,width=12
                                                                                                                           ),
                                                                                                                           
                                                                                                                         ),
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )  ,  
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text6_P),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                     )
                                                                                         )
                                                        )
                                                        
                                                        )
                                               ),
                                               
                                               ### Activités humaines ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Activités humaines et leurs pressions"), icon=icon("city"),
                                                        
                                                        tabsetPanel(type="tabs",tabPanel(strong("ACTIVITÉS ET LEURS PRESSIONS"), br(),
                                                                                         tabsetPanel(type="pills",
                                                                                                     
                                                                                                     
                                                                                                     tabPanel(h5(strong("Répartition des activités humaines")),
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              tabsetPanel(type = "tabs",
                                                                                                                          
                                                                                                                          
                                                                                                                          
                                                                                                                          tabPanel("Activités intrinsèques au site",
                                                                                                                                   fluidPage(      
                                                                                                                                     fluidRow(
                                                                                                                                       column(3),column(6,align="center",selectInput("A0",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                                     c( "France entière" = "1",
                                                                                                                                                                                        "France métropolitaine" = "2",
                                                                                                                                                                                        "Territoires d'Outre-mer" = "3")
                                                                                                                                                                                     
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     fluidRow(   
                                                                                                                                       column(6, align="center",
                                                                                                                                              h3("Nombre de sites humides en fonction du type d'activités humaines",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                box(highchartOutput("plot_A02",height=580),height=581,width=12),
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,  align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text1_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              h3(textOutput("title0"),style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_A01",height=580),height=581,width=12),
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10,  offset=1,align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text2_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                   )
                                                                                                                          ),
                                                                                                                          
                                                                                                                          
                                                                                                                          
                                                                                                                          
                                                                                                                          tabPanel("Activités dans un rayon de 5 km autour du site",
                                                                                                                                   fluidPage(
                                                                                                                                     fluidRow(
                                                                                                                                       column(3),column(6,align="center",selectInput("A00",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                                     c( "France entière" = "1",
                                                                                                                                                                                        "France métropolitaine" = "2",
                                                                                                                                                                                        "Territoires d'Outre-mer" = "3")
                                                                                                                                                                                     
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       column(6, align="center",
                                                                                                                                              h3("Nombre de sites humides en fonction du type d'activités humaines",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("plot_A42",height=580),height=581,width=12),
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,  align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text3_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              h3(textOutput("title4"),style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              box(highchartOutput("plot_A41",height=580),height=581,width=12),
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text4_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                   )
                                                                                                                          )
                                                                                                                          
                                                                                                                          
                                                                                                              )
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     
                                                                                                     tabPanel(h5(strong("Étendue et intensité des activités humaines")),
                                                                                                              fluidPage(  
                                                                                                                
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  tabsetPanel(type = "tabs",
                                                                                                                              
                                                                                                                              
                                                                                                                              tabPanel("Situation actuelle",
                                                                                                                                       
                                                                                                                                       fluidRow(
                                                                                                                                         column(3),column(6,align="center",selectInput("A5",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                                       c( "France entière" = "1",
                                                                                                                                                                                          "France métropolitaine" = "2",
                                                                                                                                                                                          "Territoires d'Outre-mer" = "3")
                                                                                                                                                                                       
                                                                                                                                         )
                                                                                                                                         
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       fluidRow(
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Étendue des activités humaines",style="color:rgb(70,105,100)"),
                                                                                                                                                br(),br(),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_A51",height=620),height=621,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text5_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Intensité des activités humaines",style="color:rgb(70,105,100)"),
                                                                                                                                                prettyRadioButtons("A000",width=150,"Niveau :",
                                                                                                                                                                   c( "Global" = "1",
                                                                                                                                                                      
                                                                                                                                                                      "Local" = "2"), inline = TRUE,
                                                                                                                                                                   animation='pulse',shape="curve",status="success"),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_A52",height=620),height=621,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text6_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                
                                                                                                                                                
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                              ),
                                                                                                                              
                                                                                                                              
                                                                                                                              
                                                                                                                              tabPanel("Évolution entre 2010 et aujourd'hui",
                                                                                                                                       
                                                                                                                                       fluidRow(
                                                                                                                                         column(3),column(6,align="center",selectInput("A55",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                                       c( "France entière" = "1",
                                                                                                                                                                                          "France métropolitaine" = "2",
                                                                                                                                                                                          "Territoires d'Outre-mer" = "3")
                                                                                                                                                                                       
                                                                                                                                         )
                                                                                                                                         
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       fluidRow(
                                                                                                                                         
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Évolution de l'étendue des différentes activités",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_A61",height=620),height=621,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset = 1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text7_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Évolution de l'intensité des différentes activités",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_A62",height=620),height=621,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text8_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                              )
                                                                                                                  )
                                                                                                                )
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                     ),
                                                                                                     tabPanel(h5(strong("Pression des activités humaines par site")),
                                                                                                              fluidPage(
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  br(),
                                                                                                                  column(12, align="center",
                                                                                                                         
                                                                                                                         dataTableOutput("table_A1")
                                                                                                                         
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(12,align="center",
                                                                                                                         p(
                                                                                                                           span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                ), 
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(12,  align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                         
                                                                                                                         p(em(text9_A),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                         
                                                                                                                  )
                                                                                                                ), br(),
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                              
                                                                                                     )
                                                                                         )
                                                        )
                                                        )
                                               ),
                                               
                                               
                                               ### Etendue des milieux ###
                                               
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 5),"Étendue des milieux humides"),icon=icon("envira"),
                                                        tabsetPanel(type="tabs",tabPanel(strong("ÉTENDUE DES MILIEUX HUMIDES"), br(),
                                                                                         
                                                                                         tabsetPanel(type="pills",  
                                                                                                     
                                                                                                     tabPanel(h5(strong("Milieux présents sur les sites humides")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(6, align="center",
                                                                                                                         
                                                                                                                         h3("Part des sites accueillant des milieux doux et salés par typologie",style="color:rgb(70,105,100)"),
                                                                                                                         br(),
                                                                                                                         box(highchartOutput("M8",height=580),height=581,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text1_M),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6, align="center",
                                                                                                                         
                                                                                                                         h3("Nombre de milieux (doux/salés) par site",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         
                                                                                                                         column(12,align="center",
                                                                                                                                dataTableOutput("M7")
                                                                                                                         ),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text2_M),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         
                                                                                                                  )
                                                                                                                  
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Étendue aujourd'hui et leur évolution")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Nombre de sites par type de milieux et leur étendue",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         
                                                                                                                         box(highchartOutput("M1",height=700),height=701,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text3_M),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Évolution de l'étendue par type de milieux entre 2010 et 2020",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         box(highchartOutput("M2",height=700),height=701,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text4_M),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Origines de l'évolution de l'étendue")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Régression de la surface des milieux",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         prettyRadioButtons("M01",width=400,label=NULL,
                                                                                                                                            c( "Origines de régressions" = "1",
                                                                                                                                               
                                                                                                                                               "Types de perte de surface" = "2"),
                                                                                                                                            inline = TRUE,
                                                                                                                                            animation='pulse',shape="curve",status="success"),
                                                                                                                         
                                                                                                                         box(highchartOutput("M3",height=720),height=721,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text5_M),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Extension de la surface des milieux",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         prettyRadioButtons("M02",width=400,label=NULL,
                                                                                                                                            c( "Origines d'extensions" = "1",
                                                                                                                                               
                                                                                                                                               "Types de gain de surface" = "2"),
                                                                                                                                            inline = TRUE,
                                                                                                                                            animation='pulse',shape="curve",status="success"),
                                                                                                                         
                                                                                                                         box(highchartOutput("M4",height=700),height=701,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text6_M),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                         
                                                                                                                         
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                              
                                                                                                     )
                                                                                         )
                                                        )
                                                        )
                                                        
                                               ),
                                               
                                               ### Etat des milieux ###  
                                               
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"État écologique des milieux humides"),icon=icon("leaf"),
                                                        tabsetPanel(type="tabs",tabPanel(strong("ÉTAT ÉCOLOGIQUE DES MILIEUX HUMIDES"), br(),
                                                                                         
                                                                                         tabsetPanel(type="pills",  
                                                                                                     
                                                                                                     tabPanel(h5(strong("État écologique aujourd'hui et leur évolution")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("État écologique par type de milieux",style="color:rgb(70,105,100)"),
                                                                                                                         prettyRadioButtons("MM03",width=150,"Niveau :",
                                                                                                                                            c( "Global" = "1",
                                                                                                                                               
                                                                                                                                               "Local" = "2"), inline = TRUE,
                                                                                                                                            animation='pulse',shape="curve",status="success"),
                                                                                                                         
                                                                                                                         
                                                                                                                         box(highchartOutput("MM1",height=700),height=701,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text1_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Évolution de l'état écologique par type de milieux entre 2010 et 2020",style="color:rgb(70,105,100)"),
                                                                                                                         br(),br(),
                                                                                                                         
                                                                                                                         
                                                                                                                         box(highchartOutput("MM2",height=710),height=711,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text2_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                              )
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Origines de l'évolution de l'état écologique")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Dégradation de l'état écologique des milieux",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         prettyRadioButtons("MM01",width=400,label=NULL,
                                                                                                                                            c( "Origines de dégradations" = "1",
                                                                                                                                               
                                                                                                                                               "Types de perte de surface" = "2"),
                                                                                                                                            inline = TRUE,
                                                                                                                                            animation='pulse',shape="curve",status="success"),
                                                                                                                         
                                                                                                                         
                                                                                                                         box(highchartOutput("MM3",height=720),height=721,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text3_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Restauration de l'état des milieux",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         prettyRadioButtons("MM02",width=400,label=NULL,
                                                                                                                                            c( "Origines d'extensions" = "1",
                                                                                                                                               
                                                                                                                                               "Types de gain de surface" = "2"),
                                                                                                                                            inline = TRUE,
                                                                                                                                            animation='pulse',shape="curve",status="success"),
                                                                                                                         
                                                                                                                         
                                                                                                                         box(highchartOutput("MM4",height=710),height=711,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text4_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                              
                                                                                                     ),
                                                                                                     tabPanel(h5(strong("Mesures de restauration des milieux humides")),
                                                                                                              
                                                                                                              tabsetPanel(type="tabs",
                                                                                                                          tabPanel("Efficacité des mesures de restauration",
                                                                                                                                   fluidPage(
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       column(6,  align="center",
                                                                                                                                              
                                                                                                                                              h3("Efficacité des mesures de restauration des milieux humides",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              prettyRadioButtons("MM07",label=NULL,
                                                                                                                                                                 c( "Milieux humides" = "1",
                                                                                                                                                                    
                                                                                                                                                                    "Typologies" = "2"),
                                                                                                                                                                 inline = TRUE,
                                                                                                                                                                 animation='pulse',shape="curve",status="success"),
                                                                                                                                              
                                                                                                                                              uiOutput("MM7_ui"),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              
                                                                                                                                              uiOutput('comment_MM1')
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       column(6,  align="center",
                                                                                                                                              
                                                                                                                                              h3("Mesures de restauration des milieux humides à renforcer",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              br(),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("MM9",height=580),height=581,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text7_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                       )
                                                                                                                                     )
                                                                                                                                     
                                                                                                                                   )
                                                                                                                          ),
                                                                                                                          
                                                                                                                          
                                                                                                                          tabPanel("Suivi scientifique",
                                                                                                                                   fluidPage(
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       column(6,  align="center",
                                                                                                                                              
                                                                                                                                              h3("Part des sites humides ayant un suivi scientifique",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("MM10",height=560),height=560,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text8_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       column(6,  align="center",
                                                                                                                                              
                                                                                                                                              h3("Part des sites humides ayant un suivi scientifique par typologie",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("MM11",height=560),height=561,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text9_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                   )
                                                                                                                          )
                                                                                                              )
                                                                                                              
                                                                                                     )
                                                                                         )
                                                        )
                                                        )
                                               ),
                                               
                                               
                                               ### Hydrologie ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Hydrologie et hydraulique"),icon=icon('faucet'),
                                                        tabsetPanel(type="tabs",tabPanel(strong("HYDROLOGIE ET HYDRAULIQUE"), br(),
                                                                                         tabsetPanel(type="pills",
                                                                                                     
                                                                                                     tabPanel(h5(strong("Dysfonctionnements hydrologiques")),
                                                                                                              fluidPage(  
                                                                                                                
                                                                                                                
                                                                                                                # Output: Tabset w/ plot, summary, and table ----
                                                                                                                tabsetPanel(type = "tabs",
                                                                                                                            
                                                                                                                            tabPanel("Situation générale",
                                                                                                                                     fluidPage(
                                                                                                                                       fluidRow(
                                                                                                                                         column(6,  align="center",
                                                                                                                                                
                                                                                                                                                h3("Part des sites humides confrontés à au moins un dysfonctionnement hydrologique",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("Dysf_global",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text1_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         column(6,  align="center",
                                                                                                                                                
                                                                                                                                                h3("Part des sites humides par classe de dysfonctionnements par typologie",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_H1",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text2_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                     )
                                                                                                                                     
                                                                                                                            ),
                                                                                                                            tabPanel("Types de dysfonctionnements hydrologiques",
                                                                                                                                     fluidPage(
                                                                                                                                       fluidRow(width = 3.5,
                                                                                                                                                
                                                                                                                                                br(),  
                                                                                                                                                column(8,offset=2,align="center",style="border-radius: 5px;",
                                                                                                                                                       
                                                                                                                                                       div(id="dysf1",prettyRadioButtons("dysf1",tags$div(h4(strong("Type de dysfonctionnements :")),style="color:#466964;text-align:center"),
                                                                                                                                                                                         
                                                                                                                                                                                         choiceNames = list('Perturbation du milieu physique',"Perturbation de la gestion des eaux","Altération de la qualité des eaux"),
                                                                                                                                                                                         choiceValues = c("1","2","3"),outline=TRUE, inline=TRUE,
                                                                                                                                                                                         animation='pulse',shape="curve",status="success"
                                                                                                                                                       )
                                                                                                                                                       )
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       tags$head(tags$style("#dysf1 .pretty{
                                                                         font-size:16px;
                                                                         font-style:italic;
                                                                         color:#466964
                                                                         }
                                                                               ")),
                                                                                                                                       
                                                                                                                                       fluidRow(
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3(textOutput("title1"),style="color:rgb(70,105,100)"), 
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_H2",height=580),height=581,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text3_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         column(6,   align="center",
                                                                                                                                                h3(textOutput("title2"),style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("plot_H3",height=580),height=581,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text4_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                     )
                                                                                                                            ),
                                                                                                                            tabPanel("Dysfonctionnements hydrologiques par site",
                                                                                                                                     fluidPage(
                                                                                                                                       fluidRow(
                                                                                                                                         br(),
                                                                                                                                         column(12,align="center",
                                                                                                                                                
                                                                                                                                                dataTableOutput("table")
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       fluidRow(
                                                                                                                                         column(12,align="center",
                                                                                                                                                p(
                                                                                                                                                  span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         
                                                                                                                                       ), 
                                                                                                                                       
                                                                                                                                       fluidRow(
                                                                                                                                         column(12, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                
                                                                                                                                                p(em(text5_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                
                                                                                                                                         )
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                     )
                                                                                                                                     
                                                                                                                                     
                                                                                                                            )
                                                                                                                            
                                                                                                                )
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Qualité de l'eau")),
                                                                                                              
                                                                                                              fluidPage(
                                                                                                                fluidRow(
                                                                                                                  column(6,  align="center",
                                                                                                                         
                                                                                                                         h3("Qualité de l'eau sur les sites humides",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         box(highchartOutput("QE",height=560),height=561,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text6_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  column(6,  align="center",
                                                                                                                         h3("Évolution de la qualité de l'eau sur les sites humides",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         box(highchartOutput("QE_evo",height=560),height=561,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text7_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                  )
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                              )      
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Niveau de l'eau")),
                                                                                                              fluidPage(
                                                                                                                
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  column(12,  align="center",
                                                                                                                         
                                                                                                                         h3("Évolution du niveau d'eau sur les sites humides",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         
                                                                                                                         box(highchartOutput("NE_evo",height=560)
                                                                                                                             ,width=12,height = 561)
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(5,offset = 4,align="center",
                                                                                                                         
                                                                                                                         p(
                                                                                                                           span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                         
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                                fluidRow(
                                                                                                                  column(5, offset=4, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                         
                                                                                                                         p(em(text8_H),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                         
                                                                                                                  )
                                                                                                                )
                                                                                                                
                                                                                                                
                                                                                                              )      
                                                                                                     )
                                                                                         )
                                                        )
                                                        )
                                                        
                                               ),
                                               
                                               
                                               ### Espèces à protéger ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Faune et flore communes et à forts enjeux"),icon=icon('frog'),
                                                        tabsetPanel(type="tabs",tabPanel(strong("FAUNE ET FLORE COMMUNES ET A FORTS ENJEUX"), br(),
                                                                                         
                                                                                         tabsetPanel(type="pills",
                                                                                                     
                                                                                                     tabPanel(h5(strong("Espèces communes")),
                                                                                                              fluidPage(
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(3),column(6,align="center",selectInput("F00",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                c("France entière" = "1",
                                                                                                                                                                  "France métropolitaine" = "2",
                                                                                                                                                                  "Territoires d'Outre-mer" = "3")
                                                                                                                  )
                                                                                                                  
                                                                                                                  )
                                                                                                                ),
                                                                                                                
                                                                                                                tabsetPanel(type="tabs",
                                                                                                                            
                                                                                                                            tabPanel("État des espèces",
                                                                                                                                     fluidPage(
                                                                                                                                       fluidRow(
                                                                                                                                         column(6,  align="center",
                                                                                                                                                
                                                                                                                                                h3("État de conservation des espèces animales et végétales communes",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                br(),
                                                                                                                                                box(highchartOutput("F1",height=560), height = 561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text1_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3(textOutput("F_title1"),style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                prettyRadioButtons("F001",width=200,label=NULL,
                                                                                                                                                                   c( "Faune" = "1",
                                                                                                                                                                      
                                                                                                                                                                      "Flore" = "2"),
                                                                                                                                                                   inline = TRUE,
                                                                                                                                                                   animation='pulse',shape="curve",status="success"),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("F2",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text2_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         )
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                     )
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                            ),
                                                                                                                            
                                                                                                                            tabPanel("Évolution de l'état des espèces",
                                                                                                                                     fluidPage( 
                                                                                                                                       fluidRow(
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Évolution de l'état de conservation des espèces animales communes",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("F4",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text3_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Évolution de l'état de conservation des espèces végétales communes",style="color:rgb(70,105,100)"),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("F5",height=560),height = 561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text4_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                     )
                                                                                                                            )
                                                                                                                            
                                                                                                                )
                                                                                                                
                                                                                                              )  
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Espèces à forts enjeux")),
                                                                                                              fluidPage(
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(3),column(6,align="center",selectInput("F01",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                c("France entière" = "1",
                                                                                                                                                                  "France métropolitaine" = "2",
                                                                                                                                                                  "Territoires d'Outre-mer" = "3")
                                                                                                                  )
                                                                                                                  
                                                                                                                  ),
                                                                                                                ),
                                                                                                                
                                                                                                                tabsetPanel(type="tabs", 
                                                                                                                            
                                                                                                                            tabPanel("État des espèces",
                                                                                                                                     fluidPage(
                                                                                                                                       fluidRow(
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("État de conservation des espèces animales à forts enjeux",style="color:rgb(70,105,100)"),
                                                                                                                                                prettyRadioButtons("F_choice1",width=400,label=NULL,
                                                                                                                                                                   c( "Tous sites confondus" = "1",
                                                                                                                                                                      
                                                                                                                                                                      "Par typologie de site" = "2"),inline=TRUE,
                                                                                                                                                                   animation='pulse',shape="curve",status="success"
                                                                                                                                                ),
                                                                                                                                                
                                                                                                                                                uiOutput("ui1"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("F6",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text5_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("État de conservation des espèces végétales à forts enjeux",style="color:rgb(70,105,100)"),
                                                                                                                                                prettyRadioButtons("F_choice2",width=400,label=NULL,
                                                                                                                                                                   c( "Tous sites confondus" = "1",
                                                                                                                                                                      
                                                                                                                                                                      "Par typologie de site" = "2"), inline=TRUE,
                                                                                                                                                                   animation='pulse',shape="curve",status="success"
                                                                                                                                                ),
                                                                                                                                                
                                                                                                                                                uiOutput("ui2"),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                                box(highchartOutput("F7",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text6_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         )
                                                                                                                                         
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                     )
                                                                                                                                     
                                                                                                                                     
                                                                                                                            ),
                                                                                                                            
                                                                                                                            tabPanel("Évolution de l'état des espèces",
                                                                                                                                     fluidPage(
                                                                                                                                       fluidRow(
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Évolution de l'état de conservation des espèces animales protégées",style="color:rgb(70,105,100)"),
                                                                                                                                                prettyRadioButtons("F_choice3",label="Niveau :",
                                                                                                                                                                   c( "National" = "1",
                                                                                                                                                                      
                                                                                                                                                                      "Local" = "2"), inline = TRUE,
                                                                                                                                                                   animation='pulse',shape="curve",status="success"),
                                                                                                                                                
                                                                                                                                                box(highchartOutput("F8",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text7_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         column(6,  align="center",
                                                                                                                                                h3("Évolution de l'état de conservation des espèces végétales protégées",style="color:rgb(70,105,100)"),
                                                                                                                                                prettyRadioButtons("F_choice4",label="Niveau :",
                                                                                                                                                                   c( "National" = "1",
                                                                                                                                                                      
                                                                                                                                                                      "Local" = "2"), inline = TRUE,
                                                                                                                                                                   animation='pulse',shape="curve",status="success"),
                                                                                                                                                box(highchartOutput("F9",height=560),height=561,width=12),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1,
                                                                                                                                                         
                                                                                                                                                         
                                                                                                                                                         p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                         p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                ), 
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text8_F),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                         )
                                                                                                                                         
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                     )
                                                                                                                            )
                                                                                                                )
                                                                                                                
                                                                                                              )
                                                                                                     )
                                                                                         )
                                                                                         
                                                                                         
                                                        )
                                                        )
                                                        
                                               ),
                                               
                                               ### Espèces menaçantes ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Faune et flore susceptibles de menacer le site"),icon=icon('pastafarianism'),
                                                        tabsetPanel(type="tabs",tabPanel(strong("FAUNE ET FLORE SUSCEPTIBLES DE MENACER LE SITE"), br(),
                                                                                         fluidPage(  
                                                                                           
                                                                                           tabsetPanel(type="pills",
                                                                                                       
                                                                                                       tabPanel(h5(strong("Situation générale")),
                                                                                                                fluidPage(
                                                                                                                  fluidRow(
                                                                                                                    
                                                                                                                    column(3),column(6,align="center",selectInput("EE1",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                  c( 
                                                                                                                                                                    "France métropolitaine" = "2",
                                                                                                                                                                    "Territoires d'Outre-mer" = "3")
                                                                                                                    )
                                                                                                                    
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  sidebarLayout(
                                                                                                                    
                                                                                                                    
                                                                                                                    sidebarPanel(
                                                                                                                      
                                                                                                                      selectInput("EE_choice1",label=HTML("<p style='color:#f5f5ff;font-size:15px'>Choix du sujet :</p>"),
                                                                                                                                  c( "Part des sites confrontés aux espèces problématiques" = "1",
                                                                                                                                     
                                                                                                                                     "Nombre moyen d'espèces sur les sites humides" = "2",
                                                                                                                                     "Espèces les plus présentes sur les sites humides" = "3"), 
                                                                                                                                  selectize=FALSE,multiple=TRUE,selected="1"),
                                                                                                                      
                                                                                                                      br(),br(),
                                                                                                                      
                                                                                                                      prettyCheckboxGroup(
                                                                                                                        inputId = "EE_choice3",
                                                                                                                        label = NULL,
                                                                                                                        shape="curve",
                                                                                                                        icon = icon("crow"),
                                                                                                                        
                                                                                                                        choiceNames = list("Espèces exotiques envahissantes",
                                                                                                                                           "Espèces indigènes à fort développement"),
                                                                                                                        choiceValues = list("1","2"),              
                                                                                                                        animation = "tada",
                                                                                                                        status = "success",
                                                                                                                        selected="1"
                                                                                                                      ),
                                                                                                                      
                                                                                                                      br(),br(),
                                                                                                                      
                                                                                                                      
                                                                                                                      tags$head(tags$style("#EE_choice3 {color: #f5f5ff;
                                 font-size: 15px;
                                 
                                 }"
                                                                                                                      )
                                                                                                                      ),
                                                                                                                      
                                                                                                                      
                                                                                                                      uiOutput("ui4"),
                                                                                                                      br(),
                                                                                                                      
                                                                                                                      uiOutput("ui6")
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                      
                                                                                                                    ),
                                                                                                                    
                                                                                                                    mainPanel(
                                                                                                                      
                                                                                                                      uiOutput("ui5"),
                                                                                                                      fluidRow(
                                                                                                                        column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                               
                                                                                                                               p(em(text1_EE),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                               
                                                                                                                        )
                                                                                                                      )
                                                                                                                      
                                                                                                                    )
                                                                                                                    
                                                                                                                  )
                                                                                                                )
                                                                                                                
                                                                                                                
                                                                                                       ),
                                                                                                       
                                                                                                       tabPanel(h5(strong("Espèces problématiques rencontrées et leur étendue")),
                                                                                                                fluidPage(  
                                                                                                                  fluidRow(
                                                                                                                    
                                                                                                                    column(3),column(6,align="center",selectInput("EE2",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                  c( 
                                                                                                                                                                    "France métropolitaine" = "2",
                                                                                                                                                                    "Territoires d'Outre-mer" = "3")
                                                                                                                    )
                                                                                                                    
                                                                                                                    ),
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  tabsetPanel(type="tab",
                                                                                                                              
                                                                                                                              tabPanel("Étendue des espèces exotiques",
                                                                                                                                       fluidPage(   
                                                                                                                                         fluidRow(
                                                                                                                                           
                                                                                                                                           column(6, align = "center",
                                                                                                                                                  
                                                                                                                                                  
                                                                                                                                                  h3(textOutput("EE_title5"),style="color:rgb(70,105,100)"),
                                                                                                                                                  
                                                                                                                                                  box(highchartOutput("plot_EE5",height=650),height=651,width=12),
                                                                                                                                                  fluidRow(
                                                                                                                                                    column(10, offset=1,
                                                                                                                                                           
                                                                                                                                                           
                                                                                                                                                           p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                           p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                           
                                                                                                                                                    )
                                                                                                                                                  ), 
                                                                                                                                                  fluidRow(
                                                                                                                                                    column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                           
                                                                                                                                                           p(em(text2_EE),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                           
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                  
                                                                                                                                           ),
                                                                                                                                           
                                                                                                                                           column(6, align = "center",
                                                                                                                                                  
                                                                                                                                                  h3(textOutput("EE_title6"),style="color:rgb(70,105,100)"),
                                                                                                                                                  
                                                                                                                                                  box(highchartOutput("plot_EE6",height=650),height=651,width=12),
                                                                                                                                                  fluidRow(
                                                                                                                                                    column(10, offset=1,
                                                                                                                                                           
                                                                                                                                                           
                                                                                                                                                           p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                           p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                           
                                                                                                                                                    )
                                                                                                                                                  ), 
                                                                                                                                                  fluidRow(
                                                                                                                                                    column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                           
                                                                                                                                                           p(em(text3_EE),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                           
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                  
                                                                                                                                           )
                                                                                                                                           
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         
                                                                                                                                       )
                                                                                                                                       
                                                                                                                              ),
                                                                                                                              
                                                                                                                              tabPanel("Espèces exotiques et indigènes par site",
                                                                                                                                       fluidPage(
                                                                                                                                         fluidRow(
                                                                                                                                           br(),
                                                                                                                                           column(12,align="center",
                                                                                                                                                  dataTableOutput("table_EE1")
                                                                                                                                           ),
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         fluidRow(
                                                                                                                                           column(12,align="center",
                                                                                                                                                  p(
                                                                                                                                                    span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                  
                                                                                                                                           ),
                                                                                                                                           
                                                                                                                                           
                                                                                                                                         ),
                                                                                                                                         
                                                                                                                                         fluidRow(
                                                                                                                                           column(12, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                  
                                                                                                                                                  p(em(text4_EE),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                  
                                                                                                                                           )
                                                                                                                                         )
                                                                                                                                         
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                       
                                                                                                                              )
                                                                                                                              
                                                                                                                  )
                                                                                                                )
                                                                                                                
                                                                                                       ),
                                                                                                       
                                                                                                       tabPanel(h5(strong("Causes des interventions et modalités de gestion ou de limitation")),
                                                                                                                fluidPage(  
                                                                                                                  fluidRow(
                                                                                                                    
                                                                                                                    column(3),column(6,align="center",selectInput("EE3",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                  c( 
                                                                                                                                                                    "France métropolitaine" = "2",
                                                                                                                                                                    "Territoires d'Outre-mer" = "3")
                                                                                                                    )
                                                                                                                    
                                                                                                                    ),
                                                                                                                  ),
                                                                                                                  
                                                                                                                  fluidRow(
                                                                                                                    
                                                                                                                    column(6, align = "center",
                                                                                                                           
                                                                                                                           
                                                                                                                           h3("Part des sites humides suivant les causes d'intervention",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_EE7",height=600),height=600,width=12),
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1,
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                    p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           ),
                                                                                                                           
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                    
                                                                                                                                    p(em(text5_EE),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           )
                                                                                                                           
                                                                                                                    ),
                                                                                                                    
                                                                                                                    column(6, align = "center",
                                                                                                                           
                                                                                                                           h3("Part des sites humides suivant les modalités de gestion",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_EE8",height=600),height=600,width=12),
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1,
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                    p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           ), 
                                                                                                                           
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                    
                                                                                                                                    p(em(text6_EE),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           )
                                                                                                                           
                                                                                                                    )
                                                                                                                    
                                                                                                                  ),
                                                                                                                  
                                                                                                                )
                                                                                                       )
                                                                                           )
                                                                                         )
                                                                                         
                                                        )
                                                        )
                                               ),
                                               
                                               
                                               ### Changement climatique ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Effets potentiels ou perçus du changement climatique"),icon=icon("cloud-sun"),
                                                        tabsetPanel(type="tabs",tabPanel(strong("EFFETS POTENTIELS OU PERÇUS DU CHANGEMENT CLIMATIQUE"), br(),
                                                                                         fluidPage(  
                                                                                           
                                                                                           tabsetPanel(type = "pills",
                                                                                                       
                                                                                                       tabPanel(h5(strong("Situation générale")),
                                                                                                                fluidPage(   
                                                                                                                  fluidRow(
                                                                                                                    column(6,  align="center",
                                                                                                                           
                                                                                                                           h3("Part des sites humides confrontés à un phénomène lié au changement climatique",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_C1",height=560),height = 560,width=12),
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1,
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                    p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           ), 
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                    
                                                                                                                                    p(em(text1_C),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                    ),
                                                                                                                    
                                                                                                                    column(6,  align="center",
                                                                                                                           
                                                                                                                           h3("Nombre moyen de phénomènes observables par typologie et par type de phénomènes",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_C2", height=600),height=601,width=12),
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1,
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                    p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           ), 
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                    
                                                                                                                                    p(em(text2_C),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           )
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                )
                                                                                                                
                                                                                                       ),
                                                                                                       
                                                                                                       tabPanel(h5(strong("Ampleur et intensité des phénomènes observés")),
                                                                                                                
                                                                                                                fluidPage(
                                                                                                                  
                                                                                                                  fluidRow(
                                                                                                                    column(3),column(6,align="center",selectInput("C3",width=200,h5(strong("Territoires concernés :")),
                                                                                                                                                                  c( "France entière" = "1",
                                                                                                                                                                     "France métropolitaine" = "2",
                                                                                                                                                                     "Territoires d'Outre-mer" = "3")
                                                                                                                                                                  
                                                                                                                    )
                                                                                                                    
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  fluidRow(
                                                                                                                    column(6,  align="center",
                                                                                                                           h3("Ampleur des phénomènes observés",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_C3", height=700),height=701,width=12),
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1,
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                    p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           ), 
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                    
                                                                                                                                    p(em(text3_C),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                    ),
                                                                                                                    
                                                                                                                    
                                                                                                                    column(6,  align="center",
                                                                                                                           h3("Intensité des phénomènes observés",style="color:rgb(70,105,100)"),
                                                                                                                           
                                                                                                                           box(highchartOutput("plot_C4", height=700),height=701,width=12),
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1,
                                                                                                                                    
                                                                                                                                    
                                                                                                                                    p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                    p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           ), 
                                                                                                                           fluidRow(
                                                                                                                             column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                    
                                                                                                                                    p(em(text4_C),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                    
                                                                                                                             )
                                                                                                                           )
                                                                                                                           
                                                                                                                           
                                                                                                                    )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                )
                                                                                                                
                                                                                                                
                                                                                                       ),
                                                                                                       
                                                                                                       tabPanel(h5(strong("Phénomènes observés par site")),
                                                                                                                fluidPage(  
                                                                                                                  fluidRow(
                                                                                                                    br(),
                                                                                                                    column(12,align="center",
                                                                                                                           dataTableOutput("C5")
                                                                                                                    ),
                                                                                                                    fluidRow(
                                                                                                                      column(12,align="center",
                                                                                                                             p(
                                                                                                                               span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                             p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                             
                                                                                                                      ),
                                                                                                                      
                                                                                                                      
                                                                                                                    ),
                                                                                                                    
                                                                                                                    fluidRow(
                                                                                                                      column(12, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                             
                                                                                                                             p(em(text5_C),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                             
                                                                                                                      )
                                                                                                                    )
                                                                                                                    
                                                                                                                  )
                                                                                                                )
                                                                                                       )
                                                                                           )
                                                                                         )
                                                        )
                                                        )
                                               ),
                                               
                                               ### Services rendus ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Services rendus et perception sociale"),icon=icon("hands"),
                                                        tabsetPanel(type="tabs",tabPanel(strong("SERVICES RENDUS ET PERCEPTION SOCIALE"), br(),
                                                                                         
                                                                                         tabsetPanel(type="pills",
                                                                                                     
                                                                                                     tabPanel(h5(strong("Services rendus à la société")),
                                                                                                              
                                                                                                              tabsetPanel(type="tab",
                                                                                                                          
                                                                                                                          tabPanel("Types de services rendus",
                                                                                                                                   fluidPage( 
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       column(6, align = "center",
                                                                                                                                              br(),br(),
                                                                                                                                              h3("Importance des services rendus selon leur type",style="color:rgb(70,105,100)"), 
                                                                                                                                              br(),br(),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("S1",height=540),height=541,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text1_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3(textOutput("S_title1"),style="color:rgb(70,105,100)"),
                                                                                                                                              selectInput("S00",width=200,"Type de services : ",
                                                                                                                                                          c( "Biens" = "1",
                                                                                                                                                             "Services de régulation" = "2",
                                                                                                                                                             "Services culturels" = "3")),
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              uiOutput("ui_S0"),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text2_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                   )
                                                                                                                                   
                                                                                                                          ),
                                                                                                                          
                                                                                                                          tabPanel("Opérabilité des services rendus",
                                                                                                                                   
                                                                                                                                   fluidPage(
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6, align = "center",
                                                                                                                                              
                                                                                                                                              h3("Niveau d'opérabilité des services",style="color:rgb(70,105,100)"),
                                                                                                                                              prettyRadioButtons("S01",label=NULL,
                                                                                                                                                                 c( "Type de milieux" = "1",
                                                                                                                                                                    
                                                                                                                                                                    "Typologie de sites" = "2"), inline = TRUE,
                                                                                                                                                                 animation='pulse',shape="curve",status="success"),
                                                                                                                                              
                                                                                                                                              uiOutput("ui_S1"),
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10,offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                ),
                                                                                                                                                fluidRow(
                                                                                                                                                  column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                         
                                                                                                                                                         p(em(text3_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                         
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                
                                                                                                                                                
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       column(6, align = "center",
                                                                                                                                              
                                                                                                                                              h3("Évolution du niveau d'opérabilité des services",style="color:rgb(70,105,100)"),
                                                                                                                                              prettyRadioButtons("S02",label=NULL,
                                                                                                                                                                 c( "Type de milieux" = "1",
                                                                                                                                                                    
                                                                                                                                                                    "Typologie de sites" = "2"), inline = TRUE,
                                                                                                                                                                 animation='pulse',shape="curve",status="success"),
                                                                                                                                              
                                                                                                                                              uiOutput("ui_S2"),
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10,offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                ),
                                                                                                                                                
                                                                                                                                                
                                                                                                                                              ),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text4_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                   )
                                                                                                                                   
                                                                                                                          ),
                                                                                                                          
                                                                                                                          tabPanel("Services rendus par site",
                                                                                                                                   fluidPage(
                                                                                                                                     fluidRow(
                                                                                                                                       column(12,align="center",
                                                                                                                                              br(),
                                                                                                                                              dataTableOutput("S7"),
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                       
                                                                                                                                     ),
                                                                                                                                     fluidRow(
                                                                                                                                       column(12,align="center",
                                                                                                                                              p(
                                                                                                                                                span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                              p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       column(12, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                              
                                                                                                                                              p(em(text5_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                              
                                                                                                                                       )
                                                                                                                                     )
                                                                                                                                     
                                                                                                                                   )
                                                                                                                                   
                                                                                                                          )
                                                                                                                          
                                                                                                              )
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     
                                                                                                     tabPanel(h5(strong("Attitude des acteurs territoriaux vis-à-vis du site humide")),
                                                                                                              
                                                                                                              tabsetPanel(type="tabs",    
                                                                                                                          
                                                                                                                          tabPanel("Perception sociale des acteurs territoriaux",         
                                                                                                                                   
                                                                                                                                   fluidPage(  
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6, align = "center",
                                                                                                                                              
                                                                                                                                              h3("Perception sociale supposée des acteurs territoriaux",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("S8",height=560),height=561,width=12)
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       column(6, align = "center",
                                                                                                                                              
                                                                                                                                              h3("Évolution de la perception sociale entre 2010 et 2020",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("S10",height=560),height=561,width=12)
                                                                                                                                              
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6,
                                                                                                                                              column(10, offset=1,
                                                                                                                                                     p(
                                                                                                                                                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                     
                                                                                                                                              ),
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       column(6,
                                                                                                                                              column(10, offset=1,
                                                                                                                                                     p(
                                                                                                                                                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                     
                                                                                                                                              )
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     fluidRow(
                                                                                                                                       column(6,
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text6_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       column(6,
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text7_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                     )
                                                                                                                                   )
                                                                                                                          ),
                                                                                                                          
                                                                                                                          
                                                                                                                          tabPanel("Impact sur le comportement des acteurs territoriaux",
                                                                                                                                   
                                                                                                                                   fluidPage(    
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3("Impact supposé sur le comportement des acteurs territoriaux",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("S9",height=560),height=561,width=12)
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3("Évolution de l'impact supposé sur le comportement entre 2010 et 2020",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("S11",height=560),height=561,width=12)
                                                                                                                                              
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6,
                                                                                                                                              column(10,offset=1,
                                                                                                                                                     p(
                                                                                                                                                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                     
                                                                                                                                              ),
                                                                                                                                       ),
                                                                                                                                       column(6,
                                                                                                                                              column(10,offset=1,
                                                                                                                                                     p(
                                                                                                                                                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                     
                                                                                                                                              )
                                                                                                                                       )
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       column(6,
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text8_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       column(6,
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text9_S),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                     )
                                                                                                                                   )
                                                                                                                          )
                                                                                                                          
                                                                                                              )
                                                                                                     )
                                                                                         )
                                                        )
                                                        
                                                        )
                                               ),
                                               
                                               
                                               ### Bilan ###
                                               
                                               tabPanel(paste(stri_dup(intToUtf8(160), 2),"Bilan 2010-2020 et perspectives 2020-2030"),icon=icon("clipboard-list"),
                                                        tabsetPanel(type="tabs",tabPanel(strong("BILAN 2010-2020 ET PERSPECTIVES 2020-2030"), br(),
                                                                                         tabsetPanel(type="pills",
                                                                                                     
                                                                                                     tabPanel(h5(strong("Bilan 2010-2020")),
                                                                                                              
                                                                                                              tabsetPanel(type="tabs",
                                                                                                                          
                                                                                                                          tabPanel("Avis de synthèse sur l'évolution des sites humides",
                                                                                                                                   fluidPage(
                                                                                                                                     
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3("Avis de synthèse des référents sur l'évolution des sites humides entre 2010 et 2020",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              
                                                                                                                                              box(highchartOutput("B1",height=560),height=561,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text1_B),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3("Avis de synthèse des référents sur l'évolution des sites humides par typologie",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("B2",height=560),height=561,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text2_B),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                   )
                                                                                                                                   
                                                                                                                          ),
                                                                                                                          
                                                                                                                          
                                                                                                                          tabPanel("Facteurs d'évolution des sites humides",
                                                                                                                                   fluidPage(    
                                                                                                                                     fluidRow(
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3("Facteurs de dégradation potentiels des sites humides",style="color:rgb(70,105,100)"),
                                                                                                                                              br(),
                                                                                                                                              box(highchartOutput("B5",height=600),height=601,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text3_B),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       ),
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       column(6, align="center",
                                                                                                                                              
                                                                                                                                              h3("Facteurs d'amélioration potentiels des sites humides",style="color:rgb(70,105,100)"),
                                                                                                                                              
                                                                                                                                              box(highchartOutput("B6",height=600),height=601,width=12),
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1,
                                                                                                                                                       
                                                                                                                                                       
                                                                                                                                                       p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              ), 
                                                                                                                                              fluidRow(
                                                                                                                                                column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                                       
                                                                                                                                                       p(em(text4_B),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                                       
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                              
                                                                                                                                       )
                                                                                                                                       
                                                                                                                                     ),
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                     
                                                                                                                                   )
                                                                                                                          )
                                                                                                              )
                                                                                                              
                                                                                                     ),
                                                                                                     
                                                                                                     tabPanel(h5(strong("Perspectives 2020-2030")),
                                                                                                              fluidPage(
                                                                                                                fluidRow(
                                                                                                                  
                                                                                                                  column(6, align="center",
                                                                                                                         
                                                                                                                         h3("Avis de synthèse des référents sur l'avenir des sites humides",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         box(highchartOutput("B3",height=560),height=561,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text5_B),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                  ),
                                                                                                                  
                                                                                                                  
                                                                                                                  
                                                                                                                  column(6, align="center",
                                                                                                                         
                                                                                                                         h3("Avis de synthèse des référents sur l'avenir des sites humides par typologie",style="color:rgb(70,105,100)"),
                                                                                                                         
                                                                                                                         box(highchartOutput("B4",height=560),height=561,width=12),
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1,
                                                                                                                                  
                                                                                                                                  
                                                                                                                                  p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                                                                                  p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         ), 
                                                                                                                         fluidRow(
                                                                                                                           column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                                                                                  
                                                                                                                                  p(em(text6_B),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                                                                                  
                                                                                                                           )
                                                                                                                         )
                                                                                                                         
                                                                                                                  )
                                                                                                                  
                                                                                                                ),
                                                                                                                
                                                                                                                
                                                                                                                
                                                                                                              )
                                                                                                     )
                                                                                         )
                                                                                         
                                                        )
                                                        
                                                        )
                                                        
                                               )
                                    ),
                                    
                                    
                                    ### Multi-thématiques ###
                                    
                                    navbarMenu("Croisements multi-thématiques",
                                               
                                               tabPanel("Activités humaines et état écologique",
                                                        
                                                        fluidPage(
                                                          
                                                          
                                                          
                                                          
                                                          
                                                          
                                                          fluidRow(
                                                            
                                                            column(12,align="center",
                                                                   
                                                                   h3("Influence des activités humaines sur l'état écologique des milieux humides",style="font-size:18px;font-weight:bold;color:rgb(70,105,100)"),
                                                                   
                                                            )
                                                            
                                                          ),
                                                          
                                                          fluidRow(
                                                            
                                                            column(5, align="center",
                                                                   
                                                                   h3("Répartition des sites humides en fonction du nombre d'activités",style="color:rgb(70,105,100)"),
                                                                   br(),br(),
                                                                   highchartOutput("CR_A_MM1",height=400)
                                                                   
                                                            ),
                                                            
                                                            column(7, align="center",
                                                                   
                                                                   h3("Indice d'état écologique selon le nombre d'activités",style="color:rgb(70,105,100)"),
                                                                   prettyRadioButtons("input_CR2", label=NULL, 
                                                                                      
                                                                                      
                                                                                      choices = list("Écarts d'indice"="1","Boîtes à moustache" = "2", "Régression linéaire"="3"),
                                                                                      inline = TRUE,
                                                                                      animation='pulse',shape="curve",status="success", icon=icon("chart-bar"),fill=FALSE,plain=FALSE,selected="3"),
                                                                   br(),
                                                                   highchartOutput("CR_A_MM3",height=400)
                                                                   
                                                            ),
                                                            
                                                            
                                                          ),
                                                          
                                                          
                                                          fluidRow(
                                                            
                                                            column(5,align="center",
                                                                   p(
                                                                     span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                            ),
                                                            
                                                            column(6, offset=1, align="center",
                                                                   p(
                                                                     span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                            )
                                                            
                                                            
                                                          ),
                                                          
                                                          br(),
                                                          fluidRow(
                                                            
                                                            column(5, align="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                   
                                                                   p(em("Ce graphique montre la répartition des sites humides en fonction du nombre d'activités selon que leur indice d'état écologique est inférieur à 0,5 (mauvais état) ou supérieur à 0,5 (bon état). Une densité élevée pour une valeur d'activités donnée signifie qu'un grand nombre de sites sont concernés par ce nombre d'activités. Les sites dont les milieux humides sont en bon état ont globalement un nombre d'activités inférieur à ceux dont les milieux sont en mauvais état."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                            ),
                                                            
                                                            
                                                            uiOutput("comment1_CR_A_MM")
                                                            
                                                            
                                                          ),
                                                          
                                                          br(),
                                                          
                                                          fluidRow(
                                                            
                                                            column(3,
                                                                   br(),br(),
                                                                   sidebarPanel(id="well0",width=3,
                                                                                
                                                                                prettyRadioButtons("input_CR1", HTML("<p style='font-size:15px;text-align:center'>Choix du territoire : </p>"), 
                                                                                                   
                                                                                                   
                                                                                                   choices = list("France entière"="1","Littoral atlantique, Manche et mer du Nord"="2","Littoral méditerranéen"="3",
                                                                                                                  "Massif à tourbières"="4", "Plaines intérieures"="5","Vallées alluviales"="6","Outre-mer"="7"),
                                                                                                   inline = FALSE,
                                                                                                   animation='tada',shape="curve",status="success", icon=icon("map-pin"),bigger=TRUE)
                                                                   )
                                                            ),
                                                            
                                                            tags$head(tags$style("#input_CR1{color: #f5f5ff;
                                 font-size: 15px;
                                 
                                 }"
                                                            )
                                                            ),
                                                            
                                                            tags$head(tags$style("#well0 {
                                 width: 350px;
                                 
                                 }"
                                                            )
                                                            ),
                                                            
                                                            
                                                            column(9,align="center",
                                                                   
                                                                   h3("Part des sites humides selon leur état en fonction du nombre d'activités",style="color:rgb(70,105,100)"),
                                                                   box(highchartOutput("CR_A_MM2",height=400),height=400,width=12)
                                                                   
                                                                   
                                                            )
                                                            
                                                          ),
                                                          fluidRow(
                                                            column(9,offset=3,
                                                                   column(11,offset=1,
                                                                          p(
                                                                            span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                          p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                   ))
                                                          ),
                                                          
                                                          fluidRow(
                                                            
                                                            column(9,offset=3,align="center",
                                                                   column(11,offset=1,style="border: 1px dotted #466964;border-radius: 5px",
                                                                          p(em(" Sur l'ensemble des sites, les activités humaines semblent avoir un effet sur l'état écologique des milieux. En effet, la part de sites en mauvais état augmente avec le nombre d'activités présentes sur le site. Cet effet paraît plus ou moins important selon la typologie du site."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                   ))
                                                          )
                                                          
                                                          
                                                          
                                                          
                                                        )
                                                        
                                               ),
                                               
                                               
                                               
                                               
                                               
                                               tabPanel("Espèces menaçantes et espèces à protéger",
                                                        
                                                        fluidPage(
                                                          
                                                          
                                                          fluidRow(
                                                            
                                                            column(12,align="center",
                                                                   
                                                                   h3("Influence des espèces exotiques et indigènes sur l'état des espèces à protéger",style="font-size:18px;font-weight:bold;color:rgb(70,105,100)"),
                                                                   
                                                            )
                                                            
                                                          ),
                                                          
                                                          
                                                          fluidRow(
                                                            
                                                            sidebarPanel(id="well1",width=3,
                                                                         
                                                                         
                                                                         
                                                                         div(id="input_CR3",prettyRadioButtons("input_CR3",label=HTML("<p style='color:#f5f5ff;font-size:15px'>Choix du territoire : </p>"),
                                                                                                               c( "France entière" = "1",
                                                                                                                  
                                                                                                                  "France métropolitaine" = "2",
                                                                                                                  "Outre-mer" = "3"), 
                                                                                                               inline = FALSE,
                                                                                                               animation='tada',shape="curve",status="success", icon=icon("map-pin"),bigger=TRUE)),
                                                                         
                                                                         br(),
                                                                         prettyRadioButtons("input_CR4", HTML("<p style='font-size:15px;text-align:center'>Espèces à protéger : </p>"), 
                                                                                            
                                                                                            
                                                                                            choices = list("Animales"="1","Végétales"="2"),
                                                                                            inline = TRUE,
                                                                                            animation='tada',shape="curve",status="success",bigger=TRUE),
                                                                         
                                                                         
                                                                         br(), br(),
                                                                         
                                                                         uiOutput("ui_CR1")
                                                            ),
                                                            
                                                            
                                                            tags$head(tags$style("#input_CR3{color: #f5f5ff;
                                 font-size: 15px;
                                 
                                 }"
                                                            )
                                                            ),
                                                            
                                                            tags$head(tags$style("#input_CR4{color: #f5f5ff;
                                 font-size: 15px;
                                 text-align:center
                                 }"
                                                            )
                                                            ),
                                                            
                                                            tags$head(tags$style("#input_CR5 .form-control{
                                 font-size: 15px;
                                 height:60px;
                                 font-weight:bold
                                 }"
                                                            )
                                                            ),
                                                            
                                                            tags$head(tags$style("#input_CR6 .form-control{
                                 font-size: 15px;
                                 height:60px;
                                 font-weight:bold
                                 }"
                                                            )
                                                            ),
                                                            
                                                            
                                                            
                                                            column(9, align="center", 
                                                                   
                                                                   
                                                                   h3("Répartition des sites humides en fonction du nombre d'espèces menaçantes",style="color:rgb(70,105,100)"),
                                                                   box(highchartOutput("CR_F_EE3",height=350),height=350,width=12)
                                                                   
                                                                   
                                                            ),
                                                            
                                                            column(9, offset=3,align="center",
                                                                   
                                                                   column(10,offset=1,
                                                                          p(
                                                                            span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                          p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                   )),
                                                            
                                                            column(9, offset=3,align="center",
                                                                   column(10,offset=1,style="border: 1px dotted #466964;border-radius: 5px",
                                                                          p(em("Ce graphique illustre la répartition des sites humides en fonction du nombre d'espèces menaçantes selon que l'état des espèces à protéger est inférieur à 0,5 (mauvais état) ou supérieur à 0,5 (bon état). Une forte densité pour un nombre d'espèces menaçantes données traduit le fait que beaucoup de sites sont concernés par cette valeur. En France métropolitaine, les sites dont les espèces se portent bien ont globalement un nombre d'espèces menaçantes inférieur à ceux dont les espèces se portent mal, qu'il s'agisse de la faune ou de la flore. Ce n'est pas le cas en Outre-mer."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                   ))
                                                            
                                                          ),
                                                          
                                                          fluidRow(
                                                            column(7,align="center",
                                                                   h3("Nombre d'espèces menaçantes en fonction de l'état des espèces à protéger",style="color:rgb(70,105,100)"),
                                                                   prettyRadioButtons("input_CR7", label=NULL, 
                                                                                      
                                                                                      
                                                                                      choices = list("Écarts d'indice"="1","Boîtes à moustache" = "2"),
                                                                                      inline = TRUE,
                                                                                      animation='pulse',shape="curve",status="success", icon=icon("chart-bar"),fill=FALSE,plain=FALSE,selected="2"),
                                                                   br(),
                                                                   box(highchartOutput("CR_F_EE2",height=350),height=350,width=12),
                                                                   column(7, 
                                                                          p(em("Note : les éventuels outliers (données statistiquement aberrantes) ont été retirés."))    
                                                                   ),
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   
                                                            ),
                                                            
                                                            column(5, align="center",
                                                                   
                                                                   fluidRow(
                                                                     
                                                                     h3("Part des sites humides dont l'état des espèces à protéger est bon ou très bon",style="color:rgb(70,105,100)"),
                                                                     br(),br(),
                                                                     box(highchartOutput("CR_F_EE1",height=350),height=350,width=12),
                                                                     
                                                                     
                                                                     
                                                                     
                                                                   )
                                                            )
                                                          ),
                                                          
                                                          fluidRow(
                                                            column(6, 
                                                                   
                                                                   column(12,offset=1,
                                                                          p(
                                                                            span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                          p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;"),
                                                                   ),
                                                                   
                                                                   fluidRow(
                                                                     
                                                                     uiOutput("comment1_CR_F_EE")
                                                                     
                                                                   ),
                                                            ),
                                                            column(5, offset=1,
                                                                   p(
                                                                     span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;"),
                                                                   
                                                                   fluidRow(
                                                                     
                                                                     
                                                                     column(12, align="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                            
                                                                            p(em("En France métropolitaine, la part des espèces à protéger en bon (ou très bon) état diminue lorsque le nombre d'espèces menaçantes augmente. Rien de tel n'a pu être montré pour les Outre-mer."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                            
                                                                            
                                                                     )
                                                                     
                                                                   )  
                                                            )
                                                          )
                                                          
                                                        )
                                               ),
                                               tabPanel("Étendue et état écologique",
                                                        
                                                        fluidPage(
                                                          
                                                          fluidRow(
                                                            
                                                            column(12,align="center",
                                                                   
                                                                   h3("Influence de l'étendue des milieux sur leur état écologique",style="font-size:18px;font-weight:bold;color:rgb(70,105,100)"),
                                                                   
                                                            )
                                                            
                                                          ),
                                                          
                                                          fluidRow(
                                                            
                                                            column(12, align="center",
                                                                   h3("Indice d'état écologique en fonction de l'indice d'étendue",style="color:rgb(70,105,100)"),
                                                                   highchartOutput("CR_M_MM1",height=400)
                                                                   
                                                            )
                                                            
                                                          ),
                                                          
                                                          column(5,
                                                                 p(em("Note : la taille des cercles du graphique est proportionnelle à la surface du site humide."))    
                                                          ),
                                                          br(),
                                                          
                                                          column(7, offset=3,
                                                                 p(
                                                                   span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                 p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                          ),
                                                          fluidRow(
                                                            
                                                            column(7,offset=3,align="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                   
                                                                   p(em("Le graphique ci-dessus représente l'indice d'état écologique de chaque site en fonction de l'indice d'étendue associé. L'indice d'étendue caractérise l'étendue des milieux humides du site. Plus il est fort, plus les milieux présents sur le site y sont dominants. Plus il est faible, plus les milieux y sont ponctuels."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                            )
                                                          ),
                                                          br(),
                                                          fluidRow(
                                                            column(5, align="center",
                                                                   
                                                                   div("Étendue des sites humides en", span(" bon état", style="font-weight:bold;font-size:16px"),style="color:#466964;font-size:16px"),
                                                                   highchartOutput("CR_M_MM11")
                                                            ),
                                                            
                                                            column(2,
                                                                   
                                                                   br(),br(),br(), br(),
                                                                   
                                                                   column(12, align="center",style="border: 1px dotted #466964;border-radius: 5px",
                                                                          p(em("L'étendue des milieux ne semble avoir aucun lien avec leur état écologique. "),style="color:black;padding-top:10px;font-size:13px")
                                                                   )  
                                                            ),
                                                            column(5, align="center",
                                                                   div("Étendue des sites humides en", span(" mauvais état", style="font-weight:bold;font-size:16px"),style="color:#466964;font-size:16px"),
                                                                   highchartOutput("CR_M_MM12")
                                                                   
                                                                   
                                                                   
                                                            )
                                                            
                                                          ),
                                                          
                                                        )
                                                        
                                                        
                                                        
                                               ),
                                               
                                               tabPanel("Relations entre toutes les thématiques",
                                                        
                                                        fluidPage(
                                                          
                                                          fluidRow(
                                                            column(3,
                                                                   
                                                                   br(),br(), br(),br(),br(),br(), br(),br(),  br(),br(),
                                                                   sidebarPanel(id="well2",width=3,
                                                                                
                                                                                
                                                                                
                                                                                prettyCheckboxGroup(
                                                                                  inputId = "input_CR8",
                                                                                  label = HTML("<p style='color:#f5f5ff;font-size:15px'>Variables à afficher : </p>"),
                                                                                  shape="curve",
                                                                                  icon = icon("check"),
                                                                                  
                                                                                  choiceNames = list("Étendue milieux",
                                                                                                     "Évolution étendue milieux",
                                                                                                     "État milieux" ,
                                                                                                     "Évolution état milieux",
                                                                                                     "État faune",
                                                                                                     "Évolution état faune",
                                                                                                     "État flore",
                                                                                                     "Évolution état flore",
                                                                                                     "Services rendus",
                                                                                                     "Activités humaines",
                                                                                                     "Phénomènes hydrologiques",
                                                                                                     "Phénomènes climatiques",
                                                                                                     "Espèces exotiques envahissantes",
                                                                                                     "Espèces indigènes à fort développement"),
                                                                                  
                                                                                  
                                                                                  choiceValues = list("Étendue milieux",
                                                                                                      "Évolution étendue milieux",
                                                                                                      "État milieux" ,
                                                                                                      "Évolution état milieux",
                                                                                                      "État faune",
                                                                                                      "Évolution état faune",
                                                                                                      "État flore",
                                                                                                      "Évolution état flore",
                                                                                                      "Services rendus",
                                                                                                      "Activités humaines",
                                                                                                      "Phénomènes hydrologiques",
                                                                                                      "Phénomènes climatiques",
                                                                                                      "Espèces exotiques envahissantes",
                                                                                                      "Espèces indigènes à fort développement"),              
                                                                                  animation = "pulse",
                                                                                  status = "success",
                                                                                  selected=c("Étendue milieux",
                                                                                             "Évolution étendue milieux",
                                                                                             "État milieux" ,
                                                                                             "Évolution état milieux",
                                                                                             "État faune",
                                                                                             "Évolution état faune",
                                                                                             "État flore",
                                                                                             "Évolution état flore",
                                                                                             "Services rendus",
                                                                                             "Activités humaines",
                                                                                             "Phénomènes hydrologiques",
                                                                                             "Phénomènes climatiques",
                                                                                             "Espèces exotiques envahissantes",
                                                                                             "Espèces indigènes à fort développement"
                                                                                  )
                                                                                )
                                                                   )
                                                            ),
                                                            
                                                            tags$head(tags$style("#input_CR8 {color: #f5f5ff;
                                 font-size: 15px;
                                 
                                 }"
                                                            )
                                                            ),
                                                            
                                                            
                                                            column(9, align="center",
                                                                   
                                                                   box(highchartOutput("CR_All",height=800),width=12),
                                                                   
                                                            ),
                                                            
                                                            column(9, offset=3,
                                                                   column(10,offset=2,
                                                                          p(
                                                                            span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                                                                          p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                                                                   )),
                                                            
                                                            fluidRow(
                                                              
                                                              column(9, offset=3,align="center",
                                                                     column(10,offset=2,   style="border: 1px dotted #466964;border-radius: 5px",
                                                                            p(em("Ce tableau représente les corrélations linéaires entre les indices calculés pour chaque thématique de l'évaluation. Il permet de caractériser les interactions positives (gradient bleu) et négatives (gradient rouge) qui existent au sein d'un site. Plus la relation entre les thématiques est étroite, plus le gradient est foncé. Les thématiques corrélées positivement évoluent dans le même sens, alors que celles corrélées négativement évoluent dans des sens opposés. Par exemple, les sites humides dont l'état des milieux s'améliore (ou se dégrade) sont généralement ceux dont l'état de la faune s'améliore (respectivement, se dégrade). Les thématiques 'étendue des milieux' et 'changement climatique' ne présentent pas de corrélation significative avec les autres thématiques de l'évaluation."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                                                                     ))
                                                            ),
                                                          )
                                                          
                                                          
                                                        )
                                               )
                                    ),
                                    
                                    
                                    ### Portraits de territoire ###
                                    
                                    tabPanel("Portraits de territoire",
                                             
                                             
                                             
                                             fluidPage(
                                               
                                               br(),br(),
                                               
                                               
                                               fluidRow(
                                                 
                                                 
                                                 div(id = "sidebar",sidebarPanel(
                                                   
                                                   
                                                   
                                                   
                                                   tags$div(id="item",selectInput('PdT1',HTML("<p style='font-weight:bold;font-size:16px;color:#f5f5ff'> Nom du site humide </p>" ), c('Choisir un site humide'='',                 list(`Littoral atlantique, Manche et mer du Nord` = PdT$Nom_site[Typologie=="Littoral atlantique, Manche et mer du Nord"],
                                                                                                                                                                                                                                         `Littoral méditerranéen` = PdT$Nom_site[Typologie=="Littoral méditerranéen"],
                                                                                                                                                                                                                                         `Massif à tourbières` = PdT$Nom_site[Typologie=="Massif à tourbières"],
                                                                                                                                                                                                                                         `Plaines intérieures` = PdT$Nom_site[Typologie=="Plaines intérieures"],
                                                                                                                                                                                                                                         `Vallées alluviales` = PdT$Nom_site[Typologie=="Vallées alluviales"],
                                                                                                                                                                                                                                         `Outre-mer` = PdT$Nom_site[Typologie=="Outre-mer"]
                                                   )), 
                                                   selectize=TRUE,multiple=FALSE),style="font-size:14px;font-weight:bold"),
                                                   
                                                   
                                                   tags$head(tags$style("#sidebar .well{height:450px}
                                                   ")),
                                                   
                                                   
                                                   
                                                   
                                                   tags$head(tags$style("#item .selectize-input{background:#f5f5ff;   border: 2px solid #ccc;color:black}
                                                    #item .selectize-input.full{background:#f5f5ff;   border: 2px solid #ccc;color:black}
                                                   #item .selectize-dropdown, .selectize-input, .selectize-input input { background:#f5f5ff;color:black}

                                                   #item .selectize-input.focus {background:#f5f5ff; border: 2px solid green;color:black}")),
                                                   
                                                   
                                                   
                                                   br(),
                                                   tags$head(tags$style("#PdT2{
                                 font-size: 14px;
                                 font-weight:bold;
                                 color:black;
                                    
}
                                 }"
                                                   )
                                                   ),
                                                   selectInput('PdT2', HTML("<p style='font-weight:bold;font-size:16px;color:#f5f5ff'> Choix de la catégorie </p>" ), c("Informations générales"="1","État"="2","Pressions"="3","Services rendus"="4" ), selectize=FALSE,multiple = TRUE,selected="1"),
                                                   
                                                   uiOutput("PdT0")
                                                   
                                                   
                                                 )),
                                                 
                                                 tags$head(tags$style("#sidebar .col-sm-4{
                                
                                 }"
                                                 )
                                                 ),
                                                 
                                                 
                                                 fluidPage(
                                                   mainPanel( width=8,
                                                              
                                                              uiOutput("PdTT"), 
                                                              uiOutput("PdT"),
                                                              
                                                              
                                                   )
                                                 ),
                                               ),
                                               
                                               fluidPage(
                                                 uiOutput("PdTTT")
                                               )
                                               
                                               
                                             )
                                             
                                             
                                             
                                    ),
                                    
                                    ### Questionnaire ###
                                    
                                    tabPanel("Questionnaire",
                                             
                                             
                                             fluidPage(
                                               
                                               fluidRow(
                                                 
                                                 
                                                 column(8,offset=2,align ="center", style="background-color:#d1dad8;border-radius: 10px",
                                                        
                                                        
                                                        strong(em(p("Inventaire des questions posées aux référents dans le cadre de
l'évaluation 2010-2020",style="color:black;margin:0 0 0px;font-size:23px;margin-top:5px;margin-bottom:5px"))),
                                                        
                                                 ),
                                                 
                                                 
                                                 
                                               ),
                                               
                                               br(),
                                               
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Renseignements généraux",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Renseignez votre nom, prénom, courriel, téléphone, intitulé de poste, organisme, adresse organisme",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Quel est votre rapport au site ? (suivi administratif, gestion technique et opérationnelle, autres",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Depuis combien d'année(s) travaillez-vous sur le site évalué ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("4- Si vous répondez avec d'autres personnes, depuis combien d'année(s) travaillent-elles sur le site ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("5- Avez-vous participé aux enquêtes précédentes ? Si oui, pour quelle(s) enquête(s) ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("6- Connaissez-vous tout le site et répondez-vous au questionnaire pour la totalité du site ? Sinon sur quelle partie (classe de proportion du site couvert)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("7- En moyenne, à quelle fréquence vous rendez-vous sur le site à titre professionnel ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("8- En moyenne, à quelle fréquence vous rendez-vous sur le site à titre personnel" ,style="text-align:justify;color:black;padding-left:15px"), 
                                                        
                                                        
                                                 ),
                                               ),
                                               br(),
                                               
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Activités humaines",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Quelles sont les activités présentes sur le site ? Indiquez pour chaque activité, son étendue (état aujourd'hui et évolution entre 2010 et aujourd'hui), son intensité (état général/local aujourd'hui et évolution entre 2010 et aujourd'hui)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Quelle(s) activité(s) a(ont) un impact significatif sur le site dans un rayon de 5 kms depuis 10 ans ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Avez-vous connaissance d'une ou plusieurs activité(s) qui serai(en)t susceptible(s) d'avoir un impact significatif sur le site dans un rayon de 5 kms dans les 10 années à venir ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Étendue des milieux",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Comment s'étendent les milieux humides sur le site (déclinaison milieux doux et milieux salés et saumâtres) et comment ont-ils évolués entre 2010 et aujourd'hui ? (étendue aujourd'hui et évolution des surfaces entre 2010 et aujourd'hui)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Si le milieu a régressé entre 2010 et aujourd'hui, quelle en a été l'origine (principale, secondaire) ? Perte de surface (diffus, concentré) ? (déclinaison par milieux doux ou milieux salés et saumâtres)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Si le milieu s'est étendu entre 2010 et aujourd'hui, quelle en a été l'origine (principale, secondaire) ? Gain de surface (diffus, concentré) ? (déclinaison par milieux doux ou milieux salés et saumâtres)",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("État des milieux",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Quel est l'état des milieux humides sur le site (déclinaison milieux doux et milieux salés et saumâtres) et
comment ont-ils évolués de 2010 jusqu'à ce jour ? (étendue aujourd'hui et évolution de l'état de ces milieux
entre 2010 et aujourd'hui)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Si le milieu s'est dégradé entre 2010 et aujourd'hui, quelle en a été l'origine ? (principale, secondaire) ? Perte de surface (diffus, concentré) ? (déclinaison par milieux doux ou milieux salés et saumâtres)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Si le milieu a été restauré, entre 2010 et aujourd'hui, quelle en a été l'origine (principale, secondaire) ? Gain de surface (diffus, concentré) ? (déclinaison par milieux doux ou milieux salés et saumâtres). Ces mesures vous semblent-elles efficaces ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("4- Y a-t-il des mesures de restauration à poursuivre ou à renforcer ? Si oui lesquelles ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("5- Y a-t-il un suivi scientifique de l'efficacité des mesures de restauration ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("État des espèces communes et à forts enjeux",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Quel est l'état global des espèces animales et végétales (déclinaison espèces communes et à forts enjeux -
protection locale/nationale) sur le site ? (déclinaison faune et flore)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Entre 2010 et aujourd'hui, comment a évolué l'état global des espèces animales et végétales (déclinaison
espèces communes et à forts enjeux - protection locale/nationale) sur le site ? (déclinaison faune et flore)",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Hydrologie et hydraulique",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Entre 2010 et aujourd'hui, y a-t-il eu un ou des phénomène(s) marquant(s) qui ont pu contribuer à un
dysfonctionnement hydrologique et/ou hydraulique notable(s) ? Si oui indiquez le type de phénomène
(perturbation milieu physique/Modification de la gestion des eaux/Altération de la qualité des eaux et pollutions) et le niveau d'importance (globalement important/localement important)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Aujourd'hui, comment est la qualité des eaux superficielles sur le site évalué ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Entre 2010 et aujourd'hui, comment a évolué la qualité des eaux superficielles sur le site évalué ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("4- Entre 2010 et aujourd'hui, comment a évolué le niveau d'eau sur le site humide évalué ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Faune et flore susceptibles de menacer le site",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Entre 2010 et aujourd'hui, est-ce que les espèces exotiques envahissantes ont exercé une pression sur le site ? Si oui, quelle sont les espèces EE (déclinaison faune et flore) qui a (ont) proliféré et dont l'expansion a menacé et/ou menace toujours les écosystèmes, les habitats, les espèces indigènes (présentes naturellement sur un milieu) voire les activités humaines ? (extension en 2010 et aujourd'hui)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Entre 2010 et aujourd'hui, est-ce que des espèces indigènes, à fort développement, ont exercé une pression sur le site ? si oui, quelle(s) espèce(s) indigène(s) (déclinaison faune et flore), à fort développement, exerce(nt) ou a (ont) exercé des pressions sur d'autres espèces, habitats, ou des activités humaines ? (extension en 2010 et aujourd'hui)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Cause(s) d'intervention : Entre 2010 et aujourd'hui, sur les espèces exotiques envahissantes ou les espèces
indigènes à fort développement que vous avez identifiées, y a-t-il eu des actions qui ont été menées pour limiter leur expansion ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("4- Modalité(s) de gestion ou de limitation : Entre 2010 et aujourd'hui, sur les espèces exotiques envahissantes ou les espèces indigènes à fort développement que vous avez identifiées, y a-t-il eu des actions qui ont été menées pour limiter leur expansion ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 ),
                                               ),
                                               
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Changement climatique",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Est-ce que les évolutions observables sur le site peuvent être liées à des phénomènes résultant du changement climatique ? Si oui, quels sont les phénomènes observables, leur impact sur le site (local et général) et leur intensité ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Services rendus",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Aujourd'hui, quel est ou quels sont le ou les service(s) rendu(s) par le site évalué à la société ? Quel est le degré d'importance (principal, secondaire) et le milieu concerné (1er milieu et 2ème milieu)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Aujourd'hui, est-ce que l'état de conservation du milieu (déclinaison par milieux doux ou milieux salés et
saumâtres) permet de rendre à la société le ou les services qu'on attend de lui ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Entre 2010 et aujourd'hui, est-ce que l'évolution de l'état de conservation du milieu (déclinaison par milieux doux ou milieux salés et saumâtres) remet en cause les services que ce dernier rendait auparavant à la société ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("4- Quelles perceptions et représentations les différents acteurs du territoire avaient-ils du site en 2010 et qu'en est-il aujourd'hui ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("5- Quel(s) rôle(s) pensez-vous qu'ils lui ont attribués en 2010 et lui attribuent aujourd'hui ? (déclinaison par acteurs)",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("6- Ce changement de perceptions et de représentations sociales a-t-il impacté en 2010 et impacte-t-il aujourd'hui leur comportement vis-à-vis du site ? (déclinaison par acteurs)",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                        
                                                        
                                                        strong(p("Bilan et perspectives",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                        
                                                        
                                                        p("1- Entre 2010 et aujourd'hui, d'une manière générale, comment le site a évolué sur un plan qualitatif ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("2- Sur la période 2020-2030, quels sont les facteurs naturels ou anthropiques aujourd'hui qui pourraient conduire à une évolution négative du site évalué ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("3- Sur la période 2020-2030, quels sont les facteurs naturels ou anthropiques susceptibles de conduire à une évolution positive du site évalué ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        p("4- Entre aujourd'hui et 2030, comment semble évoluer la situation du site ?",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 ),
                                               ),
                                               br(),
                                               fluidRow(
                                                 
                                                 column(3,offset=4,align="center",
                                                        
                                                        prettyRadioButtons("Q_buttons", HTML("<p style='font-size:14px;padding-left:20px;'>Télécharger le questionnaire :</p>"), 
                                                                           
                                                                           
                                                                           choices = list("France métropolitaine"="FM","Outre-mer"="OM"),
                                                                           inline = TRUE,
                                                                           animation='tada',shape="round",status="success", icon=icon("download"),bigger=TRUE)
                                                        
                                                 ), 
                                                 
                                                 column(2, 
                                                        
                                                        downloadButton('Q_DL', 'Télécharger', class = "butt",style = "color:#f5f5ff;background-color:#466964;border-style: none;" 
                                                                       
                                                        )
                                                 ),
                                                 
                                                 
                                               ),br(),
                                               
                                               
                                             )
                                             
                                    ),
                                    
                                    tabPanel("Mentions légales",
                                             
                                             fluidPage(
                                               
                                               fluidRow(
                                                 
                                                 
                                                 column(2,offset=5,align ="center", style="background-color:#d1dad8;border-radius: 10px",
                                                        
                                                        
                                                        strong(em(p("Mentions légales",style="color:black;margin: 0 0 0px;font-size:23px"))),
                                                        
                                                 ),
                                                 
                                                 
                                                 
                                               ),
                                               br(),
                                               
                                               
                                               fluidRow(
                                                 
                                                 column(12, style="",
                                                        
                                                        p("Les publications et données disponibles sur ce site sont des informations publiques dont la réutilisation est gratuite et n'est soumise à aucune autre condition que celles de la loi du 17 juillet 1978. ",style="text-align:justify;color:black;padding-left:15px"),
                                                        
                                                 )
                                                 
                                               ),
                                               
                                               
                                               column(6,
                                                      
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Objet",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               
                                                               p("Cet outil de datavisualisation a pour objet d'assurer la diffusion, sur Internet, des résultats de l’Évaluation nationale des sites humides emblématiques (campagne 2010-2020) » réalisée en 2019 par le Service des données et études statistiques (SDES) du ministère de la Transition écologique. ",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Tout utilisateur connecté à ce site est réputé avoir pris connaissance des mentions légales et conditions d'utilisation ci-après et les accepter sans réserve.",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                               
                                                        )  
                                                      ),
                                                      
                                                      br(),
                                                      
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Éditeur",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("Ce site est sous la responsabilité du SDES. ",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                               
                                                        ),
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Développement",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("L'application « ENSHE 2010-2020 » a été développée par",span('Jérémie Schoffit,',style="font-weight:bold")," dans le cadre d’un stage de Master II au sein de la sous-direction de l’information environnementale du SDES. L’ensemble du dispositif de l’évaluation a été supervisé par",span("Alexis Cerisier-Auger,",style="font-weight:bold")," chargé de mission biodiversité (espèces) et milieux humides à la sous-direction de l’information environnementale. ",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Cet outil a été élaboré à partir du logiciel R version 4.0.0 (2020-04-24).  Le script, accessible depuis la plateforme GitHub, est disponible sur ",a("https://github.com/jschoffit/enshe-2010-2020",href="https://github.com/jschoffit/enshe-2010-2020", target="_blank"),".",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                               
                                                        ),
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Réutilisation des informations publiques",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("Les conditions de libre réutilisation fixées ci-après s'appliquent uniquement aux contenus et données diffusés par cet outil de datavisualisation et constituant des informations publiques au sens du livre III du code des relations entre le public et l'administration.",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Les publications et bases de données disponibles sur ce site sont des œuvres originales dont le SDES concède des droits de reproduction et de diffusion dans les conditions de la licence ouverte version 2.0 accessible sur le site :",a( "www.data.gouv.fr/fr/licences",href="https://www.data.gouv.fr/fr/licences", target="_blank"),".",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Pour toute demande de dérogation à ces conditions, veuillez vous adresser à" ,a("diffusion.sdes.cgdd@developpement-durable.gouv.fr",href="mailto:diffusion.sdes.cgdd@developpement-durable.gouv.fr", target="_blank"),".",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                        ),
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Données personnelles",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("Ce site enregistre votre adresse IP et l'associe à des cookies enregistrés par votre navigateur Internet. Ces données sont utilisées à des fins statistiques, pour une durée limitée et ne sont en aucun cas transmises à des tiers.",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                        ),
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Liens vers ce site",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("L'administration est favorable à la création de liens hypertextes vers les pages de son site. De façon générale, tout lien établi doit indiquer de façon claire à l'internaute qu'il est dirigé vers cette application, en faisant notamment mention intégrale et visible de son adresse.",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Les liens directs et profonds vers les fichiers (PDF, XLS) téléchargeables des publications ou données diffusés sur ce site sont susceptibles de changer à tout moment : il est suggéré de créer principalement des liens vers les pages décrivant ces publications et données et donnant accès à ces fichiers.",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Le SDES se réserve le droit de demander la dissolution des liens dont elle estimera qu'ils sont de nature à porter préjudice à son image ou à ses droits.",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                               
                                                        ),
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Disponibilité du service",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("L'administration s'efforce d'ouvrir l'accès à ce site 24 heures sur 24, 7 jours sur 7, sauf en cas de force majeure et sous réserve d'éventuelles pannes et d'interventions de maintenance. L'administration peut être amenée à interrompre ce site ou une partie des services, à tout moment et sans préavis.",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("La responsabilité de l'administration ne saurait être engagée en cas d'impossibilité d'accès à ce site ou d'utilisation de ces services.",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                        ),
                                                      ),
                                                      
                                                      br(),
                                                      fluidRow(
                                                        column(12, align ="center",style="background-color:#d1dad8;border-radius: 10px;",
                                                               
                                                               
                                                               strong(p("Informations contenues sur ce site",style="text-align:justify;color:black;padding:15px;font-size:20px")),
                                                               
                                                               
                                                               p("Les informations figurant sur ce site proviennent de sources considérées comme étant fiables. Toutefois, elles sont susceptibles de contenir des erreurs que l'administration se réserve le droit de corriger dès qu'elles sont portées à sa connaissance. Les informations disponibles sur ce site sont susceptibles d'être modifiées à tout moment, et peuvent avoir fait l'objet de mises à jour depuis leur dernière consultation.",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("Les sites internet ou informations tierces référencées sur ce site par des liens ont été sélectionnés et mis à disposition des utilisateurs à titre d'information. Malgré le soin apporté à leur sélection, ceux-ci peuvent contenir des informations inexactes ou sujettes à interprétation.",style="text-align:justify;color:black;padding-left:15px"),
                                                               p("L'administration ne pourra en aucun cas être tenue responsable de tout dommage de quelque nature qu'il soit résultant de l'interprétation ou de l'utilisation des informations disponibles sur ce site ou sur les sites tiers.",style="text-align:justify;color:black;padding-left:15px"),
                                                               
                                                               
                                                               
                                                        ),
                                                      ),
                                                      br(),
                                               ),
                                               
                                               
                                               column(6,
                                                      br(),
                                                      fluidRow(
                                                        column(5, offset=1,
                                                               br(),
                                                               imageOutput("image4", height="250px")
                                                        ),
                                                        column(5, offset=1,
                                                               
                                                               imageOutput("image5", height="250px")
                                                        )
                                                      ),
                                                      tags$head(tags$style(
                                                        type="text/css",
                                                        "#image4 img {height:250px;width:300px}"
                                                      )),
                                                      
                                                      tags$head(tags$style(
                                                        type="text/css",
                                                        "#image5 img {height:250px;width:175px}"
                                                      )),
                                                      
                                                      column(12,
                                                             p("Ministère de la Transition écologique",style="text-align:center;color:black;padding-left:15px;font-style:italic"),
                                                             p("Commissariat général au développement durable (CGDD)",style="text-align:center;color:black;padding-left:15px;font-style:italic"),
                                                             br(),
                                                             p("Site d'Orléans - 5 route d'Olivet",style="text-align:center;color:black;padding-left:15px"),
                                                             p("CS 16105 – 45061 Orléans cedex 2",style="text-align:center;color:black;padding-left:15px"),
                                                             
                                                      ),
                                                      
                                                      
                                                      column(12,
                                                             br(),br(),
                                                             p("Directrice de publication : Béatrice Sedillot, Cheffe du service des données et études statistiques.",style="text-align:justify;color:black;padding-left:15px"),
                                                             p("Contact : ",a("diffusion.sdes.cgdd@developpement-durable.gouv.fr",href="mailto:diffusion.sdes.cgdd@developpement-durable.gouv.fr", target="_blank"),style="text-align:justify;color:black;padding-left:15px"),
                                                             
                                                      )
                                               ),
                                               
                                               
                                               
                                               
                                             )
                                             
                                    )
                                    
                         )
)
)










########################################################################################################
### SERVER ###
########################################################################################################



server <- shinyServer(function(input, output, session) {
  
  ### PRESENTATION GLOBALE ###  
  
  output$plot_P1 <- renderHighchart2({
    
    P1_plot
    
  })
  
  
  output$table_P1 <- renderDataTable(
    
    DT::datatable({
      P2
    },
    extensions = 'Buttons',
    options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE
    ))
  
  
  plot_P3 <- reactive(switch(input$P30, 
                             "1" = P3_plot,
                             "2" = P3_FM_plot,
                             "3" = P3_OM_plot
  ))
  
  
  
  
  plot_P4 <- reactive(switch(input$P30, 
                             "1" = P4_plot,
                             "2" = P4_FM_plot,
                             "3" = P4_OM_plot
  ))
  
  
  
  plot_P5 <- reactive(switch(input$P30, 
                             "1" = P5_plot,
                             "2" = P5_FM_plot,
                             "3" = P5_OM_plot
  ))
  
  
  
  
  output$plot_P3 <- renderHighchart2({
    
    plot_P3()
    
  })
  
  output$plot_P4 <- renderHighchart2({
    
    plot_P4()
    
  })
  
  
  output$plot_P5 <- renderHighchart2({
    
    plot_P5()
    
  })
  
  output$plot_P6 <- renderHighchart2({
    
    if (input$P30 == "1"){
      
      if(input$P60 == "1"){return(P61_plot)}
      if(input$P60 == "2"){return(P62_plot)}
      if(input$P60 == "3"){return(P63_plot)}
      
    }
    
    if (input$P30 == "2"){
      
      if(input$P60 == "1"){return(P6_FM1_plot)}
      if(input$P60 == "2"){return(P6_FM2_plot)}
      if(input$P60 == "3"){return(P6_FM3_plot)}
      
    }
    
    if (input$P30 == "3"){
      
      if(input$P60 == "1"){return(P6_OM1_plot)}
      if(input$P60 == "2"){return(P6_OM2_plot)}
      if(input$P60 == "3"){return(P6_OM3_plot)}
      
    }
    
    
  })
  
  
  
  
  
  plot_P6 <- function(){
    
    if (input$P30 == "1"){
      
      if(input$P60 == "1"){return(P61_plot)}
      if(input$P60 == "2"){return(P62_plot)}
      if(input$P60 == "3"){return(P63_plot)}
      
    }
    
    if (input$P30 == "2"){
      
      if(input$P60 == "1"){return(P6_FM1_plot)}
      if(input$P60 == "2"){return(P6_FM2_plot)}
      if(input$P60 == "3"){return(P6_FM3_plot)}
      
    }
    
    if (input$P30 == "3"){
      
      if(input$P60 == "1"){return(P6_OM1_plot)}
      if(input$P60 == "2"){return(P6_OM2_plot)}
      if(input$P60 == "3"){return(P6_OM3_plot)}
      
    }
    
    
  }
  
  
  output$P6_ui <- renderUI({
    
    if (input$P60=="1"){
      
      box(highchartOutput("plot_P6",height=700),height=701,width=12)
      
    } else {
      
      box(highchartOutput("plot_P6",height=580),height=581,width=12)
      
    }
    
  }
  )
  
  
  ### ACTIVITES HUMAINES ###
  
  plot_A01 <- reactive(switch(input$A0, 
                              "1" = A3_plot,
                              "2" = A3_FM_plot,
                              "3" = A3_OM_plot
  ))
  
  
  
  
  plot_A02 <- reactive(switch(input$A0, 
                              "1" = A0_plot,
                              "2" = A0_FM_plot,
                              "3" = A0_OM_plot
  ))
  
  
  text0 <- reactive(switch(input$A0, 
                           "1" = "Répartition des activités humaines intrinsèques par typologie",
                           "2" = "Répartition des activités humaines intrinsèques dans les sites humides de métropole",
                           "3" = "Répartition des activités humaines intrinsèques dans les sites humides d'Outre-mer"
  ))
  
  output$title0 <- renderText({
    
    text0()
    
  })
  
  
  output$plot_A01 <- renderHighchart2({
    
    plot_A01()
    
  })
  
  output$plot_A02 <- renderHighchart2({
    
    plot_A02()
    
  })
  
  plot_A41 <- reactive(switch(input$A00, 
                              "1" = A5_plot,
                              "2" = A5_FM_plot,
                              "3" = A5_OM_plot
                              
  ))
  
  
  
  plot_A42 <- reactive(switch(input$A00, 
                              "1" = A4_plot,
                              "2" = A4_FM_plot,
                              "3" = A4_OM_plot
  ))
  
  
  
  text4 <- reactive(switch(input$A00, 
                           "1" = "Répartition des activités humaines aux alentours du site par typologie",
                           "2" = "Répartition des activités humaines aux alentours du site dans les sites humides de métropole",
                           "3" = "Répartition des activités humaines aux alentours du site dans les sites humides d'Outre-mer"
  ))
  
  output$title4 <- renderText({
    
    text4()
    
  })
  
  
  output$plot_A41 <- renderHighchart2({
    
    plot_A41()
    
  })
  
  output$plot_A42 <- renderHighchart2({
    
    plot_A42()
    
  })
  
  plot_A51 <- reactive(switch(input$A5, 
                              "1" = A6_plot,
                              "2" = A6_FM_plot,
                              "3" = A6_OM_plot
  ))
  
  
  
  
  
  output$plot_A51 <- renderHighchart2({
    
    
    plot_A51()
    
  })
  
  output$plot_A52 <- renderHighchart2({
    
    
    if (input$A5 == "1"){
      
      if(input$A000 == "1"){return(A71_plot)}
      if(input$A000 == "2"){return(A72_plot)}
      
      
    }
    
    if (input$A5 == "2"){
      
      if(input$A000 == "1"){return(A71_FM_plot)}
      if(input$A000 == "2"){return(A72_FM_plot)}
      
      
    }
    
    if (input$A5 == "3"){
      
      if(input$A000 == "1"){return(A71_OM_plot)}
      if(input$A000 == "2"){return(A72_OM_plot)}
      
      
    }
    
  })
  
  plot_A6 <- function(){
    
    
    if (input$A5 == "1"){
      
      if(input$A000 == "1"){return(A71_plot)}
      if(input$A000 == "2"){return(A72_plot)}
      
      
    }
    
    if (input$A5 == "2"){
      
      if(input$A000 == "1"){return(A71_FM_plot)}
      if(input$A000 == "2"){return(A72_FM_plot)}
      
      
    }
    
    if (input$A5 == "3"){
      
      if(input$A000 == "1"){return(A71_OM_plot)}
      if(input$A000 == "2"){return(A72_OM_plot)}
      
      
    }
    
  }
  
  
  
  
  
  plot_A61 <- reactive(switch(input$A55, 
                              "1" = A8_plot,
                              "2" = A8_FM_plot,
                              "3" = A8_OM_plot
  ))
  
  
  plot_A62 <- reactive(switch(input$A55, 
                              "1" = A9_plot,
                              "2" = A9_FM_plot,
                              "3" = A9_OM_plot
  ))
  
  
  
  
  
  output$plot_A61 <- renderHighchart2({
    
    
    plot_A61()
    
  })
  
  output$plot_A62 <- renderHighchart2({
    
    
    plot_A62()
    
  })
  
  
  
  output$table_A1 <- renderDataTable(
    
    formatStyle(
      formatStyle(
        DT::datatable({
          A2
        },
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE),
        "Pression exercée",target="cell",backgroundColor = styleEqual(c("Très forte","Forte","Moyenne","Modérée","Faible"),c("Red","Orange","Yellow","lightgreen","lightblue"))),
      
      "Nom du site",fontWeight = 'bold',fontSize = '110%'))
  
  
  
  
  
  
  ### HYDROLOGIE ###  
  
  data2 <- reactive(switch(input$dysf2, 
                           "1" = Dysf[,c(1,5)],
                           "2" = Dysf[,c(1,2)],
                           "3" = Dysf[,c(1,3)],
                           "4" = Dysf[,c(1,4)]))
  
  
  output$plot_H1 <- renderHighchart2({
    
    H1_plot
    
  })
  
  
  my_plots1 <- reactive(switch(input$dysf1, 
                               "1" = H21_plot,
                               "2" = H22_plot,
                               "3" = H23_plot
  ))
  
  my_plots2 <- reactive(switch(input$dysf1, 
                               "1" = H31_plot,
                               "2" = H32_plot,
                               "3" = H33_plot
  ))
  
  text1 <- reactive(switch(input$dysf1, 
                           "1" = "perturbation du milieu physique",
                           "2" = "perturbation de la gestion des eaux",
                           "3" = "altération de la qualité des eaux"
  ))
  
  text2 <- reactive(switch(input$dysf1, 
                           "1" = "la perturbation du milieu physique",
                           "2" = "la perturbation de la gestion des eaux",
                           "3" = "l'altération de la qualité des eaux"
  ))
  
  output$title1 <- renderText({
    
    paste("Nombre de sites humides concernés par une ",text1()," selon son importance")
    
  })
  
  output$title2 <- renderText({
    
    paste("Part des sites humides suivant l'importance de ",text2()," par typologie")
    
  })
  
  output$plot_H2 <- renderHighchart2({
    
    my_plots1()
    
  })
  
  
  
  
  
  output$plot_H3 <- renderHighchart2({
    
    my_plots2()
    
  })
  
  
  
  output$table <- renderDataTable(
    
    formatStyle(
      formatStyle(
        DT::datatable({
          Dysf
        },
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE
        
        )
        ,"Nom du site",fontWeight = 'bold',fontSize = '110%'
      ),
      'TOTAL',background=styleColorBar(Dysf$TOTAL,'#91ae4f'), backgroundSize = '100% 90%',  
      backgroundRepeat = 'no-repeat', 
      backgroundPosition = 'center'
    )
  )
  
  output$Dysf_global <- renderHighchart2({
    
    Dysf_global_plot
    
  })
  
  
  
  output$QE <- renderHighchart2({
    
    QE_plot
    
  })
  
  output$QE_evo <- renderHighchart2({
    
    QE_evo_plot
    
  })
  
  output$NE_evo <- renderHighchart2({
    
    NE_evo_plot
    
  })
  
  
  ### CHANGEMENT CLIMATIQUE ###  
  
  output$plot_C1 <- renderHighchart2({
    
    
    C1_plot
    
  })
  
  
  output$plot_C2 <- renderHighchart2 ({
    
    return(C2_plot)
    
  })
  
  
  
  plot_C31 <- reactive(switch(input$C3, 
                              "1" = C3_plot,
                              "2" = C3_FM_plot,
                              "3" = C3_OM_plot
  ))
  
  
  
  
  plot_C32 <- reactive(switch(input$C3, 
                              "1" = C4_plot,
                              "2" = C4_FM_plot,
                              "3" = C4_OM_plot
  ))
  
  
  
  output$plot_C3 <- renderHighchart2({
    
    
    plot_C31()
    
  })
  
  output$plot_C4 <- renderHighchart2({
    
    
    return(plot_C32())
    
  })
  
  
  
  output$C5 <- renderDataTable(
    
    formatStyle(
      formatStyle(
        DT::datatable({
          C5
        },
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE
        
        ),
        "Nom du site",fontSize = '110%',fontWeight = 'bold'),
      'TOTAL',background=styleColorBar(C5$TOTAL,'#91ae4f',angle=90), backgroundSize = '100% 90%',  
      backgroundRepeat = 'no-repeat', 
      backgroundPosition = 'center'
    )
  )
  
  ### Espèces envahissantes ###  
  
  plot_EE1 <- reactive(switch(input$EE1, 
                              "2" = EE1_FM_plot,
                              "3" = EE1_OM_plot
  ))
  
  plot_EE2 <- reactive(switch(input$EE1, 
                              "2" = EE2_FM_plot,
                              "3" = EE2_OM_plot
  ))
  
  plot_EE3 <- reactive(switch(input$EE1, 
                              "2" = EE3_FM_plot,
                              "3" = EE3_OM_plot
                              
  ))
  
  plot_EE4 <- reactive(switch(input$EE1, 
                              "2" = EE4_FM_plot,
                              "3" = EE4_OM_plot
  ))
  
  
  
  output$plot_EE1 <- renderHighchart2({
    
    
    plot_EE1()
    
  })
  
  
  
  output$plot_EE2 <- renderHighchart2 ({
    
    plot_EE2()
    
  })
  
  
  output$plot_EE3 <- renderHighchart2 ({
    
    plot_EE3()
    
  })
  
  
  
  
  plot_EI1 <- reactive(switch(input$EE1, 
                              "2" = EI1_FM_plot,
                              "3" = EI1_OM_plot
  ))
  
  plot_EI2 <- reactive(switch(input$EE1, 
                              "2" = EI2_FM_plot,
                              "3" = EI2_OM_plot
  ))
  
  plot_EI3 <- reactive(switch(input$EE1, 
                              "2" = EI3_FM_plot,
                              "3" = EI3_OM_plot
  ))
  
  plot_EI4 <- reactive(switch(input$EE1, 
                              "2" = EI4_FM_plot,
                              "3" = NULL
                              
                              
  ))
  
  
  
  output$plot_EI1 <- renderHighchart2({
    
    
    plot_EI1()
    
  })
  
  
  output$plot_EI2 <- renderHighchart2 ({
    
    plot_EI2()
    
  })
  
  output$plot_EI3 <- renderHighchart2 ({
    
    plot_EI3()
    
  })
  
  output$plot_EI4 <- renderD3tree ({
    
    plot_EI4()
    
  })
  
  
  
  text6 <- reactive(switch(input$EE1, 
                           
                           "2" = "Part des sites humides confrontés à des espèces exotiques envahissantes par typologie",
                           "3" = "Part des sites humides confrontés à des espèces exotiques envahissantes en Outre-mer"
  ))
  
  text7 <- reactive(switch(input$EE1, 
                           
                           "2" = "Nombre moyen d'espèces exotiques envahissantes par typologie",
                           "3" = "Nombre moyen d'espèces exotiques envahissantes en Outre-mer"
  ))
  
  output$EE_title1 <- renderText({
    
    text6()
    
  })
  
  output$EE_title2 <- renderText({
    
    text7()
    
  })
  
  
  text8 <- reactive(switch(input$EE1, 
                           
                           "2" = "Part des sites humides confrontés à des espèces indigènes par typologie",
                           "3" = "Part des sites humides confrontés à des espèces indigènes en Outre-mer"
  ))
  
  text9 <- reactive(switch(input$EE1, 
                           
                           "2" = "Nombre moyen d'espèces indigènes par typologie",
                           "3" = "Nombre moyen d'espèces indigènes en Outre-mer"
  ))
  
  output$EE_title3 <- renderText({
    
    text8()
    
  })
  
  output$EE_title4 <- renderText({
    
    text9()
    
  })
  
  
  plot_EE5 <- reactive(switch(input$EE2, 
                              "2" = Et_A_FM_plot,
                              "3" = Et_A_OM_plot
                              
                              
  ))
  
  
  
  
  plot_EE6 <- reactive(switch(input$EE2, 
                              "2" = Et_V_FM_plot,
                              "3" = Et_V_OM_plot
                              
                              
  ))
  
  
  text10 <- reactive(switch(input$EE2, 
                            
                            "2" = "Étendue des 20 principales espèces animales sur les sites humides de métropole",
                            "3" = "Étendue des 10 principales espèces animales sur les sites humides d'Outre-mer"
  ))
  
  
  
  text11 <- reactive(switch(input$EE2, 
                            
                            "2" = "Étendue des 20 principales espèces végétales sur les sites humides de métropole",
                            "3" = "Étendue des 10 principales espèces végétales sur les sites humides d'Outre-mer"
  ))
  
  output$plot_EE5 <- renderHighchart2 ({
    
    plot_EE5()
    
  })
  
  
  output$plot_EE6 <- renderHighchart2({
    
    plot_EE6()
    
  })
  
  output$EE_title5 <- renderText({
    
    text10()
    
  })
  
  output$EE_title6 <- renderText({
    
    text11()
    
  })
  
  
  table_EE1 <- reactive(switch(input$EE2, 
                               "2" = E_table_FM,
                               "3" = E_table_OM
                               
                               
  ))
  
  output$table_EE1 <- renderDataTable(
    
    formatStyle(
      formatStyle(
        formatStyle(
          DT::datatable({
            table_EE1()
          },
          extensions = 'Buttons',
          options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                         initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                           "}"),
                         columnDefs=list(list(className='dt-center',targets="_all"))
          ),
          selection = 'multiple',
          style = 'bootstrap',
          class = 'cell-border stripe',
          rownames = FALSE
          
          ),
          "Nom du site",fontSize = '110%',fontWeight = 'bold'),
        'TOTAL espèces exotiques',background=styleColorBar((table_EE1())$'TOTAL espèces exotiques','#91ae4f'), backgroundSize = '100% 90%',  
        backgroundRepeat = 'no-repeat', 
        backgroundPosition = 'center'
      ),
      'TOTAL espèces indigènes',background=styleColorBar((table_EE1())$'TOTAL espèces indigènes','#91ae4f'), backgroundSize = '100% 90%',  
      backgroundRepeat = 'no-repeat', 
      backgroundPosition = 'center'
    )
  )
  
  plot_EE7 <- reactive(switch(input$EE3, 
                              "2" = E_causes_FM_plot,
                              "3" = E_causes_OM_plot
                              
                              
  ))
  
  plot_EE8 <- reactive(switch(input$EE3, 
                              "2" = E_gestion_FM_plot,
                              "3" = E_gestion_OM_plot
                              
                              
  ))
  
  
  
  
  output$plot_EE7 <- renderHighchart2 ({
    
    plot_EE7()
    
  })
  
  
  output$plot_EE8 <- renderHighchart2 ({
    
    plot_EE8()
    
  })
  
  
  
  
  
  
  ### ETAT DES ESPECES COMMUNES & A FORTS ENJEUX ###
  
  plot_F1 <- reactive(switch(input$F00, 
                             "1" = F1_plot,
                             "2" = F1_FM_plot,
                             "3" = F1_OM_plot
                             
                             
  ))
  
  
  
  
  
  output$F1 <- renderHighchart2({
    
    
    plot_F1()
    
  })
  
  
  output$F2 <- renderHighchart2({
    
    
    if (input$F00 == "1"){
      
      if(input$F001 == "1"){return(F2_plot)}
      if(input$F001 == "2"){return(F3_plot)}
      
      
    }
    
    if (input$F00 == "2"){
      
      if(input$F001 == "1"){return(F2_FM_plot)}
      if(input$F001 == "2"){return(F3_FM_plot)}
      
      
    }
    
    if (input$F00 == "3"){
      
      if(input$F001 == "1"){return(F2_OM_plot)}
      if(input$F001 == "2"){return(F3_OM_plot)}
      
      
    }
    
  })
  
  
  
  plot_F2 <- function(){
    
    
    if (input$F00 == "1"){
      
      if(input$F001 == "1"){return(F2_plot)}
      if(input$F001 == "2"){return(F3_plot)}
      
      
    }
    
    if (input$F00 == "2"){
      
      if(input$F001 == "1"){return(F2_FM_plot)}
      if(input$F001 == "2"){return(F3_FM_plot)}
      
      
    }
    
    if (input$F00 == "3"){
      
      if(input$F001 == "1"){return(F2_OM_plot)}
      if(input$F001 == "2"){return(F3_OM_plot)}
      
      
    }
    
  }
  
  
  
  
  
  
  plot_F4 <- reactive(switch(input$F00, 
                             "1" = F4_plot,
                             "2" = F4_FM_plot,
                             "3" = F4_OM_plot
                             
                             
  ))
  
  
  
  plot_F5 <- reactive(switch(input$F00, 
                             "1" = F5_plot,
                             "2" = F5_FM_plot,
                             "3" = F5_OM_plot
                             
                             
  ))
  
  
  
  
  output$F4 <- renderHighchart2({
    
    
    plot_F4()
    
  })
  
  
  output$F5 <- renderHighchart2({
    
    
    plot_F5()
    
  })
  
  
  F_text1 <- reactive(switch(input$F001, 
                             "1" = "animales",
                             
                             "2" = "végétales"
  ))
  
  F_text2 <- reactive(switch(input$F00, 
                             "1" = paste0("État de conservation des espèces ", F_text1()," communes par typologie"),
                             "2" = paste0("État de conservation des espèces ",F_text1()," communes en métropole et sur l'ensemble des sites"),
                             "3" = paste0("État de conservation des espèces ",F_text1()," communes en Outre-mer et sur l'ensemble des sites")
  ))
  
  
  output$F_title1 <- renderText({
    
    
    F_text2()
    
  })
  
  
  F_text3 <- reactive(switch(input$F01, 
                             "1" = "État de conservation des espèces animales à forts enjeux par typologie",
                             "2" = "État de conservation des espèces animales à forts enjeux en France métropolitaine",
                             "3" = "État de conservation des espèces animales à forts enjeux en Outre-mer"
  ))
  
  output$F_title2 <- renderText({
    
    
    F_text3()
    
  })
  
  F_text4 <- reactive(switch(input$F01, 
                             "1" = "État de conservation des espèces végétales à forts enjeux par typologie",
                             "2" = "État de conservation des espèces végétales à forts enjeux en France métropolitaine",
                             "3" = "État de conservation des espèces végétales à forts enjeux en Outre-mer"
  ))
  
  output$F_title3 <- renderText({
    
    
    F_text4()
    
  })
  
  
  plot_F6 <- reactive(switch(input$F01, 
                             "1" = F6_plot,
                             "2" = F6_FM_plot,
                             "3" = F6_OM_plot
                             
                             
  ))
  
  
  
  
  plot_F7 <- reactive(switch(input$F01, 
                             "1" = F7_plot,
                             "2" = F7_FM_plot,
                             "3" = F7_OM_plot
                             
                             
  ))
  
  
  
  
  
  plot_F8 <- reactive(switch(input$F01, 
                             "1" = F8_plot,
                             "2" = F8_FM_plot,
                             "3" = F8_OM_plot
                             
                             
  ))
  
  
  
  
  plot_F9 <- reactive(switch(input$F01, 
                             "1" = F9_plot,
                             "2" = F9_FM_plot,
                             "3" = F9_OM_plot
                             
                             
  ))
  
  
  output$F8 <- renderHighchart2({
    
    
    if (input$F01 == "1"){
      
      if(input$F_choice3 == "1"){return(F81_plot)}
      if(input$F_choice3 == "2"){return(F82_plot)}
      
      
    }
    
    if (input$F01 == "2"){
      
      if(input$F_choice3 == "1"){return(F81_FM_plot)}
      if(input$F_choice3 == "2"){return(F82_FM_plot)}
      
      
    }
    
    if (input$F01 == "3"){
      
      if(input$F_choice3 == "1"){return(F81_OM_plot)}
      if(input$F_choice3 == "2"){return(F82_OM_plot)}
      
      
    }
    
  })
  
  
  output$F9 <- renderHighchart2({
    
    
    
    if (input$F01 == "1"){
      
      if(input$F_choice4 == "1"){return(F91_plot)}
      if(input$F_choice4 == "2"){return(F92_plot)}
      
      
    }
    
    if (input$F01 == "2"){
      
      if(input$F_choice4 == "1"){return(F91_FM_plot)}
      if(input$F_choice4 == "2"){return(F92_FM_plot)}
      
      
    }
    
    if (input$F01 == "3"){
      
      if(input$F_choice4 == "1"){return(F91_OM_plot)}
      if(input$F_choice4 == "2"){return(F92_OM_plot)}
      
      
    }
    
    
  })
  
  
  output$ui1 <- renderUI({
    if (is.null(input$F_choice1))
      return()
    
    switch(input$F_choice1,
           
           "1" =NULL,
           
           "2" =     tagList(br(),         selectInput("F011",width=200,label=NULL,
                                                       c( "Protection locale" = "1",
                                                          
                                                          "Protection nationale" = "2"),
                                                       selected = "2"))
    )
  }
  )
  
  
  
  
  
  
  
  
  output$ui2 <- renderUI({
    if (is.null(input$F_choice2))
      return()
    
    switch(input$F_choice2,
           
           "1" = NULL,
           
           "2" =      tagList(br(),        selectInput("F012",width=180,label=NULL,
                                                       c( "Protection locale" = "1",
                                                          
                                                          "Protection nationale" = "2"),
                                                       selected = "2"))
    )
  }
  )
  
  
  output$F6 <- renderHighchart2({
    
    if (input$F_choice1 == "1"){
      
      return(plot_F6())
      
    }
    
    if (input$F_choice1 == "2"){
      
      if (input$F01 == "1"){
        
        if(input$F011 == "1"){return(F10L_plot)}
        if(input$F011 == "2"){return(F10N_plot)}
        
      }
      
      if (input$F01 == "2"){
        
        if(input$F011 == "1"){return(F10L_FM_plot)}
        if(input$F011 == "2"){return(F10N_FM_plot)}
        
      }
      
      if (input$F01 == "3"){
        
        if(input$F011 == "1"){return(F10L_OM_plot)}
        if(input$F011 == "2"){return(F10N_OM_plot)}
        
      }
    }
    
  })
  
  
  plot_F66 <- function(){
    
    if (input$F_choice1 == "1"){
      
      return(plot_F6())
      
    }
    
    if (input$F_choice1 == "2"){
      
      if (input$F01 == "1"){
        
        if(input$F011 == "1"){return(F10L_plot)}
        if(input$F011 == "2"){return(F10N_plot)}
        
      }
      
      if (input$F01 == "2"){
        
        if(input$F011 == "1"){return(F10L_FM_plot)}
        if(input$F011 == "2"){return(F10N_FM_plot)}
        
      }
      
      if (input$F01 == "3"){
        
        if(input$F011 == "1"){return(F10L_OM_plot)}
        if(input$F011 == "2"){return(F10N_OM_plot)}
        
      }
    }
    
  }
  
  
  
  
  output$F7 <- renderHighchart2({
    
    
    if (input$F_choice2 == "1"){
      
      return(plot_F7())
      
    }
    
    if (input$F_choice2 == "2"){
      
      if (input$F01 == "1"){
        
        if(input$F012 == "1"){return(F11L_plot)}
        if(input$F012 == "2"){return(F11N_plot)}
        
      }
      
      if (input$F01 == "2"){
        
        if(input$F012 == "1"){return(F11L_FM_plot)}
        if(input$F012 == "2"){return(F11N_FM_plot)}
        
      }
      
      if (input$F01 == "3"){
        
        if(input$F012 == "1"){return(F11L_OM_plot)}
        if(input$F012 == "2"){return(F11N_OM_plot)}
        
      }
      
    }
  })
  
  
  plot_F77 <- function(){
    
    
    if (input$F_choice2 == "1"){
      
      return(plot_F7())
      
    }
    
    if (input$F_choice2 == "2"){
      
      if (input$F01 == "1"){
        
        if(input$F012 == "1"){return(F11L_plot)}
        if(input$F012 == "2"){return(F11N_plot)}
        
      }
      
      if (input$F01 == "2"){
        
        if(input$F012 == "1"){return(F11L_FM_plot)}
        if(input$F012 == "2"){return(F11N_FM_plot)}
        
      }
      
      if (input$F01 == "3"){
        
        if(input$F012 == "1"){return(F11L_OM_plot)}
        if(input$F012 == "2"){return(F11N_OM_plot)}
        
      }
      
    }
  }
  
  
  
  
  
  ### SERVICES RENDUS ###
  
  
  
  output$S1 <- renderHighchart2({
    
    S1_plot
    
  })
  
  
  plot_S2 <- reactive(switch(input$S00, 
                             "1" = S21_plot,
                             "2" = S22_plot,
                             "3" = S23_plot
                             
                             
  ))
  
  
  
  S2 <- reactive(switch(input$S00, 
                        "1" = S21,
                        "2" = S22,
                        "3" = S23
                        
                        
  ))
  
  
  output$S2 <- renderHighchart2({
    
    plot_S2()
    
  })
  
  
  S_text1 <- reactive(switch(input$S00, 
                             "1" = "Nombre de sites concernés par les biens",
                             "2" = "Nombre de sites concernés par les services de régulation",
                             "3" = "Nombre de sites concernés par les services culturels"
  ))
  
  
  output$S_title1 <- renderText({
    
    
    S_text1()
    
  })
  
  plot_S3 <- reactive(switch(input$S01, 
                             "1" = S3_plot,
                             "2" = S5_plot
                             
  ))
  
  
  
  
  output$S3 <- renderHighchart2({
    
    plot_S3()
    
  })
  
  
  plot_S4 <- reactive(switch(input$S02, 
                             "1" = S4_plot,
                             "2" = S6_plot
                             
  ))
  
  
  
  
  
  output$S4 <- renderHighchart2({
    
    plot_S4()
    
  })
  
  
  
  output$S7 <- renderDataTable(
    
    formatStyle(
      formatStyle(
        DT::datatable({
          S7
        },
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE
        
        ),
        "Nom du site",fontSize = '110%',fontWeight = 'bold'),
      'TOTAL',background=styleColorBar(S7$TOTAL,'#91ae4f',angle=90), backgroundSize = '100% 90%',  
      backgroundRepeat = 'no-repeat', 
      backgroundPosition = 'center'
    )
  )
  
  
  output$S8 <- renderHighchart2({
    
    S8_plot
    
  })
  
  
  output$S9 <- renderHighchart2({
    
    S9_plot
    
  })
  
  
  output$S10 <- renderHighchart2({
    
    S10_plot
    
  })
  
  
  output$S11 <- renderHighchart2({
    
    S11_plot
    
  })
  
  
  output$ui_S0 <- renderUI({
    
    
    if (input$S00=="1"){
      
      
      box(highchartOutput("S2",height=640),height=641,width=12)
      
    } else if (input$S00=="2") {
      
      
      box(highchartOutput("S2",height=640),height=641,width=12)
      
      
    } else {
      
      box(highchartOutput("S2",height=560),height=561,width=12)
      
    }
    
    
  })
  
  
  output$ui_S1 <- renderUI({
    
    
    if (input$S01=="1"){
      
      
      box(highchartOutput("S3",height=700),height=701,width=12)
      
    } else {
      
      
      box(highchartOutput("S3",height=560),height=561,width=12)
      
      
    }
    
    
  })
  
  
  
  
  
  
  output$ui_S2 <- renderUI({
    
    
    if (input$S02=="1"){
      
      
      box(highchartOutput("S4",height=700),height=701,width=12)
      
    } else {
      
      
      box(highchartOutput("S4",height=560),height=561,width=12)
      
      
    }
    
    
  })
  
  
  ### BILAN ET PERSPECTIVES ###
  
  
  
  
  output$B1 <- renderHighchart2({
    
    
    B1_plot
    
  })
  
  output$B2 <- renderHighchart2({
    
    
    B2_plot
    
  })
  
  
  output$B3 <- renderHighchart2({
    
    
    B3_plot
    
  })
  
  output$B4 <- renderHighchart2({
    
    
    B4_plot
    
  })
  
  output$B5 <- renderHighchart2({
    
    
    B5_plot
    
  })
  
  output$B6 <- renderHighchart2({
    
    
    B6_plot
    
  })
  
  ### ETENDUE DES MILIEUX ###
  
  output$M1 <- renderHighchart2({
    
    
    M1_plot
    
  })
  
  output$M2 <- renderHighchart2({
    
    
    M2_plot
    
  })
  
  plot_M35 <- reactive(switch(input$M01, 
                              "1" = M3_plot,
                              "2" = M5_plot
                              
                              
                              
  ))
  
  
  
  output$M3 <- renderHighchart2({
    
    
    plot_M35()
    
  })
  
  plot_M46 <- reactive(switch(input$M02, 
                              "1" = M4_plot,
                              "2" = M6_plot
                              
                              
                              
  ))
  
  
  
  output$M4 <- renderHighchart2({
    
    
    plot_M46()
    
  })
  
  output$M7 <- renderDataTable(
    
    formatStyle(
      formatStyle(
        DT::datatable({
          M7
        },
        extensions = 'Buttons',
        options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),lengthMenu=list(c(10,50,100,189),c('10','50','100','All')),
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': 'rgb(70,105,100)', 'color': '#f5f5ff'});",
                         "}"),
                       columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE
        
        )
        ,"Nom du site",fontWeight = 'bold',fontSize = '110%'
      ),
      'TOTAL',background=styleColorBar(Dysf$TOTAL,'#91ae4f'), backgroundSize = '100% 90%',  
      backgroundRepeat = 'no-repeat', 
      backgroundPosition = 'center'
    )
  )
  
  
  
  output$M8 <- renderHighchart({
    
    
    M8_plot
    
  })
  
  
  output$ui_M1 <- renderUI({
    
    
    if (input$M02=="1"){
      
      
      box(showOutput("M4","nvd3"),height=940,width=12)
      
    } else {
      
      
      box(showOutput("M4","nvd3"),height=730,width=12)
      
      
    }
    
    
  })
  
  
  
  ### ETAT DES MILIEUX ###
  
  
  
  plot_MM1 <- reactive(switch(input$MM03, 
                              "1" = MM1_G_plot,
                              "2" = MM1_L_plot,
                              
                              
                              
  ))
  
  
  
  output$MM1 <- renderHighchart2({
    
    
    plot_MM1()
    
  })
  
  output$MM2 <- renderHighchart2({
    
    
    MM2_plot
    
  })
  
  plot_MM35 <- reactive(switch(input$MM01, 
                               "1" = MM3_plot,
                               "2" = MM5_plot
                               
                               
                               
  ))
  
  
  
  output$MM3 <- renderHighchart2({
    
    
    plot_MM35()
    
  })
  
  plot_MM46 <- reactive(switch(input$MM02, 
                               "1" = MM4_plot,
                               "2" = MM6_plot,
                               
                               
                               
  ))
  
  
  
  
  plot_MM78 <- reactive(switch(input$MM07, 
                               "1" = MM7_plot,
                               "2" = MM8_plot,
                               
                               
                               
  ))
  
  
  
  output$MM7 <- renderHighchart2({
    
    
    plot_MM78()
    
  })
  
  output$MM7_ui <- renderUI({
    
    if (input$MM07=="1"){
      
      box(highchartOutput("MM7",height=700),height=701,width=12)
      
    } else {
      
      box(highchartOutput("MM7",height=560),height=561,width=12)
      
    }
    
  }
  )
  
  output$MM9 <- renderHighchart2({
    
    
    MM9_plot
    
  })
  
  output$MM10 <- renderHighchart2({
    
    
    MM10_plot
    
  })
  
  output$MM11 <- renderHighchart2({
    
    
    MM11_plot
    
  })
  
  
  
  
  
  output$MM4 <- renderHighchart2({
    
    
    plot_MM46()
    
  })
  
  
  
  
  # Suite de espèces envahissantes #
  
  
  output$ui4 <- renderUI({
    if (is.null(input$EE_choice1))
      return()
    
    if (input$EE_choice1 == "1"){
      
      return(
        tagList(prettyRadioButtons("EE_choice2",label=NULL,
                                   c( "Général" = "1",
                                      
                                      "Par typologie" = "2"), 
                                   animation='pulse',shape="square",status="success",inline = TRUE),br(),
                
                tags$head(tags$style("#EE_choice2{color: #f5f5ff;
                                 font-size: 15px;
                                 text-align: center;
                                 }"
                )
                )
                
                
        )
      )
    }
    
    if (input$EE_choice1 == "2"){}
    
    if (input$EE_choice1 == "3"){ 
      
      if (input$EE1 =="2"){
        
        if (is.null(input$EE_choice3)){return()}
        
        else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
          
          
          return(
            tagList(sliderInput("EE_choice4", HTML("<p style='font-size:13px;color:#f5f5ff'> TOP X espèces les plus présentes</p>" ),
                                min = 5, max = 20, value = 10,width='80%')
                    
            )
          )
          
        } else if (input$EE_choice3[1]=="2"){
          
          return(
            tagList(sliderInput("EE_choice4", HTML("<p style='font-size:13px;color:#f5f5ff'> TOP X espèces les plus présentes</p>" ),
                                min = 5, max = 7, value = 5,width='80%')
                    
            )
          )
          
        } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
          
          return(
            
            tagList(sliderInput("EE_choice41", HTML("<p style='font-size:13px;color:#f5f5ff'> TOP X espèces exotiques les plus présentes</p>" ),
                                min = 5, max = 20, value = 10,width='80%'),
                    
                    
                    sliderInput("EE_choice42",HTML("<p style='font-size:13px;color:#f5f5ff'> TOP X espèces indigènes les plus présentes</p>" ),
                                min = 5, max = 7, value = 5,width='80%'),
                    
                    
            )
          )
          
        }
        
      } else if (input$EE1 =="3"){
        
        if (is.null(input$EE_choice3)){return()}
        
        else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
          
          
          return(
            tagList(sliderInput("EE_choice4", HTML("<p style='font-size:13px;color:#f5f5ff'> TOP X espèces les plus présentes</p>" ),
                                min = 5, max = 10, value = 7,width='80%')
                    
            )
          )
          
        } else if (input$EE_choice3[1]=="2"){
          
          return(
            
          )
          
        } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
          
          return(
            
            return(
              tagList(sliderInput("EE_choice4", HTML("<p style='font-size:13px;color:#f5f5ff'> TOP X espèces les plus présentes</p>" ),
                                  min = 5, max = 10, value = 7,width='80%')
                      
              )
            )
          )
          
        }
        
      }
      
    }
  }
  )
  
  output$ui5 <- renderUI(
    
    
    if(input$EE1 == "2"){
      
      if (input$EE_choice1=="1"){
        
        if (input$EE_choice2=="1"){
          
          if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
          
          else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
            
            
            
            tagList(
              
              fluidRow(
                
                box(
                  column(12,align="center",
                         h3("Part des sites humides confrontés à des espèces exotiques envahissantes",style="color:rgb(70,105,100)"),
                         highchartOutput("id1",height=560)
                  )
                  ,height=560,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
              
              
            )
            
            
          } else if (input$EE_choice3[1]=="2"){
            
            tagList(
              fluidRow(
                
                box(
                  column(12,align="center",
                         h3("Part des sites humides confrontés à des espèces indigènes à fort développement",style="color:rgb(70,105,100)"),
                         highchartOutput("id2",height=560)
                  )
                  ,height=560,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
            )
            
          } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
            
            tagList(
              fluidRow(
                
                
                column(6, align="center",
                       h3("Part des sites humides confrontés à des espèces exotiques envahissantes",style="color:rgb(70,105,100)"),
                       highchartOutput("id1",height=560)
                ),
                
                
                
                column(6,align="center",
                       h3("Part des sites humides confrontés à des espèces indigènes à fort développement",style="color:rgb(70,105,100)"),
                       highchartOutput("id2",height=560)
                )
                
                
              ), br(),br(),br(),
              fluidRow(
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                ),
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              ),
              
              
            )
            
          } 
          
        } else if(input$EE_choice2=="2"){
          
          if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
          
          else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
            
            
            
            tagList(
              
              fluidRow(
                
                box(
                  column(12,align='center',
                         h3("Part des sites humides confrontés à des espèces exotiques envahissantes par typologie",style="color:rgb(70,105,100)"),
                         highchartOutput("id3",height=560)
                  )
                  ,height=561,width=12
                ),
                
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
              
              
            )
            
            
          } else if (input$EE_choice3[1]=="2"){
            
            tagList(
              fluidRow(
                
                
                box(
                  column(12,align="center",
                         h3("Part des sites humides confrontés à des espèces indigènes à fort développement par typologie",style="color:rgb(70,105,100)"),
                         highchartOutput("id4",height=560)
                  )
                  ,height=561,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
            )
            
          } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
            
            tagList(
              
              fluidRow(
                column(6, align='center',
                       h3("Part des sites humides confrontés à des espèces exotiques envahissantes par typologie",style="color:rgb(70,105,100)")
                ),
                column(6,align='center',
                       h3("Part des sites humides confrontés à des espèces indigènes à fort développement par typologie",style="color:rgb(70,105,100)")
                )
              ),
              
              fluidRow(
                
                
                column(6,align="center",
                       
                       highchartOutput("id3",height=561)
                       
                ),
                
                
                column(6,align="center",
                       
                       highchartOutput("id4",height=561)
                )
                
                
              ),
              fluidRow(
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                ),
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              ),
              
              
              
              
              
            )
            
          } 
          
          
        }
        
      } else if (input$EE_choice1 == "2"){
        
        if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
        
        else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
          
          
          
          tagList(
            
            fluidRow(
              
              
              
              box(
                column(12,align="center",
                       h3("Nombre moyen d'espèces exotiques par typologie",style="color:rgb(70,105,100)"),
                       highchartOutput("id5",height=561)
                )
                ,height=561,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
            
            
          )
          
          
        } else if (input$EE_choice3[1]=="2"){
          
          tagList(
            fluidRow(
              
              
              box(
                column(12,align="center",
                       h3("Nombre moyen d'espèces indigènes par typologie",style="color:rgb(70,105,100)"),
                       highchartOutput("id6",height=560)
                )
                ,height=561,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
          )
          
        } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
          
          tagList(
            
            fluidRow(
              column(6, align='center',
                     h3("Part des sites humides confrontés à des espèces exotiques envahissantes par typologie",style="color:rgb(70,105,100)")
              ),
              column(6,align='center',
                     h3("Part des sites humides confrontés à des espèces indigènes à fort développement par typologie",style="color:rgb(70,105,100)")
              )
            ),
            
            fluidRow(
              
              
              column(6,align="center",
                     
                     highchartOutput("id5",height=560)
                     
              ),
              
              
              column(6,align="center",
                     
                     highchartOutput("id6",height=560)
              )
              
              
            ),
            fluidRow(
              column(6,
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              ),
              column(6,
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            ),
            
            
            
            
            
          )
          
        } 
        
        
        
      }else if (input$EE_choice1 == "3"){
        
        if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
        
        else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
          
          
          
          tagList(
            
            tags$head(
              tags$style('text.label{font-size: 11px !important}')
            ),
            
            fluidRow(
              
              box(
                column(12,align="center",
                       h3(paste("TOP",input$EE_choice4,"espèces exotiques envahissantes animales et végétales"),style="color:rgb(70,105,100)"),
                       d3treeOutput("id7"),
                       
                )
                ,height=500,width=12
              )
            ),
            fluidRow(
              column(9,
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            ),
            
            
            fluidRow(
              column(10, align ="center",
                     
                     p(em("Le nombre entre parenthèses correspond au nombre de sites humides sur lesquels cette espèce est présente.",style="color:black;padding-top:10px;font-size:14px;text-align:justify"))
                     
              )
            )
            
            
          )
          
          
        } else if (input$EE_choice3[1]=="2"){
          
          
          
          tagList(
            
            
            fluidRow(
              
              box(
                column(12,align="center",
                       h3(paste("TOP",input$EE_choice4,"espèces indigènes animales et végétales"),style="color:rgb(70,105,100)"),
                       d3treeOutput("id8")
                )
                ,height=500,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
          )
          
        } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
          
          tagList(
            fluidRow(
              
              
              column(6,align="center",
                     tags$head(
                       tags$style('text.label{font-size: 8px !important}')
                     ),
                     h3(paste("TOP",input$EE_choice41,"espèces exotiques animales et végétales"),style="color:rgb(70,105,100)"),
                     d3treeOutput("id9")
                     
              ),
              
              
              
              
              column(6,align="center",
                     h3(paste("TOP",input$EE_choice42,"espèces indigènes animales et végétales"),style="color:rgb(70,105,100)"),
                     
                     d3treeOutput("id10")
              )
              
              
              
            )
            
          )
          
        } 
        
        
        
      }
      
    } else {
      
      if (input$EE_choice1=="1"){
        
        if (input$EE_choice2=="1"){
          
          if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
          
          else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
            
            
            
            tagList(
              
              fluidRow(
                
                box(
                  column(12,align="center",
                         h3("Part des sites humides confrontés à des espèces exotiques envahissantes",style="color:rgb(70,105,100)"),
                         highchartOutput("id1_OM",height=560)
                  )
                  ,height=561,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
              
              
            )
            
            
          } else if (input$EE_choice3[1]=="2"){
            
            tagList(
              fluidRow(
                
                box(
                  column(12,align="center",
                         h3("Part des sites humides confrontés à des espèces indigènes à fort développement",style="color:rgb(70,105,100)"),
                         highchartOutput("id2_OM",height=560)
                  )
                  ,height=561,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
            )
            
          } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
            
            tagList(
              fluidRow(
                
                
                column(6, align="center",
                       h3("Part des sites humides confrontés à des espèces exotiques envahissantes",style="color:rgb(70,105,100)"),
                       highchartOutput("id1_OM",height=560)
                ),
                
                
                
                column(6,align="center",
                       h3("Part des sites humides confrontés à des espèces indigènes à fort développement",style="color:rgb(70,105,100)"),
                       highchartOutput("id2_OM",height=560)
                ),
                
                
              ), 
              fluidRow(
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                ),
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              ),
              
              
            )
            
          } 
          
        } else if(input$EE_choice2=="2"){
          
          if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
          
          else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
            
            
            
            tagList(
              
              fluidRow(
                
                box(
                  column(12,
                         h3("Part des sites humides confrontés à des espèces exotiques envahissantes en Outre-mer",style="color:rgb(70,105,100)"),
                         div(highchartOutput("id3_OM",height=560)),style = 'align:center;'
                  )
                  ,height=561,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
              
              
            )
            
            
          } else if (input$EE_choice3[1]=="2"){
            
            tagList(
              fluidRow(
                
                box(
                  column(12,align="center",
                         h3("Part des sites humides confrontés à des espèces indigènes à fort développement en Outre-mer",style="color:rgb(70,105,100)"),
                         highchartOutput("id4_OM",height=560)
                  )
                  ,height=561,width=12
                )
              ),
              fluidRow(
                column(9,offset = 2,align="center",
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              )
            )
            
          } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
            
            tagList(
              
              fluidRow(
                
                column(6, align='center',
                       h3("Part des sites humides confrontés à des espèces exotiques envahissantes en Outre-mer",style="color:rgb(70,105,100)")
                ),
                column(6,align='center',
                       h3("Part des sites humides confrontés à des espèces indigènes à fort développement en Outre-mer",style="color:rgb(70,105,100)")
                )
              ),
              
              fluidRow(
                
                
                column(6,align="center",
                       
                       highchartOutput("id3_OM",height=560)
                ),
                
                
                
                
                column(6,align="center",
                       
                       highchartOutput("id4_OM",height=560)
                )
                
                
              ),
              fluidRow(
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                ),
                column(6,
                       p(
                         span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                       p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                )
              ),
              
              
              
              
              
            )
            
          } 
          
          
        }
        
      } else if (input$EE_choice1 == "2"){
        
        if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
        
        else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
          
          
          
          tagList(
            
            fluidRow(
              
              box(
                column(12,align="center",
                       h3("Nombre moyen d'espèces exotiques en Outre-mer",style="color:rgb(70,105,100)"),
                       highchartOutput("id5_OM",height=560)
                )
                ,height=561,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
            
            
          )
          
          
        } else if (input$EE_choice3[1]=="2"){
          
          tagList(
            fluidRow(
              
              box(
                column(12,align="center",
                       h3("Nombre moyen d'espèces indigènes en Outre-mer",style="color:rgb(70,105,100)"),
                       highchartOutput("id6_OM",height=560)
                )
                ,height=561,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
          )
          
        } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
          
          tagList(
            
            fluidRow(
              column(6, align='center',
                     h3("Nombre moyen d'espèces exotiques en Outre-mer",style="color:rgb(70,105,100)")
              ),
              column(6,align='center',
                     h3("Nombre moyen d'espèces indigènes en Outre-mer",style="color:rgb(70,105,100)")
              )
            ),
            
            fluidRow(
              
              
              column(6,align="center",
                     
                     highchartOutput("id5_OM",height=560)
              ),
              
              
              
              column(6,align="center",
                     
                     highchartOutput("id6_OM",height=560)
              )
              
              
            ),
            fluidRow(
              column(6,
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              ),
              column(6,
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            ),
            
            
            
            
            
          )
          
        } 
        
        
        
      }else if (input$EE_choice1 == "3"){
        
        if (is.null(input$EE_choice3)){return(column(12,align="center",p(strong("Choisir un type d'espèces."))))}
        
        else if (input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2])){
          
          
          
          tagList(
            
            tags$head(
              tags$style('text.label{font-size: 11px !important}')
            ),
            
            fluidRow(
              
              box(
                column(12,align="center",
                       h3(paste("TOP",input$EE_choice4,"espèces exotiques envahissantes animales et végétales"),style="color:rgb(70,105,100)"),
                       d3treeOutput("id7_OM")
                )
                ,height=500,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
            
            
          )
          
          
        } else if (input$EE_choice3[1]=="2"){
          
          
          
          tagList(
            
            p(strong("Non disponible"))
            
          )
          
        } else if (input$EE_choice3[1]=="1" & input$EE_choice3[2]=="2"){
          tagList(
            
            tags$head(
              tags$style('text.label{font-size: 11px !important}')
            ),
            
            fluidRow(
              
              box(
                column(12,align="center",
                       h3(paste("TOP",input$EE_choice4,"espèces exotiques envahissantes animales et végétales"),style="color:rgb(70,105,100)"),
                       d3treeOutput("id7_OM")
                )
                ,height=500,width=12
              )
            ),
            fluidRow(
              column(9,offset = 2,align="center",
                     p(
                       span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
              )
            )
            
            
          )
          
        } 
        
        
        
      }
      
    }
    
    
  )
  
  output$id1 <- renderHighchart2({
    
    EE1_FM_plot
    
  })
  
  
  
  output$id2 <- renderHighchart2({
    
    EI1_FM_plot
    
  })
  
  
  output$id3 <- renderHighchart2({
    
    EE2_FM_plot
    
  })
  
  output$id33 <- renderHighchart2({
    
    EE22_FM_plot
    
  })
  
  output$id4 <- renderHighchart2({
    
    EI2_FM_plot
    
  })
  
  
  output$id44 <- renderHighchart2({
    
    EI22_FM_plot
    
  })
  
  output$id5 <- renderHighchart2({
    
    EE3_FM_plot
    
  })
  
  
  output$id55 <- renderHighchart2({
    
    EE33_FM_plot
    
  })
  
  
  output$id6 <- renderHighchart2({
    
    EI3_FM_plot
    
  })
  
  output$id66 <- renderHighchart2({
    
    EI33_FM_plot
    
  })
  
  
  output$id7 <- renderD3tree({
    
    n=input$EE_choice4
    
    
    return(L1[[n-4]])
    
  })
  
  output$ui6 <- renderUI({
    
    if (is.null(input$EE_choice3)) {return()}
    if ((input$EE_choice3[1]=="1" & is.na(input$EE_choice3[2]=="2")) |  input$EE_choice3[1]=="2"){
      
      return(
        
      )
    } else {return()}
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$id8 <- renderD3tree({
    
    n=input$EE_choice4
    
    
    return(L3[[n-4]])
    
  })
  
  output$id9 <- renderD3tree({
    
    n=input$EE_choice41
    
    
    return(L1[[n-4]])
    
  })
  
  output$id10 <- renderD3tree({
    
    n=input$EE_choice42
    
    
    return(L3[[n-4]])
    
  })
  
  
  
  
  
  
  output$id1_OM <- renderHighchart2({
    
    EE1_OM_plot
    
  })
  
  
  
  output$id2_OM <- renderHighchart2({
    
    EI1_OM_plot
    
  })
  
  
  output$id3_OM <- renderHighchart2({
    
    EE2_OM_plot
    
  })
  
  output$id33_OM <- renderHighchart2({
    
    EE22_OM_plot
    
  })
  
  output$id4_OM <- renderHighchart2({
    
    EI2_OM_plot
    
  })
  
  
  output$id44_OM <- renderHighchart2({
    
    EI22_OM_plot
    
  })
  
  output$id5_OM <- renderHighchart2({
    
    EE3_OM_plot
    
  })
  
  
  output$id55_OM <- renderHighchart2({
    
    EE33_OM_plot
    
  })
  
  
  output$id6_OM <- renderHighchart2({
    
    EI3_OM_plot
    
  })
  
  output$id66_OM <- renderHighchart2({
    
    EI33_OM_plot
    
  })
  
  
  output$id7_OM <- renderD3tree({
    
    n=input$EE_choice4
    
    
    return(L2[[n-4]])
    
  })
  
  
  
  
  
  
  
  
  
  
  output$id11 <- renderHighchart2({
    
    EE11_FM_plot
    
  })
  
  output$id22 <- renderHighchart2({
    
    EI11_FM_plot
    
  })
  
  
  output$id11_OM <- renderHighchart2({
    
    EE11_OM_plot
    
  })
  
  
  output$id22_OM <- renderHighchart2({
    
    EI11_OM_plot
    
  })
  
  
  ## IMAGES & LOGOS ##   
  
  output$image1 <- renderImage({
    
    list(src = "Images/image1.png")
  }, deleteFile = FALSE)
  
  
  output$image2 <- renderImage({
    
    list(src = "Images/image2.png")
  }, deleteFile = FALSE)
  
  
  output$image3 <- renderImage({
    
    list(src = "Images/bandeau_enshe.png")
  }, deleteFile = FALSE)
  
  
  output$image4 <- renderImage({
    
    list(src = "Images/image2.png")
  }, deleteFile = FALSE)
  
  
  output$image5 <- renderImage({
    
    list(src = "Images/StatPublique.png")
  }, deleteFile = FALSE)
  
  
  
  
  
  
  
  ##############################################################################  
  # PORTAITS TERRITOIRES #
  ####################################################################################    
  
  
  
  
  
  
  
  
  
  output$PdT3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Surf_zones_humides/Surf_totale`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Surf_zones_humides/Surf_totale"],0)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r, suffix='%'),
      number=list(suffix='%'),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        borderwidth = 3,
        bordercolor = "#466964",
        axis =list(range = list(0, 100),
                   ticksuffix='%'),
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  output$PdT6 <- renderPlotly({
    
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Surf_eau/Surf_totale`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Surf_eau/Surf_totale"],0)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      number=list(suffix='%'),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        borderwidth = 3,
        bordercolor = "#466964",
        axis =list(range = list(0, 100),
                   ticksuffix='%'),
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),
    ) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    
    return(fig)
    
  })
  
  
  
  output$PdT_A1 <- renderPlotly({
    
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Surf_artificialisée/Surf_totale`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Surf_artificialisée/Surf_totale"],0)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      number=list(suffix='%'),
      
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        borderwidth = 3,
        bordercolor = "#466964",
        axis =list(range = list(0, 100),
                   ticksuffix='%'),
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),
    ) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=30),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_A2 <- renderPlotly({
    
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Surf_agricole/Surf_totale`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Surf_agricole/Surf_totale"],0)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      number=list(suffix='%'),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        borderwidth = 3,
        bordercolor = "#466964",
        axis =list(range = list(0, 100),
                   ticksuffix='%'),
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),
    ) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    
    return(fig)
    
  })
  
  
  
  output$PdT_A3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Nombre_activités`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Nombre_activités"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Nombre_activités`,na.rm=TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_H1 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Dysfonctionnements_TOTAL`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Dysfonctionnements_TOTAL"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Dysfonctionnements_TOTAL`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_H2 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Pertubation_milieux`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Pertubation_milieux"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Pertubation_milieux`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height=110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_H3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Pertubation_gestion_eau`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Pertubation_gestion_eau"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Pertubation_gestion_eau`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height=110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_H4 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Altération_qualité_eau`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Altération_qualité_eau"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Altération_qualité_eau`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height=110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_S3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Services_biens`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Services_biens"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "green"),decreasing = list(color = "red")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Services_biens`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height=110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  output$PdT_S4 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Services_régulations`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Services_régulations"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "green"),decreasing = list(color = "red")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Services_régulations`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height=110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  output$PdT_S5 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Services_cultures`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Services_cultures"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "green"),decreasing = list(color = "red")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Services_cultures`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height=110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  output$PdT_EE1 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Nombre_EspècesExotiques`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Nombre_EspècesExotiques"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Nombre_EspècesExotiques`,na.rm=TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_EE2 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Nombre_EspècesIndigènes`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Nombre_EspècesIndigènes"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,max(PdT$`Nombre_EspècesIndigènes`,na.rm=TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_C1 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Phénomènes_TOTAL`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Phénomènes_TOTAL"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Phénomènes_TOTAL`,na.rm=TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_C2 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Phénomènes_climatiques`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Phénomènes_climatiques"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Phénomènes_climatiques`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height =110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_C3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Phénomènes_hydrologiques`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Phénomènes_hydrologiques"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Phénomènes_hydrologiques`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height =110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_C4 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Phénomènes_qualité_eau`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Phénomènes_qualité_eau"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Phénomènes_qualité_eau`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height =110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_C5 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Phénomènes_modifications_physiques`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Phénomènes_modifications_physiques"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Phénomènes_modifications_physiques`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height =110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_C6 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Phénomènes_espèces`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Phénomènes_espèces"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r,increasing = list(color = "red"),decreasing = list(color = "green")),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Phénomènes_espèces`,na.rm = TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      ),height =110) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_S1 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Pourcentage_services_opérants`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Pourcentage_services_opérants"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      number=list(suffix='%'),
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,100),
                   ticksuffix='%'),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_S2 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Pourcentage_Evolution_rendant_services_opérants`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Pourcentage_Evolution_rendant_services_opérants"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      number=list(suffix='%'),
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,100),
                   ticksuffix='%'),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_S0 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Services_TOTAL`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Services_TOTAL"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0, max(PdT$`Services_TOTAL`,na.rm=TRUE))),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_M1 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Surf_forêts/Surf_totale`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Surf_forêts/Surf_totale"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      number=list(suffix='%'),
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,100),
                   ticksuffix='%'),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_M2 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Nb_milieux_doux`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Nb_milieux_doux"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      gauge = list(
        
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,max(PdT$Nb_milieux_doux,na.rm=TRUE))
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_M3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Nb_milieux_sales`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Nb_milieux_sales"],1)
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      delta = list(reference = r),
      gauge = list(
        
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,max(PdT$Nb_milieux_sales,na.rm=TRUE))
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT_F1 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Etat_especes_communes`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Etat_especes_communes"],1)
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,1)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0,t=20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_F2 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Etat_especes_forts_enjeux`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Etat_especes_forts_enjeux"],1)
    
    
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,1)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0, t= 20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_F3 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Evo_etat_especes_communes`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Evo_etat_especes_communes"],1)
    
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,2)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0,t=20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  output$PdT_F4 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Evo_etat_especes_forts_enjeux`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Evo_etat_especes_forts_enjeux"],1)
    
    
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 2)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      
      delta = list(reference = r),
      gauge = list(
        bar = list(color = "#3d9970",thickness=0.75),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,2)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=0,r=0,t=20),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  output$PdT <- renderUI({
    
    if (input$PdT1==""){
      
      tagList(
        
        fluidRow(
          
          column(12,align = "center",style="background-color:#d1dad8;border-radius: 10px;height:450px",
                 
                 
                 br(),
                 p("Cet espace dédié au référent permet d’avoir une visibilité du site dont il a la charge ou la gestion sur l’ensemble des pans de l’évaluation.",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p("Une comparaison avec les sites d’une même typologie pourra être faite.",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p("D’autres informations provenant de sources externes viendront enrichir les résultats territoriaux de l’évaluation (Corine Land Cover, Recensement agricole, etc.). ",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p("Après une sélection du site dans le menu déroulant (classé par type de zones humides), le référent est invité à sélectionner une rubrique :",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p(" - ",  span("Information générale",style="text-decoration:underline;font-size:calc(10px + 0.32vw)")," : localisation, surface, participation aux évaluations précédentes, nombre de répondants à l’évaluation, ancienneté moyenne des référents, fréquence sur le site ;",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p(" - ",span("État",style="text-decoration:underline;font-size:calc(10px + 0.32vw);")," : type d’espaces protégés rencontrés, nombre de milieux doux/salés, état et évolution de l'état des milieux doux/salés, surface en eau et en zones humides, surface en forêts, état et évolution de l'état des espèces communes et à forts enjeux ;",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p(" - ",span("Pressions",style="text-decoration:underline;font-size:calc(10px + 0.32vw)")," : nombre d’activités humaines, surface artificialisée, surface agricole, nombre de dysfonctionnements hydrologiques, de perturbations des milieux, de la gestion des eaux, d’altérations de la qualité des eaux, nombre d’espèces exotiques envahissantes et indigènes à fort développement, nombre de phénomènes liés au changement climatique ;",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 p(" - ",span("Services rendus",style="text-decoration:underline;font-size:calc(10px + 0.32vw)")," : nombre de services rendus, nombre de biens, de services de régulation, de services culturels, part de services opérants, part de services rendus opérants.",style="text-align:justify;color:black;padding-left:15px;font-style:normal;font-size:calc(10px + 0.32vw)"), 
                 
                 
                 
          )
          
        )
      )
      
      
    }else{ 
      
      if (input$PdT2 == "1"){
        
        j <-  which(PdT$Nom_site == input$PdT1)
        
        s1 <- round(PdT[j,"Surface_totale"],0)
        s3 <- round(PdT[j,"Surface_eau"],0)
        s2 <- round(PdT[j,"Surface_zones_humides"],0)
        
        
        
        t <- PdT[j,"Typologie"]
        r <- PdT[j,"Région1"] ; r2 <- PdT[j,"Région2"]
        n <- PdT[j,"Nb_référents"] ; a <- PdT[j,"AgeMoyen_référents"]
        f1 <- PdT[j,"Fréquentation_pro"] ; f2 <- PdT[j,"Fréquentation_perso"]
        fontsize <- "font-size:calc(12px + 0.2vw)"
        if (!is.na(r2)) {r <- paste(r,r2,sep = " - "); }
        if (PdT$Nom_site[j] == "Belledonne et Grandes Rousses"){fontsize <- "font-size:0.85vw;margin:0 0 0px"}
        p <- sum(PdT[j,6:23]!="Non")
        P <- colnames(PdT[j,6:23])[PdT[j,6:23]!="Non"]
        
        Col3 <- "yellow" ; Col4 <- "yellow"
        
        if (PdT$Evalue_en_1990_2000[j] !="Non"){E3 ="check";Col3="green";titre1="Site évalué en"} else {E3 ="times";Col3="red";titre1="Site non évalué en"}
        if (PdT$Evalue_en_2000_2010[j] !="Non"){E4 ="check";Col4="green";titre2="Site évalué en"} else {E4 ="times";Col4="red";titre2="Site non évalué en"}
        
        
        
        tagList(
          
          fluidRow(
            
            column(12,
                   
                   infoBox(width=4,title="Région(s)",tags$p(r,style=fontsize ),fill=TRUE,color="fuchsia",icon=icon("map-marker-alt")),
                   
                   
                   infoBox(width=4,title="Typologie",tags$p(t,style=fontsize),fill=TRUE,color="fuchsia",icon=icon("map-pin")),
                   
                   div(infoBox(value=tags$p(s1,span("(ha)",style=fontsize),style="font-size:calc(16px + 1vw)"),width=4,title="SURFACE TOTALE",icon=icon("map"),color="fuchsia",fill=TRUE)),
                   
                   
                   #infoBox(title= tags$p(P[i], style = "font-size: 25px;"),'58000 ha', icon = icon("cloud"),color="olive",fill=TRUE)
            )
          ), 
          
          fluidRow(
            
            
            column(4, offset=2, align="center",
                   
                   
                   infoBox(title=titre1,HTML('<p style="font-size: calc(12px + 1vw);">1990 - 2000</p>'),fill=TRUE,width=12,color=Col3,icon=icon(E3)),
            ),
            column(4, align="center",
                   
                   infoBox(title=titre2,HTML('<p style="font-size: calc(12px + 1vw);">2000 - 2010</p>'),fill=TRUE,width=12,color=Col4,icon=icon(E4))
                   
            ),
            
            
          ),
          
          fluidRow(
            column(12,
                   h3(em("Cartographie du site humide"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ), br(),
          
          
          
          fluidRow(
            tags$style(type = "text/css", "#mymap {height: calc(110vh - 110px) !important;}"),
            leafletOutput("mymap"),
            br(),
            div(a( "Accéder au module cartographique pour afficher l'ensemble des sites de l'évaluation",href="https://ssm-ecologie.shinyapps.io/carto_enquete_enzh/", target="_blank"),style="text-align:center;font-style:italic;font-size:16px;color:#466964")
            
          ),
          
          br(),br(),
          
          fluidRow(
            
            column(12,
                   h3(em("Caractéristiques des référents"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ), br(),br(),
          
          
        )
        
      } else if (input$PdT2 == "3"){
        
        j <-  which(PdT$Nom_site == input$PdT1)
        
        s1 <- round(PdT[j,"Surface_artificialisée"],0)
        s2 <- round(PdT[j,"Surface_agricole"],0)
        
        
        t <- PdT[j,"Typologie"]
        
        title0=paste0("Nombre d'activités moyen en ")
        title1=paste0("Taux de couverture moyen en  ")
        title2=paste0("Taux de couverture moyen en  ")
        title3=paste0("Nombre de dysfonctionnements hydrologiques moyen en ")
        title4=paste0("Nombre d'espèces exotiques moyen en ")
        title5=paste0("Nombre d'espèces indigènes moyen en ")
        title6=paste0("Nombre moyen de phénomènes liés au changement climatique en ")
        
        v <- PdT$`Nombre_activités`[j]
        r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Nombre_activités"],1)
        
        Press <- PdT$Pression_activites[j]
        
        Press_color='';Press_icon='';
        
        if (is.na(Press)){Press <- "NA";Press_color <- "black"; Press_icon <- "question"}
        if (Press == "Faible") {Press_color <- "green"; Press_icon <- "grin-beam"}
        if (Press == "Modérée") {Press_color <- "lime";Press_icon <- "smile"}
        if (Press == "Moyenne") {Press_color <- "yellow";Press_icon <- "meh"}
        if (Press == "Forte") {Press_color <- "orange";Press_icon <- "frown-open"}
        if (Press == "Très forte") {Press_color <- "red";Press_icon <- "exclamation-triangle"}
        
        
        tagList(
          
          fluidRow(
            column(12,
                   h3(em("Activités humaines"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ),
          fluidRow(
            column(8, align="center",
                   br(),
                   div("Nombre d'activités", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_A3",height="210px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title0, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            ),
            
            column(4, 
                   br(),br(),br(),
                   shinydashboard::infoBox(title="Pression",value=tags$p(Press,style="font-size:calc(14px + 1vw);font-weight:bold"),icon=icon(Press_icon),color=Press_color,width=12,fill=TRUE)
            ),
          ), br(),
          
          fluidRow(
            column(8, align="center",
                   
                   
                   div("Taux de couverture artificielle", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_A1",height="210px"),
                   p(span("-------          ", style = "color:purple;;font-weight:bold;font-size:22px;"),title1, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : CORINE Land Cover.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   br(),
                   
            ),
            column(4,
                   br(),br(),br(),br(),
                   div(infoBox(value=tags$p(s1,span("(ha)",style="font-size:calc(12px + 0.5vw)"),style="font-size:calc(16px + 1vw)"),width=12,title="SURFACE ARTIFICIALISEE",icon=icon("city"),color="fuchsia",fill=TRUE)),
                   
                   
            )
            
          ), br(),
          fluidRow(
            column(8,
                   
                   div("Taux de couverture agricole", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_A2",height="210px"),
                   
                   p(span("-------  ", style = "color:purple;;font-weight:bold;font-size:22px;"),title2, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : CORINE Land Cover.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   br(),
            ),
            
            column(4,
                   br(),br(),br(),br(),
                   div(infoBox(value=tags$p(s2,span("(ha)",style="font-size:calc(12px + 0.5vw)"),style="font-size:calc(16px + 1vw)"),width=12,title="SURFACE AGRICOLE",icon=icon("truck-monster"),color="fuchsia",fill=TRUE)),
                   
                   
            )
            
          ), br(),br(),
          
          fluidRow(
            column(12,
                   
                   h3(em("Hydrologie et hydraulique"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ),
          
          fluidRow(
            column(6, align="center",
                   br(),
                   p("Nombre de dysfonctionnements hydrologiques", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_H1",height="270px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title3, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            ),
            column(6, align="center",
                   br(),
                   p("Nombre de perturbations des milieux", style = "color:#466964;font-weight:bold;font-size:16px;text-align:center;margin-bottom:0px"),
                   plotlyOutput("PdT_H2",height="80px"),
                   br(),
                   p("Nombre de perturbations de la gestion des eaux", style = "color:#466964;font-weight:bold;font-size:16px;text-align:center;margin-bottom:0px"),
                   plotlyOutput("PdT_H3",height="80px"),
                   br(),
                   p("Nombre d'altérations de la qualité des eaux", style = "color:#466964;font-weight:bold;font-size:16px;text-align:center;margin-bottom:0px"),
                   plotlyOutput("PdT_H4",height="80px"),
                   br(),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                   
            )
          ), br(),br(),
          fluidRow(
            column(12,
                   
                   h3(em("Espèces susceptibles de menacer le site"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
            
          ) ,
          fluidRow(
            column(6, align="center",
                   br(),
                   p("Nombre d'espèces exotiques envahissantes", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_EE1",height="250px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title4, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                   
            ),
            column(6, align="center",
                   br(),
                   p("Nombre d'espèces indigènes à fort développement", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_EE2",height="250px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title5, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            )
          ), br(),br(),
          fluidRow(
            column(12,
                   
                   h3(em("Effets potentiels ou perçus liés au changement climatique"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ),
          
          fluidRow(
            column(6, align="center",
                   br(),
                   p("Nombre de phénomènes liés au changement climatique", style = "color:#466964;font-weight:bold;font-size:14px;text-align:center"),
                   plotlyOutput("PdT_C1",height="270px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title6, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            ),
            
            column(6,
                   fluidRow(
                     column(6, align="center",
                            br(),
                            p("Nombre de phénomènes liés au climat", style = "color:#466964;font-weight:bold;font-size:14px;text-align:center;margin-bottom:0px"),
                            plotlyOutput("PdT_C6",height="120px"),
                            
                     ),
                     
                     
                     column(6, align="center",
                            br(),
                            p("Nombre de phénomènes liés aux espèces", style = "color:#466964;font-weight:bold;font-size:14px;text-align:center;margin-bottom:0px"),
                            plotlyOutput("PdT_C2",height="120px"),
                            
                     ),br(),br(),
                     fluidRow(
                       column(6, align="center",
                              br(),
                              p("Nombre de phénomènes liés à l'hydrologie", style = "color:#466964;font-weight:bold;font-size:14px;text-align:center;margin-bottom:0px"),
                              plotlyOutput("PdT_C3",height="120px"),
                              
                       ),
                       
                       column(6, align="center",
                              br(),
                              p("Nombre de phénomènes liés à la qualité des eaux", style = "color:#466964;font-weight:bold;font-size:14px;text-align:center;margin-bottom:0px"),
                              plotlyOutput("PdT_C4",height="120px"),
                              
                       )
                     ),
                     fluidRow(
                       column(6, align="center",
                              br(),
                              p("Nombre de phénomènes liés aux modifications physiques", style = "color:#466964;font-weight:bold;font-size:14px;text-align:center;margin-bottom:0px"),
                              plotlyOutput("PdT_C5",height="120px"),
                              
                       )
                     ),
                     p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                     p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                   )
            )
          ),
          
          
          
        )
        
      }  else if (input$PdT2 == "2"){
        
        j <-  which(PdT$Nom_site == input$PdT1)
        
        
        s3 <- round(PdT[j,"Surface_eau"],0)
        s2 <- round(PdT[j,"Surface_zones_humides"],0)
        s4 <- round(PdT[j,"Surface_forêts"],0)
        
        
        t <- PdT[j,"Typologie"]
        
        legend1=paste0("Taux de couverture moyen en  ")
        title21=paste0("Nombre moyen de milieux doux en ")
        title22=paste0("Nombre moyen de milieux salés en ")
        legend3=paste0("Indice d'état moyen en  ")
        legend4=paste0("Indice d'évolution de l'état moyen en  ")
        
        v1 <- PdT$`Etat_especes_communes`[j]
        
        if (v1<0.25 &!is.na(v1)) {title1= "Très mauvais état" ; color1 = "darkred";icon1="exclamation-triangle"}
        if (between(v1,0.25,0.499)&!is.na(v1)) {title1="Mauvais état" ; color1 = "tomato";icon1="frown-open"}
        if (v1==0.5&!is.na(v1)) {title1="État moyen"; color1 = "orange";icon1="meh"}
        if (between(v1,0.501,0.75)&!is.na(v1)) {title1="Bon état"; color1 = "#91ae4f";icon1="smile"}
        if (v1>0.75&!is.na(v1)) {title1="Très bon état"; color1 = "darkgreen";icon1="grin-beam"}
        
        if(is.na(v1)){title1="Non renseigné"; color1 = "black";icon1="times"}
        
        style1 = paste("color:",color1,";font-weight:bold;font-size:20px")
        
        v2 <- PdT$`Etat_especes_forts_enjeux`[j]
        
        if (v2<0.25 &!is.na(v2)) {title2= "Très mauvais état" ; color2 = "darkred";icon2="exclamation-triangle"}
        if (between(v2,0.25,0.499)&!is.na(v2)) {title2="Mauvais état" ; color2 = "tomato";icon2="frown-open"}
        if (v2==0.5&!is.na(v2)) {title2="État moyen"; color2 = "orange";icon2="meh"}
        if (between(v2,0.501,0.75)&!is.na(v2)) {title2="Bon état"; color2 = "#91ae4f";icon2="smile"}
        if (v2>0.75&!is.na(v2)) {title2="Très bon état"; color2 = "darkgreen";icon2="grin-beam"}
        
        
        if(is.na(v2)){title2="Non renseigné"; color2 = "black";icon2="times"}
        
        style2 = paste("color:",color2,";font-weight:bold;font-size:20px")
        
        v3 <- PdT$`Evo_etat_especes_communes`[j]
        
        if (v3 < 0.5 &!is.na(v3)) {title3= "En forte dégradation" ; color3 = "darkred";icon3="exclamation-triangle"}
        if (between(v3,0.5,0.999)&!is.na(v3)) {title3="En dégradation" ; color3 = "tomato";icon3="frown-open"}
        if (v3==1 &!is.na(v3)) {title3="Stable"; color3 = "orange";icon3="meh"}
        if (between(v3,1.001,1.5)&!is.na(v3)) {title3="En amélioration"; color3 = "#91ae4f";icon3="smile"}
        if (v3>1.5 &!is.na(v3)) {title3="En forte amélioration"; color3 = "darkgreen";icon3="grin-beam"}
        
        if(is.na(v3)){title3="Non renseigné"; color3 = "black";icon3="times"}
        
        
        style3 = paste("color:",color3,";font-weight:bold;font-size:20px")
        
        v4 <- PdT$`Evo_etat_especes_forts_enjeux`[j]
        
        
        if (v4 < 0.5 &!is.na(v4)) {title4= "En forte dégradation" ; color4 = "darkred";icon4="exclamation-triangle"}
        if (between(v4,0.5,0.999)&!is.na(v4)) {title4="En dégradation" ; color4 = "tomato";icon4="frown-open"}
        if (v4==1 &!is.na(v4)) {title4="Stable"; color4 = "orange";icon4="meh"}
        if (between(v4,1.001,1.5)&!is.na(v4)) {title4="En amélioration"; color4 = "#91ae4f";icon4="smile"}
        if (v4> 1.5 &!is.na(v4)) {title4="En forte amélioration"; color4 = "darkgreen";icon4="grin-beam"}
        
        if(is.na(v4)){title4="Non renseigné"; color4 = "black";icon4="times"}
        
        style4 = paste("color:",color4,";font-weight:bold;font-size:20px")
        
        tagList(
          
          
          fluidRow(
            column(12,
                   h3(em("État des milieux"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ), br(),
          
          fluidRow(
            column(8, align="center",
                   p("Nombre de milieux doux", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center;"),
                   plotlyOutput("PdT_M2",height="210px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title21, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   
            ),
            
            uiOutput("PdT_milieuxD"),
            
            
          ), 
          fluidRow(
            column(8, 
                   
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            )
          ),
          br(), br(),
          
          
          
          fluidRow(
            column(8, align="center",
                   p("Nombre de milieux salés", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center;"),
                   plotlyOutput("PdT_M3",height="210px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title22, style = "color:#466964;font-size:14px;text-align:center;",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   
            ),
            
            uiOutput("PdT_milieuxS")
          ), 
          fluidRow(
            column(8, 
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            )
          ),
          fluidRow(
            
            column(8, align="center",
                   
                   div("Taux de couverture en eau", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT3",height="210px"),
                   
                   p(span("-------  ", style = "color:purple;;font-weight:bold;font-size:22px;"),legend1, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : CORINE Land Cover.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   br(),
                   
            ),
            
            column(4,
                   br(),br(),br(),br(),
                   div(infoBox(value=tags$p(s2,span("(ha)",style="font-size:calc(12px + 0.5vw)"),style="font-size:calc(16px + 1vw)"),width=12,title="SURFACE EN EAU",icon=icon("tint"),color="fuchsia",fill=TRUE)),
                   
                   
            )
            
          ),
          
          fluidRow(
            
            column(8, align="center",
                   
                   div("Taux de couverture en zones humides", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT6",height="210px"),
                   
                   p(span("-------  ", style = "color:purple;;font-weight:bold;font-size:22px;"),legend1, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : CORINE Land Cover.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   br(),
            ),
            
            column(4,
                   br(),br(),br(),br(),
                   div(infoBox(value=tags$p(s3,span("(ha)",style="font-size:calc(12px + 0.5vw)"),style="font-size:calc(16px + 1vw)"),width=12,title="SURFACE EN ZONES HUMIDES",icon=icon("water"),color="fuchsia",fill=TRUE)),
                   
                   
            )
            
          ),
          
          
          fluidRow(
            
            column(8, align="center",
                   
                   div("Taux de couverture en forêts et milieux semi-naturels", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_M1",height="210px"),
                   
                   p(span("-------  ", style = "color:purple;;font-weight:bold;font-size:22px;"),legend1, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : CORINE Land Cover.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   br(),
                   
            ),
            
            column(4,
                   br(),br(),br(),br(),
                   div(infoBox(value=tags$p(s4,span("(ha)",style="font-size:calc(12px + 0.5vw)"),style="font-size:calc(16px + 1vw)"),width=12,title="SURFACE FORESTIÈRE et MILIEUX SEMI-NATURELS",icon=icon("tree"),color="fuchsia",fill=TRUE)),
                   
                   
            )
            
          ), br(),br(),
          
          fluidRow(
            column(12,
                   h3(em("État des espèces"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ), br(),
          
          fluidRow(
            
            column(6, align="center",
                   
                   br(),
                   p("État des espèces communes", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   p(title1,span(icon(icon1)),style = style1),
                   
                   plotlyOutput("PdT_F1",height="210px"),
                   
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),legend3, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;"),
                   
            ),
            
            column(6, align="center",
                   br(),
                   p("État des espèces à forts enjeux", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   p(title2,span(icon(icon2)),style = style2),
                   
                   
                   plotlyOutput("PdT_F2",height="210px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),legend3, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            )
            
            
          ),
          
          fluidRow(
            
            column(6, align="center",
                   
                   br(),
                   p("Évolution de l'état des espèces communes", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   p(title3,span(icon(icon3)),style = style3),
                   plotlyOutput("PdT_F3",height="210px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),legend4, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            ),
            
            column(6, align="center",
                   br(),
                   p("Évolution de l'état des espèces à forts enjeux", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   p(title4,span(icon(icon4)),style = style4),
                   plotlyOutput("PdT_F4",height="210px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),legend4, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            )
            
            
          ),
        )
        
      } else if (input$PdT2 == "4"){
        
        j <-  which(PdT$Nom_site == input$PdT1)
        
        
        t <- PdT[j,"Typologie"]
        
        title7=paste0("Nombre moyen de services rendus à la société en ")
        title8=paste0("Taux moyen de services opérants en ")
        title9=paste0("Taux moyen de services rendus opérants en ")
        
        
        tagList(
          
          fluidRow(
            column(12,
                   h3(em("Services rendus à la société"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ), br(),
          
          
          
          fluidRow(
            column(6, align="center",
                   br(),
                   p("Nombre de services rendus à la société", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_S0",height="270px"),
                   p(span("-------          ", style = "color:purple;font-weight:bold;font-size:22px;"),title7, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            ),
            
            column(6, align="center",
                   br(),
                   p("Nombre de biens", style = "color:#466964;font-weight:bold;font-size:16px;text-align:center;margin-bottom:0px"),
                   plotlyOutput("PdT_S3",height="80px"),
                   br(),
                   p("Nombre de services de régulation", style = "color:#466964;font-weight:bold;font-size:16px;text-align:center;margin-bottom:0px"),
                   plotlyOutput("PdT_S4",height="80px"),
                   br(),
                   p("Nombre de services culturels", style = "color:#466964;font-weight:bold;font-size:16px;text-align:center;margin-bottom:0px"),
                   plotlyOutput("PdT_S5",height="80px"),
                   br(),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                   
            )
            
            
            
          ), br(),br(),
          
          fluidRow(
            column(12,
                   h3(em("Opérabilité des services"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                   ,style="background:#466964;border-radius:12px")
          ), br(),
          
          fluidRow(
            column(6,
                   
                   div("Part de services opérants", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),       
                   plotlyOutput("PdT_S1",height="210px"),
                   
                   p(span("-------  ", style = "color:purple;;font-weight:bold;font-size:22px;"),title8, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
            ),
            column(6,align="center",
                   
                   div("Part de services rendus opérants entre 2010-2020", style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                   plotlyOutput("PdT_S2",height="210px"),
                   p(span("-------          ", style = "color:purple;;font-weight:bold;font-size:22px;"),title9, style = "color:#466964;font-size:14px;text-align:center",span(t,style = "color:#466964;font-size:14px;text-align:center;font-weight:bold")),
                   p(span("Source",style="font-weight:bold;")," : Évaluation nationale des sites humides emblématiques (2010-2020), janvier 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;margin:0px"),
                   p(span("Traitements",style="font-weight:bold;"),": CGDD/SDES, mars 2020.",style="text-align:justify;color:black;font-size:12px;padding:0px;")
                   
            ),
          )
          
        )
        
      }
      
    }
    
  })
  
  
  
  
  
  
  output$PdTT <- renderUI({
    
    if (input$PdT1==""){return()}else{ 
      
      if (input$PdT2 == "2"){
        j <-  which(PdT$Nom_site == input$PdT1)
        
        s1 <- round(PdT[j,"Surface_totale"],0)
        s3 <- round(PdT[j,"Surface_eau"],0)
        s2 <- round(PdT[j,"Surface_zones_humides"],0)
        
        
        
        t <- PdT[j,"Typologie"]
        r <- PdT[j,"Région1"] ; r2 <- PdT[j,"Région2"]
        if (!is.na(r2)) {r <- paste(r,r2,sep = " - ")}
        p <- sum(PdT[j,60:77]!="Non")
        P <- colnames(PdT[j,60:77])[PdT[j,60:77]!="Non"]
        
        if (PdT$'Site Ramsar'[j]!="Non"){p <- p + 1 ; P <- c(P,"Site Ramsar")}
        
        
        
        info=NULL; n = NULL;
        
        if(p!=0){
          for (k in 1:length(P)){info = c(info,P[k]) ;  n = c(n,get_number(PdT[[P[k]]][j])) }
        }
        
        if (p==0){
          
          tagList(
            
            fluidRow(
              
              
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              column(12,align="center",
                     column(5,offset=3,align="center",  
                            
                            infoBox(title="",value=tags$p("Aucun espace protégé", style = "font-size: 1.5vw;"),fill=TRUE,width=11,color = "red",icon=icon("times")),
                     )
              ),
              
              
              
            ),
            br(),br()
            
          )
          
          
        }  else if (p == 1){
          
          tagList(
            
            
            fluidRow(
              
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              column(5,offset=4,align="center", 
                     
                     
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 1vw;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award")),
              ),
              
              
            ),
            br(),br()
            
            
          )
          
        } else if (p == 2){
          
          tagList(
            
            fluidRow(
              column(12,
                     h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                     ,style="background:#466964;border-radius:12px")
            ), br(),
            fluidRow(
              
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[1],value=tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[2],value=tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
              
            ),
            br(),br()
            
            
          )
          
        } else if (p == 3){
          
          tagList(
            
            fluidRow(
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     #d3treeOutput("PdT_tree3"),
              ),
              
              
            ),
            br(),br()
            
          )
          
        } else if (p == 4){
          
          tagList(
            
            
            fluidRow(
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
              
            ),
            br(),br()
            
            
          )
          
        }  else if (p == 5){
          
          tagList(
            
            fluidRow(
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[5],tags$p(info[5], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
            ),
            br(),br()
            
            
          )
          
        } else if (p == 7){
          
          tagList(
            
            
            fluidRow(
              
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[5],tags$p(info[5], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[6],tags$p(info[6], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[7],tags$p(info[7], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
              
            ),
            br(),br()
            
            
          )
          
        } else if (p == 6){
          
          tagList(
            
            fluidRow(
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[5],tags$p(info[5], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[6],tags$p(info[6], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              
            ),
            br(),br()
            
            
          )
          
        } else if (p == 8){
          
          tagList(
            
            
            fluidRow(
              
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[5],tags$p(info[5], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[6],tags$p(info[6], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[7],tags$p(info[7], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[8],tags$p(info[8], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
            ),
            br(),br()
            
            
          )
          
        } else if (p == 9){
          
          tagList(
            
            fluidRow(
              
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[5],tags$p(info[5], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[6],tags$p(info[6], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     infoBox(title="",subtitle=n[7],tags$p(info[7], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[8],tags$p(info[8], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[9],tags$p(info[9], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              
            ),
            br(),br()
            
            
          )
          
        }   else if (p == 10){
          
          tagList(
            
            
            fluidRow(
              
              fluidRow(
                column(12,
                       h3(em("Espaces protégés"),style="font-size:25px;color:#f5f5ff;margin-top: 10px;")
                       ,style="background:#466964;border-radius:12px")
              ), br(),
              
              
              column(12,align="center",
                     infoBox(title="",subtitle=n[1],tags$p(info[1], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[2],tags$p(info[2], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[3],tags$p(info[3], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     infoBox(title="",subtitle=n[4],tags$p(info[4], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[5],tags$p(info[5], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
                     infoBox(title="",subtitle=n[6],tags$p(info[6], style = "font-size: 15px;"),fill=TRUE,width=4,color = "fuchsia",icon=icon("award")),
              ),
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[7],tags$p(info[7], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[8],tags$p(info[8], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
              column(12,align="center",
                     column(5,offset=1,
                            
                            infoBox(title="",subtitle=n[9],tags$p(info[9], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                            
                     ),
                     
                     column(5,
                            infoBox(title="",subtitle=n[10],tags$p(info[10], style = "font-size: 15px;"),fill=TRUE,width=11,color = "fuchsia",icon=icon("award"))
                     ),
              ),
              
            ),
            br(),br()
            
            
          )
          
        }
        
      }
      
      
    }
    
  })
  
  
  
  
  
  
  
  output$PdTTT <- renderUI({
    
    if (input$PdT1==""){return()}else{ 
      
      if (input$PdT2 == "1"){
        
        
        j <-  which(PdT$Nom_site == input$PdT1)
        
        s1 <- round(PdT[j,"Surface_totale"],0)
        s3 <- round(PdT[j,"Surface_eau"],0)
        s2 <- round(PdT[j,"Surface_zones_humides"],0)
        
        
        
        t <- PdT[j,"Typologie"]
        r <- PdT[j,"Région1"] ; r2 <- PdT[j,"Région2"]
        n <- PdT[j,"Nb_référents"] ; a <- PdT[j,"AgeMoyen_référents"]
        f1 <- PdT[j,"Fréquentation_pro"] ; f2 <- PdT[j,"Fréquentation_perso"]
        if (!is.na(r2)) {r <- paste(r,r2,sep = " - ")}
        p <- sum(PdT[j,6:23]!="Non")
        P <- colnames(PdT[j,6:23])[PdT[j,6:23]!="Non"]
        
        
        Col3 <- "yellow" ; Col4 <- "yellow"
        
        if (PdT$Evalue_en_1990_2000[j] !="Non"){E3 ="check";Col3="green";titre1="Site évalué en"} else {E3 ="times";Col3="red";titre1="Site non évalué en"}
        if (PdT$Evalue_en_2000_2010[j] !="Non"){E4 ="check";Col4="green";titre2="Site évalué en"} else {E4 ="times";Col4="red";titre2="Site non évalué en"}
        
        
        
        tagList(
          
          
          
          fluidRow(
            
            column(4,align="center",
                   
                   # column(10, offset=2,
                   
                   
                   #   infoBox(title=titre1,HTML('<p style="font-size: calc(12px + 1vw);">1990 - 2000</p>'),fill=TRUE,width=12,color=Col3,icon=icon(E3)),
                   # ),
                   #column(10, offset=2,
                   
                   #infoBox(title=titre2,HTML('<p style="font-size: calc(12px + 1vw);">2000 - 2010</p>'),fill=TRUE,width=12,color=Col4,icon=icon(E4))
                   
                   # ),
            ),
            
            
            column(8, align="center",
                   
                   fluidRow(
                     
                     column(5, offset=1, 
                            
                            
                            infoBox(title="Nombre de référent(s)",tags$p(n,style="font-size:calc(14px + 1vw);"),fill=TRUE,width=11,color="fuchsia",icon=icon("users")),
                     ),
                     column(5, 
                            
                            infoBox(title="Fréquentation professionnelle",tags$p(f1,style="font-size:calc(12px + 0.5vw);"),fill=TRUE,width=11,color="fuchsia",icon=icon("walking"))
                            
                     ),
                   ),
                   
                   fluidRow(
                     
                     
                     column(5, offset=1, 
                            
                            
                            infoBox(title="Ancienneté moyenne des référents",tags$p(a,span("ans",style="font-size:calc(12px + 0.5vw);"),style="font-size:calc(14px + 1vw);"),fill=TRUE,width=11,color="fuchsia",icon=icon("sort-numeric-up")),
                     ),
                     column(5, 
                            
                            infoBox(title="Fréquentation personnelle",tags$p(f2,style="font-size:calc(12px + 0.5vw);"),fill=TRUE,width=11,color="fuchsia",icon=icon("hiking"))
                            
                     )
                   ),
            )
          )
        )
        
        
        
      }
      
      
    }
    
  })
  
  
  
  
  ## PARTIE CARTO ##
  
  output$PdT0 <- renderUI({
    
    if (input$PdT1==""){return()}else{ 
      
      if (input$PdT2 == "1"){
        
        tagList(
          
          br(),
          #Legende CLC
          h5("Afficher la légende de la carte", style='font-weight:bold;font-size:16px;color:#f5f5ff'),
          div(id="action1",actionButton("ShowLegendCLCMet", "CORINE Land Cover métropole")),
          div(id="action2",actionButton("ShowLegendCLCDOM", "CORINE Land Cover DOM")),
          div(id="action3",actionButton("ShowLegendMPH", "Milieux potentiellement humides de métropole")),          
          #tagList(a("Afficher la lÃ©gende CORINE Land Cover mÃ©tropole :", href="https://wxs.ign.fr/static/legends/CLC.png"),
          #        a("Afficher la lÃ©gende CORINE Land Cover DOM :", href="https://wxs.ign.fr/static/legends/CLC.png"))
          
          
          
          tags$head(tags$style("#action1 .btn-default {
                                        color: black;
                                        background-color: #f5f5ff;
                                        border-color: #97989A;
                                        
                                        font-size: 12px;
                                        width:100%;
                                        font-weight: bold;
                                        
                                        border-radius: 12px;
                                        border: 2px solid #ccc;
                                        margin-bottom:2px
                                    }
                                    ")),
          
          tags$head(tags$style("#action2 .btn-default {
                                        color: black;
                                        background-color: #f5f5ff;
                                        border-color: #97989A;
                                        
                                        font-size: 12px;
                                        width:100%;
                                        font-weight: bold;
                                        
                                        border-radius: 12px;
                                        border: 2px solid #ccc;
                                        margin-bottom:2px
                                    }
                                    ")),
          
          tags$head(tags$style("#action3 .btn-default {
                                        color: black;
                                        background-color: #f5f5ff;
                                        border-color: #97989A;
                                        
                                        font-size: 12px;
                                        width:100%;
                                        font-weight: bold;
                                        
                                        border-radius: 12px;
                                        border: 2px solid #ccc;
                                        margin-bottom:2px
                                    }
                                    "))
          
        )
        
      }
      
    }
    
  })
  
  
  
  
  
  
  labels <- sprintf("<strong>%s</strong><br/>%s (ha)",
                    ZH_Layer$NOM , round(ZH_Layer$SURF_HA,0)) %>% lapply(htmltools::HTML)
  
  
  
  ZH_selected_Layer <- reactive({ZH_Layer %>% filter(CODE == input$PdT1)})
  
  bounding_zoom <- reactive({st_bbox(ZH_selected_Layer())})
  
  label_selected_zh <- reactive({sprintf("<strong>%s</strong><br/>%s (ha)",
                                         ZH_selected_Layer()$NOM, round(ZH_selected_Layer()$SURF_HA,0)) %>% lapply(htmltools::HTML)})
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>% fitBounds(lng1 =  as.numeric(bounding_zoom()$xmin),
                            lat1 =  as.numeric(bounding_zoom()$ymin), 
                            lng2 = as.numeric(bounding_zoom()$xmax), 
                            lat2 = as.numeric(bounding_zoom()$ymax)) %>%
      addPolygons(data=ZH_selected_Layer(),
                  color = "red",
                  weight = 3, 
                  
                  fillOpacity = 0.2,
                  
                  fillColor="grey",
                  label = label_selected_zh(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>% 
      
      
      addTiles("http://wxs.ign.fr/vp2cxyan0t7ufwrfbyl3eg7s/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=CORINE%20Land%20Cover&TILEMATRIXSET=PM&FORMAT=image/png&LAYER=LANDCOVER.CLC18&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
               options = WMSTileOptions(tileSize = 256),
               group = "CORINE Land Cover 2018")  %>%
      addTiles("http://wxs.ign.fr/vp2cxyan0t7ufwrfbyl3eg7s/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE=normal&TILEMATRIXSET=PM&FORMAT=image/jpeg&LAYER=GEOGRAPHICALGRIDSYSTEMS.MAPS.SCAN-EXPRESS.STANDARD&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}",
               options = WMSTileOptions(tileSize = 256),
               group = "SCAN") %>% 
      addProviderTiles("Esri.WorldTopoMap", group = "Cartes topographiques") %>% 
      addProviderTiles("Esri.WorldImagery", group = "Orthophotographies") %>% 
      addWMSTiles(
        "http://geowww.agrocampus-ouest.fr/geoserver/wms?",
        layers = "mntsurf:mph_fr",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        group = "Milieux potentiellement humides") %>%       
      addLayersControl(baseGroups = c("Cartes topographiques", "Orthophotographies","SCAN","Milieux potentiellement humides","CORINE Land Cover 2018"),options = layersControlOptions(collapsed = FALSE)) %>%
      addSearchOSM() 
  }) 
  
  
  
  observeEvent(input$ShowLegendCLCMet, {
    showModal(modalDialog(
      title = "Légende CORINE Land Cover métropole",
      HTML('<img src="https://wxs.ign.fr/static/legends/CLC_FR.png" height="850px">'),
      easyClose = TRUE,
      footer = modalButton("Fermer la légende")
    ))
  }) 
  
  observeEvent(input$ShowLegendCLCDOM, {
    showModal(modalDialog(
      title = "Légende CORINE Land Cover DOM",
      HTML('<img src="https://wxs.ign.fr/static/legends/CLC.png" height="1000px">'),
      easyClose = TRUE,
      footer = modalButton("Fermer la légende")
    ))
  })  
  
  observeEvent(input$ShowLegendMPH, {
    showModal(modalDialog(
      title = "Légende des milieux potentiellement humides de métropole",
      HTML('<img src="http://geowww.agrocampus-ouest.fr/geoserver/ows?service=WMS&request=GetLegendGraphic&format=image%2Fpng&width=20&height=20&layer=mntsurf%3Amph_fr" height="190px">'),
      easyClose = TRUE,
      footer = modalButton("Fermer la légende")
    ))
  })    
  
  
  
  
  
  ########
  
  output$PdT_milieuxD <- renderUI({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    
    
    
    t <- PdT[j,"Typologie"]
    
    
    
    Etat_M_D <- PdT$Etat_milieux_D[j]
    
    Etat_M_D_color='';Etat_M_D_icon='';
    
    subtitle1 = "État des milieux doux";
    
    
    if (is.na(Etat_M_D)){Etat_M_D <- "Non renseigné";Etat_M_D_color <- "black"; Etat_M_D_icon <- "question"}
    if (Etat_M_D == "Très bon état") {Etat_M_D <- "Très bon";Etat_M_D_color <- "green"; Etat_M_D_icon <- "grin-beam"}
    if (Etat_M_D == "Bon état") {Etat_M_D <- "Bon";Etat_M_D_color <- "lime";Etat_M_D_icon <- "smile"}
    if (Etat_M_D == "Etat moyen") {Etat_M_D <- "Moyen";Etat_M_D_color <- "yellow";Etat_M_D_icon <- "meh"}
    if (Etat_M_D == "Mauvais état") {Etat_M_D <- "Mauvais";Etat_M_D_color <- "orange";Etat_M_D_icon <- "frown-open"}
    if (Etat_M_D == "Très mauvais état") {Etat_M_D <- "Très mauvais";Etat_M_D_color <- "red";Etat_M_D_icon <- "exclamation-triangle";   }
    
    
    if (Etat_M_D != "Non renseigné") {value1 <- tags$p(Etat_M_D,style="font-size:1.6vw;font-weight:bold;margin:0 0 0px") } else {
      value1 <- tags$p(Etat_M_D,style="font-size:1.2vw;font-weight:bold;margin:0 0 0px")
    }
    
    Evo_Etat_M_D <- PdT$Evolution_etat_milieux_D[j]
    
    Evo_Etat_M_D_color='';Evo_Etat_M_D_icon='';
    
    subtitle3 = "Évolution de l'état des milieux";
    
    if (is.na(Evo_Etat_M_D)){Evo_Etat_M_D <- "Non renseigné";Evo_Etat_M_D_color <- "black"; Evo_Etat_M_D_icon <- "question"}
    if (Evo_Etat_M_D == "En forte amélioration") {Evo_Etat_M_D <- "Forte amélioration";Evo_Etat_M_D_color <- "green"; Evo_Etat_M_D_icon <- "grin-beam"}
    if (Evo_Etat_M_D == "En amélioration") {Evo_Etat_M_D <- "Amélioration";Evo_Etat_M_D_color <- "lime";Evo_Etat_M_D_icon <- "smile"}
    if (Evo_Etat_M_D == "Stable") {Evo_Etat_M_D <- "Stable";Evo_Etat_M_D_color <- "yellow";Evo_Etat_M_D_icon <- "meh"}
    if (Evo_Etat_M_D == "En dégradation") {Evo_Etat_M_D <- "Dégradation";Evo_Etat_M_D_color <- "orange";Evo_Etat_M_D_icon <- "frown-open"}
    
    if (Evo_Etat_M_D != "Non renseigné") {value3 <- tags$p(Evo_Etat_M_D,style="font-size:1.6vw;font-weight:bold;margin:0 0 0px") } else {
      value3 <- tags$p(Evo_Etat_M_D,style="font-size:1.2vw;font-weight:bold;margin:0 0 0px")
    }
    
    if (Evo_Etat_M_D == "Forte amélioration" | Evo_Etat_M_D == "Forte dégradation" ) {value3 <- tags$p(Evo_Etat_M_D,style="font-size:1.4vw;font-weight:bold;margin:0 0 0px") } 
    
    
    
    if (Evo_Etat_M_D == "En forte dégradation") {Evo_Etat_M_D <- "Forte dégradation";Evo_Etat_M_D_color <- "red";Evo_Etat_M_D_icon <- "exclamation-triangle";    value3 <- tags$p(Evo_Etat_M_D,style="font-size:1.45vw;font-weight:bold;margin:0 0 0px")   }
    if (Evo_Etat_M_D == "En forte amélioration") {Evo_Etat_M_D <- "Forte amélioration";Evo_Etat_M_D_color <- "red";Evo_Etat_M_D_icon <- "exclamation-triangle";    value3 <- tags$p(Evo_Etat_M_D,style="font-size:1.45vw;font-weight:bold;margin:0 0 0px")   }
    
    
    
    
    
    
    v1 <- PdT$`Etat_milieux_doux`[j]
    
    
    if (v1<0.25 &!is.na(v1)) {titre1= "Très mauvais" ; color1 = "darkred";icon1="exclamation-triangle"}
    if (between(v1,0.25,0.499)&!is.na(v1)) {titre1="Mauvais" ; color1 = "tomato";icon1="frown-open"}
    if (v1==0.5&!is.na(v1)) {titre1="Moyen"; color1 = "orange";icon1="meh"}
    if (between(v1,0.501,0.75)&!is.na(v1)) {titre1="Bon"; color1 = "#91ae4f";icon1="smile"}
    if (v1>0.75&!is.na(v1)) {titre1="Très bon"; color1 = "darkgreen";icon1="grin-beam"}
    
    
    
    if(is.na(v1)){titre1="Non renseigné"; color1 = "black";icon1="times"}
    
    style1 = paste("color:",color1,";font-weight:bold;font-size:20px")
    
    
    v2 <- PdT$`Evolution_etat_milieux_doux`[j]
    
    if (v2<0.7 &!is.na(v2)) {titre2= "En forte dégradation" ; color2 = "darkred";icon2="exclamation-triangle"}
    if (between(v2,0.7,0.899)&!is.na(v2)) {titre2="En dégradation" ; color2 = "tomato";icon2="frown-open"}
    if (between(v2,0.9,1.1) &!is.na(v2)) {titre2="Stable"; color2 = "orange";icon2="meh"}
    if (between(v2,1.001,1.3)&!is.na(v2)) {titre2="En amélioration"; color2 = "#91ae4f";icon2="smile"}
    if (v2>1.3 &!is.na(v2)) {titre2="En forte amélioration"; color2 = "darkgreen";icon2="grin-beam"}
    
    
    
    
    if(is.na(v2)){titre2="Non renseignée"; color2 = "black";icon2="times"}
    
    style2 = paste("color:",color2,";font-weight:bold;font-size:20px")
    
    
    if ((!is.na(PdT$Nb_milieux_doux[j]) & PdT$Nb_milieux_doux[j]!= 0) | is.na(PdT$Nb_milieux_doux[j])){
      
      tagList(
        column(4,align="center",
               fluidRow(
                 
                 #shinydashboard::infoBox(title=subtitle1,value=value1,icon=icon(Etat_M_D_icon ),color=Etat_M_D_color ,width=12,fill=TRUE)
                 #,style="padding-top: 15px;padding-left:10px"
                 p("État : ", span(titre1,style=style1),span(icon(icon1),style=style1),style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                 
                 plotlyOutput("PdT_M22",height="100px")
               ),
               
               fluidRow(
                 #shinydashboard::infoBox(title=subtitle3,value=value3,icon=icon(Evo_Etat_M_D_icon ),color=Evo_Etat_M_D_color ,width=12,fill=TRUE)
                 #,style="padding-top: 15px;padding-left:10px"
                 p("Évolution : ",span(titre2,style=style2),span(icon(icon2),style=style2),style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                 plotlyOutput("PdT_M23",height="100px")
               )
               
        )
      )
      
    } else if (!is.na(PdT$Nb_milieux_doux[j]) & PdT$Nb_milieux_doux[j]== 0){
      
      tagList(
        
        column(4, 
               br(),br(),
               shinydashboard::infoBox(title="",value=tags$p("Aucun milieu doux",style="calc(14px + 1vw);font-weight:bold;margin:0 0 0px"),icon=icon("times"),color="light-blue" ,width=12,fill=TRUE)
               ,style="padding-top: 15px;padding-left:10px")
      )
      
    } 
    
  }
  )
  
  
  
  
  
  output$PdT_milieuxS <- renderUI({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    
    
    
    t <- PdT[j,"Typologie"]
    
    
    subtitle1 = "État des milieux salés";
    
    
    Etat_M_S <- PdT$Etat_milieux_S[j]
    
    
    
    Etat_M_S_color='';Etat_M_S_icon='';
    
    
    if (is.na(Etat_M_S)){Etat_M_S <- "Non renseigné";Etat_M_S_color <- "black"; Etat_M_S_icon <- "question"}
    if (Etat_M_S == "Très bon état") {Etat_M_S <- "Très bon";Etat_M_S_color <- "green"; Etat_M_S_icon <- "grin-beam"}
    if (Etat_M_S == "Bon état") {Etat_M_S <- "Bon";Etat_M_S_color <- "lime";Etat_M_S_icon <- "smile"}
    if (Etat_M_S == "Etat moyen") {Etat_M_S <- "Moyen";Etat_M_S_color <- "yellow";Etat_M_S_icon <- "meh"}
    if (Etat_M_S == "Mauvais état") {Etat_M_S <- "Mauvais";Etat_M_S_color <- "orange";Etat_M_S_icon <- "frown-open"}
    if (Etat_M_S == "Très mauvais état") {Etat_M_S <- "Très mauvais";Etat_M_S_color <- "red";Etat_M_S_icon <- "exclamation-triangle"; } 
    
    
    if (Etat_M_S != "Non renseigné") {value1 <- tags$p(Etat_M_S,style="font-size:1.6vw;font-weight:bold;margin:0 0 0px") } else {
      value1 <- tags$p(Etat_M_S,style="font-size:1.2vw;font-weight:bold;margin:0 0 0px")
    }
    
    Evo_Etat_M_S <- PdT$Evolution_etat_milieux_S[j]
    
    Evo_Etat_M_S_color='';Evo_Etat_M_S_icon='';
    
    subtitle3 = "Évolution de l'état des milieux";
    
    if (is.na(Evo_Etat_M_S)){Evo_Etat_M_S <- "Non renseigné";Evo_Etat_M_S_color <- "black"; Evo_Etat_M_S_icon <- "question"}
    if (Evo_Etat_M_S == "En forte amélioration") {Evo_Etat_M_S <- "Forte amélioration";Evo_Etat_M_S_color <- "green"; Evo_Etat_M_S_icon <- "grin-beam"}
    if (Evo_Etat_M_S == "En amélioration") {Evo_Etat_M_S <- "Amélioration";Evo_Etat_M_S_color <- "lime";Evo_Etat_M_S_icon <- "smile"}
    if (Evo_Etat_M_S == "Stable") {Evo_Etat_M_S <- "Stable";Evo_Etat_M_S_color <- "yellow";Evo_Etat_M_S_icon <- "meh"}
    if (Evo_Etat_M_S == "En dégradation") {Evo_Etat_M_S <- "Dégradation";Evo_Etat_M_S_color <- "orange";Evo_Etat_M_S_icon <- "frown-open"}
    
    if (Evo_Etat_M_S != "Non renseigné") {value3 <- tags$p(Evo_Etat_M_S,style="font-size:1.6vw;font-weight:bold;margin:0 0 0px") } else {
      value3 <- tags$p(Evo_Etat_M_S,style="font-size:1.2vw;font-weight:bold;margin:0 0 0px")
    }
    
    
    if (Evo_Etat_M_S == "Forte amélioration" | Evo_Etat_M_S == "Forte dégradation" ) {value3 <- tags$p(Evo_Etat_M_S,style="font-size:1.4vw;font-weight:bold;margin:0 0 0px") } 
    
    
    if (Evo_Etat_M_S == "En forte dégradation") {Evo_Etat_M_S <- "Forte dégradation";Evo_Etat_M_S_color <- "red";Evo_Etat_M_S_icon <- "exclamation-triangle";    value3 <- tags$p(Evo_Etat_M_S,style="font-size:1.45vw;font-weight:bold;margin:0 0 0px")   }
    if (Evo_Etat_M_S == "En forte amélioration") {Evo_Etat_M_S <- "Forte amélioration";Evo_Etat_M_S_color <- "red";Evo_Etat_M_S_icon <- "exclamation-triangle";    value3 <- tags$p(Evo_Etat_M_S,style="font-size:1.45vw;font-weight:bold;margin:0 0 0px")   }
    
    
    
    
    
    v1 <- PdT$`Etat_milieux_sales`[j]
    
    
    if (v1<0.25 &!is.na(v1)) {titre1= "Très mauvais" ; color1 = "darkred";icon1="exclamation-triangle"}
    if (between(v1,0.25,0.499)&!is.na(v1)) {titre1="Mauvais" ; color1 = "tomato";icon1="frown-open"}
    if (v1==0.5&!is.na(v1)) {titre1="Moyen"; color1 = "orange";icon1="meh"}
    if (between(v1,0.501,0.75)&!is.na(v1)) {titre1="Bon"; color1 = "#91ae4f";icon1="smile"}
    if (v1>0.75&!is.na(v1)) {titre1="Très bon"; color1 = "darkgreen";icon1="grin-beam"}
    
    
    
    if(is.na(v1)){titre1="Non renseigné"; color1 = "black";icon1="times"}
    
    style1 = paste("color:",color1,";font-weight:bold;font-size:20px")
    
    
    v2 <- PdT$`Evolution_etat_milieux_sales`[j]
    
    if (v2<0.7 &!is.na(v2)) {titre2= "En forte dégradation" ; color2 = "darkred";icon2="exclamation-triangle"}
    if (between(v2,0.7,0.899)&!is.na(v2)) {titre2="En dégradation" ; color2 = "tomato";icon2="frown-open"}
    if (between(v2,0.9,1.1) &!is.na(v2)) {titre2="Stable"; color2 = "orange";icon2="meh"}
    if (between(v2,1.001,1.3)&!is.na(v2)) {titre2="En amélioration"; color2 = "#91ae4f";icon2="smile"}
    if (v2>1.3 &!is.na(v2)) {titre2="En forte amélioration"; color2 = "darkgreen";icon2="grin-beam"}
    
    
    
    
    if(is.na(v2)){titre2="Non renseignée"; color2 = "black";icon2="times"}
    
    style2 = paste("color:",color2,";font-weight:bold;font-size:20px")
    
    
    
    if ((!is.na(PdT$Nb_milieux_sales[j]) & PdT$Nb_milieux_sales[j]!= 0) | is.na(PdT$Nb_milieux_sales[j])){
      
      tagList(
        column(4, align="center",
               fluidRow(
                 #shinydashboard::infoBox(title=subtitle1,value=value1,icon=icon(Etat_M_S_icon ),color=Etat_M_S_color ,width=12,fill=TRUE)
                 #,style="padding-top: 15px;padding-left:10px"
                 p("État : ", span(titre1,style=style1),span(icon(icon1),style=style1),style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                 
                 plotlyOutput("PdT_M32",height="100px")
               ),
               
               fluidRow(
                 # shinydashboard::infoBox(title=subtitle3,value=value3,icon=icon(Evo_Etat_M_S_icon ),color=Evo_Etat_M_S_color ,width=12,fill=TRUE)
                 # ,style="padding-top: 15px;padding-left:10px")
                 p("Évolution : ",span(titre2,style=style2),span(icon(icon2),style=style2),style = "color:#466964;font-weight:bold;font-size:18px;text-align:center"),
                 plotlyOutput("PdT_M33",height="100px")
               )
               
        )
      )
      
    } else if (!is.na(PdT$Nb_milieux_sales[j]) & PdT$Nb_milieux_sales[j]== 0){
      
      tagList(
        
        column(4, 
               br(),br(),
               shinydashboard::infoBox(title="",value=tags$p("Aucun milieu salé",style="calc(14px + 1vw);font-weight:bold;margin:0 0 0px"),icon=icon("times"),color="light-blue" ,width=12,fill=TRUE)
               ,style="padding-top: 15px;padding-left:10px")
      )
      
    }
    
  }
  )
  
  
  
  output$PdT_M22 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Etat_milieux_doux`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Etat_milieux_doux"],1)
    
    if (v<0.25 &!is.na(v)) {title= "Très mauvais état" ; color = "darkred"}
    if (between(v,0.25,0.499)&!is.na(v)) {title="Mauvais état" ; color = "tomato"}
    if (v==0.5&!is.na(v)) {title="État moyen"; color = "orange"}
    if (between(v,0.501,0.75)&!is.na(v)) {title="Bon état"; color = "green"}
    if (v>0.75&!is.na(v)) {title="Très bon état"; color = "darkgreen"}
    
    if (!is.na(v)){title <- paste("<b>",title,"</b>")}
    
    if(is.na(v)){title=""; color = "black"}
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      #title=list(text=title,font=list(family="'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif",size=20,color=color)),
      delta = list(reference = r),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,1)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0,t=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_M23 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Evolution_etat_milieux_doux`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Evolution_etat_milieux_doux"],1)
    
    if (v<0.7 &!is.na(v)) {title= "En forte dégradation" ; color = "darkred"}
    if (between(v,0.7,0.899)&!is.na(v)) {title="En dégradation" ; color = "tomato"}
    if (between(v,0.9,1.1) &!is.na(v)) {title="Stable"; color = "orange"}
    if (between(v,1.001,1.3)&!is.na(v)) {title="En amélioration"; color = "green"}
    if (v>1.3 &!is.na(v)) {title="En forte amélioration"; color = "darkgreen"}
    
    if (!is.na(v)){title <- paste("<b>",title,"</b>")}
    
    if(is.na(v)){title=""; color = "black"}
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      #title=list(text=title,font=list(family="'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif",size=20,color=color)),
      delta = list(reference = r),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,2)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0,t=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  
  
  
  
  output$PdT_M32 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Etat_milieux_sales`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Etat_milieux_sales"],1)
    
    if (v<0.25 &!is.na(v)) {title= "Très mauvais état" ; color = "darkred"}
    if (between(v,0.25,0.499)&!is.na(v)) {title="Mauvais état" ; color = "tomato"}
    if (v==0.5&!is.na(v)) {title="État moyen"; color = "orange"}
    if (between(v,0.501,0.75)&!is.na(v)) {title="Bon état"; color = "green"}
    if (v>0.75&!is.na(v)) {title="Très bon état"; color = "darkgreen"}
    
    if (!is.na(v)){title <- paste("<b>",title,"</b>")}
    
    if(is.na(v)){title=""; color = "black"}
    
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      #title=list(text=title,font=list(family="'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif",size=20,color=color)),
      delta = list(reference = r),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,1)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0,t=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  output$PdT_M33 <- renderPlotly({
    
    j <-  which(PdT$Nom_site == input$PdT1)
    t <- PdT[j,"Typologie"]
    
    v <- PdT$`Evolution_etat_milieux_sales`[j]
    r <- round(PdT_typologie[PdT_typologie$Typologie==t,"Evolution_etat_milieux_sales"],1)
    
    if (v<0.7 &!is.na(v)) {title= "En forte dégradation" ; color = "darkred"}
    if (between(v,0.7,0.899)&!is.na(v)) {title="En dégradation" ; color = "tomato"}
    if (between(v,0.9,1.1) &!is.na(v)) {title="Stable"; color = "orange"}
    if (between(v,1.001,1.3)&!is.na(v)) {title="En amélioration"; color = "green"}
    if (v>1.3 &!is.na(v)) {title="En forte amélioration"; color = "darkgreen"}
    
    if (!is.na(v)){title <- paste("<b>",title,"</b>")}
    
    if(is.na(v)){title=""; color = "black"}
    
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = v,
      type = "indicator",
      mode = "gauge+number+delta",
      #title=list(text=title,font=list(family="'Source Sans Pro','Helvetica Neue',Helvetica,Arial,sans-serif",size=20,color=color)),
      delta = list(reference = r),
      gauge = list(
        shape="bullet",
        bar = list(color = "#3d9970",thickness=0.5),
        #steps = list(
        # list(range = c(0, 3), color = "#38ec64"),
        #list(range = c(3, 6), color = "#98e533"),
        # list(range = c(6, 9), color = "#cde219"),
        #list(range = c(9, 12), color = "#f2df07"),
        # list(range = c(12, 15), color = "#ffc81d"),
        # list(range = c(15, 18), color = "#ff9121"),
        #list(range = c(18, 21), color = "#ff7204"),
        #list(range = c(21, 24), color = "#f28721"),
        # list(range = c(24, 27), color = "#e96d23"),
        # list(range = c(17, 30), color = "#e64630")
        # ),
        borderwidth = 3,
        bordercolor = "#466964",
        bgcolor="-RdGy",
        axis =list(range = list(0,2)
        ),
        
        threshold = list(
          line = list(color = "purple", width = 4),
          thickness = 1,
          value = r)
      )) 
    fig <- fig %>%
      layout(autosize=TRUE,margin = list(l=10,r=0,t=0),
             font = list(color = "#466964", family = "Arial"),plot_bgcolor=rgb(1/254, 1/247, 1/234),paper_bgcolor="transparent")
    
    return(fig)
    
  })
  
  
  ## Télécharger le questionnaire ##
  
  
  output$Q_DL <- downloadHandler(
    filename = function() {
      paste0("ENSHE 2010-2020", ".pdf")
    },
    content = function(file) {
      if(input$Q_buttons == "FM") {
        file.copy("Images/LimeSurvey - ENSHE 2010-2020 (METROPOLE).pdf", file)
      } 
      else if(input$Q_buttons == "OM") {
        file.copy("Images/LimeSurvey - ENSHE 2010-2020 (OUTRE-MER).pdf", file)
      } 
      
    }
  ) 
  
  
  
  
  ## CROISEMENT MULTI THEMATIQUES ##
  
  
  output$CR_M_MM1<- renderHighchart2({
    
    CR_M_MM1_plot
    
  })
  
  
  output$CR_M_MM11 <- renderHighchart2({
    
    CR_M_MM11_plot
    
  })
  
  output$CR_M_MM12<- renderHighchart2({
    
    CR_M_MM12_plot
    
  })
  
  
  
  f_CR_A_MM2 <- reactive(switch(input$input_CR1, 
                                "1" = CR_A_MM2,
                                "2" = CR_A_T1_MM2,
                                "3" = CR_A_T2_MM2,
                                "4" = CR_A_T3_MM2,
                                "5" = CR_A_T5_MM2,
                                "6" = CR_A_T6_MM2,
                                "7" = CR_A_T4_MM2,
  ))
  
  
  
  output$CR_A_MM1 <- renderHighchart2({
    
    
    CR_A_MM1
  })
  
  output$CR_A_MM2 <- renderHighchart2({
    
    f_CR_A_MM2()
    
  })
  
  output$CR_A_MM3 <- renderHighchart2({
    
    
    
    
    if (input$input_CR2=="1"){
      CR_A_MM3
    } else if (input$input_CR2=="2") {
      
      CR_A_MM4
    } else {
      
      CR_A_MM5
      
    }
    
  })
  
  
  output$CR_F_EE1 <- renderHighchart2({
    
    if (input$input_CR3=="1"){
      
      if (input$input_CR4=="1"){
        
        if (input$input_CR5=="1"){
          
          CR_F_EE1_AniUni
          
        } else {
          
          CR_F_EE1_AniAll
          
        }
        
      } else {
        
        if (input$input_CR5=="1"){
          
          CR_F_EE1_VegUni
          
        } else {
          
          CR_F_EE1_VegAll
          
        }
        
      }
      
    } else if (input$input_CR3=="2"){
      
      
      
      if (input$input_CR4=="1"){
        
        if (input$input_CR5=="1"){
          
          CR_F_EE1_AniUni_FM
          
        } else {
          
          CR_F_EE1_AniAll_FM
          
        }
        
      } else {
        
        if (input$input_CR5=="1"){
          
          CR_F_EE1_VegUni_FM
          
        } else {
          
          CR_F_EE1_VegAll_FM
          
        }
        
      }
      
      
    } else {
      
      
      if (input$input_CR4=="1"){
        
        if (input$input_CR5=="1"){
          
          CR_F_EE1_AniUni_OM
          
        } else {
          
          CR_F_EE1_AniAll_OM
          
        }
        
      } else {
        
        if (input$input_CR5=="1"){
          
          CR_F_EE1_VegUni_OM
          
        } else {
          
          CR_F_EE1_VegAll_OM
          
        }
        
      }
      
    }
    
  })
  
  output$CR_F_EE2 <- renderHighchart2({
    
    if(input$input_CR7=="2"){
      
      if (input$input_CR3=="1"){
        
        if (input$input_CR4=="1"){
          
          if (input$input_CR5=="1"){
            
            CR_F_EE2_AniUni
            
          } else {
            
            CR_F_EE2_AniAll
            
          }
          
        } else {
          
          if (input$input_CR5=="1"){
            
            CR_F_EE2_VegUni
            
          } else {
            
            CR_F_EE2_VegAll
            
          }
          
        }
        
      } else if (input$input_CR3=="2"){
        
        
        
        if (input$input_CR4=="1"){
          
          if (input$input_CR5=="1"){
            
            CR_F_EE2_AniUni_FM
            
          } else {
            
            CR_F_EE2_AniAll_FM
            
          }
          
        } else {
          
          if (input$input_CR5=="1"){
            
            CR_F_EE2_VegUni_FM
            
          } else {
            
            CR_F_EE2_VegAll_FM
            
          }
          
        }
        
        
      } else {
        
        
        if (input$input_CR4=="1"){
          
          if (input$input_CR5=="1"){
            
            CR_F_EE2_AniUni_OM
            
          } else {
            
            CR_F_EE2_AniAll_OM
            
          }
          
        } else {
          
          if (input$input_CR5=="1"){
            
            CR_F_EE2_VegUni_OM
            
          } else {
            
            CR_F_EE2_VegAll_OM
            
          }
          
        }
        
      }
      
      
      
      
    } else {
      
      if (input$input_CR3=="1"){
        
        if (input$input_CR4=="1"){
          
          if (input$input_CR5=="1"){
            
            CR_F_EE4_AniUni
            
          } else {
            
            CR_F_EE4_AniAll
            
          }
          
        } else {
          
          if (input$input_CR5=="1"){
            
            CR_F_EE4_VegUni
            
          } else {
            
            CR_F_EE4_VegAll
            
          }
          
        }
        
      } else if (input$input_CR3=="2"){
        
        
        
        if (input$input_CR4=="1"){
          
          if (input$input_CR5=="1"){
            
            CR_F_EE4_AniUni_FM
            
          } else {
            
            CR_F_EE4_AniAll_FM
            
          }
          
        } else {
          
          if (input$input_CR5=="1"){
            
            CR_F_EE4_VegUni_FM
            
          } else {
            
            CR_F_EE4_VegAll_FM
            
          }
          
        }
        
        
      } else {
        
        
        if (input$input_CR4=="1"){
          
          if (input$input_CR5=="1"){
            
            CR_F_EE4_AniUni_OM
            
          } else {
            
            CR_F_EE4_AniAll_OM
            
          }
          
        } else {
          
          if (input$input_CR5=="1"){
            
            CR_F_EE4_VegUni_OM
            
          } else {
            
            CR_F_EE4_VegAll_OM
            
          }
          
        }
        
      }
      
    }
    
  })
  
  
  output$CR_F_EE3 <- renderHighchart2({
    
    if (input$input_CR3=="1"){
      
      if (input$input_CR4=="1"){
        
        if (input$input_CR5=="1"){
          
          CR_F_EE3_AniUni
          
        } else {
          
          CR_F_EE3_AniAll
          
        }
        
      } else {
        
        if (input$input_CR5=="1"){
          
          CR_F_EE3_VegUni
          
        } else {
          
          CR_F_EE3_VegAll
          
        }
        
      }
      
    } else if (input$input_CR3=="2"){
      
      
      
      if (input$input_CR4=="1"){
        
        if (input$input_CR5=="1"){
          
          CR_F_EE3_AniUni_FM
          
        } else {
          
          CR_F_EE3_AniAll_FM
          
        }
        
      } else {
        
        if (input$input_CR5=="1"){
          
          CR_F_EE3_VegUni_FM
          
        } else {
          
          CR_F_EE3_VegAll_FM
          
        }
        
      }
      
      
    } else {
      
      
      if (input$input_CR4=="1"){
        
        if (input$input_CR5=="1"){
          
          CR_F_EE3_AniUni_OM
          
        } else {
          
          CR_F_EE3_AniAll_OM
          
        }
        
      } else {
        
        if (input$input_CR5=="1"){
          
          CR_F_EE3_VegUni_OM
          
        } else {
          
          CR_F_EE3_VegAll_OM
          
        }
        
      }
      
    }
    
  })
  
  output$ui_CR1 <- renderUI({
    
    if (input$input_CR4=="1"){
      
      tagList(
        
        div(id="input_CR5",selectInput("input_CR5",label=HTML("<p style='color:#f5f5ff;font-size:15px'>Choix du sujet : </p>"),
                                       c( "Influence des espèces menaçantes animales" = "1",
                                          
                                          "Influence de toutes les espèces menaçantes" = "2"
                                       ), 
                                       selectize=FALSE,multiple=TRUE,selected="1")),
        
      )
      
    } else {
      
      tagList( 
        div(id="input_CR5",selectInput("input_CR5",label=HTML("<p style='color:#f5f5ff;font-size:15px'>Choix du sujet : </p>"),
                                       c( "Influence des espèces menaçantes végétales" = "1",
                                          
                                          "Influence de toutes les espèces menaçantes" = "2"
                                       ), 
                                       selectize=FALSE,multiple=TRUE,selected="1")),
        
      )
      
    }
    
  }
  )
  
  output$CR_All <- renderHighchart2({
    
    hchart(as.matrix(Tab_corr[input$input_CR8,input$input_CR8])) %>%
      hc_title(text="<span style='font-size:16px'>Tableau des corrélations</span>",style=list(color="#466964",useHTML=TRUE)) %>%
      hc_exporting(enabled=TRUE,filename='ENSHE 2010-2020',formAttributes = list(target = "_blank"), buttons=list(contextButton=list(theme=list(fill='#f5f5ff'),symbol='url(https://zupimages.net/up/20/45/8fsg.png)',symbolSize=16,symbolStrokeWidth=0,symbolFill='#f5f5ff',symbolStroke='#f5f5ff',text=HTML("<p style='color:#466964;font-size:14px;font-weight:bold;font-style:italic'>Télécharger</p>"),symbolX=25,symbolY=20)))
    
  })
  
  
  
  
  
  
  
  
  # COMMENTAIRES #
  
  output$comment_MM1 <- renderUI({
    
    if (input$MM07 == "1"){
      
      tagList(
        
        fluidRow(
          column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                 
                 p(em(text5_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                 
          )
        ))
    } else {
      
      tagList(
        
        fluidRow(
          column(10, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
                 
                 p(em(text6_MM),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
                 
          )
        ))
      
    }
    
    
  })
  
  
  
  
  output$comment1_CR_A_MM <- renderUI({
    
    if (input$input_CR2 == "1"){
      
      tagList(
        
        
        column(6, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
               
               p(em("Ce graphique représente les indices d'état écologique minimum et maximum calculés sur les sites selon le nombre d'activités humaines qu'ils accueillent. L'indice d'état écologique, compris entre 0 (très mauvais état) et 1 (très bon état), caractérise l'état écologique des milieux des sites."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
               
        )
      )
    } else if (input$input_CR2 == "2") {
      
      tagList(
        
        
        column(6, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
               
               p(em("Ce graphique illustre la répartition de l'indice d'état écologique selon le nombre d'activités sur les sites. L'indice d'état écologique, compris entre 0 (très mauvais état) et 1 (très bon état), caractérise l'état écologique des milieux des sites. Cette réprésentation met en évidence le fait que l'indice d'état écologique moyen (et médian) baisse quand le nombre d'activités sur le site humide augmente. "),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
               
        )
      )
      
    } else {
      
      
      tagList(
        
        
        column(6, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
               
               p(em("Ce graphique représente l' indice d'état écologique de chaque site humide et le nombre d'activités associé. L'indice d'état écologique, compris entre 0 (très mauvais état) et 1 (très bon état), caractérise l'état écologique des milieux des sites. Une modélisation (régression) linéaire de cet indice en fonction du nombre d'activités montre que l'état des sites humides tend à décroître quand le nombre d'activités augmente."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
               
        )
      )
      
    }
    
    
  })
  
  
  
  
  
  output$comment1_CR_F_EE <- renderUI({
    
    if (input$input_CR7 == "1"){
      
      tagList(
        
        
        column(11, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
               
               p(em("Ce graphique représente les nombres minimum et maximum d'espèces menaçantes selon l'état des espèces à protéger sur les sites. L'indice d'état des espèces, compris entre 0 (espèces en très mauvais état) et 1 (espèces en très bon état), caractérise l'état des espèces à protéger sur les sites."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
               
        )
      )
    } else if (input$input_CR7 == "2") {
      
      tagList(
        
        
        column(11, offset=1, align ="center",style="border: 1px dotted #466964;border-radius: 5px",
               
               p(em("Ce graphique représente la répartition du nombre d'espèces menaçantes selon l'indice d'état des espèces à protéger sur les sites. L'indice d'état des espèces, compris entre 0 (espèces très mauvais état) et 1 (espèces en très bon état), caractérise l'état des espèces à protéger sur les sites. On observe que, en France métropolitaine, le nombre moyen (et médian) d'espèces menaçantes diffère selon l'état des espèces à protéger. Cela tend à montrer que le nombre d'espèces menaçantes sur un site influe sur l'état des espèces à protéger."),style="color:black;padding-top:10px;font-size:13px;text-align:justify")
               
        )
      )
      
    } 
    
  })
  
  
  
})



# Create Shiny app ----
shinyApp(ui, server)
