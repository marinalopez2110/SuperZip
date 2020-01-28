library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Séléctionez l'échele spatiale:" = "echele",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage(div(img(src='MFFP.png', width="100px", align="left")), id="nav",
           
           tabPanel("Carte Interactive",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Portraits Climatiques"),
                                      
                                      selectInput("Echele", "Séléctionez l'échele spatiale:",
                                                  choices=c("Domaines bioclimatiques", "Sous-domaines bioclimatiques", "Régions écologiques", 
                                                            "Sous-région écologiques", "Territoires guides", "Secteurs des opérations régionales",
                                                            "Régions forestières", "Unités d’aménagement (UA)")),
                                      conditionalPanel(condition = "input.Echele == 'Domaines bioclimatiques'",
                                                       selectInput("Domaines", "Séléctionez le domaine:",
                                                                   choices=c('Pessière à mousses', 'Pessière à lichens',
                                                                             'Toundra arctique arbustive', 'Érablière à caryer cordiforme',
                                                                             'Érablière à tilleul', 'Érablière à bouleau jaune',
                                                                             'Sapinière à bouleau jaune', 'Golfe du Saint-Laurent',
                                                                             'Sapinière à bouleau blanc', 'Toundra forestière',
                                                                             'Toundra arctique herbacée' ))),
                                      conditionalPanel(condition = "input.Echele == 'Sous-domaines bioclimatiques'",
                                                       selectInput("Sous-domaines", "Séléctionez le sous-domaine:",
                                                                   choices=c("Érablière à tilleul de l'Est", "Érablière à tilleul de l'Ouest",
                                                                             "Érablière à bouleau jaune de l'Ouest",
                                                                             "Sapinière à bouleau jaune de l'Ouest",
                                                                             "Sapinière à bouleau blanc de l'Est",
                                                                             "Sapinière à bouleau blanc de l'Ouest",
                                                                             "Pessière à mousses de l'Est", 'Érablière à caryer cordiforme',
                                                                             "Érablière à bouleau jaune de l'Est",
                                                                             "Sapinière à bouleau jaune de l'Est", 'Golfe du Saint-Laurent',
                                                                             "Pessière à mousses de l'Ouest" ))),
                                      conditionalPanel(condition = "input.Echele == 'Régions écologiques'",
                                                       selectInput("RegEcol", "Séléctionez la région:",
                                                                   choices=c('Collines de la basse Gatineau',
                                                                             "Collines de l'Outaouais et du Témiscamingue",
                                                                             'Plaines et coteaux du lac Simard', "Plaine de l'Abitibi",
                                                                             'Plaine du lac Matagami', 'Plaine du Saint-Laurent',
                                                                             'Collines du lac Nominingue', 'Coteaux du réservoir Cabonga',
                                                                             'Coteaux du réservoir Gouin', "Coteaux de l'Estrie",
                                                                             'Hautes collines du bas Saint-Maurice',
                                                                             'Collines du moyen Saint-Maurice',
                                                                             'Collines du haut Saint-Maurice', 'Plaine du lac Opémisca',
                                                                             'Coteaux des basses Appalaches',
                                                                             'Hautes collines de Charlevoix et du Saguenay',
                                                                             'Collines ceinturant le lac Saint-Jean', 'Coteaux du lac Assinica',
                                                                             'Plaine du lac Saint-Jean et du Saguenay',
                                                                             'Massif du lac Jacques-Cartier',
                                                                             'Coteaux de la rivière Nestaocano', 'Massif du mont Valin',
                                                                             'Coteaux du lac Mistassini', 'Coteaux du lac Manouane',
                                                                             'Collines du lac Péribonka', 'Haut massif gaspésien',
                                                                             'Hautes collines du réservoir aux Outardes',
                                                                             'Hautes collines du lac Cacaoui', 'Collines du lac Grandmesnil',
                                                                             'Collines du lac Musquaro', 'Coteaux du lac Caopacho',
                                                                             'Coteaux des lacs Matonipi et Jonquet', 'Massif des monts Groulx',
                                                                             "Plaine du bas Outaouais et de l'archipel de Montréal",
                                                                             'Plaine de la baie de Rupert', 'Collines des moyennes Appalaches',
                                                                             'Côte de la baie des Chaleurs',
                                                                             'Hautes collines de Baie-Comeau-Sept-Îles', 'Côte gaspésienne',
                                                                             'Massif gaspésien', "Île d'Anticosti", 'Île Mingan',
                                                                             'Golfe du Saint- Laurent', 'Îles-de-la-Madeleine',
                                                                             'Coteaux de la rivière à la Croix et du lac au Griffon',
                                                                             'Collines de Havre-Saint-Pierre et de Blanc-Sablon',
                                                                             'Coteaux du lac Fonteneau' ))),
                                      conditionalPanel(condition = "input.Echele == 'Sous-région écologiques'",
                                                       selectInput("SousRegEcol", "Séléctionez la sous-région:",
                                                                   choices=c('Collines du lac Dumont', 'Hautes collines du lac Simon',
                                                                             'Collines du lac Notawassi', 'Collines de Saint-Jérôme-Grand-Mère',
                                                                             'Hautes collines du lac Édouard',
                                                                             'Collines du Grand-Lac-Bostonnais',
                                                                             'Coteaux de la rivière Chaudière',
                                                                             'Hautes collines de Saint-Tite-des-Caps',
                                                                             'Collines du lac Simoncouche', 'Collines du lac Témiscouata',
                                                                             'Collines du lac Kipawa', 'Coteaux du lac Yser',
                                                                             'Massif du Mont-Tremblant', 'Collines du lac Trenche',
                                                                             'Hautes collines du lac Jacques-Cartier',
                                                                             'Hautes collines du lac Poulin-De Courval', 'Monts du Mont-Albert',
                                                                             'Hautes collines du lac Guinecourt',
                                                                             'Hautes collines des lacs Nipissis et Magpie',
                                                                             'Collines de la basse Gatineau', 'Collines du lac Saint-Patrice',
                                                                             'Plaines et coteaux du lac Simard', "Plaine de l'Abitibi",
                                                                             'Plaine du lac Matagami', 'Plaine du Saint-Laurent',
                                                                             'Collines du réservoir Kiamika', 'Coteaux du réservoir Dozois',
                                                                             'Coteaux du réservoir Gouin', "Coteaux de l'Estrie",
                                                                             'Hautes collines de Val-David-Lac-Mékinac',
                                                                             'Collines de la rivière Vermillon', 'Plaine du lac Opémisca',
                                                                             'Coteaux du lac Etchemin',
                                                                             'Hautes collines du mont des Éboulements',
                                                                             'Collines du lac Onatchiway', 'Coteaux du lac Assinica',
                                                                             'Plaine du lac Saint-Jean et du Saguenay',
                                                                             'Coteaux de la rivière Nestaocano', 'Mont du lac des Savanes',
                                                                             'Coteaux du lac Mistassini', 'Coteaux du lac Manouane',
                                                                             'Collines du lac Péribonka', 'Monts de Murdochville',
                                                                             'Hautes collines du réservoir Manic 3',
                                                                             'Hautes collines des lacs Walker et Beetz',
                                                                             'Collines du lac Grandmesnil', 'Collines du lac Musquaro',
                                                                             'Collines du lac Fonteneau', 'Coteaux du lac Caopocho',
                                                                             'Coteaux des lacs Matonipi et Jonquet', 'Massif des monts Groulx',
                                                                             'Collines du Mont-Mégantic', 'Collines du lac Humqui',
                                                                             'Collines des lacs Musquanousse et du Vieux Fort',
                                                                             "Plaine du bas Outaouais et de l'archipel de Montréal",
                                                                             'Plaine de la baie de Rupert', 'Collines du lac Lareau',
                                                                             'Monts du lac des Martres',
                                                                             'Collines et coteaux du lac Pohénégamook',
                                                                             'Côte de la baie des Chaleurs',
                                                                             'Hautes collines de Baie-Comeau-Sept-Îles', 'Côte gaspésienne',
                                                                             'Massif gaspésien', 'Golfe du Saint-Laurent',
                                                                             'Îles-de-la-Madeleine',
                                                                             'Coteaux de la rivière à la Croix et du lac au Griffon',
                                                                             'Coteaux de Natashquan'))),
                                      conditionalPanel(condition = "input.Echele == 'Territoires guides'",
                                                       selectInput("Territoires", "Séléctionez le territoire:",
                                                                   choices=c("Tous les Territoires", '1a', '2a', '2b', '2c', '3ab', '3c', '3d', '4a', '4bc', '4de',
                                                                             '4f', '4gh', '5a', '5bcd', '5ef', '5g', '5hi', '5jk', '6ab',
                                                                             '6cdefg', '6hi', '6j', '6kl', '6mn', '6opqr'))),
                                      conditionalPanel(condition = "input.Echele == 'Secteurs des opérations régionales'",
                                                       selectInput("Secteurs", "Séléctionez le secteur:",
                                                                   choices=c('Sud-Ouest', 'Centre du Québec', 'Nord-est',
                                                                             'Secteur métropolitain et sud', 'Sud-est', 'Nord-ouest'))),                
                                      conditionalPanel(condition = "input.Echele == 'Régions forestières'",
                                                       selectInput("RegForest", "Séléctionez la région:",
                                                                   choices=c('Toutes les Régions', 'Bas-Saint-Laurent', 'Saguenay -Lac-Saint-Jean',
                                                                             'Capitale-Nationale-Chaudière-Appalaches',
                                                                             'Mauricie-Centre-Du-Québec', 'Estrie-Montérégie-Laval-Montréal',
                                                                             'Outaouais', 'Abitibi-Témiscamingue', 'Cote-Nord',
                                                                             'Nord-Du-Québec', 'Gaspésie-Iles-De-La-Madeleine', 'Lanaudière',
                                                                             'Laurentides'
                                                                   ))),                
                                      conditionalPanel(condition = "input.Echele == 'Unités d’aménagement (UA)'",
                                                       selectInput("UA", "Séléctionez l'unité:",
                                                                   choices=c('05151', '06452', '07151', '01171', '09352', '02571', '08351',
                                                                             '08751', '07451', '08451', '04352', '04251', '02471', '08764',
                                                                             '11262', '09751', '04351', '09351', '08762', '06151', '08663',
                                                                             '07251', '07352', '03571', '09471', '06471', '03153', '03171',
                                                                             '04151', '02661', '06271', '02666', '07351', '08551', '11263',
                                                                             '11161', '02651', '08763', '07152', '02664', '08251', '02663',
                                                                             '09551', '08652', '08666', '08651', '08151', '02665', '02662',
                                                                             '08665', '01272', '08152', '02751', '03351', '08462', '08664',
                                                                             '03451', '02371', '08562'))),         
                                      
                                      ###Variable
                                      selectInput("Variable", "Séléctionez la variable climatique:",
                                                  choices=c("Températures moyennes, min et max","Précipitations totales et sous forme de neige",
                                                            "Degrés-jours de croissance", "Évènements gel-dégel", "Saison de croissance")),
                                      conditionalPanel(condition = "input.Variable == 'Températures moyennes, min et max'",
                                                       actionButton("Moyenne", "Températures moyennes (°C)"),
                                                       actionButton("Maximum", "Moyenne des températures maximales quotidiennes (°C)"),
                                                       actionButton("Minimum", "Moyenne des températures minimales quotidiennes (°C)")),
                                      conditionalPanel(condition = "input.Variable == 'Précipitations totales et sous forme de neige'",
                                                       actionButton("PrecTotale", "Précipitations totales (mm)"),
                                                       actionButton("Neige", "Précipitations sous forme de neige (cm)")),
                                      br(),
                                      
                                      
                                      ###Saisonnalité
                                      selectInput("Saisonnalite", "Séléctionez la saisonnalité:",
                                                  choices=c("Annuel", "Saissoniers", "Mensuel" )),
                                      conditionalPanel(condition = "input.Saisonnalite == 'Saissoniers'",
                                                       actionButton("Hiver", "Hiver"),
                                                       actionButton("Printemps", "Printemps"),
                                                       actionButton("Été", "Été"),
                                                       actionButton("Automne", "Automne")),
                                      conditionalPanel(condition = "input.Saisonnalite == 'Mensuel'",
                                                       selectInput("Mois", "Séléctionez le mois:",
                                                                   choices=c("Janvier", "Février", "Mars", "Avril","Mai","Juin",
                                                                             "Julliet","Aout","Septembre","Octobre","Novembre","Decembre"))
                                      ),
                                      
                                      ###Scenario
                                      
                                      radioButtons("Scenario", "Séléctionez le scenario d'émissions:",
                                                   c("Modérées (RCP4.5)" = "Moderees",
                                                     "Élevées (RCP8.5)" = "Elevees")),
                                      
                                      
                                      ###Percentile
                                      sliderInput("Percentile", "Séléctionez le percentile:",
                                                  min=10, max=90, value= 50, step=40),
                                      
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Données compilées par ', tags$em('Ouranos - Ministère des Forêts, de la Faune et des Parcs')
                        )
                    )
           ),
           
           tabPanel((div(icon("table"),"Sommaire")),
                    br(),
                    img(src='sommaire.png', width="900px", align="center"),
                    # downloadButton("dowloadData2", "Télécharger")
                    div(icon("download"), tags$a(href="Moyenne2.csv", "Télécharger CSV")),
                    hr(),
                    #DT::dataTableOutput("ziptable")
           ),
           
           tabPanel(div(icon("image"), "Graphique"),
                    br(),
                    img(src='temMoy.png', width="900px", align="center"),
                    #downloadButton("dowloadData", "Download")
                    div(icon("download"), tags$a(href="Moyenne.csv", "Télécharger CSV"))
           ),
           
           conditionalPanel("false", icon("crosshair"))
)