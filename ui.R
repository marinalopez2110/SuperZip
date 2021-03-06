library(shiny)
options (shiny.port=80)
library(leaflet)
library(shinyWidgets)
library(dygraphs)

DomainesC = c('Pessière à mousses', 'Pessière à lichens','Toundra arctique arbustive', 'Érablière à caryer cordiforme','Érablière à tilleul', 'Érablière à bouleau jaune', 'Sapinière à bouleau jaune', 'Golfe du Saint-Laurent',
              'Sapinière à bouleau blanc', 'Toundra forestière','Toundra arctique herbacée' )
SousdomainesC = c("Érablière à tilleul de l'Est", "Érablière à tilleul de l'Ouest","Érablière à bouleau jaune de l'Ouest","Sapinière à bouleau jaune de l'Ouest", "Sapinière à bouleau blanc de l'Est",
  "Sapinière à bouleau blanc de l'Ouest", "Pessière à mousses de l'Est", 'Érablière à caryer cordiforme', "Érablière à bouleau jaune de l'Est", "Sapinière à bouleau jaune de l'Est", 'Golfe du Saint-Laurent',
  "Pessière à mousses de l'Ouest")
RegEcolC= c('Collines de la basse Gatineau',  "Collines de l'Outaouais et du Témiscamingue",  'Plaines et coteaux du lac Simard', "Plaine de l'Abitibi",  'Plaine du lac Matagami', 'Plaine du Saint-Laurent',
  'Collines du lac Nominingue', 'Coteaux du réservoir Cabonga',  'Coteaux du réservoir Gouin', "Coteaux de l'Estrie",  'Hautes collines du bas Saint-Maurice',  'Collines du moyen Saint-Maurice',  'Collines du haut Saint-Maurice', 'Plaine du lac Opémisca',
  'Coteaux des basses Appalaches',  'Hautes collines de Charlevoix et du Saguenay',  'Collines ceinturant le lac Saint-Jean', 'Coteaux du lac Assinica',  'Plaine du lac Saint-Jean et du Saguenay',
  'Massif du lac Jacques-Cartier',  'Coteaux de la rivière Nestaocano', 'Massif du mont Valin',  'Coteaux du lac Mistassini', 'Coteaux du lac Manouane',  'Collines du lac Péribonka', 'Haut massif gaspésien',
  'Hautes collines du réservoir aux Outardes',  'Hautes collines du lac Cacaoui', 'Collines du lac Grandmesnil',  'Collines du lac Musquaro', 'Coteaux du lac Caopacho',  'Coteaux des lacs Matonipi et Jonquet', 'Massif des monts Groulx',
  "Plaine du bas Outaouais et de l'archipel de Montréal",  'Plaine de la baie de Rupert', 'Collines des moyennes Appalaches',  'Côte de la baie des Chaleurs',  'Hautes collines de Baie-Comeau-Sept-Îles', 'Côte gaspésienne',
  'Massif gaspésien', "Île d'Anticosti", 'Île Mingan',  'Golfe du Saint- Laurent', 'Îles-de-la-Madeleine',  'Coteaux de la rivière à la Croix et du lac au Griffon',  'Collines de Havre-Saint-Pierre et de Blanc-Sablon',
  'Coteaux du lac Fonteneau' )
TerritoiresC = c( '1a', '2a', '2b', '2c', '3ab', '3c', '3d', '4a', '4bc', '4de','4f', '4gh', '5a', '5bcd', '5ef', '5g', '5hi', '5jk', '6ab','6cdefg', '6hi', '6j', '6kl', '6mn', '6opqr')
RegForestC = c( 'Bas-Saint-Laurent', 'Saguenay -Lac-Saint-Jean','Capitale-Nationale-Chaudière-Appalaches','Mauricie-Centre-Du-Québec', 'Estrie-Montérégie-Laval-Montréal','Outaouais', 'Abitibi-Témiscamingue', 'Cote-Nord',
                'Nord-Du-Québec', 'Gaspésie-Iles-De-La-Madeleine', 'Lanaudière','Laurentides')
SecteursC = c('Sud-Ouest', 'Centre du Québec', 'Nord-est','Secteur métropolitain et sud', 'Sud-est', 'Nord-ouest')
UAC= c('05151', '06452', '07151', '01171', '09352', '02571', '08351', '08751', '07451', '08451', '04352', '04251', '02471', '08764','11262', '09751', '04351', '09351', '08762', '06151', '08663',
       '07251', '07352', '03571', '09471', '06471', '03153', '03171', '04151', '02661', '06271', '02666', '07351', '08551', '11263', '11161', '02651', '08763', '07152', '02664', '08251', '02663',
       '09551', '08652', '08666', '08651', '08151', '02665', '02662', '08665', '01272', '08152', '02751', '03351', '08462', '08664', '03451', '02371', '08562')
SousRegEcolC = c('Collines du lac Dumont', 'Hautes collines du lac Simon', 'Collines du lac Notawassi', 'Collines de Saint-Jérôme-Grand-Mère', 'Hautes collines du lac Édouard', 'Collines du Grand-Lac-Bostonnais',
                 'Coteaux de la rivière Chaudière','Hautes collines de Saint-Tite-des-Caps','Collines du lac Simoncouche', 'Collines du lac Témiscouata','Collines du lac Kipawa', 'Coteaux du lac Yser',
                 'Massif du Mont-Tremblant', 'Collines du lac Trenche','Hautes collines du lac Jacques-Cartier','Hautes collines du lac Poulin-De Courval', 'Monts du Mont-Albert','Hautes collines du lac Guinecourt',
                 'Hautes collines des lacs Nipissis et Magpie','Collines de la basse Gatineau', 'Collines du lac Saint-Patrice','Plaines et coteaux du lac Simard', "Plaine de l'Abitibi",
                 'Plaine du lac Matagami', 'Plaine du Saint-Laurent','Collines du réservoir Kiamika', 'Coteaux du réservoir Dozois', 'Coteaux du réservoir Gouin', "Coteaux de l'Estrie",
                 'Hautes collines de Val-David-Lac-Mékinac','Collines de la rivière Vermillon', 'Plaine du lac Opémisca','Coteaux du lac Etchemin','Hautes collines du mont des Éboulements',
                 'Collines du lac Onatchiway', 'Coteaux du lac Assinica','Plaine du lac Saint-Jean et du Saguenay','Coteaux de la rivière Nestaocano', 'Mont du lac des Savanes', 'Coteaux du lac Mistassini', 'Coteaux du lac Manouane',
                 'Collines du lac Péribonka', 'Monts de Murdochville', 'Hautes collines du réservoir Manic 3','Hautes collines des lacs Walker et Beetz', 'Collines du lac Grandmesnil', 'Collines du lac Musquaro',
                 'Collines du lac Fonteneau', 'Coteaux du lac Caopocho','Coteaux des lacs Matonipi et Jonquet', 'Massif des monts Groulx', 'Collines du Mont-Mégantic', 'Collines du lac Humqui',
                 'Collines des lacs Musquanousse et du Vieux Fort', "Plaine du bas Outaouais et de l'archipel de Montréal", 'Plaine de la baie de Rupert', 'Collines du lac Lareau', 'Monts du lac des Martres',
                 'Collines et coteaux du lac Pohénégamook', 'Côte de la baie des Chaleurs', 'Hautes collines de Baie-Comeau-Sept-Îles', 'Côte gaspésienne', 'Massif gaspésien', 'Golfe du Saint-Laurent',
                 'Îles-de-la-Madeleine','Coteaux de la rivière à la Croix et du lac au Griffon', 'Coteaux de Natashquan')

navbarPage(div(img(src='MFFP.png', width="100px", align="left")), id="nav",
           
           tabPanel(div(icon("map"), "Carte Interactive"),
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
                                      actionButton("Nettoyer", "Nettoyer la carte"),
                                      
                                ###Échele spatiale
                                      selectInput("Echele", "Sélectionnez  l'échele spatiale:",
                                                  choices=c("Territoires guides",  "Secteurs des opérations régionales")), 
                                      radioButtons("Sousregions", "Nombre de sous-régions que vous souhaitez voir:",
                                                   c("1" = "un",  "2" = "deux", "3" = "trois", "Toutes" =  "Toutes"), inline = TRUE),
                                      
                                  #### Une région     
                                      
                                      conditionalPanel(condition = "input.Echele == 'Territoires guides' && (input.Sousregions =='un' || input.Sousregions =='deux' || input.Sousregions =='trois')",
                                                       selectInput("Territoires", "Sélectionnez  le territoire:",
                                                                   choices= TerritoiresC)),
                                      conditionalPanel(condition = "input.Echele == 'Secteurs des opérations régionales' && (input.Sousregions =='un' || input.Sousregions =='deux' || input.Sousregions =='trois')",
                                                       selectInput("Secteurs", "Sélectionnez  le secteur:",
                                                                   choices= SecteursC)),                
                                      
                                #### Deux régions     
                                conditionalPanel(condition = "input.Echele == 'Territoires guides' && (input.Sousregions =='deux' || input.Sousregions =='trois')",
                                                 selectInput("Territoires2", "Sélectionnez  le territoire 2:",
                                                             choices=TerritoiresC)),
                                conditionalPanel(condition = "input.Echele == 'Secteurs des opérations régionales' && (input.Sousregions =='deux' || input.Sousregions =='trois')",
                                                 selectInput("Secteurs2", "Sélectionnez  le secteur 2:",
                                                             choices=SecteursC)),                
                                
                                
                                #### Trois régions     
                               
                                conditionalPanel(condition = "input.Echele == 'Territoires guides' && input.Sousregions =='trois'",
                                                 selectInput("Territoires3", "Sélectionnez  le territoire 3:",
                                                             choices=TerritoiresC)),
                                conditionalPanel(condition = "input.Echele == 'Secteurs des opérations régionales' && input.Sousregions =='trois'",
                                                 selectInput("Secteurs3", "Sélectionnez  le secteur 3:",
                                                             choices=SecteursC)),                
                                
                                      
                                      ###Variable
                                      selectInput("Variable", "Sélectionnez  la variable climatique:",
                                                  choices=c("Températures moyennes", "Températures minimales", "Températures maximales", "Précipitations totales",  
                                                            "Précipitations sous forme de neige",
                                                            "Degrés-jours de croissance", "Évènements gel-dégel", "Saison de croissance")),
                                      
                                      
                                      ###Saisonnalité
                                conditionalPanel(condition = "input.Variable != 'Saison de croissance'",
                                                 selectInput("Saisonnalite", "Sélectionnez  la saisonnalité:",
                                                  choices=c("Annuel", "Saisonier", "Mensuel" )),
                                      conditionalPanel(condition = "input.Saisonnalite == 'Saisonier'",
                                                       radioButtons("season", "Sélectionnez  la saison:",
                                                        c("Hiver"="winter", "Printemps"= "spring",
                                                      "Été"= "summer","Automne"= "fall"), inline = TRUE)),
                                      conditionalPanel(condition = "input.Saisonnalite == 'Mensuel'",
                                                       selectInput("Mois", "Sélectionnez  le mois:",
                                                                   choices=c("Janvier" = "january", "Février" = "february", "Mars" = "march", "Avril" = "april","Mai"= "may",
                                                                             "Juin" = "june", "Julliet" ="july","Aout"="august","Septembre"="september","Octobre"="october",
                                                                             "Novembre"="november","Decembre"="december")))),
                                
                                ###Horizon de temps
                                radioButtons("Horizon", "Sélectionnez  l'horizon de temps:",
                                             c("Historique", "2041-2070", "2071-2100"),inline = TRUE),      
                                
                                      ###Scenario
                                conditionalPanel(condition = "input.Horizon == '2041-2070' || input.Horizon == '2071-2100'",      
                                      radioButtons("Scenario", "Sélectionnez  le scenario d'émissions:",
                                                   c("Modérées (RCP4.5)" = "rcp45",
                                                     "Élevées (RCP8.5)" = "rcp85"))),
                                      
                                      
                                      ###Percentile
                                conditionalPanel(condition = "input.Horizon == '2041-2070' || input.Horizon == '2071-2100'",
                                radioButtons("Percentile", "Sélectionnez  le percentile:",
                                             c("10" = "10", "50" = "50", "90" = "90"), selected = "50", inline = TRUE)), 
                                
                                ###DOWNLOAD GEOJSON
                                downloadButton("downloadData", "Télécharger GeoJson")
                               
                        ),
                        
                        tags$div(id="cite",
                                 'Données compilées par ', tags$em('Ouranos - Ministère des ForÃªts, de la Faune et des Parcs')
                        ))),
           
           tabPanel((div(icon("table"),"Sommaire")),
                    sidebarLayout(
                      
                      sidebarPanel(
                        ###Échele spatiale
                        selectInput("EcheleT", "Sélectionnez  l'échele spatiale:",
                                    choices=c("Territoires guides", "Domaines bioclimatiques", "Sous-domaines bioclimatiques", "Régions écologiques", 
                                              "Sous-région écologiques",  "Secteurs des opérations régionales",
                                              "Régions forestières", "Unités d’aménagement (UA)")), 
                                                #### Une région     
                        conditionalPanel(condition = "input.EcheleT == 'Domaines bioclimatiques'",
                                         selectInput("DomainesT", "Sélectionnez  le domaine :",
                                                     choices= DomainesC)),
                        conditionalPanel(condition = "input.EcheleT == 'Sous-domaines bioclimatiques' ",
                                         selectInput("SousdomainesT", "Sélectionnez  le sous-domaine:",
                                                     choices= SousdomainesC)),
                        conditionalPanel(condition = "input.EcheleT == 'Régions écologiques' ",
                                         selectInput("RegEcolT", "Sélectionnez  la région:",
                                                     choices= RegEcolC)),
                        conditionalPanel(condition = "input.EcheleT == 'Sous-région écologiques' ",
                                         selectInput("SousRegEcolT", "Sélectionnez  la sous-région:",
                                                     choices= SousRegEcolC)),
                        conditionalPanel(condition = "input.EcheleT == 'Territoires guides' ",
                                         selectInput("TerritoiresT", "Sélectionnez  le territoire:",
                                                     choices= TerritoiresC)),
                        conditionalPanel(condition = "input.EcheleT == 'Secteurs des opérations régionales' ",
                                         selectInput("SecteursT", "Sélectionnez  le secteur:",
                                                     choices= SecteursC)),                
                        conditionalPanel(condition = "input.EcheleT == 'Régions forestières' ",
                                         selectInput("RegForestT", "Sélectionnez  la région:",
                                                     choices=RegForestC)),                
                        conditionalPanel(condition = "input.EcheleT == 'Unités d’aménagement (UA)' ",
                                         selectInput("UAT", "Sélectionnez  l'unité:",
                                                     choices= UAC)),  
                        ###Variable
                        selectInput("VariableT", "Sélectionnez  la variable climatique:",
                                    choices=c("Températures moyennes", "Températures minimales", "Températures maximales", "Précipitations totales",  
                                              "Précipitations sous forme de neige",
                                              "Évènements gel-dégel")),
                        
                        ###DOWNLOAD TABLE CSV
                        downloadButton("downloadDataT", "Télécharger CSV")
                        
                        
                      ),
                      
                      mainPanel(
                        tableOutput("tabletest")
                      )
                    ),
                    p("Le tableau représente les changements projetés selon deux scénarios d’émissions de gaz à effet de serre, le scénario modéré (RCP 4.5), qui suppose une stabilisation des émissions d’ici la fin du siècle et le scénario élevé (RCP 8.5), qui suppose une augmentation des émissions jusqu’à la fin du siècle."),
                    p("Les saisons représentent des périodes de trois mois : l’hiver (décembre-janvier-février), le printemps (mars-avril-mai), l’été (juin-juillet-août) et l’automne (septembre-octobre-novembre)."),
                    p("Les valeurs représentent des moyennes pour la région sélectionnée, calculées à partir d’un ensemble de simulations climatiques globales de l'ensemble CMIP5 pour la période de référence 1981-2010, la période 2041-2070 (l’horizon 2050) et la période 2071-2100 (l’horizon 2080). L’intervalle dans le tableau indique les 10e et 90e percentiles des 11 simulations climatiques utilisées. Ainsi, les 10e et 90e percentiles représentent la sensibilité des différents modèles climatiques aux émissions de gaz à effet de serre utilisés comme forçage ainsi qu’à la variabilité naturelle du climat.")
                     
           ),
           tabPanel(div(icon("image"), "Graphique"),
                    sidebarLayout(
                      
                      sidebarPanel(
                        ###Échele spatiale
                        selectInput("echelleTS", "Sélectionnez  l'échele spatiale:",
                                    choices=c("Territoires guides", "Domaines bioclimatiques", "Sous-domaines bioclimatiques", "Régions écologiques", 
                                              "Sous-région écologiques",  "Secteurs des opérations régionales",
                                              "Régions forestières", "Unités d’aménagement (UA)")), 
                        #### Une région     
                        conditionalPanel(condition = "input.echelleTS == 'Domaines bioclimatiques'",
                                         selectInput("DomainesTS", "Sélectionnez  le domaine :",
                                                     choices= DomainesC)),
                        conditionalPanel(condition = "input.echelleTS == 'Sous-domaines bioclimatiques' ",
                                         selectInput("SousdomainesTS", "Sélectionnez  le sous-domaine:",
                                                     choices= SousdomainesC)),
                        conditionalPanel(condition = "input.echelleTS == 'Régions écologiques' ",
                                         selectInput("RegEcoLTS", "Sélectionnez  la région:",
                                                     choices= RegEcolC)),
                        conditionalPanel(condition = "input.echelleTS == 'Sous-région écologiques' ",
                                         selectInput("SousRegEcoLTS", "Sélectionnez  la sous-région:",
                                                     choices= SousRegEcolC)),
                        conditionalPanel(condition = "input.echelleTS == 'Territoires guides' ",
                                         selectInput("TerritoiresTS", "Sélectionnez  le territoire:",
                                                     choices= TerritoiresC)),
                        conditionalPanel(condition = "input.echelleTS == 'Secteurs des opérations régionales' ",
                                         selectInput("SecteursTS", "Sélectionnez  le secteur:",
                                                     choices= SecteursC)),                
                        conditionalPanel(condition = "input.echelleTS == 'Régions forestières' ",
                                         selectInput("RegForestTS", "Sélectionnez  la région:",
                                                     choices=RegForestC)),                
                        conditionalPanel(condition = "input.echelleTS == 'Unités d’aménagement (UA)' ",
                                         selectInput("UATS", "Sélectionnez  l'unité:",
                                                     choices= UAC)),  
                        ###Variable
                        selectInput("VariableTS", "Sélectionnez  la variable climatique:",
                                    choices=c("Températures moyennes",  "Précipitations totales",  
                                              "Précipitations sous forme de neige")),
                        ###Saisonnalité
                        conditionalPanel(condition = "input.VariableTS != 'Saison de croissance'",
                                         selectInput("SaisonnaliteTS", "Sélectionnez  la saisonnalité:",
                                                     choices=c("Annuel" ))), #, "Saisonier", "Mensuel"
                                         # conditionalPanel(condition = "input.SaisonnaliteTS == 'Saisonier'",
                                         #                  radioButtons("seasonTS", "Sélectionnez  la saison:",
                                         #                               c("Hiver"="winterTS", "Printemps"= "springTS",
                                         #                                 "Été"= "summerTS","Automne"= "fallTS"), inline = TRUE)),
                                         # conditionalPanel(condition = "input.SaisonnaliteTS == 'Mensuel'",
                                         #                  selectInput("MoisTS", "Sélectionnez  le mois:",
                                         #                              choices=c("Janvier" = "januaryTS", "Février" = "februaryTS", "Mars" = "marchTS", "Avril" = "aprilTS","Mai"= "mayTS",
                                         #                                        "Juin" = "juneTS", "Julliet" ="julyTS","Aout"="augustTS","Septembre"="septemberTS","Octobre"="octoberTS",
                                         #                                        "Novembre"="novemberTS","Decembre"="decemberTS")))),
                        ###DOWNLOAD TABLE CSV
                        downloadButton("downloadDataTS", "Télécharger CSV")
                        
                        
                      ),
                      
                      mainPanel(
                        dygraphOutput("dygraph"),
                      )
                    ),
                    
                    p("La figure représente les changements projetés dans le temps selon deux scénarios d'émissions de gaz à effet de serre : le scénario modéré (RCP 4.5), qui suppose une stabilisation des émissions d'ici la fin du siècle, ainsi que le scénario élevé (RCP 8.5), qui suppose une augmentation des émissions jusqu'à la fin du siècle. Les valeurs sont calculées à partir d'un ensemble de simulations climatiques globales produit à partir de l'ensemble CMIP5."),
                    p("Le tracé vert représente les observations interpolées à partir de stations météorologiques pour la période 1951-2013;"),
                    p("Le tracé bleu représente la tendance médiane de l'ensemble des simulations avec le scénario modéré d'émissions (RCP 4.5);"),
                    p("Le tracé rouge représente la tendance médiane de l'ensemble des simulations avec le scénario élevé d'émissions (RCP 8.5);"),
                    p("Les enveloppes grise, bleue et rouge représentent la plage des valeurs annuelles de l'ensemble, soit les données comprises entre le 10e et le 90e percentile des simulations utilisées pour la période historique (1951-2005) et future (2006-2100).
                      Les projections futures sont présentées pour les émissions modérées (RCP 4.5) et élevées (RCP 8.5) par des enveloppes rouges et bleues, respectivement.")),
           tabPanel(div(icon("info"), "Information"),
                    br(),
                    div(
                    p(span("Températures moyennes (°C)", style = "color:blue;font-weight:bold")," - La moyenne des températures quotidiennes."),
                    p(span("Moyenne des température maximales quotidiennes (°C)", style = "color:blue;font-weight:bold")," - La moyenne des températures maximales quotidiennes."),
                    p(span("Moyenne des températures minimales quotidiennes (°C)", style = "color:blue;font-weight:bold")," - La moyenne des températures minimales quotidiennes."),
                    p(span("Précipitations totales (mm)", style = "color:blue;font-weight:bold")," - Somme de la pluie totale et de l'équivalent en eau de la neige totale en millimètres (mm)."),
                    p(span("Précipitation sous forme de neige (cm)", style = "color:blue;font-weight:bold")," - Estimées sous forme de précipitations lorsque la température moyenne quotidienne <0 ° C"),
                    p(span("Degrés-jours de croissance (DJC)", style = "color:blue;font-weight:bold")," - Un degré-jour est l'écart, en degrés Celsius, qui sépare la température moyenne quotidienne d'une valeur de base de 4°C. Si la valeur est égale ou inférieure à 4°C, la journée à zéro degré-jour de croissance."),
                    p(span("Événements de gel-dégel (jours)", style = "color:blue;font-weight:bold")," - Un événement quotidien de gel-dégel survient quand, dans une période de 24 heures, la température minimale est inférieure à 0°C et la température maximale est supérieure à 0°C."),
                    p(span("Saison de croissance", style = "color:blue;font-weight:bold")," - Nombre annuel de jours entre la première occurrence d'au moins six jours consécutifs avec une température quotidienne moyenne supérieure à 5,0 ° C et la première occurrence d'au moins 6 jours consécutifs avec une température quotidienne moyenne inférieure à 5,0 ° C après le 07-01."),
                    p("Les résultats pour la période de références et les horizons futurs sont calculés à partir d'une série de 11 simulations climatiques produites à partir de l'ensemble CMIP5. Les résultats pour la période de référence 1981-2010 sont affichés dans le panneau de gauche, tandis que diverses commandes permettent l'affichage des résultats futurs dans le panneau de droite."),
                    p("Bouton émissions : permet d'afficher les changements projetés sous les deux scénarios d'émissions de gaz à effet de serre, le scénario modéré (RCP 4.5), qui suppose une stabilisation des émissions d'ici la fin du siècle et le scénario élevé (RCP 8.5), qui suppose une augmentation des émissions jusqu'à la fin du siècle."),
                    p("Bouton horizon : permet d'afficher les résultats futurs de l'horizon 2041-2070 ou l'horizon 2071-2100."),
                    p("Bouton percentile : permet d'explorer la gamme des résultats des 11 simulations climatiques individuelles pour un scénario d'émissions donné à l'aide des options 50, 10 ou 90 qui correspondent aux percentiles de l'ensemble. Cette gamme permet d'étudier la sensibilité des différents modèles climatiques aux mÃªmes émissions de gaz à effet de serre, ainsi que de la variabilité naturelle du climat.")))
)
