library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(fclust)


path='/Users/surafeltilahun/OneDrive/OneDrive-2021-08-05/Clustering_data_year_to_date/'

all_files_names<-list.files(pattern = '*.csv',path = path,full.names = TRUE)
all_year_to_date_files<-lapply(all_files_names,read.csv)
all_year_to_date_files<-lapply(all_year_to_date_files,data.frame)
all_year_to_date_files=lapply(all_year_to_date_files,column_to_rownames,'X')

shinyUI(fluidPage(
  headerPanel("IOG Players Clustering"),
  sidebarPanel(
    selectizeInput('tournaments',label='Choose Tournaments',choice=list('Desert Classic'= 1,
                                                                        'Farmers Insurance Open'= 2,
                                                                        'AT&T Pebble Beach Pro-Am' = 3,
                                                                        'Sony Open in Hawaii' = 	4,
                                                                        'Arnold Palmer Invitational presented by Mastercard' = 5,
                                                                        'The Honda Classic'=	6,
                                                                        'Wyndham Championship'	= 7,
                                                                        'Sentry Tournament of Champions' =	8,
                                                                        'Charles Schwab Challenge' =	9,
                                                                        'the Memorial Tournament presented by Nationwide' =	10,
                                                                        'PGA Championship'=	11,
                                                                        'Travelers Championship' =	12,
                                                                        'Sanderson Farms Championship' =	13,
                                                                        'TOUR Championship' =	14,
                                                                        'Mayakoba Golf Classic'=	15,
                                                                        'Safeway Open'=	16,
                                                                        'Barracuda Championship' =	17,
                                                                        'World Golf Championships-Mexico Championship' =	18,
                                                                        'Puerto Rico Open' =	19,
                                                                        'World Golf Championships-HSBC Champions'=	20,
                                                                        'THE CJ CUP @ NINE BRIDGES'=	21,
                                                                        'Rocket Mortgage Classic'=	22,
                                                                        '3M Open'=	23),select=14),
    selectInput(inputId = 'ranking','Players Ranking',choices = c('Top 30'=1,'Top 20'=2,'Top 10'=3,'All Players'=4),selected = 4),
    checkboxInput(inputId = 'scale',label = strong('Scale (Normalize The Data)'),value = FALSE),
    selectInput(inputId = 'method', 'Correlation Method',
                c("pearson", "kendall", "spearman"), selected="pearson"),
    selectInput(inputId = 'distMethod', 'Distance Method for Clustering',
                c("euclidean", "maximum", "manhattan", "canberra",
                  "binary", "minkowski"), selected="manhattan"),
    selectInput(inputId = 'clustMethod', 'Agglomeration Method for Clustering',
                c("complete", "ward", "single", "average",
                  "mcquitty", "median", "centroid"), selected="complete"),
    sliderInput(inputId = 'num',value = 3,min = 2,max = 10,step = 1,label = 'Insert Number of Clusters'),
    selectizeInput(inputId = 'model',label="Choose Clustering Model",choices=list(
                                                   "EM - can be slow to converge"=3,
                                                   "Kmeans" = 5,
                                                   "DBSCAN"=7,
                                                   "Isolation Forest"=8,
                                                   "Fuzzy kmeans"=9,
                                                   "Fuzzy kmeans - Gustafson and Kessel"=10,
                                                   "Fuzzy k-means with polynomial fuzzifier"=11,
                                                   "Partitioning Around Medoids (pam)"=12,
                                                   "FBOD"=13,
                                                   "SOD"=14,
                                                   'SVM'=15
                                                   ),selected = 5),
    tags$hr(),
    checkboxInput(inputId = "rem",label = strong("Choose Features to Exclude"),value = FALSE),
    uiOutput("condPanel1"),conditionalPanel(condition = "input.rem == true"),
    downloadButton("downloadData", "Download")
    
  ),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Baseline Model", plotOutput('plot1', height = "900px", width = "900px")),
                tabPanel("Row Data Summary", verbatimTextOutput("summary")),
                tabPanel('Row Data',tableOutput('table')),
                tabPanel('Models and Statstics',
                         plotOutput('plot2',height = "500px", width = "1000px"),
                         plotOutput('siloute',height = "500px", width = "1000px"),
                         tableOutput('stats1'),
                         plotOutput('stats2',height = "500px", width = "1200px"),
                         plotOutput('interaction',"1500px", width = "1500px")),
                tabPanel('Cluster Data',tableOutput('clusterData'))
    )
  )
)
)
