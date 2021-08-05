library(gplots)                         
library(RColorBrewer)
library(ggplot2)
library(cluster)
library(ggfortify)
library(ggrepel)
library(tidyr)
library(ppclust)
library (vegan)
library(clusterSim)
library(shiny)
library(shinydashboard)
library(textshape)
library(GGally)

path='/Users/surafeltilahun/OneDrive/OneDrive-2021-08-05/Clustering_data_year_to_date/'

all_files_names<-list.files(pattern = '*.csv',path = path,full.names = TRUE)
file_names<-list.files(path)
all_year_to_date_files<-lapply(all_files_names,read.csv)
all_year_to_date_files<-lapply(all_year_to_date_files,data.frame)
all_year_to_date_files=lapply(all_year_to_date_files,column_to_rownames,'X')


shinyServer(function(input, output, session) {
  
  tournaments_data<-reactive({
        if(input$tournaments==1)
        {
          dat<-all_year_to_date_files[[1]]
          return(dat)
          
        }
        else if(input$tournaments==2)
        {
          dat<-all_year_to_date_files[[2]]
          return(dat)
          
        }
        else if(input$tournaments==3)
        {
          dat<-all_year_to_date_files[[3]]
          return(dat)
          
        }
        else if(input$tournaments==4)
        {
          dat<-all_year_to_date_files[[4]]
          return(dat)
          
        }
        else if(input$tournaments==5)
        {
          dat<-all_year_to_date_files[[5]]
          return(dat)
          
        }
        else if(input$tournaments==6)
        {
          dat<-all_year_to_date_files[[6]]
          return(dat)
          
        }
        else if(input$tournaments==7)
        {
          dat<-all_year_to_date_files[[7]]
          return(dat)
          
        }
        else if(input$tournaments==8)
        {
          dat<-all_year_to_date_files[[8]]
          return(dat)
          
        }
        else if(input$tournaments==9)
        {
          dat<-all_year_to_date_files[[9]]
          return(dat)
          
        }
        else if(input$tournaments==10)
        {
          dat<-all_year_to_date_files[[10]]
          return(dat)
          
        }
        else if(input$tournaments==11)
        {
          dat<-all_year_to_date_files[[11]]
          return(dat)
          
        }
        else if(input$tournaments==12)
        {
          dat<-all_year_to_date_files[[12]]
          return(dat)
          
        }
        else if(input$tournaments==13)
        {
          dat<-all_year_to_date_files[[13]]
          return(dat)
          
        }
        else if(input$tournaments==14)
        {
          dat<-all_year_to_date_files[[14]]
          return(dat)
          
        }
        else if(input$tournaments==15)
        {
          dat<-all_year_to_date_files[[15]]
          return(dat)
          
        }
        else if(input$tournaments==16)
        {
          dat<-all_year_to_date_files[[16]]
          return(dat)
          
        }
        else if(input$tournaments==17)
        {
          dat<-all_year_to_date_files[[17]]
          return(dat)
          
        }
        else if(input$tournaments==18)
        {
          dat<-all_year_to_date_files[[18]]
          return(dat)
          
        }
        else if(input$tournaments==19)
        {
          dat<-all_year_to_date_files[[19]]
          return(dat)
          
        }
        else if(input$tournaments==20)
        {
          dat<-all_year_to_date_files[[20]]
          return(dat)
          
        }
        else if(input$tournaments==21)
        {
          dat<-all_year_to_date_files[[21]]
          return(dat)
          
          
        }
        else if(input$tournaments==22)
        {
          dat<-all_year_to_date_files[[22]]
          return(dat)
          
        }
        else (input$tournaments==23)
        {
          dat<-all_year_to_date_files[[23]]
          return(dat)
          
        }
    })
  
  file_name<-reactive({
    if(input$tournaments==1)
    {
      dat<-file_names[1]
      return(dat)
      
    }
    else if(input$tournaments==2)
    {
      dat<-file_names[2]
      return(dat)
      
    }
    else if(input$tournaments==3)
    {
      dat<-file_names[3]
      return(dat)
      
    }
    else if(input$tournaments==4)
    {
      dat<-file_names[4]
      return(dat)
      
    }
    else if(input$tournaments==5)
    {
      dat<-file_names[5]
      return(dat)
      
    }
    else if(input$tournaments==6)
    {
      dat<-file_names[6]
      return(dat)
      
    }
    else if(input$tournaments==7)
    {
      dat<-file_names[7]
      return(dat)
      
    }
    else if(input$tournaments==8)
    {
      dat<-file_names[8]
      return(dat)
      
    }
    else if(input$tournaments==9)
    {
      dat<-file_names[9]
      return(dat)
      
    }
    else if(input$tournaments==10)
    {
      dat<-file_names[10]
      return(dat)
      
    }
    else if(input$tournaments==11)
    {
      dat<-file_names[11]
      return(dat)
      
    }
    else if(input$tournaments==12)
    {
      dat<-file_names[12]
      return(dat)
      
    }
    else if(input$tournaments==13)
    {
      dat<-file_names[13]
      return(dat)
      
    }
    else if(input$tournaments==14)
    {
      dat<-file_names[14]
      return(dat)
      
    }
    else if(input$tournaments==15)
    {
      dat<-file_names[15]
      return(dat)
      
    }
    else if(input$tournaments==16)
    {
      dat<-file_names[16]
      return(dat)
      
    }
    else if(input$tournaments==17)
    {
      dat<-file_names[17]
      return(dat)
      
    }
    else if(input$tournaments==18)
    {
      dat<-file_names[18]
      return(dat)
      
    }
    else if(input$tournaments==19)
    {
      dat<-file_names[19]
      return(dat)
      
    }
    else if(input$tournaments==20)
    {
      dat<-file_names[20]
      return(dat)
      
    }
    else if(input$tournaments==21)
    {
      dat<-file_names[21]
      return(dat)
      
      
    }
    else if(input$tournaments==22)
    {
      dat<-file_names[22]
      return(dat)
      
    }
    else (input$tournaments==23)
    {
      dat<-file_names[23]
      return(dat)
      
    }
  })
  
  ranking_players<-reactive({
    if(input$ranking==1)
    {
      dat<-tournaments_data()%>%filter(Rank<=30)  
      return(dat)
    }
    else if(input$ranking==2)
    {
      dat<-tournaments_data()%>%filter(Rank<=20)   
      return(dat)
      
    }
    else if(input$ranking==3)
    {
      dat<-tournaments_data()%>%filter(Rank<=10)
      return(dat)
    }
    else
    {
      dat<-tournaments_data()
      return(dat)
    }
  })
  
  myData <- reactive({
    ## Update the data with checkbox input
        if(input$scale)
        {
          data<-ranking_players()%>%data.frame()
          data<-data.Normalization(data,type="n1",normalization="column")
          return(data)
        }
        else
        {
          data<-ranking_players()
          return(data)
        }
    
  })
  

  output$condPanel1 <- renderUI({
    conditionalPanel(
      condition = "input.rem == true",
      selectizeInput('toRm', "Exclude",
                     choices=sort(colnames(myData())),
                     multiple=TRUE)
    )
  })

  output$summary <- renderPrint({
    dat<-myData()
    colnames(dat)<-c('Tee To Green','Driving Distance','Average SG:Putting','Average Approach The Green','Average Off The.Tee','Average SG:Around The Green','Scoring Average','Rank','Height','Weight','Sand Saves','Bunkers','Hit Fairway')
    summary(dat)
  })

  ## Combine the selected variables into a new data frame
  selectedData <- reactive({
    features <- colnames(myData())
    samples <- rownames(myData())
    # if (input$rem && input$incl) {
    #   stop("Cannot select both features to include and features to exclude")
    # }
    if(input$rem) 
      {
        features <- setdiff(features, input$toRm)
        dat<-myData()[,features]
        return(dat)
      } 
    else
    {
      dat<-myData()
      return(dat)
    }
    dat<-myData()[,features]
    return(dat)
  })
  
  output$table<-renderTable({
    dat<-selectedData()
    dat%>%data.frame()%>%rownames_to_column('Players')

  })
  
  output$plot1 <- renderPlot({
    selDat <- selectedData()
    corMat <- cor(selDat, method=input$method)
    op <- par(mar = c(12, 4.1, 2, 15), oma=c(6, 0, 0, 6))
    hmcols <- colorRampPalette(c("white","orangered4"))(256)
    hc <- hclust(dist(corMat, method=input$distMethod),
                 method=input$clustMethod)
    hc <- hclust(dist(t(selDat), method=input$distMethod),
                 method=input$clustMethod)
    hc <- hclust(as.dist(1-cor(corMat, method="spearman")), method=input$clustMethod)
    heatmap.2(corMat,
              Colv=as.dendrogram(hc), Rowv=as.dendrogram(hc),
              dendrogram="column", trace="none",
              col=hmcols, scale="none")
    par(op)})
# 
#   output$plot2 <- renderPlot({
#     selDat <- selectedData()
#     autoplot(stats::kmeans(selDat, input$num), data = selDat,label=TRUE)

  
  clusters<-reactive({
    if(input$model==5)
    {
      #kmeans
      selDat <- selectedData()
      results <- kmeans(selDat, input$num)
      df<-prcomp(selDat)
      selDat$cluster <- results$cluster
      selDat$PC1<-df$x[,1]
      selDat$PC2<-df$x[,2]
      selDat$cluster<-as.factor(selDat$cluster)
      selDat=as.data.frame(selDat)
      return(selDat)
    }
    else(input$model==9)
    {
      selDat <- selectedData()
      results <- fcm(selDat, input$num)
      df<-prcomp(selDat)
      selDat$cluster <- results$cluster
      selDat$PC1<-df$x[,1]
      selDat$PC2<-df$x[,2]
      selDat$cluster<-as.factor(selDat$cluster)
      selDat=as.data.frame(selDat)
      return(selDat)
    }
    if(input$modl==12)
    {
      selDat <- selectedData()
      results<-pam(selDat, input$num, metric = "euclidean", stand = FALSE)
      df<-prcomp(selDat)
      selDat$cluster <- results$clustering
      selDat$PC1<-df$x[,1]
      selDat$PC2<-df$x[,2]
      selDat$clustering<-as.factor(selDat$clustering)
      selDat=as.data.frame(selDat)
      return(selDat)
    }
    if(input$model==9)
    {
      selDat <- selectedData()
      results <- fcm(x, centers=3)
      df<-prcomp(selDat)
      selDat$cluster <- results$cluster
      selDat$PC1<-df$x[,1]
      selDat$PC2<-df$x[,2]
      selDat$clustering<-as.factor(selDat$cluster)
      selDat=as.data.frame(selDat)
      return(selDat)
      
    }
    
   })
  
  output$plot2<-renderPlot({

    p <- ggplot(data = clusters(), aes(x = PC1, y = PC2, color = cluster, shape = cluster))+
      geom_point()
    #
    p + geom_label_repel(aes(label=rownames(clusters())),
                         box.padding   = 0.35,
                         point.padding = 0.5,
                         segment.color = 'grey50') +  theme_classic() })
  
  output$stats2<-renderPlot({
    
    if(input$scale)
    {
      data<-selectedData()
      cluster_with_PCS<-clusters()[,14:16]
      combined<-cbind(unscale(data),cluster_with_PCS)
      
      combined %>% pivot_longer(cols = -c(cluster,PC1,PC2)) %>%
        ggplot(aes(x=value,fill=name))+
        geom_histogram()+
        facet_grid(cluster~name,scales = 'free',switch = "y")+
        theme_bw()+
        theme(panel.grid = element_blank(),
              axis.text = element_text(face='bold',color='black'),
              axis.title = element_text(face='bold',color='black'),
              strip.text = element_text(face='bold',color='black'),
              legend.text = element_text(face='bold',color='black'),
              legend.title = element_text(face='bold',color='black'),
              strip.background = element_blank())+ggtitle(paste0('Distribution of Each Feature in Each Cluster'))
      
    }
    else
    {
      
      clusters() %>% pivot_longer(cols = -c(cluster,PC1,PC2)) %>%
        ggplot(aes(x=value,fill=name))+
        geom_histogram()+
        facet_grid(cluster~name,scales = 'free',switch = "y")+
        theme_bw()+
        theme(panel.grid = element_blank(),
              axis.text = element_text(face='bold',color='black'),
              axis.title = element_text(face='bold',color='black'),
              strip.text = element_text(face='bold',color='black'),
              legend.text = element_text(face='bold',color='black'),
              legend.title = element_text(face='bold',color='black'),
              strip.background = element_blank())+ggtitle(paste0('Distribution of Each Feature in Each Cluster'))
    }
 
  })
  
  rename<-reactive({
    names<-c('Tee To Green',	'Driving Distance',	'Average SG:Putting','Average Approach The Green',	'Average
             Off The.Tee',	'Average SG:Around The Green',	'Scoring Average',	'Rank',	'Height',	'Weight',	'Sand Saves',	'Bunkers',	'Hit Fairway','Cluster','PC1','PC2')
    dat<-clusters()
    colnames(dat)<-names
    dat
    })
    
    output$stats1<-renderTable({
      dat<-rename()
      dat<-dat[,1:14]
      mean<-dat%>%group_by(Cluster)%>%summarize_all(list(mean))
    })
    
    output$clusterData<-renderTable({
      clusters()%>%rownames_to_column('Players')
    })
    
    clusters_data<-reactive({
      clusters()%>%rownames_to_column('Players')
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(file_name(),".csv", sep = "")
      },
      content = function(file) {
        write.csv(clusters_data(), file, row.names = FALSE)
      }
    )

    output$siloute<-renderPlot({

      dat<-clusters()
      cluster<-dat[,1:14]
      row_data<-dat[,1:13]
      si2 <- silhouette(as.numeric(cluster[['cluster']]), dist(row_data, "canberra"))
      plot(si2, col = 2+ as.integer(cluster$cluster) %/% 1000,
         main ="Silhouette Score Plot")})
    
    output$interaction<-renderPlot({
      ggpairs(data=clusters(),columns=1:13,mapping=aes(color=cluster))
    })
    
})
  


  
  
  
  
  

