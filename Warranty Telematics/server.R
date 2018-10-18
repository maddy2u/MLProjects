library(shiny)
library(datasets)
library(ggplot2)
library(psych)
library(dplyr)
library(plotly)
library(reshape2)
library(DT)
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  
  ################ Data Snapshot Page ######################
  
  # Warranty Data
  
  read_warranty_file <- reactive({
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    warranty_data = read.csv(inFile1$datapath, header = TRUE)
    warranty_data
    
  })
  
  # DTC Data
  
  read_DTC_file <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    
    DTC_data = read.csv(inFile2$datapath, header = TRUE)
    DTC_data
    
  })
  
  output$Dataset_snapshot=renderDataTable(
    {
      
      if(input$type=="Warranty")
      {
        a<-read_warranty_file()
        if(is.null(a)) return(NULL)
        df=a
        return(head(df,1000))
        # do.call(data.frame,df)
      }
      else if(input$type=="DTC")
      {
        a<-read_DTC_file()
        if(is.null(a)) return(NULL)
        df=a
        return(head(df,1000))
        #do.call(data.frame,df)
      }
    }, options = list(lengthMenu =c(5,10,20,50,100), pageLength = 10,scrollX = TRUE, scrollY =300))
  
  
  ################ EDA Page ######################
  
  #Start Divs EDA Code
  
  
  shiny::observe({
    
    if(input$eda_uni_var =="Warranty Data")
    {
      a <- read_warranty_file()
      if(is.null(a)) return(NULL)
      warranty_data_div <- as.data.frame(a)
      pq <- colnames(warranty_data_div)
      updateSelectInput(session, "eda_uni_var1", choices = colnames(warranty_data_div),
                        selected = head(pq,13) )
      # warranty_data_div$'input$eda_uni_var1'
    }
    else
    {
      b <- read_DTC_file()
      if(is.null(b)) return(NULL)
      DTC_data_div <- as.data.frame(b)
      updateSelectInput(session, "eda_uni_var1", choices = colnames(DTC_data_div))
    }
  })
  
  ###Univarite plot   'input$eda_uni_var1'
  output$univariate_plot<-renderPlot(
    {
      if (is.null(read_warranty_file()))
        return(NULL)
      if(input$eda_uni_var =="Warranty Data")
      {
        a <- read_warranty_file()
        warranty_data_div <- as.data.frame(a)
        bc <- input$eda_uni_var1
        if(is.factor(warranty_data_div[[bc]]))
        {
          barplot(table(warranty_data_div[[bc]]), 
                  main = "Barplot for the variable", 
                  xlab = bc, 
                  ylab =  "Frequency",
                  col = "green")
        }  
        else if(is.numeric(warranty_data_div[[bc]]))
        {
          hist(warranty_data_div[[bc]],
               main = "Histogram for variable", 
               xlab = bc, 
               ylab =  "Frequency",
               col = "green")
        } 
      }
      else
      {
        a <- read_DTC_file()
        DTC_data_div <- as.data.frame(a)
        bc <- input$eda_uni_var1
        if(is.factor(DTC_data_div[[bc]]))
        {
          barplot(table(DTC_data_div[[bc]]), 
                  main = "Barplot for the variable", 
                  xlab = bc, 
                  ylab =  "Frequency",
                  col = "green")
        }  
        else if(is.numeric(DTC_data_div[[bc]]))
        {
          hist(warranty_data_div[[bc]],
               main = "Histogram for variable", 
               xlab = bc, 
               ylab =  "Frequency",
               col = "green")
        }
      }
    }
  )
  
  
  #     ###Bivariate plot
  
  shiny::observe({
    
    if(input$eda_bi_var =="Warranty Data")
    {
      a <- read_warranty_file()
      if(is.null(a)) return(NULL)
      warranty_data_div <- as.data.frame(a)
      pq <- colnames(warranty_data_div)
      updateSelectInput(session, "eda_bi_var1", choices = pq,
                        selected = head(pq,9))
      updateSelectInput(session, "eda_bi_var2", choices = pq,
                        selected = head(pq,16))
    }
    else
    {
      b <- read_DTC_file()
      if(is.null(b)) return(NULL)
      DTC_data_div <- as.data.frame(b)
      pq <- colnames(DTC_data_div)
      updateSelectInput(session, "eda_bi_var1", choices = pq, 
                        selected = 1)
      updateSelectInput(session, "eda_bi_var2", choices = pq, 
                        selected = 1)
    }
  })
  
  output$bivariate_plot<-renderPlot(
    {
      if (is.null(read_warranty_file()))
        return(NULL)
      if(input$eda_bi_var =="Warranty Data")
      {
        warranty_data_div <- as.data.frame(read_warranty_file())
        var1 <- input$eda_bi_var1
        var2 <- input$eda_bi_var2
        
        if(is.numeric(warranty_data_div[[var1]]) & is.numeric(input$eda_bi_var2)){
          plot(x = warranty_data_div[[var1]],y = warranty_data_div[[var2]],type = "b", xlab = var1, ylab = var2,main = "Univariate Plot")   
        }  
        else if(is.numeric(warranty_data_div[[var1]]) & is.factor(warranty_data_div[[var2]])){
          ggplot(data = warranty_data_div, aes(x= warranty_data_div[[var1]] ,y = warranty_data_div[[var2]])) + geom_boxplot(aes(fill =  warranty_data_div[[var1]])) +labs(x = var1, y = var2)
        }  
        else if(is.factor(warranty_data_div[[var1]]) & is.numeric(warranty_data_div[[var2]])){
          ggplot(data = warranty_data_div, aes(x= warranty_data_div[[var1]], y= warranty_data_div[[var2]] )) + geom_boxplot(aes(fill =  warranty_data_div[[var2]])) +labs(x = var1, y = var2)
        }  
        else if(is.factor(warranty_data_div[[var1]]) & is.factor(warranty_data_div[[var2]])){
          ggplot(warranty_data_div, aes(warranty_data_div[[var1]], warranty_data_div[[var2]])) +   geom_bar(aes(fill = warranty_data_div[[var1]]), position = "dodge", stat="identity") +labs(x = var1, y = var2)+ guides(fill =FALSE) 
        }
      }
      else
      {
        DTC_data_div <- as.data.frame(read_DTC_file())
        var1 <- input$eda_bi_var1
        var2 <- input$eda_bi_var2
        
        if(is.numeric(DTC_data_div[[var1]]) & is.numeric(DTC_data_div[[var2]])){
          plot(x = DTC_data_div[[var1]],y = DTC_data_div[[var2]],type = "b", xlab = var1, ylab = var2,main = "Univariate Plot")   
        }  
        else if(is.numeric(DTC_data_div[[var1]]) & is.factor(DTC_data_div[[var2]])){
          ggplot(data = DTC_data_div, aes(x= DTC_data_div[[var1]] ,y = DTC_data_div[[var2]])) + geom_boxplot(aes(fill =  DTC_data_div[[var1]])) + labs(x = var1, y = var2) 
        }  
        else if(is.factor(DTC_data_div[[var1]]) & is.numeric(DTC_data_div[[var2]])){
          ggplot(data = DTC_data_div, aes(x= DTC_data_div[[var1]], y= DTC_data_div[[var2]] )) + geom_boxplot(aes(fill =  DTC_data_div[[var2]])) + labs(x = var1, y = var2)
        }  
        else if(is.factor(DTC_data_div[[var1]]) & is.factor(DTC_data_div[[var2]])){
          ggplot(DTC_data_div, aes(DTC_data_div[[var1]], DTC_data_div[[var2]])) +   geom_bar(aes(fill = DTC_data_div[[var1]]), position = "dodge", stat="identity") + labs(x = var1, y = var2) + guides(fill =FALSE)
        }
      }
    }
  )
  
  # Distribution Graphs
  
  shiny::observe({
    
    if(input$eda_dist_var =="Warranty Data")
    {
      a <- read_warranty_file()
      if(is.null(a)) return(NULL)
      warranty_data_div <- as.data.frame(a)
      pq <- colnames(warranty_data_div)
      updateSelectInput(session, "eda_distri", 
                        choices = colnames(warranty_data_div), 
                        selected = tail(pq,3) )
      # Column number 1-6,12,26,36 have only one value. So need to be omitted
    }
    else
    {
      b <- read_DTC_file()
      if(is.null(b)) return(NULL)
      DTC_data_div <- as.data.frame(b)
      pq <- colnames(DTC_data_div)
      updateSelectInput(session, "eda_distri", 
                        choices = colnames(DTC_data_div), 
                        selected = tail(pq,3))
    }
  })
  
  
  # output$distri_graph<-renderPlot(
  #   {
  #     if(input$eda_dist_var =="Warranty Data")
  #     {
  #       warranty_data_div <- as.data.frame(read_warranty_file())
  #       selectedcolumns <- input$eda_distri
  #       pairs.panels(warranty_data_div[,selectedcolumns], scale = TRUE)
  #     }
  #     else
  #     {
  #       DTC_data_div <- as.data.frame(read_DTC_file())
  #       selectedcolumns <- input$eda_distri
  #       pairs.panels(DTC_data_div[,selectedcolumns], scale = TRUE)
  #     }
  #   } 
  # )
  
  output$distri_graph<-renderPlot(
    {
      if(input$eda_dist_var =="Warranty Data")
      {
        warranty_data_div <- as.data.frame(read_warranty_file())
        selectedcolumns <- input$eda_distri
        if(length(selectedcolumns) <= 1)
        {
          shiny::validate(
            shiny::need(
              try(distri_graph != "")
              ,
              "Select more than one variable"
            )
          )
        }
        else
        {
          j <- length(selectedcolumns)
          for(i in 1:j)
          {
            validate(
              need(
                try(length(unique(warranty_data_div[[selectedcolumns[i]]])) != 1)
                ,
                "Selected variable has only one unique value. Please select other variable." 
              )
            )
          }
          pairs.panels(warranty_data_div[,selectedcolumns], scale = TRUE)
        }
      }
      else
      {
        DTC_data_div <- as.data.frame(read_DTC_file())
        selectedcolumns <- input$eda_distri
        if(length(selectedcolumns) <= 1)
        {
          shiny::validate(
            shiny::need(
              try(distri_graph != "")
              ,
              "Select more than one variable"
            )
          )
        }
        else
        {
          j <- length(selectedcolumns)
          for(i in 1:j)
          {
            validate(
              need(
                try(length(unique(DTC_data_div[[selectedcolumns[i]]])) != 1)
                ,
                "Selected variable has only one unique value. Please select other variable." 
              )
            )
          }
          pairs.panels(DTC_data_div[,selectedcolumns], scale = TRUE)
        }
        
      }
    } 
  )
  
  ################### Cluster Page ################
  library(tm)
  library(wordcloud)
  library(topicmodels)
  
  
  data_warranty <- eventReactive(input$actionbu,{
    
    if (is.null(read_warranty_file()))
      return(NULL)
    d <- read_warranty_file()
    ## forming the corpus
    docs <- Corpus(VectorSource(d$Description))
    docs <- tm_map(docs,tolower)
    docs <- tm_map(docs,PlainTextDocument)
    ## removing DNC-FPV
    docs <- tm_map(docs, content_transformer(gsub), pattern = "\\b(dnc-fpv)\\b", replacement = "")
    docs <- tm_map(docs, content_transformer(gsub), pattern = "\\b(jdt)\\b", replacement = "")
    
    ### pre-processing
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs, stemDocument)
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs,PlainTextDocument)
    
    stoptext<-unlist(strsplit(input$stopwords,","))
    docs <- tm_map(docs, removeWords, c(stopwords(),stoptext))
    docs
  })
  
  output$plot <-renderPlot({
    
    
    if (is.null(data_warranty()))
      return(NULL)
    docs <- data_warranty()
    dt_matrix <- DocumentTermMatrix(docs)
    
    temp<- as.data.frame(sort(colSums(as.matrix(dt_matrix)), decreasing = TRUE))
    temp$words <- row.names(temp)
    colnames(temp)[1] <-"freq"
    wordcloud(words=temp$words,freq = temp$freq,min.freq=10,random.order=F)
  })
  
  output$Clusters <- renderDataTable({
    
    if (is.null(data_warranty()))
      return(NULL)
    docs <- data_warranty()
    dt_matrix <- DocumentTermMatrix(docs)
    df_dtm <-(as.matrix(dt_matrix))
    
    if (is.null(read_warranty_file()))
      return(NULL)
    d <- read_warranty_file()
    row.names(df_dtm)=seq(1:nrow(d))
    findFreqTerms(dt_matrix,lowfreq = 10)
    
    raw.sum =apply(df_dtm,1,FUN=sum)
    raw.sum <- as.data.frame(raw.sum)
    dtm1<- df_dtm[!(raw.sum$raw.sum == 0),]
    d1 <- d[!(raw.sum$raw.sum == 0),]
    
    ntopics <- input$slider1
    set.seed(2343)
    lda <- LDA(dtm1,ntopics,method="Gibbs")
    ## the topics
    topics <- as.data.frame(topics(lda))
    ### top 5 terms in the topic
    terms <- as.data.frame(terms(lda,5))
    terms
    
  },options = list(lengthMenu =c(5,10,20,50,100), pageLength = 10,scrollX = TRUE, scrollY =300))
  
  
  warranty_finalclusters <- reactive({
    if (is.null(data_warranty()))
      return(NULL)
    docs <- data_warranty()
    dt_matrix <- DocumentTermMatrix(docs)
    df_dtm <-(as.matrix(dt_matrix))
    
    if (is.null(read_warranty_file()))
      return(NULL)
    d <- read_warranty_file()
    row.names(df_dtm)=seq(1:nrow(d))
    findFreqTerms(dt_matrix,lowfreq = 10)
    
    raw.sum =apply(df_dtm,1,FUN=sum)
    raw.sum <- as.data.frame(raw.sum)
    dtm1<- df_dtm[!(raw.sum$raw.sum == 0),]
    d1 <- d[!(raw.sum$raw.sum == 0),]
    
    ntopics <- input$slider1
    lda <- LDA(dtm1,ntopics,method="Gibbs")
    ## the topics
    topics <- as.data.frame(topics(lda))
    ### top 5 terms in the topic
    terms <- as.data.frame(terms(lda,5))
    updated <- as.data.frame(cbind(d1,topics=topics$`topics(lda)`))
    updated
  })
  
  output$Replacement_Rate <-renderDataTable({
    clusters<-group_by(warranty_finalclusters(),topics,Part.Number)
    clusters1<-group_by(warranty_finalclusters(),topics)
    totalbycluster<-summarize(clusters1,totalByCluster=n())
    replacementrate<-summarize(clusters,count=n(),replacement=sum(as.numeric(Replacement)))
    replacementrate$replacementrate<-round(replacementrate$replacement*100/replacementrate$count,1)
    replacementrate<-merge(replacementrate,totalbycluster)
    replacementrate$percentagecluster<-round(replacementrate$count*100/replacementrate$totalByCluster,1)
    replacementrate<-replacementrate[,c(1,2,5,7)]
    replacementrate
  },options = list(lengthMenu =c(5,10,20,50,100), pageLength = 10,scrollX = TRUE, scrollY =300))
  
  output$plot4 <-renderPlotly({
    clusters<-group_by(warranty_finalclusters(),topics)
    WarrantyHours<-summarize(clusters,AverageWarrantyHours=mean(Warranty.Hours), StdDev.WarrantyHours=sd(Warranty.Hours))
    WarrantyHours$topics<- as.factor(WarrantyHours$topics)
    WarrantyHours.l<-melt(WarrantyHours,id.vars = "topics")
    ggplot(data=WarrantyHours.l,aes(fill=variable,x=topics,y=value))+ stat_summary(fun.y = sum, geom = "bar"
                                                                                   ,position=position_dodge(1)
    ) + scale_color_discrete("topics")
  })
  
  output$plot2 <-renderPlotly({
    nclusters <- input$numberin
    updatedcluster <-filter(warranty_finalclusters(),topics==nclusters)
    warhrs<-as.data.frame(as.numeric(updatedcluster$new_warr))
    names(warhrs)<-"warrantyHours"
    warhrs$warrantyHours<-sort(warhrs$warrantyHours,decreasing = FALSE)
    
    #Adding Ranks to the data
    nranks<-nrow(warhrs)
    warhrs$rank<-seq(1:nranks)
    
    #Calculating Median Ranks
    warhrs$medianRank<-(warhrs$rank-0.3)/(nranks+0.4)
    
    #Calculating ln(ln(1/(1-Median Rank)))
    warhrs$y<-log(log(1/(1-warhrs$medianRank)))
    
    #Calculating ln(Warranty Hours)
    warhrs$x<-log(warhrs$warrantyHours)
    
    #Obtaining the Weibull Estimates of shape and lifetime value
    linreg<-lm(warhrs$y~warhrs$x)
    alpha<-exp(-linreg$coefficients[1]/linreg$coefficients[2])
    beta<-linreg$coefficients[2]
    
    
    # Reliability Curve
    ndigits<-nchar(as.character(round(alpha)))
    pwr<-10^ndigits
    firstdigit<-floor(alpha/pwr*10)
    relcal<-as.data.frame(seq(100,1000,100))
    names(relcal)<-"warrantyHours"
    
    relcal$failureProbability<-pweibull(relcal$warrantyHours,beta,alpha)
    relcal$reliability<-1-relcal$failureProbability
    
    #Plotting the Reliability Curve
    ggplot(data=relcal,aes(x=warrantyHours,y=reliability))+geom_line(size=1)+geom_point(size=3,fill="white")+xlab("Warranty Hours")+ylab("Reliability")+ggtitle("Reliability Curve")+theme_bw()
    
  })
  
  
  #################### DTC Page ################
  
  DTC_DropDown<-read.csv("www/Error Drop Down.csv",header = T)
  Machine_Data<-read.csv("www/modeldata_DT.csv",header = T)
  DTC_Sequence_Time_Distribution<-read.csv("www/DTC Sequence Time Distribution.csv",header = T)
  #Machine_Data<-Machine_Data[,c(1,4)]
  colnames(Machine_Data)[1]<- 'Machine Number'
  Machine_Data$'Machine Number'<-(substring(Machine_Data$`Machine Number`, 9))
  
  # output$Machine_Identification <-renderDataTable({
  #   clusternumb <- input$dtc_clusters
  #   dtc_select_no <- input$dtc_select
  #   Cluster1_Error1<-Machine_Data[Machine_Data$DTC.Code.x %in% DTC_DropDown[[clusternumb]][dtc_select_no],]
  #   Cluster1_Error1
  # },options = list(lengthMenu =c(5,10,20,50,100), pageLength = 10,scrollX = TRUE, scrollY =300))

  # output$text1 <- renderText({ 
  #   clusternumb <- input$dtc_clusters
  #   dtc_select_no <- input$dtc_select
  #   Cluster1_Error1<-Machine_Data[Machine_Data$DTC.Code.x %in% DTC_DropDown[[clusternumb]][dtc_select_no],]
  #   percentmachines <- (length(unique(Cluster1_Error1$`Machine Number`))/length(unique(Machine_Data$'Machine Number')))*100
  #   paste(round(percentmachines,1), "% Machines at DTC")
  # })
  
  # output$text2 <- renderText({ 
  #   clusternumb <- input$dtc_clusters
  #   paste(DTC_Sequence_Time_Distribution[clusternumb,2], "Lead time before IInd Error")
  # })
  # output$text3 <- renderText({ 
  #   clusternumb <- input$dtc_clusters
  #   paste(DTC_Sequence_Time_Distribution[clusternumb,3], "Lead time before Machine fails")
  # })
  
  output$Machine_Identification <- renderDataTable({
    clusternumb <- input$dtc_clusters
    dtc_error <- as.numeric(input$inRadioButtons102)
    Cluster1_Error1<-Machine_Data[Machine_Data$DTC.Code.x %in% DTC_DropDown1[[clusternumb]][dtc_error],]
    Cluster1_Error1
  },options = list(lengthMenu =c(5,10,20),pageLength = 10,scrollX = TRUE, scrollY =300, pagingType = "simple")
  )
  
  output$text1 <- renderText({ 
    clusternumb <- input$dtc_clusters
    dtc_error <- as.numeric(input$inRadioButtons102)
    Cluster1_Error1<-Machine_Data[Machine_Data$DTC.Code.x %in% DTC_DropDown1[[clusternumb]][dtc_error],]
    percentmachines <- (length(unique(Cluster1_Error1$`Machine Number`))/length(unique(Machine_Data$'Machine Number')))*100
    paste(round(percentmachines,1), "% Machines at DTC")
  })
  output$text2 <- renderText({ 
    clusternumb <- input$dtc_clusters
    dtc_error <- as.numeric(input$inRadioButtons102)
    
    if(dtc_error == 1)
    {
      paste(DTC_Sequence_Time_Distribution[clusternumb,2], "days Lead time before IInd Error")
    }
    else if(dtc_error == 2)
    {
      paste(DTC_Sequence_Time_Distribution[clusternumb,3], "days Lead time before Machine fails")
    }
    else
    {
      return(NULL)
    }
  })
  
  output$text106 <- renderText({ 
    i <- input$dtc_clusters
    paste( DTC_Sequence[i,2], "->", DTC_Sequence[i,3], "->", DTC_Sequence[i,4] )
  })
  
  output$text107 <- renderText(
    {
      paste("Error" ,"Sequence for the cluster is :") 
    }
  )
  
  
  ###############Summary Page - DTC Sequence##########
  DTC_Sequence <- read.csv("www/Top 13 sequences.csv", header = T)
  DTC_DropDown1 <- read.csv("www/Error Drop Down1.csv",header = T)
  
  output$Machine_Identification1 <- renderDataTable({
    clusternumb <- input$dtc_clusters1
    dtc_error <- as.numeric(input$inRadioButtons2)
    Cluster1_Error1<-Machine_Data[Machine_Data$DTC.Code.x %in% DTC_DropDown1[[clusternumb]][dtc_error],]
    Cluster1_Error1
  },options = list(lengthMenu =c(5,10),pageLength = 5,scrollX = TRUE, scrollY =100,pagingType = "simple")
  ) 
  
  output$text4 <- renderText({ 
    clusternumb <- input$dtc_clusters1
    dtc_error <- as.numeric(input$inRadioButtons2)
    Cluster1_Error1<-Machine_Data[Machine_Data$DTC.Code.x %in% DTC_DropDown1[[clusternumb]][dtc_error],]
    percentmachines <- (length(unique(Cluster1_Error1$`Machine Number`))/length(unique(Machine_Data$'Machine Number')))*100
    paste(round(percentmachines,1), "% Machines at DTC")
  })
  
  output$text5 <- renderText({ 
    clusternumb <- input$dtc_clusters1
    dtc_error <- as.numeric(input$inRadioButtons2)
    
    if(dtc_error == 1)
    {
      paste(DTC_Sequence_Time_Distribution[clusternumb,2], "days Lead time before IInd Error")
    }
    else if(dtc_error == 2)
    {
      paste(DTC_Sequence_Time_Distribution[clusternumb,3], "days Lead time before Machine fails")
    }
    else
    {
      return(NULL)
    }
  })
  
  output$text6 <- renderText({ 
    i <- input$dtc_clusters1
    paste( DTC_Sequence[i,2], "->", DTC_Sequence[i,3], "->", DTC_Sequence[i,4] )
  })
  
  output$text7 <- renderText(
    {
      paste("Error" ,"Sequence for the cluster is :") 
    }
  )
  
})
