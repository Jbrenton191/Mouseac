#setwd("/Users/Jonathan/Downloads/MOUSEAC/MOUSEAC.2 3")
rstudioapi::getActiveDocumentContext

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(ggplot2)
library(data.table)
library(Rmisc)
library(shinycssloaders)
library(shinythemes)
library(magrittr)

jscode <- '\n$(function() {\nvar $els = $(\"[data-proxy-click]\");\n$.each(\n$els,\nfunction(idx, el) {\nvar $el = $(el);\nvar $proxy = $(\"#\" + $el.data(\"proxyClick\"));\n$el.keydown(function (e) {\nif (e.keyCode == 13) {\n$proxy.click();\n}\n});\n}\n);\n});\n'

noms<-read.table("genenames.csv", header = FALSE, sep = ",", dec=".", row.names = NULL, stringsAsFactors = FALSE, colClasses="character")
#noms<-names(fread("New_cureAD.csv", stringsAsFactors = F, header=TRUE))[-c(1:7)]
noms2<-read.table("arraynames.csv", header = FALSE, sep = ",", dec=".", row.names = NULL, stringsAsFactors = FALSE, colClasses="character")
noms3<-read.table("Cure_AD_genenames.csv", header = TRUE, sep = ",", dec=".", row.names = NULL, stringsAsFactors = FALSE, colClasses="character")
noms3<-noms3$x

ui<-fluidPage( theme = shinytheme("cerulean"),
               tags$head(tags$style(HTML("@import url(http://fonts.googleapis.com/css?family=Magra:400,700);"))),
               
               tags$head(tags$script(HTML(jscode))),
               # tags$head(includeScript("google-analytics.js")),
               #includeCSS("style.css"),
               fluidRow(
                 column(6, 
                        tags$h1(id="big-heading", "MOUSEAC", style = "font-family: ''Magra', sans-serif;
                                font-weight: 200; line-height: 0px; 
                                color: #ff6666;"),
                        tags$br(),
                        tags$h5("Web server for data from the Mouse Dementia Network", style = "font-family: ''Magra', sans-serif;
                                font-weight: 200; line-height: 0px; 
                                color: #ff6666;")),
                 column(2,""),
                 column(1, tags$a(href="Mouseac_documentation.pdf", target="_blank", "Documentation", style = "font-family: ''Magra', sans-serif;
                                  font-weight: 200; line-height: 0px; 
                                  color: white;")),
                 column(1,""),
                 column(1, tags$a(href="MouseacAboutUs.pdf", target="_blank", "About/Contact Us", style = "font-family: ''Magra', sans-serif;
                                  font-weight: 200; line-height: 0px; 
                                  color: white;")),
                 style = "background-color:#3A4B79;"
                 ),
               sidebarLayout(
                 sidebarPanel( 
                   tabsetPanel(id = "options", type = "tabs",
                               tabPanel("APP and MAPT knock-ins - RNASeq",
                                        tags$br(),
                                        tagAppendAttributes(
                                          selectizeInput("genename3", "Gene Name", choices =  NULL, 
                                                         multiple= F, selected=NULL, 
                                                         options = list(maxOptions=12, placeholder = 'Select a Gene',
                                                                        onInitialize = I('function() { this.setValue(""); }'))), `data-proxy-click` = "action3"),
                                        
                                        # options =  list(`data-proxy-click` = "action", maxOptions=25, maxItems=1, placeholder = 'Select a Gene',
                                        #                   onInitialize = I('function() { this.setValue(""); }')))),
                                        tags$div(tags$u(tags$h5("Select a Mouse Model"))),
                                        checkboxInput("NLF", HTML("APP <sup>NL-F/NL-F</sup>"), value = F),
                                        checkboxInput("NLGF", HTML("APP <sup>NL-G-F/NL-G-F</sup>"), value = F),
                                        ###HTML part of shiny used for script
                                        checkboxInput("WT3", label = HTML('Wild type (App<sup>wt/wt</sup>)'), value = T),
                                        tags$br(),
                                        tags$u(tags$h5("Plot Options")),
                                        checkboxInput("Gender", label = "Plot Male and Female Groups Separately", value = F),
                                        actionButton("action3", "Submit", width='100%'),
                                        tags$br(),
                                        tags$br(),
                                        downloadButton('downloadData3', 'Download Data by Gene'),
                                        tags$br(),
                                        downloadButton('downloadPlot3', 'Download Plot'),
                                        tags$br(),
                                        downloadButton("downloadDataTranscript3", label = "Download full RNASeq expression data"),
                                        tags$br(),
                                        tags$a(href="https://www.cell.com/article/S2211-1247%2814%2901094-8/abstract?code=cell-site", "In reference to Mouseac, please quote Matarin et al. 2015")
                               ),
                               
                               tabPanel("APP and MAPT transgenics - RNASeq",
                                        tags$br(),
                                        tagAppendAttributes(selectizeInput("genename", "Gene Name", choices =  NULL, 
                                                                           multiple= F, selected=NULL, 
                                                                           options = list(maxOptions=12, placeholder = 'Select a Gene',
                                                                                          onInitialize = I('function() { this.setValue(""); }'))), `data-proxy-click` = "action"),
                                        
                                        tags$div(tags$u(tags$h5("Select a Mouse Model"))),
                                        checkboxInput("HET_TASTPM","HET_TASTPM (heterozygous TASTPM)", value = F),
                                        checkboxInput("HO_TASTPM","HO_TASTPM (homozygous TASTPM)", value = F),
                                        checkboxInput("TAS10", "TAS10 (APP: K670N/M671L)", value = F),
                                        checkboxInput("TAU", "Tau (MAPT P301L)", value = F),
                                        checkboxInput("TPM", "TPM (PSEN1: M146V)", value = F),
                                        checkboxInput("WT", "Wild Type", value = T),
                                        actionButton("action", "Submit", width='100%'),
                                        tags$br(),
                                        tags$br(),
                                        downloadButton('downloadData', 'Download Data by Gene'),
                                        tags$br(),
                                        downloadButton('downloadPlot', 'Download Plot'),
                                        tags$br(),
                                        downloadButton("downloadDataTranscript", label = "Download full RNASeq expression data"),
                                        tags$br(),
                                        tags$a(href="https://www.cell.com/article/S2211-1247%2814%2901094-8/abstract?code=cell-site", "In reference to Mouseac, please quote Matarin et al. 2015")
                               ),
                               tabPanel("APP and MAPT transgenics - Microarray",
                                        tags$br(),
                                        tagAppendAttributes(selectizeInput("genename2", "Gene Name", choices =  NULL,
                                                                           multiple= F, selected=NULL, 
                                                                           options = list(maxOptions=12, placeholder = 'Select a Gene',
                                                                                          onInitialize = I('function() { this.setValue(""); }'))), `data-proxy-click` = "action2"),
                                        tags$div(tags$u(tags$h5("Select a Mouse Model"))),
                                        checkboxInput("HET_TASTPM2","HET_TASTPM (heterozygous TASTPM)", value = F),
                                        checkboxInput("HO_TASTPM2","HO_TASTPM (homozygous TASTPM)", value = F),
                                        checkboxInput("TAS102", "TAS10 (APP: K670N/M671L)", value = F),
                                        checkboxInput("TAU2", "Tau (MAPT P301L)", value = F),
                                        checkboxInput("TPM2", "TPM (PSEN1: M146V)", value = F),
                                        checkboxInput("WT2", "Wild Type", value = T),
                                        actionButton("action2", "Submit", width='100%'),
                                        tags$br(),
                                        tags$br(),
                                        downloadButton('downloadData2', 'Download Data by Gene'),
                                        tags$br(),
                                        downloadButton('downloadPlot2', 'Download Plot'),
                                        tags$br(),
                                        downloadButton("downloadDataTranscript2", label = "Download full Microarray expression data"),
                                        tags$br(),
                                        tags$a(href="https://www.cell.com/article/S2211-1247%2814%2901094-8/abstract?code=cell-site", "In reference to Mouseac, please quote Matarin et al. 2015")
                               ))
                 ),
                 mainPanel(
                   h1(withSpinner(plotOutput("plot", width = "100%", height  = "800px")))
                 )
                 
                 
               )
                 )




server <- function(input, output, session) {
  
  ###### new Knock-In data
  
  updateSelectizeInput(session, "genename3", selected = " ", label = "Gene name (Click and start typing to search)", choices = noms3, server = TRUE)
  
  expressionPlot3 <- reactive ({
    
    if(input$Gender){
      
      geneName3 <- (paste(input$genename3))
      #geneName <-"Trem2"
      sel <- c("Age","Genotype", "Gender", geneName3)
      scores3<-fread("New_cureAD.csv", select = sel, stringsAsFactors = F, header=TRUE)
      colnames(scores3) <- c("Age","Genotype", "Gender", "Expression")
      
      pathologyScoresHipp3 <- fread("Knock_in_Pathology.csv", stringsAsFactors = F, header=TRUE)
      
      
      primaryColors3 <- c("blue","red","black")
     dotShapes4 <- c(23,7,1)
      dotShapes3 <- c("\u2642","\u2640")
     
      if(!input$NLF){
        scores3 <- scores3[which(scores3$Genotype!="NLF"),]
        pathologyScoresHipp3 <- pathologyScoresHipp3[which(pathologyScoresHipp3$Genotype!="NLF"),]
        primaryColors3 <-primaryColors3[-which(primaryColors3=="blue")]
        dotShapes4 <- dotShapes3[-which(dotShapes4==23)]
      }
      
      if(!input$NLGF){
        scores3 <- scores3[which(scores3$Genotype!="NLGF"),]
        pathologyScoresHipp3 <- pathologyScoresHipp3[which(pathologyScoresHipp3$Genotype!="NLGF"),]
        primaryColors3 <-primaryColors3[-which(primaryColors3=="red")]
           dotShapes4 <- dotShapes3[-which(dotShapes4==7)]
      }
      
      if(!input$WT3){
        scores3 <- scores3[which(scores3$Genotype!="WT"),]
        pathologyScoresHipp3 <- pathologyScoresHipp3[which(pathologyScoresHipp3$Genotype!="WT"),]
        primaryColors3 <-primaryColors3[-which(primaryColors3=="black")]
         dotShapes4 <- dotShapes3[-which(dotShapes4==1)]
      }
      
      pathologyScoresHipp3 <- pathologyScoresHipp3[which(scores3$Genotype!="WT"),]
      
      #pathologyScoresHipp3<-na.omit(pathologyScoresHipp3)
      #scores3<-na.omit(scores3)
      
      par(mfrow=c(2,1))
      
      
      expPlot <- ggplot(data=scores3, aes_string(x = "Age", y = "Expression", colour="Genotype", shape = "Gender") )+  # Genotype becomes a classifying factor
        stat_summary(fun.y="mean", geom="line", size = 1, aes(linetype=Gender))+
        # geom_point(aes(color=Genotype, shape=Gender))+
        stat_summary(aes(color=Genotype, shape=Gender,fill=Genotype),fun.y="mean", geom="point", size = 10)+
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
        scale_shape_manual(values=dotShapes3)+
        scale_colour_manual(values = primaryColors3)+
        scale_fill_manual(values = primaryColors3)+
        scale_size_manual(values=c(100,100))+
        labs(x = "Age(Months)",y="TPM (Transcripts per Million)", colour  = "Genotype", shape="Gender", fill="Genotype")+
        scale_x_continuous(breaks=as.numeric(levels(factor(scores3$Age))))+
        #guides(fill=FALSE,shape=FALSE,colour=FALSE)+
        ggtitle(bquote(atop(.(geneName3), atop(italic(.("Hippocampus"), "")))))+
        theme(legend.position="bottom")
      
      
      ## Pathology plots
      # pathologyScoresHipp3<-na.omit(pathologyScoresHipp3)
      
      
      
      pathologyHipp <- ggplot(data=pathologyScoresHipp3, aes_string(x = "Age", y = "Pathology", color="Genotype", group="Genotype") ) +  # Genotype becomes a classifying factor
        stat_summary(fun.y="mean", geom="line", size = 1)+
        ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
        stat_summary(aes(color=Genotype, shape=Genotype,fill=Genotype),fun.y="mean", geom="point", size = 5)+
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
        scale_shape_manual(values = dotShapes4)+
        scale_colour_manual(values = primaryColors3)+
        scale_fill_manual(values = primaryColors3)+
        guides(fill=FALSE,shape=FALSE,colour=FALSE)+ ##remove the legend
        scale_x_continuous(breaks=as.numeric(levels(factor(pathologyScoresHipp3$Age))))+
        labs(x = "Age(Months)",y="Relative Plaque/Tangle density")+
        ggtitle(bquote(atop(.("Pathology"), atop(italic(.("Hippocampus"), "")))))
      
      rm(pathologyScoresHipp3)
      
      
      #system(paste("echo",geneName,">> /var/shiny-server/log/logGenes.txt"))
      return(list("exp" = expPlot, "PATHIPP"=pathologyHipp))
    }
    
    if(!input$Gender){
      
      
      geneName3 <- (paste(input$genename3))
      sel <- c("Age","Genotype", geneName3)
      scores3<-fread("New_cureAD.csv", select = sel, stringsAsFactors = F, header=TRUE)
      colnames(scores3) <- c("Age","Genotype","Expression")
      
      pathologyScoresHipp3 <- fread("Knock_in_Pathology.csv", stringsAsFactors = F, header=TRUE)
      
      
      primaryColors3 <- c("blue", "red", "black")
      dotShapes3 <- c(23,7,1)
      # dotShapes3 <- c("\u2642","\u2640")
      
      if(!input$NLF){
        scores3 <- scores3[which(scores3$Genotype!="NLF"),]
        pathologyScoresHipp3 <- pathologyScoresHipp3[which(pathologyScoresHipp3$Genotype!="NLF"),]
        primaryColors3 <-primaryColors3[-which(primaryColors3=="royalblue4")]
        dotShapes3 <- dotShapes3[-which(dotShapes3==18)]
      }
      
      
      if(!input$NLGF){
        scores3 <- scores3[which(scores3$Genotype!="NLGF"),]
        pathologyScoresHipp3 <- pathologyScoresHipp3[which(pathologyScoresHipp3$Genotype!="NLGF"),]
        primaryColors3 <-primaryColors3[-which(primaryColors3=="turquoise")]
        dotShapes3 <- dotShapes3[-which(dotShapes3==7)]
      }
      
      if(!input$WT3){
        scores3 <- scores3[which(scores3$Genotype!="WT"),]
        pathologyScoresHipp3 <- pathologyScoresHipp3[which(pathologyScoresHipp3$Genotype!="WT"),]
        primaryColors3 <-primaryColors3[-which(primaryColors3=="turquoise")]
        dotShapes3 <- dotShapes3[-which(dotShapes3==1)]
      }
      
      pathologyScoresHipp3 <- pathologyScoresHipp3[which(scores3$Genotype!="WT"),]
      
      #pathologyScoresHipp3<-na.omit(pathologyScoresHipp3)
      #scores3<-na.omit(scores3)
      
      par(mfrow=c(2,1))
      
      
      expPlot <- ggplot(data=scores3, aes_string(x = "Age", y = "Expression", colour="Genotype", group = "Genotype") )+  # Genotype becomes a classifying factor
        stat_summary(fun.y="mean", geom="line", size = 1)+
        #geom_point(aes(color=Genotype, shape=Gender))+
        stat_summary(aes(color=Genotype, shape=Genotype,fill=Genotype),fun.y="mean", geom="point", size = 5)+
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
        scale_shape_manual(values=dotShapes3)+
        scale_colour_manual(values = primaryColors3)+
        scale_fill_manual(values = primaryColors3)+
        labs(x = "Age(Months)",y="TPM (Transcripts per Million)", colour  = "Genotype", shape="Genotype", fill="Genotype")+
        scale_x_continuous(breaks=as.numeric(levels(factor(scores3$Age))))+
        #guides(fill=FALSE,shape=FALSE,colour=FALSE)+
        
        theme(legend.position="bottom")+
        ggtitle(bquote(atop(.(geneName3), atop(italic(.("Hippocampus"), "")))))
      
      
      ## Pathology plots
      # pathologyScoresHipp3<-na.omit(pathologyScoresHipp3)
      
      pathologyHipp <- ggplot(data=pathologyScoresHipp3, aes_string(x = "Age", y = "Pathology", color="Genotype", group="Genotype") ) +  # Genotype becomes a classifying factor
        stat_summary(fun.y="mean", geom="line", size = 1)+
        ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
        stat_summary(aes(color=Genotype, shape=Genotype,fill=Genotype),fun.y="mean", geom="point", size = 5)+
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
        scale_shape_manual(values = dotShapes3)+
        scale_colour_manual(values = primaryColors3)+
        scale_fill_manual(values = primaryColors3)+
        guides(fill=FALSE,shape=FALSE,colour=FALSE)+ ##remove the legend
        scale_x_continuous(breaks=as.numeric(levels(factor(pathologyScoresHipp3$Age))))+
        labs(x = "Age(Months)",y="Relative Plaque/Tangle density")+
        ggtitle(bquote(atop(.("Pathology"), atop(italic(.("Hippocampus"), "")))))
      
      rm(pathologyScoresHipp3)
      
      
      #system(paste("echo",geneName,">> /var/shiny-server/log/logGenes.txt"))
      return(list("exp" = expPlot, "PATHIPP"=pathologyHipp))
    }
    
    
    
    
    
    
    
    
  }) 
  
  
  
  output$downloadData3 <- downloadHandler(
    
    filename = function() {
      "Knock_in_RNASeqexpression.csv"
      
    }, 
    
    
    content  = function(SPECIALFILE){
      
      geneName3 <- (paste(input$genename3))
      sel <- c("Age","Genotype", "Gender", "Pathology", geneName3)
      out3<-fread("New_cureAD.csv", select = sel, stringsAsFactors = F, header=TRUE)
      colnames(out3) <- c("Age (Months)","Genotype", "Gender", "Pathology", paste(geneName3, "Expression", sep= " "))
      #  out$Age <- out$Age/4
      ## include the pathology scores
      # pathHipp <- read.csv("HippocampusPathology.csv")
      ## merge with the pathology scores 
      #out3 <- merge(pathHipp, out3, by = c("ID"))
      ## sort the table
      # out3 <- out3[order(out3[,2],out3[,4],out3[,3],decreasing=FALSE),]
      write.table(out3, file=SPECIALFILE, sep=",", quote=FALSE, row.names=F)      
    }
  ) 
  
  
  output$downloadPlot3 <- downloadHandler(
    
    filename = function() { paste('Knock_in_RNASeqexpressionPlot.jpeg')},
    
    content = function(file) {
      plots <- expressionPlot3()
      jpeg(file,width = 1024, height = 884)
      multiplot(plots$exp,plots$PATHIPP,cols=1)
      dev.off()
    },
    contentType = 'jpeg'
  )     
  
  output$downloadDataTranscript3 <- downloadHandler(
    filename = function() {
      ### here you put the name of the file to be downloaded, in my case is "transcript.rds"
      paste("full_Knock_in_RNASeqexpression", ".csv", sep = "")
    },
    content = function(file) {
      file.copy(from="New_cureAD.csv", to=file)
    }
  )
  
  ##### back to old models
  
  updateSelectizeInput(session, "genename", selected = " ",
                       label = "Gene name (Click and start typing to search)", choices = noms[5:48530,],  server = TRUE)
  
  expressionPlot <- reactive ({
    
    geneName <- (paste(input$genename))
    ### load the pathology scores   
    pathologyScoresHipp <- read.csv(file="SummaryPathoHipp.csv",header=T)
    
    
    sel <- c("Age","Type", geneName)
    scores<-fread("reads.csv", select = sel, stringsAsFactors = F, header=TRUE)
    colnames(scores) <- c("Age","MODELDIS","Expression")
    scores$MODELDIS <- gsub("WILD","WT",scores$MODELDIS)
    scores$MODELDIS <- gsub("TAU","TAU (MAPT P301L)",scores$MODELDIS)
    scores$MODELDIS <- gsub("TAS10","TAS10 (APP: K670N/M671L)",scores$MODELDIS)
    scores$MODELDIS <- gsub("\\<TPM\\>","TPM (PSEN1: M146V)",scores$MODELDIS)
    scores$MODELDIS <- gsub("HET_TASTPM","HET_TASTPM (heterozygous TASTPM)",scores$MODELDIS)
    scores$MODELDIS <- gsub("HO_TASTPM","HO_TASTPM (homozygous TASTPM)",scores$MODELDIS)
    
    pathologyScoresHipp$MODELDIS <- gsub("HET-TASTPM","HET_TASTPM",pathologyScoresHipp$MODELDIS)
    pathologyScoresHipp$MODELDIS <- gsub("HO-TASTPM","HO_TASTPM",pathologyScoresHipp$MODELDIS)
    
    scores$Age <- scores$Age/4 
    
    
    primaryColors <- c("brown","red","lightcoral","blue","green4","black")
    dotShapes <- c(20,15,24,4,25,1)
    
    if(!input$HET_TASTPM){ 
      scores <- scores[which(scores$MODELDIS!="HET_TASTPM (heterozygous TASTPM)"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="HET_TASTPM"),]
      primaryColors <-primaryColors[-which(primaryColors=="brown")]
      dotShapes <- dotShapes[-which(dotShapes==20)]
    }
    if(!input$HO_TASTPM){ 
      scores <- scores[which(scores$MODELDIS!="HO_TASTPM (homozygous TASTPM)"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="HO_TASTPM"),]
      primaryColors <-primaryColors[-which(primaryColors=="red")]
      dotShapes <- dotShapes[-which(dotShapes==15)]
      
    }
    
    if(!input$TAS10){ 
      scores <- scores[which(scores$MODELDIS!="TAS10 (APP: K670N/M671L)"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="TAS10"),]
      primaryColors <-primaryColors[-which(primaryColors=="lightcoral")]
      dotShapes <- dotShapes[-which(dotShapes==24)]
    }
    
    if(!input$TAU){ 
      scores <- scores[which(scores$MODELDIS!="TAU (MAPT P301L)"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="TAU"),]  
      primaryColors <-primaryColors[-which(primaryColors=="blue")]
      dotShapes <- dotShapes[-which(dotShapes==4)]
    }
    
    if(!input$TPM){ 
      scores <- scores[which(scores$MODELDIS!="TPM (PSEN1: M146V)"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="TPM"),]
      primaryColors <-primaryColors[-which(primaryColors=="green4")]
      dotShapes <- dotShapes[-which(dotShapes==25)]
    }
    
    if(!input$WT){ 
      scores <- scores[which(scores$MODELDIS!="WT"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="WT"),]
      primaryColors <-primaryColors[-which(primaryColors=="black")]
      dotShapes <- dotShapes[-which(dotShapes==1)]
    }
    
    
    par(mfrow=c(2,1))
    
    
    expPlot <- ggplot(data=scores, aes_string(x = "Age", y = "Expression", colour="MODELDIS", group = "MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=MODELDIS, shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes)+
      scale_colour_manual(values = primaryColors)+
      scale_fill_manual(values = primaryColors)+
      labs(x = "Age(Months)",y="TPM (Transcripts per Million)", colour  = "Genotype", shape="Genotype", fill="Genotype")+
      scale_x_continuous(breaks=as.numeric(levels(factor(scores$Age))))+
      #guides(fill=FALSE,shape=FALSE,colour=FALSE)+
      
      theme(legend.position="bottom")+
      ggtitle(bquote(atop(.(geneName), atop(italic(.("Hippocampus"), "")))))
    
    
    ## Pathology plots
    
    
    pathologyHipp <- ggplot(data=pathologyScoresHipp, aes_string(x = "Age", y = "expr", color="MODELDIS", group="MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes)+
      scale_colour_manual(values = primaryColors)+
      scale_fill_manual(values =primaryColors)+
      guides(fill=FALSE,shape=FALSE,colour=FALSE)+ ##remove the legend
      scale_x_continuous(breaks=c(2,4,8,18))+
      labs(x = "Age(Months)",y="Relative Plaque/Tangle density")+
      ggtitle(bquote(atop(.("Pathology"), atop(italic(.("Hippocampus"), ""))))) 
    
    rm(pathologyScoresHipp)
    
    
    #system(paste("echo",geneName,">> /var/shiny-server/log/logGenes.txt"))
    return(list("exp" = expPlot, "PATHIPP"=pathologyHipp))
    
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      "RNASeqexpression.csv"
      
    }, 
    
    
    content  = function(SPECIALFILE){
      
      geneName <- (paste(input$genename))
      sel <- c("ID","Age","Type", geneName)
      out<-fread("reads.csv", header=TRUE , sep=",", select=sel)
      colnames(out) <- c("ID", "Age (Months)","Genotype", paste(geneName, "Expression", sep= " "))
      out$Age <- out$Age/4
      ## include the pathology scores
      pathHipp <- read.csv("HippocampusPathology.csv")
      ## merge with the pathology scores 
      out <- merge(pathHipp, out, by = c("ID"))
      ## sort the table
      out <- out[order(out[,2],out[,4],out[,3],decreasing=FALSE),]
      write.table(out, file=SPECIALFILE, sep=",", quote=FALSE, row.names=F)      
    }
  ) 
  
  
  output$downloadPlot <- downloadHandler(
    
    filename = function() { paste('RNASeqexpressionPlot.jpeg')},
    
    content = function(file) {
      plots <- expressionPlot()
      jpeg(file,width = 1024, height = 884)
      multiplot(plots$exp,plots$PATHIPP,cols=1)
      dev.off()
    },
    contentType = 'jpeg'
  )     
  
  output$downloadDataTranscript <- downloadHandler(
    filename = function() {
      ### here you put the name of the file to be downloaded, in my case is "transcript.rds"
      paste("fullRNASeqexpression", ".csv", sep = "")
    },
    content = function(file) {
      file.copy(from="geneQuantification_TPM_sampleinfo.csv", to=file)
    }
  )
  
  updateSelectizeInput(session, "genename2", label = "Gene name (Click and start typing to search)", choices = noms2[8:48530,],  server = TRUE, selected=" ", options = NULL)
  
  expressionPlot2 <- reactive ({
    
    geneName2 <- paste(input$genename2)
    pathologyScoresCortex <- read.csv(file="SummaryPathoCortex.csv",header=T)
    pathologyScoresCerb <- read.csv(file="SummaryPathoCerb.csv",header=T)
    pathologyScoresHipp <- read.csv(file="SummaryPathoHipp.csv",header=T)
    
    
    sel <- c("REGION","AGE_cat","MODELDIS",geneName2)
    scores2<-fread("mouseac.csv", select = sel, stringsAsFactors = F, header=TRUE)
    colnames(scores2) <- c("REGION","Age","MODELDIS","Expression")
    scores2$MODELDIS <- gsub("WILD","WT",scores2$MODELDIS)
    scores2$MODELDIS <- gsub("TAU","TAU (MAPT P301L)",scores2$MODELDIS)
    scores2$MODELDIS <- gsub("TAS10","TAS10 (APP: K670N/M671L)",scores2$MODELDIS)
    scores2$MODELDIS <- gsub("\\<TPM\\>","TPM (PSEN1: M146V)",scores2$MODELDIS)
    scores2$MODELDIS <- gsub("HET_TASTPM","HET_TASTPM (heterozygous TASTPM)",scores2$MODELDIS)
    scores2$MODELDIS <- gsub("HO_TASTPM","HO_TASTPM (homozygous TASTPM)",scores2$MODELDIS)
    
    pathologyScoresCortex$MODELDIS <- gsub("HET-TASTPM","HET_TASTPM",pathologyScoresCortex$MODELDIS)
    pathologyScoresCortex$MODELDIS <- gsub("HO-TASTPM","HO_TASTPM",pathologyScoresCortex$MODELDIS)
    pathologyScoresCerb$MODELDIS <- gsub("HET-TASTPM","HET_TASTPM",pathologyScoresCerb$MODELDIS)
    pathologyScoresCerb$MODELDIS <- gsub("HO-TASTPM","HO_TASTPM",pathologyScoresCerb$MODELDIS)
    pathologyScoresHipp$MODELDIS <- gsub("HET-TASTPM","HET_TASTPM",pathologyScoresHipp$MODELDIS)
    pathologyScoresHipp$MODELDIS <- gsub("HO-TASTPM","HO_TASTPM",pathologyScoresHipp$MODELDIS)
    
    scores2$Age <- scores2$Age/4 
    
    exprCERB <- scores2[scores2$REGION=="Cerebellum",]
    exprHIP <- scores2[scores2$REGION=="HIP",]
    exprCortex <- scores2[scores2$REGION=="Cortex",]
    primaryColors2 <- c("brown","red","lightcoral","blue","green4","black")
    dotShapes2 <- c(20,15,24,4,25,1)
    
    if(!input$HET_TASTPM2){ 
      exprCERB <- exprCERB[which(exprCERB$MODELDIS!="HET_TASTPM (heterozygous TASTPM)"),]
      exprHIP <- exprHIP[which(exprHIP$MODELDIS!="HET_TASTPM (heterozygous TASTPM)"),]
      exprCortex <- exprCortex[which(exprCortex$MODELDIS!="HET_TASTPM (heterozygous TASTPM)"),]
      pathologyScoresCortex <- pathologyScoresCortex[which(pathologyScoresCortex$MODELDIS!="HET_TASTPM"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="HET_TASTPM"),]
      pathologyScoresCerb <- pathologyScoresCerb[which(pathologyScoresCerb$MODELDIS!="HET_TASTPM"),]
      primaryColors2 <-primaryColors2[-which(primaryColors2=="brown")]
      dotShapes2 <- dotShapes2[-which(dotShapes2==20)]
      
    }
    
    if(!input$HO_TASTPM2){ 
      exprCERB <- exprCERB[which(exprCERB$MODELDIS!="HO_TASTPM (homozygous TASTPM)"),]
      exprHIP <- exprHIP[which(exprHIP$MODELDIS!="HO_TASTPM (homozygous TASTPM)"),]
      exprCortex <- exprCortex[which(exprCortex$MODELDIS!="HO_TASTPM (homozygous TASTPM)"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="HO_TASTPM"),]
      pathologyScoresCortex <- pathologyScoresCortex[which(pathologyScoresCortex$MODELDIS!="HO_TASTPM"),]
      pathologyScoresCerb <- pathologyScoresCerb[which(pathologyScoresCerb$MODELDIS!="HO_TASTPM"),]
      primaryColors2 <-primaryColors2[-which(primaryColors2=="red")]
      dotShapes2 <- dotShapes2[-which(dotShapes2==15)]
      
    }
    if(!input$TAS102){ 
      exprCERB <- exprCERB[which(exprCERB$MODELDIS!="TAS10 (APP: K670N/M671L)"),]
      exprHIP <- exprHIP[which(exprHIP$MODELDIS!="TAS10 (APP: K670N/M671L)"),]
      exprCortex <- exprCortex[which(exprCortex$MODELDIS!="TAS10 (APP: K670N/M671L)"),]
      pathologyScoresCortex <- pathologyScoresCortex[which(pathologyScoresCortex$MODELDIS!="TAS10"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="TAS10"),]
      pathologyScoresCerb <- pathologyScoresCerb[which(pathologyScoresCerb$MODELDIS!="TAS10"),]
      primaryColors2 <-primaryColors2[-which(primaryColors2=="lightcoral")]
      dotShapes2 <- dotShapes2[-which(dotShapes2==24)]
    }
    if(!input$TAU2){ 
      exprCERB <- exprCERB[which(exprCERB$MODELDIS!="TAU (MAPT P301L)"),]
      exprHIP <- exprHIP[which(exprHIP$MODELDIS!="TAU (MAPT P301L)"),]
      exprCortex <- exprCortex[which(exprCortex$MODELDIS!="TAU (MAPT P301L)"),]
      pathologyScoresCortex <- pathologyScoresCortex[which(pathologyScoresCortex$MODELDIS!="TAU"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="TAU"),]  
      pathologyScoresCerb <- pathologyScoresCerb[which(pathologyScoresCerb$MODELDIS!="TAU"),]  
      primaryColors2 <-primaryColors2[-which(primaryColors2=="blue")]
      dotShapes2 <- dotShapes2[-which(dotShapes2==4)]
    }
    if(!input$TPM2){ 
      exprCERB <- exprCERB[which(exprCERB$MODELDIS!="TPM (PSEN1: M146V)"),]
      exprHIP <- exprHIP[which(exprHIP$MODELDIS!="TPM (PSEN1: M146V)"),]
      exprCortex <- exprCortex[which(exprCortex$MODELDIS!="TPM (PSEN1: M146V)"),]
      pathologyScoresCortex <- pathologyScoresCortex[which(pathologyScoresCortex$MODELDIS!="TPM"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="TPM"),]
      pathologyScoresCerb <- pathologyScoresCerb[which(pathologyScoresCerb$MODELDIS!="TPM"),]
      primaryColors2 <-primaryColors2[-which(primaryColors2=="green4")]
      dotShapes2 <- dotShapes2[-which(dotShapes2==25)]
    }
    if(!input$WT2){ 
      exprCERB <- exprCERB[which(exprCERB$MODELDIS!="WT"),]
      exprHIP <- exprHIP[which(exprHIP$MODELDIS!="WT"),]
      exprCortex <- exprCortex[which(exprCortex$MODELDIS!="WT"),]
      pathologyScoresCortex <- pathologyScoresCortex[which(pathologyScoresCortex$MODELDIS!="WT"),]
      pathologyScoresHipp <- pathologyScoresHipp[which(pathologyScoresHipp$MODELDIS!="WT"),]
      pathologyScoresCerb <- pathologyScoresCerb[which(pathologyScoresCerb$MODELDIS!="WT"),]
      primaryColors2 <-primaryColors2[-which(primaryColors2=="black")]
      dotShapes2 <- dotShapes2[-which(dotShapes2==1)]
    }
    
    
    
    par(mfrow=c(2,1))
    
    
    cerbPlot <- ggplot(data=exprCERB, aes(x = Age, y = Expression, color=MODELDIS, group =MODELDIS ) )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes2)+
      scale_colour_manual(values = primaryColors2)+
      scale_fill_manual(values =  primaryColors2)+
      ##opts(title = "Cerebellum")+
      labs(x = "Age(Months)",y="Log2 normalised expression", colour  = "Genotype", shape="Genotype", fill="Genotype")+
      scale_x_continuous(breaks=as.numeric(levels(factor(scores2$Age))))+
      guides(fill=FALSE,shape=FALSE,colour=FALSE)+
      ggtitle(bquote(atop(.(geneName2), atop(italic(.("Cerebellum"), ""))))) 
    
    ## plot hipp
    hipPlot <- ggplot(data=exprHIP, aes_string(x = "Age", y = "Expression", colour="MODELDIS", group = "MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=MODELDIS, shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes2)+
      scale_colour_manual(values = primaryColors2)+
      scale_fill_manual(values = primaryColors2)+
      labs(x = "Age(Months)",y="Log2 normalised expression", colour  = "Genotype", shape="Genotype", fill="Genotype")+
      scale_x_continuous(breaks=as.numeric(levels(factor(scores2$Age))))+
      guides(fill=FALSE,shape=FALSE,colour=FALSE)+
      ggtitle(bquote(atop(.(geneName2), atop(italic(.("Hippocampus"), ""))))) 
    
    
    cortexPlot <- ggplot(data=exprCortex, aes_string(x = "Age", y = "Expression", colour="MODELDIS", group = "MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=MODELDIS, shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar",  width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes2)+
      scale_colour_manual(values = primaryColors2)+
      scale_fill_manual(values = primaryColors2)+
      labs(x = "Age(Months)",y="Log2 normalised expression", colour  = "Genotype", shape="Genotype", fill="Genotype")+
      scale_x_continuous(breaks=as.numeric(levels(factor(scores2$Age))))+
      guides(fill=FALSE,shape=FALSE,colour=FALSE)+
      ggtitle(bquote(atop(.(geneName2), atop(italic(.("Cortex"), ""))))) 
    
    ## Pathology plots
    
    
    pathologyHipp <- ggplot(data=pathologyScoresHipp, aes_string(x = "Age", y = "expr", color="MODELDIS", group="MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes2)+
      scale_colour_manual(values = primaryColors2)+
      scale_fill_manual(values =primaryColors2)+
      ## guides(fill=FALSE,shape=FALSE,colour=FALSE)+ ##remove the legend
      scale_x_continuous(breaks=c(2,4,8,18))+
      ##opts(title = "Cerebellum")+
      labs(x = "Age(Months)",y="Relative Plaque/Tangle density")+
      ggtitle(bquote(atop(.("Pathology"), atop(italic(.("Hippocampus"), ""))))) 
    
    rm(pathologyScoresHipp)
    
    
    
    pathologyCortex <-  ggplot(data=pathologyScoresCortex, aes_string(x = "Age", y = "expr", color="MODELDIS", group = "MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes2)+
      scale_colour_manual(values =primaryColors2)+
      scale_fill_manual(values = primaryColors2)+
      scale_x_continuous(breaks=c(2,4,8,18))+
      labs(x = "Age(Months)",y="Relative Plaque/Tangle density")+
      ##guides(fill=FALSE,shape=FALSE,colour=FALSE)+ ## remove the legend
      ggtitle(bquote(atop(.("Pathology"), atop(italic(.("Cortex"), ""))))) 
    
    rm(pathologyScoresCortex)
    
    
    
    pathologyCerb <-  ggplot(data=pathologyScoresCerb, aes_string(x = "Age", y = "expr", color="MODELDIS", group = "MODELDIS") )+  # Modelis becomes a classifying factor
      stat_summary(fun.y="mean", geom="line", size = 1)+
      ##geom_point(aes(color=c(4:10), shape=MODELDIS))+
      stat_summary(aes(color=MODELDIS, shape=MODELDIS,fill=MODELDIS),fun.y="mean", geom="point", size = 5)+
      ##stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.4, mult = 1)+
      scale_shape_manual(values=dotShapes2)+
      scale_colour_manual(values =primaryColors2)+
      scale_fill_manual(values = primaryColors2)+
      annotate("text", x = 0, y = 0, size=8, label = "No Cerebellum Pathology") + 
      labs(x = "Age(Months)",y="Relative Plaque/Tangle density")+
      ggtitle(bquote(atop(.("Pathology"), atop(italic(.("Cerebellum"), ""))))) 
    
    rm(pathologyScoresCerb)
    
    return(list("CERB" = cerbPlot, "HIPP" = hipPlot,"CORTEX"=cortexPlot,"PATHIPP"=pathologyHipp,"PATHCOR"=pathologyCortex,"PATHCER"=pathologyCerb))
    
  })
  
  
  output$downloadData2 <- downloadHandler(
    
    filename = function() {
      "Microarrayexpression.csv"
    }, 
    
    
    content  = function(SPECIALFILE){
      geneName2 <- (paste(input$genename2))
      sel <- c("Sample","REGION","AGE_cat","MODELDIS",geneName2)
      out2<-fread("mouseac.csv", header=TRUE , sep=",", select=sel)
      colnames(out2) <- c("SampleID","REGION","Age (Months)","Genotype", paste(geneName2, "Expression", sep= " "))
      out2$Age <- out2$Age/4
      ## include the pathology scores
      pathTMP <- read.csv("pathology.csv")
      ## merge with the pathology scores 
      out2 <- merge(out2,pathTMP, by = c("SampleID","REGION"))
      ## sort the table
      out2 <- out2[order(out2[,2],out2[,4],out2[,3],decreasing=FALSE),]
      write.table(out2, file=SPECIALFILE, sep=",", quote=FALSE, row.names=F)      
    }
  )  
  
  
  output$downloadPlot2 <- downloadHandler(
    
    filename = function() { paste('MicroarrayexpressionPlot.jpeg')},
    
    content = function(file) {
      plots <- expressionPlot2()
      jpeg(file,width = 1024, height = 884)
      multiplot(plots$HIPP,plots$CORTEX,plots$CERB,plots$PATHIPP,plots$PATHCOR,plots$PATHCER,cols=2)
      dev.off()
    },
    contentType = 'jpeg'
  )     
  
  output$downloadDataTranscript2 <- downloadHandler(
    filename = function() {
      ### here you put the name of the file to be downloaded, in my case is "transcript.rds"
      paste("fullMicroarrayexpression", ".csv", sep = "")
    },
    content = function(file) {
      file.copy(from="mouseac.csv", to=file)
    }
  )
  
  output$plot <- renderPlot({
    if(input$action3 && input$options=="APP and MAPT knock-ins - RNASeq"){
      isolate({
        plots <- expressionPlot3()
        print(multiplot(plots$exp,plots$PATHIPP,cols=1))
      })
    }
    
    else if(input$action && input$options=="APP and MAPT transgenics - RNASeq"){
      isolate({
        plots <- expressionPlot()
        print(multiplot(plots$exp,plots$PATHIPP,cols=1))
      })
    }
    
    else if(input$action2 && input$options=="APP and MAPT transgenics - Microarray"){
      isolate({
        plots <- expressionPlot2()
        print(multiplot(plots$HIPP,plots$CORTEX,plots$CERB,plots$PATHIPP,plots$PATHCOR,plots$PATHCER,cols=2))
      })
    }
    else {  ## empty landing page
      
      plot.new()
      plot.window(xlim=c(0,1),ylim=c(0,1)) 
      text(0.5,0.9, "Please enter a gene on the left", cex=2)
    }
  })
}   
shinyApp(ui= ui, server = server)
