#LOAD BIBLIS
  library("effsize")
  library("httpuv")
  library("plotly")

  
#CONFIGURANDO ÁREA DE TRABALHO E INCLUSÃO DE BIBLIOTECAS -------------------------------------------------------------------------------------
  #SIZE: 3.794.312 
  #setwd("~/GitHub/AnaliseDosProjetosScratch_Empirica/Atividade")
  setwd("C:/Users/renan/Desktop/AnaliseDosProjetosScratch_Empirica/Atividade")

  #IMPORTANTO BASE DE DADOS --------------------------------------------------------------------------------------------------------------------
  table_csv <- read.delim("Full_Data.csv")
  
#FUNÇÕES AUXILIARES --------------------------------------------------------------------------------------------------------------------------
  cohens_d <- function(x, y) {
    lx <- length(x)- 1
    ly <- length(y)- 1
    md  <- abs(mean(x) - mean(y))        
    csd <- lx * var(x) + ly * var(y)
    csd <- csd/(lx + ly)
    csd <- sqrt(csd)                     
    
    cd  <- md/csd                        
  }
  
#TRATANDO OS DADOS --------------------------------------------------------------------------------------------------------------------------
  table_csv <- table_csv[table_csv$username!='unknown',]
  
#CALCULANDO DA MÉDIA DOS PROJETOS -----------------------------------------------------------------------------------------------------------
  #EM VIEWS
    mediana_Views = median(table_csv$totalViews, na.rm = TRUE)
  #EM LOVES
    mediana_Love = median(table_csv$totalLoves, na.rm = TRUE)
  #EM REMIX
    mediana_Remix = median(table_csv$totalRemixes, na.rm = TRUE)

#SELEÇÃO DOS PROJETOS ABAIXO E ACIMA DA MEDIANA ---------------------------------------------------------------------------------------------
  #SELEÇÃO EM TOTAL VIEWS
    butt_Views <- table_csv[which(table_csv$totalViews <= mediana_Views),]
    top_Views <- table_csv[which(table_csv$totalViews > mediana_Views),]
    
  #SELEÇÃO EM TOTAL LOVES
    butt_Loves <- table_csv[which(table_csv$totalLoves <= mediana_Love),]
    top_Loves <- table_csv[which(table_csv$totalLoves > mediana_Love),]
  
  #SELEÇÃO EM TOTAL VIEWS E LOVES
    butt_ViewsLoves <- table_csv[which((table_csv$totalViews <= mediana_Views) & (table_csv$totalLoves <= mediana_Love)),]
    top_ViewsLoves <- table_csv[which((table_csv$totalViews > mediana_Views) & (table_csv$totalLoves > mediana_Love)),]
  
  #SELEÇÃO EM REMIX
    butt_Remix <- table_csv[which(table_csv$totalRemixes <= mediana_Remix),]
    top_Remix <- table_csv[which(table_csv$totalRemixes > mediana_Remix),]
  
  #SELEÇÃO EM LOVES E REMIX  
    butt_LovesRemix <- table_csv[which((table_csv$totalLoves <= mediana_Love) & (table_csv$totalRemixes <= mediana_Remix)),]
    top_LovesRemix <-  table_csv[which((table_csv$totalLoves > mediana_Love) & (table_csv$totalRemixes > mediana_Remix)),]
    
  #SELEÇÃO EM VIEWS, LOVES E REMIX
    butt_ViewsLovesRemix <- table_csv[which((table_csv$totalViews <= mediana_Views) & (table_csv$totalLoves <= mediana_Love) & (table_csv$totalRemixes <= mediana_Remix)),]
    top_ViewsLovesRemix <- table_csv[which((table_csv$totalViews > mediana_Views) & (table_csv$totalLoves > mediana_Love) & (table_csv$totalRemixes > mediana_Remix)),]
    
    
#ANALISES SOBRE OS DADOS SELECIONADOS EM VIEWS -----------------------------------------------------------------------------------------------
  #WILCOX PARA VERIFICAR SE EXISTE RELAÇÃO ENTRE AS AMOSTRAS
    wilcox.test(butt_Views$Abstraction, top_Views$Abstraction)                  #p-value < 2.2e-16             
    wilcox.test(butt_Views$Parallelism, top_Views$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_Views$Logic, top_Views$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_Views$Synchronization, top_Views$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_Views$FlowControl, top_Views$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_Views$UserInteractivity, top_Views$UserInteractivity)      #p-value < 2.2e-16
    wilcox.test(butt_Views$DataRepresentation, top_Views$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_Views$Mastery, top_Views$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_Views$Clones, top_Views$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_Views$CustomBlocks, top_Views$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_Views$InstancesSprites, top_Views$InstancesSprites)        #p-value < 2.2e-16
    wilcox.test(butt_Views$scriptRank, top_Views$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_Views$totalBlocks, top_Views$totalBlocks)                  #p-value < 2.2e-16
    
  #BOXPLOT PARA GERAR A VISUALIZAÇÃO DA RELAÇÃO
    boxplot(butt_Views$Abstraction, top_Views$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Parallelism, top_Views$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Logic, top_Views$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Synchronization, top_Views$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_Views$FlowControl, top_Views$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_Views$UserInteractivity, top_Views$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_Views$DataRepresentation, top_Views$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Mastery, top_Views$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_Views$CustomBlocks, top_Views$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_Views$InstancesSprites, top_Views$InstancesSprites, outline = FALSE, border = TRUE)
    boxplot(butt_Views$scriptRank, top_Views$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_Views$totalBlocks, top_Views$totalBlocks, outline = FALSE, border = TRUE)  
  
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
      res <- cohens_d(butt_Views$Abstraction, top_Views$Abstraction)                #0.1806
      res <- cohens_d(top_Views$Parallelism,butt_Views$Parallelism )                #0.2875
      res <- cohens_d(butt_Views$Logic, top_Views$Logic)                            #0.1919
      res <- cohens_d(butt_Views$Synchronization, top_Views$Synchronization)        #0.2265
      res <- cohens_d(butt_Views$FlowControl, top_Views$FlowControl)                #0.1994
      res <- cohens_d(butt_Views$UserInteractivity, top_Views$UserInteractivity)    #0.0057
      res <- cohens_d(butt_Views$DataRepresentation, top_Views$DataRepresentation)  #0.1637
      res <- cohens_d(butt_Views$Mastery, top_Views$Mastery)                        #0.2681
      res <- cohens_d(butt_Views$CustomBlocks, top_Views$CustomBlocks)              #0.0290
      res <- cohens_d(butt_Views$InstancesSprites, top_Views$InstancesSprites)      #0.1425
      res <- cohens_d(butt_Views$scriptRank, top_Views$scriptRank)                  #0.0378
      res <- cohens_d(butt_Views$totalBlocks, top_Views$totalBlocks)                #0.0003
      
  #SUMARIZANDO COLUNAS RELEVANTES CONFORME COHENS
    summary(butt_Views$Parallelism); summary(top_Views$Parallelism);
    summary(butt_Views$Mastery); summary(top_Views$Mastery);

    
#ANALISES SOBRE OS DADOS SELECIONADOS EM LOVES -----------------------------------------------------------------------------------------------
  #WILCOX PARA VERIFICAR SE EXISTE RELAÇÃO ENTRE AS AMOSTRAS
    wilcox.test(butt_Loves$Abstraction, top_Loves$Abstraction)                  #p-value < 2.2e-16             
    wilcox.test(butt_Loves$Parallelism, top_Loves$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_Loves$Logic, top_Loves$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_Loves$Synchronization, top_Loves$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_Loves$FlowControl, top_Loves$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_Loves$UserInteractivity, top_Loves$UserInteractivity)      #p-value < 2.2e-16
    wilcox.test(butt_Loves$DataRepresentation, top_Loves$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_Loves$Mastery, top_Loves$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_Loves$Clones, top_Loves$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_Loves$CustomBlocks, top_Loves$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_Loves$InstancesSprites, top_Loves$InstancesSprites)        #p-value < 2.2e-16
    wilcox.test(butt_Loves$scriptRank, top_Loves$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_Loves$totalBlocks, top_Loves$totalBlocks)                  #p-value < 2.2e-16
    
  #BOXPLOT PARA GERAR A VISUALIZAÇÃO DA RELAÇÃO
    boxplot(butt_Loves$Abstraction, top_Loves$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Parallelism, top_Loves$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Logic, top_Loves$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Synchronization, top_Loves$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$FlowControl, top_Loves$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$UserInteractivity, top_Loves$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$DataRepresentation, top_Loves$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Mastery, top_Loves$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$CustomBlocks, top_Loves$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_Loves$InstancesSprites, top_Loves$InstancesSprites, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$scriptRank, top_Loves$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$totalBlocks, top_Loves$totalBlocks, outline = FALSE, border = TRUE)  
    
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
      res <- cohens_d(butt_Loves$Abstraction, top_Loves$Abstraction)                #0.1524
      res <- cohens_d(butt_Loves$Parallelism, top_Loves$Parallelism)                #0.1871
      res <- cohens_d(butt_Loves$Logic, top_Loves$Logic)                            #0.0826
      res <- cohens_d(butt_Loves$Synchronization, top_Loves$Synchronization)        #0.1305
      res <- cohens_d(butt_Loves$FlowControl, top_Loves$FlowControl)                #0.1228
      res <- cohens_d(butt_Loves$UserInteractivity, top_Loves$UserInteractivity)    #0.0007
      res <- cohens_d(butt_Loves$DataRepresentation, top_Loves$DataRepresentation)  #0.0668
      res <- cohens_d(butt_Loves$Mastery, top_Loves$Mastery)                        #0.1573
      res <- cohens_d(butt_Loves$CustomBlocks, top_Loves$CustomBlocks)              #0.0452
      res <- cohens_d(butt_Loves$InstancesSprites, top_Loves$InstancesSprites)      #0.1321
      res <- cohens_d(butt_Loves$scriptRank, top_Loves$scriptRank)                  #0.1209
      res <- cohens_d(butt_Loves$totalBlocks, top_Loves$totalBlocks)                #0.0275
    
  #SUMARIZANDO COLUNAS RELEVANTES CONFORME COHENS
    summary(butt_Loves$Abstraction); summary(top_Loves$Abstraction);
    summary(butt_Loves$Parallelism); summary(top_Loves$Parallelism);
    summary(butt_Loves$Mastery); summary(top_Loves$Mastery);
  
    
#ANALISES SOBRE OS DADOS SELECIONADOS EM VIEWS E LOVES ---------------------------------------------------------------------------------------
  #WILCOX PARA VERIFICAR SE EXISTE RELAÇÃO ENTRE AS AMOSTRAS
    wilcox.test(butt_ViewsLoves$Abstraction, top_ViewsLoves$Abstraction)                  #p-value < 2.2e-16             
    wilcox.test(butt_ViewsLoves$Parallelism, top_ViewsLoves$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$Logic, top_ViewsLoves$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$Synchronization, top_ViewsLoves$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$FlowControl, top_ViewsLoves$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$UserInteractivity, top_ViewsLoves$UserInteractivity)      #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$DataRepresentation, top_ViewsLoves$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$Mastery, top_ViewsLoves$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$Clones, top_ViewsLoves$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$CustomBlocks, top_ViewsLoves$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$InstancesSprites, top_ViewsLoves$InstancesSprites)        #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$scriptRank, top_ViewsLoves$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_ViewsLoves$totalBlocks, top_ViewsLoves$totalBlocks)                  #p-value < 2.2e-16
    
  #BOXPLOT PARA GERAR A VISUALIZAÇÃO DA RELAÇÃO
    boxplot(butt_ViewsLoves$Abstraction, top_ViewsLoves$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$Parallelism, top_ViewsLoves$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$Logic, top_ViewsLoves$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$Synchronization, top_ViewsLoves$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$FlowControl, top_ViewsLoves$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$UserInteractivity, top_ViewsLoves$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$DataRepresentation, top_ViewsLoves$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$Mastery, top_ViewsLoves$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$CustomBlocks, top_ViewsLoves$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_ViewsLoves$InstancesSprites, top_ViewsLoves$InstancesSprites, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$scriptRank, top_ViewsLoves$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLoves$totalBlocks, top_ViewsLoves$totalBlocks, outline = FALSE, border = TRUE)  
    
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
      res <- cohens_d(butt_ViewsLoves$Abstraction, top_ViewsLoves$Abstraction)                #0.2421
      res <- cohens_d(butt_ViewsLoves$Parallelism, top_ViewsLoves$Parallelism)                #0.3214
      res <- cohens_d(butt_ViewsLoves$Logic, top_ViewsLoves$Logic)                            #0.1880
      res <- cohens_d(butt_ViewsLoves$Synchronization, top_ViewsLoves$Synchronization)        #0.2417
      res <- cohens_d(butt_ViewsLoves$FlowControl, top_ViewsLoves$FlowControl)                #0.2314
      res <- cohens_d(butt_ViewsLoves$UserInteractivity, top_ViewsLoves$UserInteractivity)    #0.0015
      res <- cohens_d(butt_ViewsLoves$DataRepresentation, top_ViewsLoves$DataRepresentation)  #0.1523
      res <- cohens_d(butt_ViewsLoves$Mastery, top_ViewsLoves$Mastery)                        #0.2933
      res <- cohens_d(butt_ViewsLoves$CustomBlocks, top_ViewsLoves$CustomBlocks)              #0.0435
      res <- cohens_d(butt_ViewsLoves$InstancesSprites, top_ViewsLoves$InstancesSprites)      #0.2002
      res <- cohens_d(butt_ViewsLoves$scriptRank, top_ViewsLoves$scriptRank)                  #0.1276
      res <- cohens_d(butt_ViewsLoves$totalBlocks, top_ViewsLoves$totalBlocks)                #0.0201
      
  #SUMARIZANDO COLUNAS RELEVANTES CONFORME COHENS
    summary(butt_ViewsLoves$Abstraction); summary(top_ViewsLoves$Abstraction);
    summary(butt_ViewsLoves$Parallelism); summary(top_ViewsLoves$Parallelism);
    summary(butt_ViewsLoves$Synchronization); summary(top_ViewsLoves$Synchronization);
    summary(butt_ViewsLoves$FlowControl); summary(top_ViewsLoves$FlowControl);
    summary(butt_ViewsLoves$Mastery); summary(top_ViewsLoves$Mastery);
    summary(butt_ViewsLoves$InstancesSprites); summary(top_ViewsLoves$InstancesSprites);
    
    
    
#ANALISES SOBRE OS DADOS SELECIONADOS EM REMIX -----------------------------------------------------------------------------------------------
  #WILCOX PARA VERIFICAR SE EXISTE RELAÇÃO ENTRE AS AMOSTRAS
    wilcox.test(butt_Remix$Abstraction, top_Remix$Abstraction)                  #p-value < 2.2e-16             
    wilcox.test(butt_Remix$Parallelism, top_Remix$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_Remix$Logic, top_Remix$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_Remix$Synchronization, top_Remix$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_Remix$FlowControl, top_Remix$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_Remix$UserInteractivity, top_Remix$UserInteractivity)      #p-value < 2.2e-16
    wilcox.test(butt_Remix$DataRepresentation, top_Remix$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_Remix$Mastery, top_Remix$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_Remix$Clones, top_Remix$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_Remix$CustomBlocks, top_Remix$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_Remix$InstancesSprites, top_Remix$InstancesSprites)        #p-value < 2.2e-16
    wilcox.test(butt_Remix$scriptRank, top_Remix$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_Remix$totalBlocks, top_Remix$totalBlocks)                  #p-value < 2.2e-16
    
  #BOXPLOT PARA GERAR A VISUALIZAÇÃO DA RELAÇÃO
    boxplot(butt_Remix$Abstraction, top_Remix$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$Parallelism, top_Remix$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$Logic, top_Remix$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$Synchronization, top_Remix$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$FlowControl, top_Remix$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$UserInteractivity, top_Remix$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$DataRepresentation, top_Remix$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$Mastery, top_Remix$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$CustomBlocks, top_Remix$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_Remix$InstancesSprites, top_Remix$InstancesSprites, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$scriptRank, top_Remix$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_Remix$totalBlocks, top_Remix$totalBlocks, outline = FALSE, border = TRUE)  
    
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
      res <- cohens_d(butt_Remix$Abstraction, top_Remix$Abstraction)                #0.4064
      res <- cohens_d(butt_Remix$Parallelism, top_Remix$Parallelism)                #0.1523
      res <- cohens_d(butt_Remix$Logic, top_Remix$Logic)                            #0.1326
      res <- cohens_d(butt_Remix$Synchronization, top_Remix$Synchronization)        #0.1195
      res <- cohens_d(butt_Remix$FlowControl, top_Remix$FlowControl)                #0.1959
      res <- cohens_d(butt_Remix$UserInteractivity, top_Remix$UserInteractivity)    #0.6100
      res <- cohens_d(butt_Remix$DataRepresentation, top_Remix$DataRepresentation)  #0.3273
      res <- cohens_d(butt_Remix$Mastery, top_Remix$Mastery)                        #0.2143
      res <- cohens_d(butt_Remix$CustomBlocks, top_Remix$CustomBlocks)              #0.3090
      res <- cohens_d(butt_Remix$InstancesSprites, top_Remix$InstancesSprites)      #0.3083
      res <- cohens_d(butt_Remix$scriptRank, top_Remix$scriptRank)                  #0.1372
      res <- cohens_d(butt_Remix$totalBlocks, top_Remix$totalBlocks)                #0.0110
    
  #SUMARIZANDO COLUNAS RELEVANTES CONFORME COHENS
    summary(butt_Remix$Abstraction); summary(top_Remix$Abstraction);
    summary(butt_Remix$UserInteractivity); summary(top_Remix$UserInteractivity);
    summary(butt_Remix$DataRepresentation); summary(top_Remix$DataRepresentation);
    summary(butt_Remix$Mastery); summary(top_Remix$Mastery);
    summary(butt_Remix$CustomBlocks); summary(top_Remix$CustomBlocks);
    summary(butt_Remix$InstancesSprites); summary(top_Remix$InstancesSprites);
    
#ANALISES SOBRE OS DADOS SELECIONADOS EM LOVES E REMIX ---------------------------------------------------------------------------------------
  #WILCOX PARA VERIFICAR SE EXISTE RELAÇÃO ENTRE AS AMOSTRAS
    wilcox.test(butt_LovesRemix$Abstraction, top_LovesRemix$Abstraction)             #p-value < 2.2e-16             
    wilcox.test(butt_LovesRemix$Parallelism, top_LovesRemix$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$Logic, top_LovesRemix$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$Synchronization, top_LovesRemix$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$FlowControl, top_LovesRemix$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$UserInteractivity, top_LovesRemix$UserInteractivity)      #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$DataRepresentation, top_LovesRemix$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$Mastery, top_LovesRemix$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$Clones, top_LovesRemix$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$CustomBlocks, top_LovesRemix$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$InstancesSprites, top_LovesRemix$InstancesSprites)        #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$scriptRank, top_LovesRemix$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_LovesRemix$totalBlocks, top_LovesRemix$totalBlocks)                  #p-value < 2.2e-16
    
   #BOXPLOT PARA GERAR A VISUALIZAÇÃO DA RELAÇÃO
    boxplot(butt_LovesRemix$Abstraction, top_LovesRemix$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$Parallelism, top_LovesRemix$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$Logic, top_LovesRemix$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$Synchronization, top_LovesRemix$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$FlowControl, top_LovesRemix$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$UserInteractivity, top_LovesRemix$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$DataRepresentation, top_LovesRemix$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$Mastery, top_LovesRemix$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$CustomBlocks, top_LovesRemix$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_LovesRemix$InstancesSprites, top_LovesRemix$InstancesSprites, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$scriptRank, top_LovesRemix$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_LovesRemix$totalBlocks, top_LovesRemix$totalBlocks, outline = FALSE, border = TRUE)  
    
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
      res <- cohens_d(butt_LovesRemix$Abstraction, top_LovesRemix$Abstraction)                #0.4241
      res <- cohens_d(butt_LovesRemix$Parallelism, top_LovesRemix$Parallelism)                #0.2224
      res <- cohens_d(butt_LovesRemix$Logic, top_LovesRemix$Logic)                            #0.1379
      res <- cohens_d(butt_LovesRemix$Synchronization, top_LovesRemix$Synchronization)        #0.1683
      res <- cohens_d(butt_LovesRemix$FlowControl, top_LovesRemix$FlowControl)                #0.2736
      res <- cohens_d(butt_LovesRemix$UserInteractivity, top_LovesRemix$UserInteractivity)    #0.2155
      res <- cohens_d(butt_LovesRemix$DataRepresentation, top_LovesRemix$DataRepresentation)  #0.2377
      res <- cohens_d(butt_LovesRemix$Mastery, top_LovesRemix$Mastery)                        #0.2800
      res <- cohens_d(butt_LovesRemix$CustomBlocks, top_LovesRemix$CustomBlocks)              #0.2679
      res <- cohens_d(butt_LovesRemix$InstancesSprites, top_LovesRemix$InstancesSprites)      #0.3698
      res <- cohens_d(butt_LovesRemix$scriptRank, top_LovesRemix$scriptRank)                  #0.0201
      res <- cohens_d(butt_LovesRemix$totalBlocks, top_LovesRemix$totalBlocks)                #0.0514
      
  #SUMARIZANDO COLUNAS RELEVANTES CONFORME COHENS
    summary(butt_LovesRemix$Abstraction); summary(top_LovesRemix$Abstraction);
    summary(butt_LovesRemix$Parallelism); summary(top_LovesRemix$Parallelism);
    summary(butt_LovesRemix$CustomBlocks); summary(top_LovesRemix$CustomBlocks);
    summary(butt_LovesRemix$FlowControl); summary(top_LovesRemix$FlowControl);
    summary(butt_LovesRemix$UserInteractivity); summary(top_LovesRemix$UserInteractivity);
    summary(butt_LovesRemix$DataRepresentation); summary(top_LovesRemix$DataRepresentation);
    summary(butt_LovesRemix$Mastery); summary(top_LovesRemix$Mastery);
    summary(butt_LovesRemix$CustomBlocks); summary(top_LovesRemix$CustomBlocks);
    summary(butt_LovesRemix$InstancesSprites); summary(top_LovesRemix$InstancesSprites);
    
#ANALISES SOBRE OS DADOS SELECIONADOS EM VIEWS, LOVES E REMIX ---------------------------------------------------------------------------------------
  #WILCOX PARA VERIFICAR SE EXISTE RELAÇÃO ENTRE AS AMOSTRAS
    wilcox.test(butt_ViewsLovesRemix$Abstraction, top_ViewsLovesRemix$Abstraction)                  #p-value < 2.2e-16             
    wilcox.test(butt_ViewsLovesRemix$Parallelism, top_ViewsLovesRemix$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$Logic, top_ViewsLovesRemix$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$Synchronization, top_ViewsLovesRemix$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$FlowControl, top_ViewsLovesRemix$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$UserInteractivity, top_ViewsLovesRemix$UserInteractivity)      #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$DataRepresentation, top_ViewsLovesRemix$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$Mastery, top_ViewsLovesRemix$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$Clones, top_ViewsLovesRemix$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$CustomBlocks, top_ViewsLovesRemix$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$InstancesSprites, top_ViewsLovesRemix$InstancesSprites)        #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$scriptRank, top_ViewsLovesRemix$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_ViewsLovesRemix$totalBlocks, top_ViewsLovesRemix$totalBlocks)                  #p-value < 2.2e-16
    
  #BOXPLOT PARA GERAR A VISUALIZAÇÃO DA RELAÇÃO
    boxplot(butt_ViewsLovesRemix$Abstraction, top_ViewsLovesRemix$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$Parallelism, top_ViewsLovesRemix$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$Logic, top_ViewsLovesRemix$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$Synchronization, top_ViewsLovesRemix$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$FlowControl, top_ViewsLovesRemix$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$UserInteractivity, top_ViewsLovesRemix$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$DataRepresentation, top_ViewsLovesRemix$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$Mastery, top_ViewsLovesRemix$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$CustomBlocks, top_ViewsLovesRemix$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_ViewsLovesRemix$InstancesSprites, top_ViewsLovesRemix$InstancesSprites, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$scriptRank, top_ViewsLovesRemix$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_ViewsLovesRemix$totalBlocks, top_ViewsLovesRemix$totalBlocks, outline = FALSE, border = TRUE)  
    
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
      res <- cohens_d(butt_ViewsLovesRemix$Abstraction, top_ViewsLovesRemix$Abstraction)                #0.4877
      res <- cohens_d(butt_ViewsLovesRemix$Parallelism, top_ViewsLovesRemix$Parallelism)                #0.3062
      res <- cohens_d(butt_ViewsLovesRemix$Logic, top_ViewsLovesRemix$Logic)                            #0.2029
      res <- cohens_d(butt_ViewsLovesRemix$Synchronization, top_ViewsLovesRemix$Synchronization)        #0.2392
      res <- cohens_d(butt_ViewsLovesRemix$FlowControl, top_ViewsLovesRemix$FlowControl)                #0.3295
      res <- cohens_d(butt_ViewsLovesRemix$UserInteractivity, top_ViewsLovesRemix$UserInteractivity)    #0.1878
      res <- cohens_d(butt_ViewsLovesRemix$DataRepresentation, top_ViewsLovesRemix$DataRepresentation)  #0.2926
      res <- cohens_d(butt_ViewsLovesRemix$Mastery, top_ViewsLovesRemix$Mastery)                        #0.3654
      res <- cohens_d(butt_ViewsLovesRemix$CustomBlocks, top_ViewsLovesRemix$CustomBlocks)              #0.2828
      res <- cohens_d(butt_ViewsLovesRemix$InstancesSprites, top_ViewsLovesRemix$InstancesSprites)      #0.4325
      res <- cohens_d(butt_ViewsLovesRemix$scriptRank, top_ViewsLovesRemix$scriptRank)                  #0.0213
      res <- cohens_d(butt_ViewsLovesRemix$totalBlocks, top_ViewsLovesRemix$totalBlocks)                #0.0467
    
  #SUMARIZANDO COLUNAS RELEVANTES CONFORME COHENS
    summary(butt_ViewsLovesRemix$Abstraction); summary(top_ViewsLovesRemix$Abstraction);
    summary(butt_ViewsLovesRemix$Parallelism); summary(top_ViewsLovesRemix$Parallelism);
    summary(butt_ViewsLovesRemix$Logic); summary(top_ViewsLovesRemix$Logic);
    summary(butt_ViewsLovesRemix$Synchronization); summary(top_ViewsLovesRemix$Synchronization);
    summary(butt_ViewsLovesRemix$FlowControl); summary(top_ViewsLovesRemix$FlowControl);
    summary(butt_ViewsLovesRemix$DataRepresentation); summary(top_ViewsLovesRemix$DataRepresentation);
    summary(butt_ViewsLovesRemix$Mastery); summary(top_ViewsLovesRemix$Mastery);
    summary(butt_ViewsLovesRemix$CustomBlocks); summary(top_ViewsLovesRemix$CustomBlocks);
    summary(butt_ViewsLovesRemix$InstancesSprites); summary(top_ViewsLovesRemix$InstancesSprites);
    
    
#VISUALIZAÇÕES UTILIZANDO GRAFICO DE COORDENADAS PARALELAS COM O PLOTLY
    qtd_Samples = 1000
    
    #SELECIONANDO AMOSTRAS
      sample_ButtViews <- butt_Views[sample(nrow(butt_Views), qtd_Samples), ]
      sample_TopViews <- top_Views[sample(nrow(top_Views), qtd_Samples), ]
      concat_Views <- rbind(sample_ButtViews, sample_TopViews)
      
      sample_ButtLoves <- butt_Loves[sample(nrow(butt_Loves), qtd_Samples), ]
      sample_TopVLoves <- top_Loves[sample(nrow(top_Loves), qtd_Samples), ]
      concat_Loves <- rbind(sample_ButtLoves, sample_TopVLoves)
      
      sample_ButtViewsLoves <- butt_ViewsLoves[sample(nrow(butt_ViewsLoves), qtd_Samples), ]
      sample_TopViewsLoves <- top_ViewsLoves[sample(nrow(top_ViewsLoves), qtd_Samples), ]
      concat_ViewsLoves <- rbind(sample_ButtViewsLoves, sample_TopViewsLoves)
      
      sample_ButtRemix <- butt_Remix[sample(nrow(butt_Remix), qtd_Samples), ]
      sample_TopRemix <- top_Remix[sample(nrow(top_Remix), qtd_Samples), ]
      concat_Remix <- rbind(sample_ButtRemix, sample_TopRemix)
      
      sample_ButtLovesRemixes  <- butt_LovesRemix[sample(nrow(butt_LovesRemix), qtd_Samples), ]
      sample_TopLovesRemixes <- top_LovesRemix[sample(nrow(top_LovesRemix), qtd_Samples), ]
      concat_LovesRemixes <- rbind(sample_ButtLovesRemixes, sample_TopLovesRemixes)
      
      sample_ButtViewsLovesRemixes <- butt_ViewsLovesRemix[sample(nrow(butt_ViewsLovesRemix), qtd_Samples), ]
      sample_TopViewsLovesRemixes <- top_ViewsLovesRemix[sample(nrow(top_ViewsLovesRemix), qtd_Samples), ]
      concat_ViewsLovesRemixes <- rbind(sample_ButtViewsLovesRemixes, sample_TopViewsLovesRemixes)
      
    #GERANDO PARA VIEWS --------------------------------------------------------------------------------------------
      coordP_Views <- plot_ly(type = 'parcoords', 
        line = list(color = 'red'),
        
        dimensions = list(
         list(range = c(~min(concat_Views$totalViews),~max(concat_Views$totalViews)),
              label = 'Total Views', values = concat_Views[,c(3)]),
         
         list(range = c(~min(concat_Views$Abstraction),~max(concat_Views$Abstraction)),
              label = 'Abstraction', values = concat_Views[,c(8)]),
         
         list(range = c(~min(concat_Views$Parallelism),~max(concat_Views$Parallelism)),
              label = 'Parallelism', values = concat_Views[,c(9)]),
         
         list(range = c(~min(concat_Views$Logic),~max(concat_Views$Logic)),
              label = 'Logic', values = concat_Views[,c(10)]),
         
         list(range = c(~min(concat_Views$Synchronization),~max(concat_Views$Synchronization)),
              label = 'Syncronization', values = concat_Views[,c(11)]),
         
         list(range = c(~min(concat_Views$UserInteractivity),~max(concat_Views$UserInteractivity)),
              label = 'User Iteractivty', values = concat_Views[,c(13)]),
         
         list(range = c(~min(concat_Views$Mastery),~max(concat_Views$Mastery)),
              label = 'Mastery', values = concat_Views[,c(15)]),
         
         list(range = c(~min(concat_Views$totalBlocks),~max(concat_Views$totalBlocks)),
              label = 'Total Blocks', values = concat_Views[,c(20)])
        )
      )
    
    #GERANDO PARA LOVES --------------------------------------------------------------------------------------------
      coordP_Loves <- plot_ly(type = 'parcoords', 
        line = list(color = 'red'),
        
        dimensions = list(
          list(range = c(~min(concat_Loves$totalLoves),~max(concat_Loves$totalLoves)),
               label = 'Total Loves', values = concat_Loves[,c(6)]),
          
          list(range = c(~min(concat_Loves$Abstraction),~max(concat_Loves$Abstraction)),
               label = 'Abstraction', values = concat_Loves[,c(8)]),
          
          list(range = c(~min(concat_Loves$Parallelism),~max(concat_Loves$Parallelism)),
               label = 'Parallelism', values = concat_Loves[,c(9)]),
          
          list(range = c(~min(concat_Loves$Logic),~max(concat_Loves$Logic)),
               label = 'Logic', values = concat_Loves[,c(10)]),
          
          list(range = c(~min(concat_Loves$Synchronization),~max(concat_Loves$Synchronization)),
               label = 'Syncronization', values = concat_Loves[,c(11)]),
          
          list(range = c(~min(concat_Loves$UserInteractivity),~max(concat_Loves$UserInteractivity)),
               label = 'User Iteractivty', values = concat_Loves[,c(13)]),
          
          list(range = c(~min(concat_Loves$Mastery),~max(concat_Loves$Mastery)),
               label = 'Mastery', values = concat_Loves[,c(15)]),
          
          list(range = c(~min(concat_Loves$totalBlocks),~max(concat_Loves$totalBlocks)),
               label = 'Total Blocks', values = concat_Loves[,c(20)])
        )
    )
    
    #GERANDO PARA VIEWS E LOVES ------------------------------------------------------------------------------------
      coordP_ViewsLoves <- plot_ly(type = 'parcoords', 
        line = list(color = 'red'),
        
        dimensions = list(
          list(range = c(~min(concat_ViewsLoves$totalViews),~max(concat_ViewsLoves$totalViews)),
               label = 'Total Views', values = concat_ViewsLoves[,c(3)]),
          
          list(range = c(~min(concat_ViewsLoves$totalLoves),~max(concat_ViewsLoves$totalLoves)),
               label = 'Total Loves', values = concat_ViewsLoves[,c(6)]),
          
          list(range = c(~min(concat_ViewsLoves$Abstraction),~max(concat_ViewsLoves$Abstraction)),
               label = 'Abstraction', values = concat_ViewsLoves[,c(8)]),
          
          list(range = c(~min(concat_ViewsLoves$Parallelism),~max(concat_ViewsLoves$Parallelism)),
               label = 'Parallelism', values = concat_ViewsLoves[,c(9)]),
          
          list(range = c(~min(concat_ViewsLoves$Logic),~max(concat_ViewsLoves$Logic)),
               label = 'Logic', values = concat_ViewsLoves[,c(10)]),
          
          list(range = c(~min(concat_ViewsLoves$Synchronization),~max(concat_ViewsLoves$Synchronization)),
               label = 'Syncronization', values = concat_ViewsLoves[,c(11)]),
          
          list(range = c(~min(concat_ViewsLoves$UserInteractivity),~max(concat_ViewsLoves$UserInteractivity)),
               label = 'User Iteractivty', values = concat_ViewsLoves[,c(13)]),
          
          list(range = c(~min(concat_ViewsLoves$Mastery),~max(concat_ViewsLoves$Mastery)),
               label = 'Mastery', values = concat_ViewsLoves[,c(15)]),
          
          list(range = c(~min(concat_ViewsLoves$totalBlocks),~max(concat_ViewsLoves$totalBlocks)),
               label = 'Total Blocks', values = concat_ViewsLoves[,c(20)])
        )
    )    
    
    #GERANDO PARA REMIX --------------------------------------------------------------------------------------------
      coordP_Remix <- plot_ly(type = 'parcoords', 
        line = list(color = 'red'),
        
        dimensions = list(
          list(range = c(~min(concat_Remix$totalRemixes),~max(concat_Remix$totalRemixes)),
               label = 'Total Remixes', values = concat_Remix[,c(4)]),
          
          list(range = c(~min(concat_Remix$Abstraction),~max(concat_Remix$Abstraction)),
               label = 'Abstraction', values = concat_Remix[,c(8)]),
          
          list(range = c(~min(concat_Remix$Parallelism),~max(concat_Remix$Parallelism)),
               label = 'Parallelism', values = concat_Remix[,c(9)]),
          
          list(range = c(~min(concat_Remix$Logic),~max(concat_Remix$Logic)),
               label = 'Logic', values = concat_Remix[,c(10)]),
          
          list(range = c(~min(concat_Remix$Synchronization),~max(concat_Remix$Synchronization)),
               label = 'Syncronization', values = concat_Remix[,c(11)]),
          
          list(range = c(~min(concat_Remix$UserInteractivity),~max(concat_Remix$UserInteractivity)),
               label = 'User Iteractivty', values = concat_Remix[,c(13)]),
          
          list(range = c(~min(concat_Remix$Mastery),~max(concat_Remix$Mastery)),
               label = 'Mastery', values = concat_Remix[,c(15)]),
          
          list(range = c(~min(concat_Remix$totalBlocks),~max(concat_Remix$totalBlocks)),
               label = 'Total Blocks', values = concat_Remix[,c(20)])
        )
    )
    
    #GERANDO PARA LOVES E REMIX ------------------------------------------------------------------------------------
      coordP_LovesRemix <- plot_ly(type = 'parcoords', 
        line = list(color = 'red'),
        
        dimensions = list(
          list(range = c(~min(concat_LovesRemixes$totalLoves),~max(concat_LovesRemixes$totalLoves)),
               label = 'Total Loves', values = concat_LovesRemixes[,c(6)]),
          
          list(range = c(~min(concat_LovesRemixes$totalRemixes),~max(concat_LovesRemixes$totalRemixes)),
               label = 'Total Remix', values = concat_LovesRemixes[,c(4)]),
          
          list(range = c(~min(concat_LovesRemixes$Abstraction),~max(concat_LovesRemixes$Abstraction)),
               label = 'Abstraction', values = concat_LovesRemixes[,c(8)]),
          
          list(range = c(~min(concat_LovesRemixes$Parallelism),~max(concat_LovesRemixes$Parallelism)),
               label = 'Parallelism', values = concat_LovesRemixes[,c(9)]),
          
          list(range = c(~min(concat_LovesRemixes$Logic),~max(concat_LovesRemixes$Logic)),
               label = 'Logic', values = concat_LovesRemixes[,c(10)]),
          
          list(range = c(~min(concat_LovesRemixes$Synchronization),~max(concat_LovesRemixes$Synchronization)),
               label = 'Syncronization', values = concat_LovesRemixes[,c(11)]),
          
          list(range = c(~min(concat_LovesRemixes$UserInteractivity),~max(concat_LovesRemixes$UserInteractivity)),
               label = 'User Iteractivty', values = concat_LovesRemixes[,c(13)]),
          
          list(range = c(~min(concat_LovesRemixes$Mastery),~max(concat_LovesRemixes$Mastery)),
               label = 'Mastery', values = concat_LovesRemixes[,c(15)]),
          
          list(range = c(~min(concat_LovesRemixes$totalBlocks),~max(concat_LovesRemixes$totalBlocks)),
               label = 'Total Blocks', values = concat_LovesRemixes[,c(20)])
        )
    )  

    #GERANDO PARA VIEWS, LOVES E REMIX ------------------------------------------------------------------------------------
      coordP_ViewsLovesRemix <- plot_ly(type = 'parcoords', 
         line = list(color = 'red'),
         
         dimensions = list(
           list(range = c(~min(concat_ViewsLovesRemixes$totalViews),~max(concat_ViewsLovesRemixes$totalViews)),
                label = 'Total Views', values = concat_ViewsLovesRemixes[,c(3)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$totalLoves),~max(concat_ViewsLovesRemixes$totalLoves)),
                label = 'Total Loves', values = concat_ViewsLovesRemixes[,c(6)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$totalRemixes),~max(concat_ViewsLovesRemixes$totalRemixes)),
                label = 'Total Remix', values = concat_ViewsLovesRemixes[,c(4)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$Abstraction),~max(concat_ViewsLovesRemixes$Abstraction)),
                label = 'Abstraction', values = concat_ViewsLovesRemixes[,c(8)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$Parallelism),~max(concat_ViewsLovesRemixes$Parallelism)),
                label = 'Parallelism', values = concat_ViewsLovesRemixes[,c(9)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$Logic),~max(concat_ViewsLovesRemixes$Logic)),
                label = 'Logic', values = concat_ViewsLovesRemixes[,c(10)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$Synchronization),~max(concat_ViewsLovesRemixes$Synchronization)),
                label = 'Syncronization', values = concat_ViewsLovesRemixes[,c(11)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$UserInteractivity),~max(concat_ViewsLovesRemixes$UserInteractivity)),
                label = 'User Iteractivty', values = concat_ViewsLovesRemixes[,c(13)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$Mastery),~max(concat_ViewsLovesRemixes$Mastery)),
                label = 'Mastery', values = concat_ViewsLovesRemixes[,c(15)]),
           
           list(range = c(~min(concat_ViewsLovesRemixes$totalBlocks),~max(concat_ViewsLovesRemixes$totalBlocks)),
                label = 'Total Blocks', values = concat_ViewsLovesRemixes[,c(20)])
         )
      )      
      
      
    htmlwidgets::saveWidget(as_widget(coordP_Views), "ViewsGraph.html")
    htmlwidgets::saveWidget(as_widget(coordP_Loves), "LovesGraph.html")
    htmlwidgets::saveWidget(as_widget(coordP_ViewsLoves), "ViewsLovesGraph.html")
    htmlwidgets::saveWidget(as_widget(coordP_Remix), "RemixGraph.html")
    htmlwidgets::saveWidget(as_widget(coordP_LovesRemix), "LovesRemixGraph.html")
    htmlwidgets::saveWidget(as_widget(coordP_ViewsLovesRemix), "ViewsLovesRemixGraph.html")
    
    
    
    
    
    
    

    
    
    
#LIMPAR VARIAVEIS
    rm(coordP_ButtTopViews)
    #htmlwidgets::saveWidget(as_widget(coordP_TopViews), "ViewsFinalgraph.html", selfcontained = FALSE)
    
#DISTRUIBUIÇÕES DE OCORRÊNCIAS DE VIEWS
  hist(as.numeric(top_Views$total.views))
  hist(as.numeric(butt_Views$total.views))
  
#EXEMPLO COM CLIFF DELTA (*muito lento)
  cliff.delta(butt_Loves$Abstraction, top_Loves$Abstraction)
  
  
  
  
  
  
  
  
end