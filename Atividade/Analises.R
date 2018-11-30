
#LOAD BIBLIS
  library("effsize")

#CONFIGURANDO ÁREA DE TRABALHO E INCLUSÃO DE BIBLIOTECAS
  setwd("~/GitHub/AnaliseDosProjetosScratch_Empirica/Atividade")
  
#IMPORTANTO BASE DE DADOS
  table_csv <- read.delim("new_Data.csv")
  
#FUNÇÕES AUXILIARES
  cohens_d <- function(x, y) {
    lx <- length(x)- 1
    ly <- length(y)- 1
    md  <- abs(mean(x) - mean(y))        
    csd <- lx * var(x) + ly * var(y)
    csd <- csd/(lx + ly)
    csd <- sqrt(csd)                     
    
    cd  <- md/csd                        
  }
  
#TRATANDO OS DADOS
  table_csv <- table_csv[!is.na(table_csv$Abstraction) 
                         & !is.na(table_csv$Parallelism) 
                         & !is.na(table_csv$Logic) 
                         & !is.na(table_csv$Synchronization) 
                         & !is.na(table_csv$FlowControl)
                         & !is.na(table_csv$UserInteractivity)
                         & !is.na(table_csv$DataRepresentation)
                         & !is.na(table_csv$Mastery)
                         & table_csv$username!='unknown',]
  
#CALCULANDO MÉDIA DE VIEWS DOS PROJETOS
    mediana_Views = median(table_csv$totalVies, na.rm = TRUE)
    mediana_Love = median(table_csv$totalLoves, na.rm = TRUE)
    
  
#SELEÇÃO DOS PROJETOS ABAIXO E ACIMA DA MEDIANA
  #seleção em TOTAL VIEWS
  butt_Views <- table_csv[which(table_csv$totalVies <= mediana_Views),]
  top_Views <- table_csv[which(table_csv$totalVies > mediana_Views),]
  
  #seleção em TOTAL LOVES
  butt_Loves <-table_csv[which(butt_Views$totalLoves <= mediana_Love),]
  top_Loves <- table_csv[which(top_Views$totalLoves > mediana_Love),]


#EM GRUPOS RELACIONADOS Á VIEWS-----------------------------------------------------------------------------------
  #WILCOX #wilcox, usado em conjuntos de amostras diferentes(corr, correlação entre atributos diferentes)
    wilcox.test(butt_Views$Abstraction, top_Views$Abstraction)                  #p-value < 2.2e-16             
    wilcox.test(butt_Views$Parallelism, top_Views$Parallelism)                  #p-value < 2.2e-16
    wilcox.test(butt_Views$Logic, top_Views$Logic)                              #p-value < 2.2e-16
    wilcox.test(butt_Views$Synchronization, top_Views$Synchronization)          #p-value < 2.2e-16
    wilcox.test(butt_Views$FlowControl, top_Views$FlowControl)                  #p-value < 2.2e-16
    wilcox.test(butt_Views$UserInteractivity, top_Views$UserInteractivity)      #p-value = 4.24e-05
    wilcox.test(butt_Views$DataRepresentation, top_Views$DataRepresentation)    #p-value < 2.2e-16
    wilcox.test(butt_Views$Mastery, top_Views$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_Views$Clones, top_Views$Clones)                            #p-value < 2.2e-16
    wilcox.test(butt_Views$Mastery, top_Views$Mastery)                          #p-value < 2.2e-16
    wilcox.test(butt_Views$CustomBlocks, top_Views$CustomBlocks)                #p-value < 2.2e-16
    wilcox.test(butt_Views$scriptRank, top_Views$scriptRank)                    #p-value < 2.2e-16
    wilcox.test(butt_Views$totalBlocks, top_Views$totalBlocks)                  #p-value < 2.2e-16
    
  #VISUALIZAÇÃO DA DIFERENÇA BOXPLOT
    boxplot(butt_Views$Abstraction, top_Views$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Parallelism, top_Views$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Logic, top_Views$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_Views$CustomBlocks, top_Views$CustomBlocks, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Synchronization, top_Views$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_Views$FlowControl, top_Views$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_Views$UserInteractivity, top_Views$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_Views$DataRepresentation, top_Views$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_Views$Mastery, top_Views$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_Views$CustomBlocks, top_Views$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_Views$scriptRank, top_Views$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_Views$totalBlocks, top_Views$totalBlocks, outline = FALSE, border = TRUE)  
  
  #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
    
    res <- cohens_d(butt_Views$Abstraction, top_Views$Abstraction)                #0.2010
    res <- cohens_d(butt_Views$Parallelism, top_Views$Parallelism)                #0.2381
    res <- cohens_d(butt_Views$Logic, top_Views$Logic)                            #0.1232
    res <- cohens_d(butt_Views$CustomBlocks, top_Views$CustomBlocks)              #0.0406
    res <- cohens_d(butt_Views$Synchronization, top_Views$Synchronization)        #0.1514
    res <- cohens_d(butt_Views$FlowControl, top_Views$FlowControl)                #0.1947
    res <- cohens_d(butt_Views$UserInteractivity, top_Views$UserInteractivity)    #0.0260
    res <- cohens_d(butt_Views$DataRepresentation, top_Views$DataRepresentation)  #0.0644
    res <- cohens_d(butt_Views$Mastery, top_Views$Mastery)                        #0.1853
    res <- cohens_d(butt_Views$CustomBlocks, top_Views$CustomBlocks)              #0.0406
    res <- cohens_d(butt_Views$scriptRank, top_Views$scriptRank)                  #0.1079
    res <- cohens_d(butt_Views$totalBlocks, top_Views$totalBlocks)                #0.0333
    
    mean(butt_Views$Abstraction); mean(top_Views$Abstraction);
    
    
    
#EM GRUPOS RELACIONADOS Á LOVES-----------------------------------------------------------------------------------
    #WILCOX #wilcox, usado em conjuntos de amostras diferentes(corr, correlação entre atributos diferentes)
    wilcox.test(butt_Loves$Abstraction, top_Loves$Abstraction)                      #p-value = 0.04267
    wilcox.test(butt_Loves$Parallelism, top_Loves$Parallelism)                      #p-value = 5.432e-10
    wilcox.test(butt_Loves$Logic, top_Loves$Logic)                                  #p-value = 0.1476
    wilcox.test(butt_Loves$Synchronization, top_Loves$Synchronization)              #p-value = 0.0001051
    wilcox.test(butt_Loves$FlowControl, top_Loves$FlowControl)                      #p-value = 0.2219
    wilcox.test(butt_Loves$UserInteractivity, top_Loves$UserInteractivity)          #p-value = 1.539e-05
    wilcox.test(butt_Loves$DataRepresentation, top_Loves$DataRepresentation)        #p-value = 9.238e-08
    wilcox.test(butt_Loves$Mastery, top_Loves$Mastery)                              #p-value = 4.071e-06
    wilcox.test(butt_Loves$Clones, top_Loves$Clones)                                #p-value = 4.833e-06
    wilcox.test(butt_Loves$Mastery, top_Loves$Mastery)                              #p-value = 4.071e-06
    wilcox.test(butt_Loves$CustomBlocks, top_Loves$CustomBlocks)                    #p-value = 0.1376
    wilcox.test(butt_Loves$scriptRank, top_Loves$scriptRank)                        #p-value = 0.001358
    wilcox.test(butt_Loves$totalBlocks, top_Loves$totalBlocks)                      #p-value = 0.5782
    
    #VISUALIZAÇÃO DA DIFERENÇA BOXPLOT
    boxplot(butt_Loves$Abstraction, top_Loves$Abstraction, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Parallelism, top_Loves$Parallelism, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Logic, top_Loves$Logic, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$CustomBlocks, top_Loves$CustomBlocks, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Synchronization, top_Loves$Synchronization, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$FlowControl, top_Loves$FlowControl, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$UserInteractivity, top_Loves$UserInteractivity, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$DataRepresentation, top_Loves$DataRepresentation, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$Mastery, top_Loves$Mastery, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$CustomBlocks, top_Loves$CustomBlocks, outline = FALSE, border = TRUE )
    boxplot(butt_Loves$scriptRank, top_Loves$scriptRank, outline = FALSE, border = TRUE)
    boxplot(butt_Loves$totalBlocks, top_Loves$totalBlocks, outline = FALSE, border = TRUE)  
    
    #CALCULO DO TAMANHO DE EFEITO
    # d < 0.2         be considered not signigicant  
    # 0.2 < d < 0.5   be considered a 'small' effect size;
    # 0.5 < d < 0.8   represents a 'medium' effect size
    # 0.8 < d <= 1    a'large' effect size.
    
    res <- cohens_d(butt_Loves$Abstraction, top_Loves$Abstraction)                #0.0286
    res <- cohens_d(butt_Loves$Parallelism, top_Loves$Parallelism)                #0.0492
    res <- cohens_d(butt_Loves$Logic, top_Loves$Logic)                            #0.0142
    res <- cohens_d(butt_Loves$CustomBlocks, top_Loves$CustomBlocks)              #0.0710
    res <- cohens_d(butt_Loves$Synchronization, top_Loves$Synchronization)        #0.0290
    res <- cohens_d(butt_Loves$FlowControl, top_Loves$FlowControl)                #0.0010
    res <- cohens_d(butt_Loves$UserInteractivity, top_Loves$UserInteractivity)    #0.0395
    res <- cohens_d(butt_Loves$DataRepresentation, top_Loves$DataRepresentation)  #0.0480
    res <- cohens_d(butt_Loves$Mastery, top_Loves$Mastery)                        #0.0389
    res <- cohens_d(butt_Loves$CustomBlocks, top_Loves$CustomBlocks)              #0.0710
    res <- cohens_d(butt_Loves$scriptRank, top_Loves$scriptRank)                  #0.0111
    res <- cohens_d(butt_Loves$totalBlocks, top_Loves$totalBlocks)                #0.0230
    
    
    
    
    
cliff.delta(butt_Loves$Abstraction, top_Loves$Abstraction)
  

  
#DISTRUIBUIÇÕES DE OCORRÊNCIAS DE VIEWS
  hist(as.numeric(top_Views$total.views))
  hist(as.numeric(butt_Views$total.views))
  

  


  
#SUMARIZAÇÃO DOS DADOS
    summary(as.numeric(top_Views$total.views))
    summary(as.numeric(butt_Views$total.views))
    summary(as.numeric(butt_Views$FlowControl))
  
end