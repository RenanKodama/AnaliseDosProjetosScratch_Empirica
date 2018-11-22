
#CONFIGURANDO ÁREA DE TRABALHO E INCLUSÃO DE BIBLIOTECAS
  setwd("~/GitHub/AnaliseDosProjetosScratch_Empirica/Atividade")
  
  
#IMPORTANTO BASE DE DADOS
  table_csv <- read.delim("new_Data.csv")
  
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
    mediana = median(table_csv$totalVies, na.rm = TRUE)
    
  
#SELEÇÃO DOS PROJETOS ABAIXO E ACIMA DA MÉDIA DE VEIWS
  butt_Views <- table_csv[which(table_csv$totalVies <= mediana),]
  top_Views <- table_csv[which(table_csv$totalVies > mediana),]


#wilcox, usado em conjuntos de amostras diferentes
#corr, correlação entre atributos diferentes
  
#WILCOX (EXISTE CORRELAÇÃO?)
  wilcox.test(top_Views$Abstraction, butt_Views$Abstraction)
  wilcox.test(top_Views$Parallelism, butt_Views$Parallelism)
  wilcox.test(top_Views$Logic, butt_Views$Logic)
  wilcox.test(top_Views$Synchronization, butt_Views$Synchronization)
  wilcox.test(top_Views$FlowControl, butt_Views$FlowControl)
  wilcox.test(top_Views$UserInteractivity, butt_Views$UserInteractivity)
  wilcox.test(top_Views$DataRepresentation, butt_Views$DataRepresentation)
  wilcox.test(top_Views$Mastery, butt_Views$Mastery)
  wilcox.test(top_Views$Clones, butt_Views$Clones)
  wilcox.test(top_Views$Mastery, butt_Views$Mastery)
  wilcox.test(top_Views$CustomBlocks, butt_Views$CustomBlocks)
  wilcox.test(top_Views$scriptRank, butt_Views$scriptRank)
  wilcox.test(top_Views$totalBlocks, butt_Views$totalBlocks)
  
#VISUALIZAÇÃO DA DIFERENÇA BOXPLOT
  boxplot(top_Views$Abstraction, butt_Views$Abstraction, border = TRUE)
  boxplot(top_Views$Parallelism, butt_Views$Parallelism, border = TRUE)
  boxplot(top_Views$Logic, butt_Views$Logic, border = TRUE)
  boxplot(top_Views$CustomBlocks, butt_Views$CustomBlocks, border = TRUE)
  boxplot(top_Views$Synchronization, butt_Views$Synchronization, border = TRUE)
  boxplot(top_Views$FlowControl, butt_Views$FlowControl, outline = FALSE, border = TRUE)
  boxplot(top_Views$UserInteractivity, butt_Views$UserInteractivity, outline = FALSE, border = TRUE)
  boxplot(top_Views$DataRepresentation, butt_Views$DataRepresentation, outline = FALSE, border = TRUE)
  boxplot(top_Views$Mastery, butt_Views$Mastery, border = TRUE)
  boxplot(top_Views$CustomBlocks, butt_Views$CustomBlocks, outline = FALSE, border = TRUE )
  boxplot(top_Views$scriptRank, butt_Views$scriptRank, outline = FALSE, border = TRUE)
  boxplot(top_Views$totalBlocks, butt_Views$totalBlocks, outline = FALSE, border = TRUE)  



#CALCULO DO TAMANHO DE EFEITO
  # d < 0.2         be considered not signigicant  
  # 0.2 < d < 0.5   be considered a 'small' effect size;
  # 0.5 < d < 0.8   represents a 'medium' effect size
  # 0.8 < d <= 1    a'large' effect size.
  
  res <- cohens_d(top_Views$Abstraction, butt_Views$Abstraction)                #0.2010
  res <- cohens_d(top_Views$Parallelism, butt_Views$Parallelism)                #0.2380
  res <- cohens_d(top_Views$Logic, butt_Views$Logic)                            #0.1231
  res <- cohens_d(top_Views$CustomBlocks, butt_Views$CustomBlocks)              #0.0405
  res <- cohens_d(top_Views$Synchronization, butt_Views$Synchronization)        #0.1513
  res <- cohens_d(top_Views$FlowControl, butt_Views$FlowControl)                #0.1947
  res <- cohens_d(top_Views$UserInteractivity, butt_Views$UserInteractivity)    #0.0260
  res <- cohens_d(top_Views$DataRepresentation, butt_Views$DataRepresentation)  #0.0644
  res <- cohens_d(top_Views$Mastery, butt_Views$Mastery)                        #0.1853
  res <- cohens_d(top_Views$CustomBlocks, butt_Views$CustomBlocks)              #0.0405
  res <- cohens_d(top_Views$scriptRank, butt_Views$scriptRank)                  #0.1079
  res <- cohens_d(top_Views$totalBlocks, butt_Views$totalBlocks)                #0.0332
  
cliff.delta(op_Views$Abstraction, butt_Views$Abstraction, return.dm=TRUE)
  

  
#DISTRUIBUIÇÕES DE OCORRÊNCIAS DE VIEWS
  hist(as.numeric(top_Views$total.views))
  hist(as.numeric(butt_Views$total.views))
  

  
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

  
#SUMARIZAÇÃO DOS DADOS
    summary(as.numeric(top_Views$total.views))
    summary(as.numeric(butt_Views$total.views))
    summary(as.numeric(butt_Views$FlowControl))
  
end