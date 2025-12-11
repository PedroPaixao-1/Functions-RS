library("reticulate")
library("bio3d")
library("dplyr")
library("readr")
library("seqinr")

#Funções Operacionais

Calcular_Distância <- function(Interesse){
  x <- Interesse[, "x"]
  y <- Interesse[, "y"]
  z <- Interesse[, "z"]
  
  n <- length(x)
  Matriz_distancias <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      Distância <- sqrt((x[i] - x[j])**2 + (y[i] - y[j])**2 + (z[i] - z[j])**2)
      Matriz_distancias[i, j] <- Distância
    }
  }
  
  return(Matriz_distancias)
}

Calcular_Contato <- function(Matriz_distância) {
  Matriz_ct <- ifelse(Matriz_distância <= 8,1,0) 
  return(Matriz_ct)
} 

Indices_Contato <- function(pdb,Matriz_ct) {
  #Separa as cadeias para o futuro
  Cadeia_A <- pdb$atom %>% filter(chain == "A")
  Cadeia_B <- pdb$atom %>% filter(chain != "A")
  
  #Calcula os min dos resnos das cadeias
  max_res_A <- max(Cadeia_A$resno)
  min_res_B <- min(Cadeia_B$resno)
  
  Indices <- arrayInd(which(Matriz_ct == 1), .dim = dim(Matriz_ct))
  Indices <- unique.array(Indices)
  Indices <- Indices[Indices[,1] < Indices[,2], ]
  Indices <- Indices[
    (Indices[,1] <= max_res_A & Indices[,2] >= min_res_B) |
      (Indices[,2] <= max_res_A & Indices[,1] >= min_res_B),
  ]
  
  return(Indices)
}

Randomizar_posicao <- function(Contatos_unicos){
  Posicao <- sample(Contatos_unicos, size = ceiling(20/100*length(Contatos_unicos)))
  return(unlist(Posicao))
}

Chamar_python <- function(Sequencia, Posicao, Temperatura) {

  Texto <- paste(
    "/home/pedro.paixao/anaconda3/condabin/conda run -n esm2-env",
    "python /home/pedro.paixao/Code/generate_sequence_esm2.py",
    "--sequence", Sequencia,
    "--position", Posicao,
    "--temperature", Temperatura
  )
  
 
  system(Texto, wait = TRUE)

  saida <- readLines("completed_sequence.txt")
  
}

Rodar_psypred <- function(arquivo_fasta){
  
  Variantes.fasta <- arquivo_fasta 
  #esqueminha pra rodar no sistema
  Texto <- paste(" python s4pred-main/run_model.py -t fas -x -z",
                 Variantes.fasta)
  system(Texto, wait = TRUE)
  #pega as preds
  Arquivos_pred <- list.files(
    "/home/pedro.paixao/s4pred-main/preds",
    pattern = "\\.fas$",
    full.names = TRUE
  )
  #lê as preds
  Dados <- lapply(Arquivos_pred, read.fasta)
  
  return(Dados)
}

Comparar_predSecs <- function()

#Funções compiladas

Processamento_pdb <- function(Arquivo_pdb,Interesse,Operação){

  #Filtra as linhas de interesse através do seu elety ou resno
  if (is.character(Interesse)) {
    Resid_filtrados <- filter(Arquivo_pdb$atom,elety == Interesse)
  } else {
    Resid_filtrados <- Resid_filtrados <- Arquivo_pdb$atom[Arquivo_pdb$atom$resno %in% Interesse, ]
  }
  
  if (Operação == 'Filtrar'){
    return(Resid_filtrados)
  } else if(Operação == 'Distancia'){
    Matriz_dist <- Calcular_Distância(Resid_filtrados)
    return(Matriz_dist)
  } else if (Operação == 'Contato'){
    Matriz <- Calcular_Distância(Resid_filtrados)
    Contato <- Calcular_Contato(Matriz_dist = Matriz)
    Resid_Ct <- Indices_Contato(Arquivo_pdb,Contato)
    Contatos_unicos <- as.list(unique(Resid_Ct[,1]))
    return(Contatos_unicos)
  }
}

Padronizar_pdb <- function(Arquivo_pdb, Limpar_Bfactor = TRUE){
  pdb_atom <- Arquivo_pdb$atom
  
  cadeias <- unique(pdb_atom$chain)
  pdb_atom$chain <- LETTERS[match(pdb_atom$chain, cadeias)]
  
  identificadores <- paste(pdb_atom$chain, pdb_atom$resno)
  novos_resnos<- match(identificadores, unique(identificadores))   
  pdb_atom$resno <- novos_resnos
  
  if (isTRUE(Limpar_Bfactor)) {
    pdb_atom$b <- 0
  }

  Arquivo_pdb$atom <- pdb_atom
  return(Arquivo_pdb)
}

Pipeline_mutação <- function(Sequencia,Contatos,Loops){
 Variantes <- vector("list",Loops) 
 MULTIFASTA <- vector("list",Loops) 
 EGFR_fasta <- read.fasta("/home/pedro.paixao/Code/EGFR.fasta")
 
    for (i in 1:Loops){
    Seq <- paste(Sequencia, collapse = "")
    Posicoes <- Randomizar_posicao(Contatos)
    Variantes[[i]] <- Chamar_python(Sequencia = Seq, Posicao = Posicoes, Temperatura = 1.5)
    } 
  for (i in 1:Loops){
    Header <- paste(">EGFR|CHAIN A|Variante",i)
    MULTIFASTA[[i]] <- paste(Header,Variantes[i], sep = "\n")
  }
  arquivo_fasta <- file("Variantes.fasta","w")
  for (linha in MULTIFASTA) {
    linha <- gsub("\\\\n", "\n", linha)
    cat(linha, "\n", file = arquivo_fasta)
  }
  close(arquivo_fasta)
  
  predSec <- Rodar_psypred(arquivo_fasta)
  

  
}
