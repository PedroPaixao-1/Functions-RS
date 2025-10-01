library("bio3d")
library("dplyr")
library("readr")
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

Calcular_Contato <- function(Matriz_distância, Distância_contato) {
  Matriz_ct <- ifelse(Matriz_distância <= 6,1,0) 
  return(Matriz_ct)
} 

#Funções compiladas

Padronizar_pdb <- function(Arquivo_pdb,Limpar_b_factor){
  pdb_atom <- Arquivo_pdb$atom
  
#Separa as cadeias para o futuro
  Cadeia_A <- pdb_atom %>% filter(chain == "A")
  Cadeia_B <- pdb_atom %>% filter(chain != "A")
  
#Renomeia a segunda cadeia para B caso não seja nomeada como B
  pdb_atom <- pdb_atom %>%
    mutate(chain = if_else(chain == "A", "A", "B")) #if_else é uma forma de conjugar if e else 
  
#Calcula os min dos resnos das cadeias
  min_res_A <- min(Cadeia_A$resno)
  min_res_B <- min(Cadeia_B$resno)
  
#Faz a porcaria do cálculo maldito pra definir o máximo já que por algum motivo length(unique()) não estava funcionando
  len_A <- max(Cadeia_A$resno - min_res_A + 1)
  
#Muda os resnos
  pdb_atom <- pdb_atom %>%
    mutate(resno = case_when(
      chain == "A" ~ resno - min_res_A + 1,
      chain == "B" ~ resno - min_res_B + 1 + len_A
    ))
  
#Muda o b facotr de todos os átomos para 0
  if (Limpar_b_factor == TRUE){pdb_atom <- pdb_atom %>%
    mutate(b = 0)} 
  
  return(pdb_atom)
}

Processamento_pdb <- function(Arquivo_pdb,Interesse,Operação){

  #Filtra as linhas de interesse através do seu elety ou resno
  if (is.character(Interesse)) {
    Resid_filtrados <- Arquivo_pdb %>%
      filter(elety == Interesse)
  } else {
    Resid_filtrados <- Arquivo_pdb %>%
      filter(resno == Interesse)
  }
  
  if (Operação == 'Filtrar'){
    return(Resid_filtrados)
  } else if(Operação == 'Distancia'){
    Matriz_dist <- Calcular_Distância(Resid_filtrados)
    return(Matriz_dist)
  } else if (Operação == 'Contato'){
    Distância_contato <- 6
    Matriz <- Calcular_Distância(Resid_filtrados)
    Contato <- Calcular_Contato(Matriz_dist = Matriz,Distância_contato)
    return(Contato)
  }
}

Padronizar_pdb_2 <- function(Arquivo_pdb, Limpar_Bfactor = TRUE) {
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
