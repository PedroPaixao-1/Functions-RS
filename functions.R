
info_resid <- function(Lista_resid,Arquivo_pdb){
  pdb <- read.pdb(Arquivo_pdb)
  pdb_atom <- pdb$atom
  Selecionados <- atom.select(pdb = pdb, resno = Lista_resid)
  Selecionados_linhas <- pdb_atom[selecionados$atom, ]
  return(selecionados_linhas)
}
Processamento_pdb <- function(Arquivo_pdb,Lista_resid,Operação){
  pdb <- read.pdb(Arquivo_pdb)
  pdb_atom <- pdb$atom
  Interesse <- atom.select(pdb = pdb, resno = Lista_resid)
  Linhas_de_interesse <- pdb_atom[Interesse$atom, ]
  
   if (Operação == 0) {
    return(Linhas_de_interesse)
  } else if (Operação == 1) {
   Matriz_Distância <- Calcular_Distância(Linhas_de_interesse)
   return(Matriz_Distância)
  } 
}

Centro_massa <- function(Linhas_de_interesse){
  eixo_x <- sum(linhas_de_interesse[ ,9])/nrow(linhas_de_interesse)
  eixo_y <- sum(linhas_de_interesse[ ,10])/nrow(linhas_de_interesse)
  eixo_z <- sum(linhas_de_interesse[ ,11])/nrow(linhas_de_interesse)
  return(c(eixo_x,eixo_y,eixo_z))
}

Calcular_Distância <- function(Linhas_de_interesse){
  x <- Linhas_de_interesse[, "x"]
  y <- Linhas_de_interesse[, "y"]
  z <- Linhas_de_interesse[, "z"]
  
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

#---------------------Funções derivadas de outras funções-------------------

Calcular_centro_massa <- function(lista_resid,arquivo_pdb){
  Linhas_de_interesse <- info_resid(lista_resid,arquivo_pdb)
  Valor_centro_massa <- Centro_massa(Linhas_de_interesse)
  return(Valor_centro_massa)
}



Calcular_Contato <- function(Matriz_distância, Distância_contato) {
  n <- nrow(Matriz_distância)
  contato <- vector("list", n)
  
  for (i in 1:n) {
    contato[[i]] <- which(Matriz_distância[i, ] <= Distância_contato & Matriz_distância[i, ] > 0)
  }
  
  return(contato)
}

Alterar_BFactor <- function(Arquivo_pdb, Contatos, Valor_bfactor) {

  for (i in seq_along(contatos)) {
    if (length(contatos[[i]]) > 0) {
      pdb_b[i] <- valor_bfactor
    }
  }
  
  pdb$atom$b <- pdb_b
  return(pdb)
}

