# The easiest way to get readr is to install the whole tidyverse:
install.packages("bio3d")
library('dplyr')
library("readr")
library("bio3d")
source("/home/pedro.paixao/Code/functions.R")
Arquivo_pdb <- read.pdb('/home/pedro.paixao/Downloads/7jzm.pdb')
pdb_atom <- Arquivo_pdb$atom
Linhas_de_interesse <- Arquivo_pdb$atom %>%
  filter(elety == "CA")
Matriz_dist <- Calcular_Distância(Linhas_de_interesse)

Cadeia_A <- Arquivo_pdb$atom %>%
  filter(chain == "A")
Cadeia_B <- Arquivo_pdb$atom %>%
  filter(chain == "B")

i<-2
 teste <- while(
  Arquivo_pdb$atom$resno[i] == Arquivo_pdb$atom$resno[i-1]
) { Arquivo_pdb$atom %>%
    mutate(,resno = i)
  i <- i + 1
  if (i > nrow(Arquivo_pdb$atom)) break
  Arquivo_pdb$atom
 }
 
 length(unique(Cadeia_A$resno))
 length(unique(Cadeia_B$resno))

  length(unique(pdb_atom2$resno))
  
  
  
  Arquivo_pdb$resid %>%
  transmute(resno = row.names(Arquivo_pdb$atom) )

####
Matriz_contato <- matrix(0, nrow = nrow(Matriz_dist), ncol = ncol(Matriz_dist))

for (i in 1:nrow(Matriz_dist)) {
  for (j in 1:ncol(Matriz_dist)) {
    if (Matriz_dist[i, j] <= 6 && Matriz_dist[i, j] != 0) {
      Matriz_contato[i, j] <- 1
    }
  }
}
###
