source("Code/Tidyverse_function.R")
Arquivo_pdb <- read.pdb("/home/pedro.paixao/Code/egfr_cetuximabe.pdb")
a <- Arquivo_pdb$atom

egfr_padronizado <- Padronizar_pdb(Arquivo_pdb = Arquivo_pdb, Limpar_b_factor = TRUE)
Arquivo_pdb$atom <- egfr_padronizado 
pdb_filtrado <- Processamento_pdb(egfr_padronizado,Interesse = 'CA', Operação = 'Filtrar')
Contatos <- Processamento_pdb(egfr_padronizado,Interesse = 'CA', Operação = 'Contato')
indices <- Indices_Contato(pdb_filtrado,Contatos)
O_solitario <- print(which(Contatos[318,] == 1))
library(bio3d)

write.pdb(Arquivo_pdb, file = "egfr_padronizado.pdb")

Arquivo_pdb_2 <- read.pdb("/home/pedro.paixao/Downloads/7jzm.pdb")
pdb2_padronizado <- Padronizar_pdb(Arquivo_pdb_2,T)
pdb2_filtrado <- Processamento_pdb(pdb2_padronizado,Interesse = 'CA', Operação = 'Filtrar')
Contatos2 <- Processamento_pdb(pdb2_padronizado,Interesse = 'CA', Operação = 'Contato')
indices2 <- Indices_Contato(pdb2_filtrado,Contatos2)