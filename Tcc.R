source("Code/Tidyverse_function.R")
Arquivo_pdb <- read.pdb("/home/pedro.paixao/Code/egfr_cetuximabe.pdb")

Arquivo_pdb_padronizado <- Padronizar_pdb(Arquivo_pdb,Limpar_Bfactor = TRUE)
Pdb_atom_padronizado <- Arquivo_pdb_padronizado$atom

pdb_filtrado <- Processamento_pdb(Pdb_atom_padronizado,Interesse = 'CA', Operação = 'Filtrar')
Contatos <- Processamento_pdb(Pdb_atom_padronizado,Interesse = 'CA', Operação = 'Contato')

seq <- pdbseq(Arquivo_pdb_padronizado)
seq_A <- seq[1:613]

Teste <- Pipeline_mutação(seq_A,Contatos,10)
cat(Teste[[1]])




