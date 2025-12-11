source("Code/Tidyverse_function.R")
Arquivo_pdb <- read.pdb("/home/pedro.paixao/Code/egfr_cetuximabe.pdb")

Arquivo_pdb_padronizado <- Padronizar_pdb(Arquivo_pdb,Limpar_Bfactor = TRUE)
Pdb_atom_padronizado <- Arquivo_pdb_padronizado$atom

pdb_filtrado <- Processamento_pdb(Arquivo_pdb_padronizado,'CA','Filtrar')
Contatos <- Processamento_pdb(Arquivo_pdb_padronizado,'CA','Contato')

seq <- pdbseq(Arquivo_pdb_padronizado)
seq_A <- seq[1:613]
Seq_A <- paste(seq_A, collapse = "")

Fasta_TCC <- Pipeline_mutação(seq_A,Contatos,10)

Paz_nos_estádios <- read.fasta("/home/pedro.paixao/s4pred-main/preds/s4_out_0.fas")


