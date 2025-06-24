library(bio3d)
source("Code/functions.R")


arquivo_pdb <- "C:/Users/Pichau/Downloads/pdb_dummy.pdb"
leitura_pdb <- read.pdb("C:/Users/Pichau/Downloads/pdb_dummy.pdb")
leitura_pdb_atom <- leitura_pdb$atom

Ca_arquivo_pdb <- atom.select(leitura_pdb, "calpha")
tabela_carbono_alfa <- Ca_arquivo_pdb$atom

Carbonos_alfa <- leitura_pdb_atom[tabela_carbono_alfa, ]


Matriz_txt_Ca <- Calcular_Distância(Carbonos_alfa)

#ACE

Arquivo_pdb_ace <- "C:/Users/Pichau/Downloads/ace2_rbd_relaxed.pdb"
Leitura_pdb_ace <- read.pdb("C:/Users/Pichau/Downloads/ace2_rbd_relaxed.pdb")

Leitura_pdb_ace_atom <- Leitura_pdb_ace$atom

Linhas_de_interesse <- atom.select(Leitura_pdb_ace, "calpha", chain = c("A","D"))
Linhas_de_interesse_ace_atom <- Leitura_pdb_ace_atom[Linhas_de_interesse$atom,]
Cadeia_A_ct <- c(1:length(which(Linhas_de_interesse_ace_atom$chain =="A")))

Matriz_txt_ace <- Calcular_Distância(Linhas_de_interesse_ace_atom)
Lista_ct_ace <- Calcular_Contato(Matriz_txt_ace, 6)

pdb_ct_ace_b1 <- Alterar_BFactor(arquivo_pdb = Arquivo_pdb_ace, contatos = Lista_ct_ace, valor_bfactor = 1)

# Matriz_ct_ace são as posições das linhas de interesse, interger é a quantidade de elementos contidos nessa posição e o valor são os elementos em si.
