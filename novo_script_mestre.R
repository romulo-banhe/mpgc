# ==============================================================================
# SCRIPT MESTRE
#  ANÁLISE QUANTITATIVA DE DADOS QUALITATIVOS
# ==============================================================================

install.packages(c(
  "tidyverse", "readxl", "entropy", "tidytext",
  "factoextra", "syuzhet", "corrplot", "igraph",
  "ggraph", "tm", "scales", "writexl", "FactoMineR", "dplyr"
))

# --- BLOCO 1: CARREGAMENTO DE BIBLIOTECAS ---
library(tidyverse)
library(readxl)
library(entropy)
library(tidytext)
library(factoextra)
library(syuzhet)
library(corrplot)
library(igraph)
library(ggraph)
library(tm)
library(scales)
library(writexl)
library(FactoMineR)
library(dplyr)


# ==============================================================================
# CARREGAR OS DADOS
# ==============================================================================

# Carregar Codebook
# Usamos read_excel para arquivos .xlsx
codebook <- read_excel("/Users/romulobanhe/Downloads/analise_mestrado/TA – Code Listv2.xlsx") %>%
  rename(
    Code_group = `Code group`,
    Code = Code,
    Comment = Comment
  ) %>%
  mutate(
    Code = as.character(Code),
    Code_group = as.character(Code_group)
  )


# Carregar Textos Completos (TF-IDF)
# Colunas esperadas: ID, Setor, Texto_Completo
dados_texto <- read_excel("/Users/romulobanhe/Downloads/analise_mestrado/textos_completos.xlsx")

# Carregar Matriz de Frequência (Para Dominância, Cluster e Correlação)
dados_freq <- read_excel("/Users/romulobanhe/Downloads/analise_mestrado/code-document_v1.xlsx")

# Carregar Matriz de Rede (Para Co-ocorrência)
# Matriz quadrada Código x Código
dados_rede <- read_excel("/Users/romulobanhe/Downloads/analise_mestrado/cooccurence_nova.xlsx")

# Validação de consistência: códigos da matriz vs codebook
codigos_matriz <- as.character(dados_freq[[1]])
codigos_faltantes <- setdiff(codigos_matriz, codebook$Code)
if (length(codigos_faltantes) > 0) {
  message("ATENÇÃO: Existem códigos na matriz de frequência que não estão no codebook. Exemplos:")
  print(head(codigos_faltantes, 20))
}
 

# ==============================================================================
# DOMINÂNCIA DAS DIMENSÕES TOE
# ==============================================================================
 
 #  Definir o mapa de cores padrão (Regra solicitada)
 cores_mapa <- c(
   "Tecnológica"    = "darkorange3", # T_
   "Organizacional" = "darkblue",    # O_
   "DR"             = "darkgreen",   # D_
   "Ambiental"      = "darkred",     # E_
   "Transversais"   = "purple4",     # EM_
   "R"              = "gray30",      # R_
   "Outros"         = "gray70"       # Fallback
 )
 
 dados_long <- dados_freq %>%
   pivot_longer(cols = -1, names_to = "Entrevista", values_to = "Frequencia") %>%
   rename(Codigo = 1) %>%
   mutate(Codigo = as.character(Codigo)) %>%
   left_join(codebook %>% select(Code, Code_group), by = c("Codigo" = "Code")) %>%
   mutate(
     # Atualizada a lógica para incluir o grupo 'R' e garantir a ordem correta
     Dimensao_TOE = case_when(
       str_detect(Codigo, "T_Infra_Barreira") ~ "Tecnológica",
       str_detect(Codigo, "T_Infra_Facilitador") ~ "Tecnológica",
       str_detect(Codigo, "^T_") ~ "Tecnológica", # Captura genérica para T se houver outros
       str_detect(Codigo, "^O_") ~ "Organizacional",
       str_detect(Codigo, "^D_") ~ "DR",
       str_detect(Codigo, "^EM_") ~ "Transversais", # EM antes de E para evitar conflito
       str_detect(Codigo, "^E_") ~ "Ambiental",
       str_detect(Codigo, "^R_") ~ "R",
       TRUE ~ "Outros"
     ),
     
     Blco_Codebook = if_else(is.na(Code_group), "(não mapeado)", Code_group)
   )
 
 # --- GRÁFICO Barras Empilhadas (Dimensões TOE) ---
 
 dados_toe_plot <- dados_long %>%
   #filter(Dimensao_TOE %in% names(cores_mapa)) # Filtra apenas o que temos mapeado
  filter(Dimensao_TOE %in% c("Tecnológica", "Organizacional", "Ambiental", "Transversais"))
 g2 <- ggplot(dados_toe_plot, aes(x = Entrevista, y = Frequencia, fill = Dimensao_TOE)) +
   geom_col(position = "fill") +
   scale_y_continuous(labels = percent) +
   theme_minimal() +
   labs(
     title = "Dominância das Dimensões TOE",
     y = "% do Discurso", x = "Entrevistado"
   ) +
   # APLICAÇÃO DAS CORES
   scale_fill_manual(values = cores_mapa)
 
 print(g2)
 
 # Tabelas consolidadas TOE  
 tabela_toe_consolidada <- dados_toe_plot %>%
   group_by(Dimensao_TOE) %>%
   summarise(
     Total_Citacoes = sum(Frequencia, na.rm = TRUE),
     Media_por_Entrevistado = mean(Frequencia, na.rm = TRUE)
   ) %>%
   mutate(Percentual_Geral = round((Total_Citacoes / sum(Total_Citacoes)) * 100, 2)) %>%
   arrange(desc(Total_Citacoes))
 
 print(tabela_toe_consolidada)
 
 tabela_toe_dominancia <- dados_toe_plot %>%
   group_by(Entrevista, Dimensao_TOE) %>%
   summarise(Soma_Freq = sum(Frequencia, na.rm = TRUE), .groups = "drop") %>%
   group_by(Entrevista) %>%
   mutate(Percentual = round((Soma_Freq / sum(Soma_Freq)) * 100, 2)) %>%
   select(-Soma_Freq) %>%
   pivot_wider(names_from = Entrevista, values_from = Percentual)
 
 print(tabela_toe_dominancia)
 
 
 # --- GRÁFICO Dominância por Bloco do Codebook ---

 # Criar um mapa de cores para os blocos
 mapa_cores_blocos <- dados_long %>%
   distinct(Blco_Codebook, Dimensao_TOE) %>%
   mutate(Cor = cores_mapa[Dimensao_TOE]) %>%
   filter(!is.na(Cor))
 
 # Converter para vetor nomeado para usar no ggplot
 vetor_cores_blocos <- setNames(mapa_cores_blocos$Cor, mapa_cores_blocos$Blco_Codebook)
 
 g2b <- ggplot(dados_long, aes(x = Entrevista, y = Frequencia, fill = Blco_Codebook)) +
   geom_col(position = "fill") +
   scale_y_continuous(labels = percent) +
   theme_minimal() +
   labs(
     title = "Dominância por Blocos (Codebook DR/TOE)",
     y = "% do Discurso", x = "Entrevistado"
   ) +
   # APLICAÇÃO DAS CORES NOS BLOCOS
   scale_fill_manual(values = vetor_cores_blocos) +
   theme(legend.position = "bottom") # Legenda embaixo pois podem haver muitos blocos
 
 print(g2b)
 
 # Tabelas de Blocos (Mantidas originais)
 tabela_blocos_entrevista <- dados_long %>%
   group_by(Entrevista, Blco_Codebook) %>%
   summarise(Soma_Freq = sum(Frequencia, na.rm = TRUE), .groups = "drop") %>%
   group_by(Entrevista) %>%
   mutate(Percentual = round((Soma_Freq / sum(Soma_Freq)) * 100, 2)) %>%
   select(-Soma_Freq) %>%
   pivot_wider(names_from = Entrevista, values_from = Percentual, values_fill = 0)
 
 print(tabela_blocos_entrevista)
 
 tabela_blocos_geral <- dados_long %>%
   group_by(Blco_Codebook) %>%
   summarise(
     Total_Citacoes = sum(Frequencia, na.rm = TRUE),
     Qtd_Codigos_Diferentes = n_distinct(Codigo)
   ) %>%
   mutate(Percentual_Geral = round((Total_Citacoes / sum(Total_Citacoes)) * 100, 2)) %>%
   arrange(desc(Total_Citacoes))
 
 print(tabela_blocos_geral)
 
# ==============================================================================
# TF-IDF (TERMOS DISTINTIVOS POR SETOR)
# ==============================================================================

stopwords_pt <- data.frame(palavra = stopwords("pt"))

lixo_fala <- c("_", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "à", "acaba", "acha",
               "acho", "acordo", "acredito", "adeus", "agora", "aham", "aí", "ainda", "além", "algo", "alguém",
               "algum", "alguma", "algumas", "alguns", "ali", "ampla", "amplas", "amplo", "amplos", "ano",
               "anos", "ante", "antes", "ao", "aonde", "aos", "apenas", "apoio", "após", "aquela", "aquelas",
               "aquele", "aqueles", "aqui", "aquilo", "área", "as", "às", "assim", "até", "atrás", "através",
               "baixo", "bastante", "beleza", "bem", "boa", "boas", "bom", "bons", "breve", "cá", "cada",
               "cara", "catorze", "cedo", "cento", "certamente", "certeza", "certo", "chega", "chego", "cima",
               "cinco", "coisa", "coisas", "colocar", "com", "começa", "comigo", "como", "concordo",
               "consegue", "conselho", "conta", "contra", "contudo", "cousa", "creio", "custa", "da", "dá",
               "daí", "dão", "daquela", "daquelas", "daquele", "daqueles", "dar", "das", "dava", "de",
               "debaixo", "dela", "delas", "dele", "deles", "demais", "dentro", "depois", "depressa", "desde",
               "dessa", "dessas", "desse", "desses", "desta", "destas", "deste", "destes", "deu", "deve",
               "devem", "devendo", "dever", "deverá", "deverão", "deveria", "deveriam", "devia", "deviam",
               "dez", "dezanove", "dezesseis", "dezessete", "dezoito", "dia", "diante", "disse", "disso",
               "disto", "dito", "diz", "dizem", "dizer", "dizia", "do", "dois", "dos", "dous", "doze", "duas",
               "dúvida", "e", "é", "ela", "elas", "ele", "eles", "em", "embora", "enfim", "enquanto", "então",
               "entendeu", "entrar", "entre", "era", "eram", "éramos", "és", "essa", "essas", "esse", "esses",
               "esta", "está", "estamos", "estão", "estar", "estas", "estás", "estava", "estavam", "estávamos",
               "este", "esteja", "estejam", "estejamos", "estes", "esteve", "estive", "estivemos", "estiver",
               "estivera", "estiveram", "estivéramos", "estiverem", "estivermos", "estivesse", "estivessem",
               "estivéssemos", "estiveste", "estivestes", "estou", "etc", "eu", "exatamente", "exato",
               "exemplo", "faça", "faço", "fala", "falando", "falar", "falei", "falo", "falou", "falta",
               "favor", "faz", "fazeis", "fazem", "fazemos", "fazendo", "fazer", "fazes", "fazia", "feita",
               "feitas", "feito", "feitos", "fez", "fica", "ficar", "fico", "ficou", "fim", "final", "fiquei",
               "fiz", "foi", "fomos", "for", "fora", "foram", "fôramos", "forem", "forma", "formos", "fosse",
               "fossem", "fôssemos", "foste", "fostes", "fui", "gente", "geral", "grande", "grandes", "grupo",
               "há", "hã", "haja", "hajam", "hajamos", "hão", "havemos", "havia", "hei", "hoje", "hora",
               "horas", "houve", "houvemos", "houver", "houvera", "houverá", "houveram", "houvéramos",
               "houverão", "houverei", "houverem", "houveremos", "houveria", "houveriam", "houveríamos",
               "houvermos", "houvesse", "houvessem", "houvéssemos", "indo", "ir", "isso", "isto", "já", "jns",
               "la", "lá", "lado", "lhe", "lhes", "lo", "local", "logo", "longe", "lugar", "maior", "maioria",
               "mais", "mal", "mas", "máximo", "me", "meio", "melhor", "menor", "mba", "menos", "mês", "meses",
               "mesma", "mesmas", "mesmo", "mesmos", "meu", "meus", "mil", "mim", "minha", "minhas", "momento",
               "monte", "muita", "muitas", "muito", "muitos", "na", "nada", "não", "naquela", "naquelas",
               "naquele", "naqueles", "nas", "né", "nele", "nem", "nenhum", "nenhuma", "nessa", "nessas",
               "nesse", "nesses", "nesta", "nestas", "neste", "nestes", "ninguém", "nisso", "nível", "no",
               "noite", "nome", "nos", "nós", "nossa", "nossas", "nosso", "nossos", "nova", "novas", "nove",
               "novo", "novos", "num", "numa", "número", "nunca", "o", "obra", "obrigada", "obrigado",
               "oitava", "oitavo", "oito", "olha", "onde", "ontem", "onze", "os", "ou", "outra", "outras",
               "outro", "outros", "para", "parece", "parte", "partir", "paucas", "pega", "pela", "pelas",
               "pelo", "pelos", "pequena", "pequenas", "pequeno", "pequenos", "per", "perante", "perfeito",
               "perto", "pô", "pode", "pôde", "podem", "podendo", "poder", "poderia", "poderiam", "podia",
               "podiam", "põe", "põem", "pois", "ponto", "pontos", "por", "porém", "porque", "porquê",
               "posição", "possível", "possivelmente", "posso", "pouca", "poucas", "pouco", "poucos", "pra",
               "primeira", "primeiras", "primeiro", "primeiros", "principalmente", "pro", "própria",
               "próprias", "próprio", "próprios", "próxima", "próximas", "próximo", "próximos", "pude",
               "puderam", "quais", "quáis", "qual", "quando", "quanto", "quantos", "quarta", "quarto",
               "quatro", "que", "quê", "quem", "quer", "quereis", "querem", "queremas", "queres", "queria",
               "quero", "questão", "quinta", "quinto", "quinze", "quis", "recentemente", "relação", "sabe",
               "sabem", "sai", "saí", "são", "se", "segunda", "segundo", "sei", "seis", "seja", "sejam",
               "sejamos", "sem", "sempre", "sendo", "senhor", "senhora", "ser", "será", "serão", "serei",
               "seremos", "seria", "seriam", "seríamos", "sete", "sétima", "sétimo", "seu", "seus", "sexta",
               "sexto", "si", "sido", "sim", "sistema", "só", "sob", "sobre", "sois", "somos", "sou", "sua",
               "suas", "tá", "tal", "talvez", "também", "tampouco", "tanta", "tantas", "tanto", "tão", "tarde",
               "te", "tem", "tém", "têm", "temos", "tendes", "tendo", "tenha", "tenham", "tenhamos", "tenho",
               "tens", "ter", "terá", "terão", "terceira", "terceiro", "terei", "teremos", "teria", "teriam",
               "teríamos", "teu", "teus", "teve", "ti", "tido", "tinha", "tinham", "tínhamos", "tive",
               "tivemos", "tiver", "tivera", "tiveram", "tivéramos", "tiverem", "tivermos", "tivesse",
               "tivessem", "tivéssemos", "tiveste", "tivestes", "tô", "toda", "todas", "todavia", "todo",
               "todos", "trabalho", "tranquilo", "trás", "traz", "três", "treze", "tu", "tua", "tuas", "tudo",
               "última", "últimas", "último", "últimos", "um", "uma", "umas", "uns", "usando", "usar", "vai",
               "vais", "vamos", "vão", "vários", "vê", "veio", "vejo", "vem", "vêm", "vendo", "vens", "ver",
               "verdade", "vez", "vezes", "vi", "viagem", "vim", "vindo", "vinte", "vir", "viu", "você",
               "vocês", "vos", "vós", "vossa", "vossas", "vosso", "vossos", "vou", "zero", "pegar", "olhando",
               "vale", "comecei", "falaria", "forte", "uso", "rampup", "mundo", "principais", "existe",
               "xxx", "xxxx", "doutorado", "pedindo", "los", "las", "aeronáutico")

stopwords_extras <- data.frame(palavra = lixo_fala)
todas_stopwords <- bind_rows(stopwords_pt, stopwords_extras)

palavras_setor <- dados_texto %>%
  unnest_tokens(palavra, Texto_Completo) %>%
  mutate(palavra = str_replace_all(palavra, "[^a-zçãõáéíóúâêôàü]", "")) %>%
  filter(nchar(palavra) > 2) %>%
  anti_join(todas_stopwords, by = "palavra") %>%
  count(Setor, palavra, sort = TRUE) %>%
  bind_tf_idf(palavra, Setor, n)

g3 <- palavras_setor %>%
  group_by(Setor) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(palavra, tf_idf, Setor), fill = Setor)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Setor, scales = "free") +
  scale_y_reordered() +
  labs(
    title = "Assinatura Léxica por Setor (TF-IDF)",
    subtitle = "Top 5 termos mais distintivos por vertical",
    x = "Importância Estatística (Score TF-IDF)", y = NULL
  ) +
  theme_minimal()

print(g3)

# Criar a tabela resumida
tabela_resumo <- palavras_setor %>%
  group_by(Setor) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  summarise(palavras = paste(palavra, collapse = ", ")) %>%
  ungroup()

# Exibir na tela
print(tabela_resumo)

# Criar a tabela detalhada
tabela_detalhada <- palavras_setor %>%
  group_by(Setor) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  select(Setor, palavra, n, tf_idf) %>%
  ungroup()

# Exibir na tela
print(tabela_detalhada)
 
 
# ==============================================================================
# REDE DE CO-OCORRÊNCIA
# ==============================================================================

rownames(dados_rede) <- dados_rede[[1]]
matriz_rede <- as.matrix(dados_rede[, -1])

g_network <- graph_from_adjacency_matrix(matriz_rede, mode = "undirected", weighted = TRUE, diag = FALSE)

g_filtered <- delete.edges(g_network, E(g_network)[weight < 1])
g_filtered <- delete.vertices(g_filtered, degree(g_filtered) == 0)

# --- LÓGICA DE CORES ---
# Criar uma propriedade no grafo para identificar o grupo baseado no prefixo do nome
V(g_filtered)$grupo_cor <- case_when(
  str_detect(V(g_filtered)$name, "^T_")  ~ "T",
  str_detect(V(g_filtered)$name, "^O_")  ~ "O",
  str_detect(V(g_filtered)$name, "^D_")  ~ "D",
  str_detect(V(g_filtered)$name, "^EM_") ~ "EM", # Verificar EM antes de E para garantir prioridade se necessário
  str_detect(V(g_filtered)$name, "^E_")  ~ "E",
  str_detect(V(g_filtered)$name, "^R_")  ~ "R",
  TRUE ~ "Outros"
)

# Definir as cores manualmente conforme solicitado
cores_personalizadas <- c(
  "T"      = "darkorange3", # Laranja escuro
  "O"      = "darkblue",    # Azul escuro
  "D"      = "darkgreen",   # Verde escuro
  "E"      = "darkred",     # Vermelho escuro
  "EM"     = "purple4",     # Roxo escuro (purple4 é um tom bem escuro)
  "R"      = "gray30",      # Cinza escuro
  "Outros" = "black"        # Caso sobre algo sem prefixo
)

g6 <- ggraph(g_filtered, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = "gray50") +
  
  # Alteração aqui: mapeamos a cor (color) ao grupo criado acima
  geom_node_point(aes(color = grupo_cor), size = 8) +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3, color = "black") + # Mantive o texto preto para leitura
  
  # Aplica as cores definidas
  scale_color_manual(values = cores_personalizadas) +
  
  theme_void() +
  labs(title = "Rede de Co-ocorrência dos Códigos", color = "Grupo") +
  theme(legend.position = "bottom") # Legenda na parte inferior (opcional)

print(g6)

tabela_nos <- data.frame(
  Nome = V(g_filtered)$name,
  Grupo = V(g_filtered)$grupo_cor, # Adicionado para conferência
  Conexoes = degree(g_filtered),
  Importancia = betweenness(g_filtered)  
)

tabela_nos <- tabela_nos[order(-tabela_nos$Conexoes), ]

print(head(tabela_nos))

# Calcular as métricas
centralidade <- data.frame(
  Codigos = V(g_filtered)$name,
  Grau = degree(g_filtered),                                      
  Intermediacao = round(betweenness(g_filtered), 2),           
  Proximidade = round(closeness(g_filtered), 4),               
  Autovetor = round(eigen_centrality(g_filtered)$vector, 2)    
)

# Ordenar pela Intermediação 
centralidade <- centralidade[order(-centralidade$Intermediacao), ]

# Visualizar a tabela final
print(centralidade)

# Extrair a lista de conexões (arestas) e seus pesos
conexoes_fortes <- as_data_frame(g_filtered, what = "edges")

# Ordenar do maior peso para o menor
conexoes_fortes <- conexoes_fortes[order(-conexoes_fortes$weight), ]

# Renomear as colunas
colnames(conexoes_fortes) <- c("Codigo_A", "Codigo_B", "Peso_Conexao")

# Ver as 100 principais conexões
print(head(conexoes_fortes, 100))



# ==============================================================================
# ANÁLISE DE CORRESPONDÊNCIA (ANACOR)
# ==============================================================================

# Lógica por Dimensao_TOE
dados_ca <- dados_long %>%
  filter(Dimensao_TOE %in% c("Tecnológica", "Organizacional", "Ambiental", "Transversais", "DR"))

tabela_ca <- dados_ca %>%
  count(Entrevista, Dimensao_TOE, wt = Frequencia) %>%
  pivot_wider(names_from = Dimensao_TOE, values_from = n, values_fill = 0) %>%
  column_to_rownames("Entrevista")

res.ca <- CA(tabela_ca, graph = FALSE)

g10 <- fviz_ca_biplot(
  res.ca,
  repel = TRUE,
  title = "Mapa de Afinidade (Análise de Correspondência)",
  col.row = "#0073C2FF",
  col.col = "#E7B800"
) +
  theme_minimal() +
  labs(subtitle = "Proximidade entre Gestores e Dimensões (Distância Qui-Quadrado)")


print(g10)

# Exibir a tabela de contingência (absoluta)
cat("\n--- Tabela de Contingência: Entrevistas vs Dimensões ---\n")
print(tabela_ca)

# leitura de ranking:
tabela_ranking <- dados_ca %>%
  group_by(Entrevista, Dimensao_TOE) %>%
  summarise(Total_Citações = sum(Frequencia), .groups = "drop") %>%
  arrange(Entrevista, desc(Total_Citações))

print(tabela_ranking)

# Coordenadas das Dimensões  
library(factoextra)

cat("\n--- Coordenadas das Dimensões no Mapa ---\n")
coords_col <- get_ca_col(res.ca)$coord
print(round(coords_col, 4))

cat("\n--- Coordenadas das Entrevistas (Top 10) ---\n")
coords_row <- get_ca_row(res.ca)$coord
print(head(round(coords_row, 4), 14))

print("Matriz de Resíduos Padronizados")
chisq <- chisq.test(tabela_ca)
print(round(chisq$stdres, 2))

# ==============================================================================
# VALIDAÇÃO DA AMOSTRA - CURVA DE SATURAÇÃO (ORDEM CRONOLÓGICA REAL)
# ==============================================================================

df_sat <- as.data.frame(t(dados_freq[, -1]))
colnames(df_sat) <- dados_freq[[1]]

ordem_real <- c("E1", "E2", "E3", "E4", "E6", "E7", "E8",
                "E10", "E11", "E9", "E12", "E13", "E14", "E5")

df_sat_ordenado <- df_sat[match(ordem_real, rownames(df_sat)), ]
df_sat_ordenado$Entrevista <- factor(rownames(df_sat_ordenado), levels = ordem_real)

calcular_saturacao <- function(df) {
  dados_num <- df[, sapply(df, is.numeric)]
  novos_por_entrevista <- numeric(nrow(df))
  acumulado <- numeric(nrow(df))
  codigos_ja_vistos <- rep(FALSE, ncol(dados_num))
  
  for (i in 1:nrow(df)) {
    linha <- as.numeric(dados_num[i, ])
    codigos_presentes <- linha > 0
    novos <- codigos_presentes & !codigos_ja_vistos
    novos_por_entrevista[i] <- sum(novos)
    codigos_ja_vistos <- codigos_ja_vistos | codigos_presentes
    acumulado[i] <- sum(codigos_ja_vistos)
  }
  
  data.frame(
    Entrevista = df$Entrevista,
    Novos = novos_por_entrevista,
    Acumulado = acumulado
  )
}

dados_saturacao <- calcular_saturacao(df_sat_ordenado)

g11 <- ggplot(dados_saturacao, aes(x = as.numeric(Entrevista))) +
  geom_line(aes(y = Acumulado, color = "Total Acumulado"), size = 1.2) +
  geom_point(aes(y = Acumulado, color = "Total Acumulado"), size = 3) +
  geom_col(aes(y = Novos, fill = "Novos Temas (Marginal)"), alpha = 0.6) +
  scale_x_continuous(breaks = 1:14, labels = dados_saturacao$Entrevista) +
  theme_minimal() +
  labs(
    title = "Curva de Saturação Teórica (Ordem Cronológica)",
    subtitle = "Evolução da descoberta de temas conforme a sequência real das entrevistas",
    x = "Sequência das Entrevistas", y = "Quantidade de Códigos",
    color = "", fill = ""
  ) +
  scale_color_manual(values = c("Total Acumulado" = "#2E9FDF")) +
  scale_fill_manual(values = c("Novos Temas (Marginal)" = "#FC4E07")) +
  theme(legend.position = "bottom")

print(g11)

# Preparar a tabela com métricas de ganho percentual
tabela_sat_final <- dados_saturacao %>%
  mutate(
    Percentual_Total = round((Acumulado / max(Acumulado)) * 100, 1),
    Ganho_Marginal = paste0("+", Novos)
  )

# Exibir na tela
print(tabela_sat_final)
