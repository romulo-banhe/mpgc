# Instalar pacotes necessários
install.packages("tm")
install.packages("slam")
# Carregar bibliotecas
library(tm)
library(slam)
# Carregar o texto das entrevistas (substitua o caminho pelo arquivo correto)
texto <- tolower(readLines("/Users/romulobanhe/Downloads/analise_mestrado/bloco1.txt", encoding = "UTF-8"))
# Criar um Corpus
corpus <- Corpus(VectorSource(texto))
# Pré-processamento do texto
corpus <- tm_map(corpus, content_transformer(tolower)) # Converter para minúsculas
corpus <- tm_map(corpus, removePunctuation) # Remover pontuações
corpus <- tm_map(corpus, removeNumbers) # Remover números
corpus <- tm_map(corpus, stripWhitespace) # Remover espaços em branco extras
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "pessoas", replacement = "pessoa")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "empresas", replacement = "empresa")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "dados", replacement = "dado")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "data", replacement = "dado")
# Unificar palavras similares.
# Lista de stop words fornecida
stop_words <- c("_", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "à", "acaba", "acha",
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
                "vocês", "vos", "vós", "vossa", "vossas", "vosso", "vossos", "vou", "zero")
# Remover as stop words
corpus <- tm_map(corpus, removeWords, stop_words)
# Criar uma matriz de termos (TF)
dtm <- DocumentTermMatrix(corpus)
# Calcular o IDF com suavização
idf <- log2(1 + (nDocs(dtm) / col_sums(dtm > 0)))
# Calcular TF normalizado (evitar divisão por zero)
row_sums_dtm <- row_sums(as.matrix(dtm))
tf <- as.matrix(dtm) / ifelse(row_sums_dtm > 0, row_sums_dtm, 1)
# Calcular TF-IDF normalizado
tf_idf_normalized <- tf * idf
# Obter os termos mais importantes
tf_idf_scores <- col_sums(tf_idf_normalized)
top_terms <- sort(tf_idf_scores, decreasing = TRUE)[1:20]
# Exibir resultados
data.frame(Termo = names(top_terms), Score = top_terms)