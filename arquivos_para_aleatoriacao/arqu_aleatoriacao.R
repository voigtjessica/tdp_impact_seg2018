#Avaliação de impacto do Umberto:

#Lista das obras

`%notin%` = function(x,y) !(x %in% y)

not_project<- c("COBERTURA DE QUADRA ESCOLAR - PROJETO PRÓPRIO",
                "COBERTURA DE QUADRA ESCOLAR GRANDE - PROJETO FNDE",
                "COBERTURA DE QUADRA ESCOLAR PEQUENA - PROJETO FNDE",
                "QUADRA ESCOLAR COBERTA - PROJETO PRÓPRIO ",
                "QUADRA ESCOLAR COBERTA COM VESTIÁRIO- PROJETO FNDE",
                "Reforma",
                "QUADRA ESCOLAR COBERTA - PROJETO PRÓPRIO",
                "Ampliação",
                "QUADRA ESCOLAR COBERTA COM PALCO- PROJETO FNDE",
                "Quadra Escolar Coberta e Vestiário - Modelo 2",
                "Ampliação Módulo Tipo B",
                "")

setwd("C:/Users/jvoig/OneDrive/Documentos/levantamentos_estados")
lista_final <- fread(file="lista_final_alertas.csv")

# Pegando as obras do app em 21/08 :
# Temos 4568 obras no app
setwd("C:/Users/jvoig/OneDrive/Documentos/tdp_impact_2_2018/arquivos_para_aleatoriacao")
obras_app <- fread(file="obras_do_app.csv")

# Pegando todas as obras que existem:

obras <- read.csv(url("http://simec.mec.gov.br/painelObras/download.php"), sep=";")

# 1 quais obras que serão fiscalizadas que não estão no app;

app <- obras_app %>%
  select(id) %>%
  mutate(esta_no_app = 1,
         id = as.character(id))

fisc <- lista_final %>%
  select(id, problema_detectado) %>%
  mutate(vai_participar_acao = 1)

obras_total <- obras %>%
  clean_names() %>%
  filter(situacao != "Obra Cancelada",
         situacao != "Concluída") %>%
  mutate(id = as.character(id),
         tipo_do_projeto = as.character(tipo_do_projeto)) %>%
  select(1,2,3,4,9,10,11,12,13,15,14,18,19,47,48) %>%
  left_join(app) %>%
  mutate(esta_no_app = ifelse(is.na(esta_no_app), 0, esta_no_app)) %>%
  left_join(fisc) %>%
  mutate(vai_participar_acao = ifelse(is.na(vai_participar_acao), 0, vai_participar_acao)) %>%
  filter(tipo_do_projeto %notin% not_project)


save(obras_total, file="obras_total.Rdata")

# 2 frequencia de obras atrasadas que não estão no app

freq <- obras_total %>%
  summarise(obras_total = n(),
            obras_no_app = sum(esta_no_app),
            obras_acao = sum(vai_participar_acao),
            vai_participar_acao_no_app = sum(vai_participar_acao[which(esta_no_app == 1)]),
            vai_participar_acao_sem_app = sum(vai_participar_acao[which(esta_no_app == 0)]))

freq  




