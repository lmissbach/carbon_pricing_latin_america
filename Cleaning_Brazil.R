if(!require("pacman")) install.packages("pacman")

p_load("tidyverse", "Hmisc", "haven", "readr", "openxlsx", "sjlabelled")

# This script is to clean household data for Brazil (POF 2017)

data_0_clean <- read_dta("../0_Data/1_Household Data/3_Brazil/1_Data_Clean/LAC_Clean/BRA_POF_2017-2018.dta")

# it builds essentially on work by Paulo Bistene Alexandrino
# https://github.com/paulobistenealexandrino/Iniciacao-Cientifica/blob/f7a52cb8efb025590ef2a8d27fb0f1b2d79ef3fd/Leitura%20dos%20Microdados%20-%20R.R

# 1.  Transform TXT to CSV ####

# MORADOR <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/MORADOR.txt",
#            widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,2,1,2,14,14,10,1,1,20,20,20,20),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", 
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
#                            "V0306", "V0401", "V04021", "V04022", "V04023",
#                            "V0403", "V0404", "V0405", "V0406", "V0407",
#                            "V0408", "V0409", "V0410", "V0411", "V0412",
#                            "V0413", "V0414", "V0415", "V0416", 
#                            "V041711", "V041712", "V041721", "V041722",
#                            "V041731", "V041732", "V041741", "V041742",
#                            "V0418", "V0419", "V0420", "V0421", "V0422",
#                            "V0423", "V0424", "V0425", "V0426", "V0427",
#                            "V0428", "V0429", "V0430", "ANOS_ESTUDO",
#                            "PESO", "PESO_FINAL", "RENDA_TOTAL",
#                            "INSTRUCAO", "COMPOSICAO", "PC_RENDA_DISP",
#                            "PC_RENDA_MONET", "PC_RENDA_NAO_MONET",
#                            "PC_DEDUCAO"), dec = ".")   
# 
# DESPESA_COLETIVA <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/DESPESA_COLETIVA.txt",
#            widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1,10,1,12,10,10,1,1,2,14,14,10,5),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
#                            "SEQ", "V9001", "V9002", "V9005", "V8000",
#                            "V9010", "V9011", "V9012", "V1904",
#                            "V1905", "DEFLATOR", "V8000_DEFLA",
#                            "V1904_DEFLA", "COD_IMPUT_VALOR",
#                            "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
#                            "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004"), dec = ".")
# 
# CADERNETA_COLETIVA <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/CADERNETA_COLETIVA.txt",
#            widths = c(2,4,1,9,2,1,2,3,7,2,10,12,10,1,2,14,14,10,9,4,5,9,5),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
#                            "SEQ", "V9001", "V9002", "V8000", "DEFLATOR",
#                            "V8000_DEFLA", "COD_IMPUT_VALOR",
#                            "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
#                            "RENDA_TOTAL",
#                            "V9005", "V9007", "V9009", "QTD_FINAL","V9004"), dec=".")   
# 
# DESPESA_INDIVIDUAL <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/DESPESA_INDIVIDUAL.txt",
#            widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2,2,1,1,1,12,10,1,2,14,14,10,5),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC",
#                            "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
#                            "V9002", "V8000", "V9010", "V9011", "V9012",
#                            "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
#                            "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
#                            "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004"), dec=".")   
# 
# ALUGUEL_ESTIMADO <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/ALUGUEL_ESTIMADO.txt",
#            widths = c(2,4,1,9,2,1,2,7,2,10,2,2,12,10,1,2,14,14,10),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
#                            "V9001", "V9002", "V8000", "V9010", "V9011",
#                            "DEFLATOR", "V8000_DEFLA", "COD_IMPUT_VALOR",
#                            "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
#                            "RENDA_TOTAL"), dec=".")  
# 
# RENDIMENTO_TRABALHO <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/RENDIMENTO_TRABALHO.txt",
#            widths = c(2,4,1,9,2,1,2,2,1,1,7,1,1,1,1,1,1,7,7,7,7,2,2,3,1,12,10,10,10,10,1,1,14,14,10,4,5),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC",
#                            "COD_INFORMANTE", "QUADRO", "SUB_QUADRO",
#                            "SEQ", "V9001", "V5302", "V53021", "V5303",
#                            "V5304", "V5305", "V5307", "V8500", "V531112",
#                            "V531122", "V531132", "V9010", "V9011",
#                            "V5314", "V5315", "DEFLATOR", "V8500_DEFLA",
#                            "V531112_DEFLA", "V531122_DEFLA",
#                            "V531132_DEFLA", "COD_IMPUT_VALOR",
#                            "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
#                            "RENDA_TOTAL","V53011","V53061"), dec=".")
# 
# OUTROS_RENDIMENTOS <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/OUTROS_RENDIMENTOS.txt", 
#            widths = c(2,4,1,9,2,1,2,2,2,7,10,10,2,2,12,10,10,1,1,14,14,10),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC",
#                            "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
#                            "V8500", "V8501", "V9010", "V9011",
#                            "DEFLATOR", "V8500_DEFLA", "V8501_DEFLA",
#                            "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
#                            "PESO", "PESO_FINAL", "RENDA_TOTAL"), dec=".")   
# 
# DOMICILIO <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/DOMICILIO.txt", 
#            widths = c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14,1),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "V0201", "V0202", 
#                            "V0203", "V0204", "V0205", "V0206", "V0207",
#                            "V0208", "V0209", "V02101", "V02102",
#                            "V02103", "V02104", "V02105", "V02111",
#                            "V02112", "V02113", "V0212", "V0213",
#                            "V02141", "V02142", "V0215", "V02161", 
#                            "V02162", "V02163", "V02164", "V0217", 
#                            "V0219", "V0220", "V0221", "PESO",
#                            "PESO_FINAL", "V6199"), dec=".")   
# 
# INVENTARIO <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/INVENTARIO.txt", 
#            widths = c(2,4,1,9,2,1,2,2,7,2,2,4,1,14,14,10),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
#                            "SEQ", "V9001", "V9005", "V9002", "V1404",
#                            "V9012", "PESO", "PESO_FINAL","RENDA_TOTAL"), dec=".")   
# 
# CARACTERISTICAS_DIETA <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/CARACTERISTICAS_DIETA.txt",
#            widths = c(2,4,1,9,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,3,14,15,10),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC",
#                            "COD_INFORMANTE", "V7101", "V7102",
#                            "V71031", "V71032", "V71033", "V71034",
#                            "V71035", "V71036", "V71037", "V71038",
#                            "V7104", "V71051", "V71052", "V71053",
#                            "V71054", "V71055", "V71056", "V71A01",
#                            "V71A02", "V72C01", "V72C02", "PESO",
#                            "PESO_FINAL", "RENDA_TOTAL"), dec=".")   
# 
# CONSUMO_ALIMENTAR <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/CONSUMO_ALIMENTAR.txt",
#            widths = c(2,4,1,9,2,1,2,2,2,4,2,7,3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,7,9,6,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,15,10,15,1),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC",
#                            "COD_INFOR,MANTE", "QUADRO", "SEQ",
#                            "V9005", "V9007", "V9001", "V9015",
#                            "V9016", "V9017", "V9018", "V9019",
#                            "V9020", "V9021", "V9022", "V9023",
#                            "V9024", "V9025", "V9026", "V9027",
#                            "V9028", "V9029", "V9030",
#                            "COD_UNIDADE_MEDIDA_FINAL",
#                            "COD_PREPARACAO_FINAL", "GRAMATURA1",
#                            "QTD", "COD_TBCA", "ENERGIA_KCAL",
#                            "ENERGIA_KJ", "PTN", "CHOTOT", "FIBRA",
#                            "LIP", "COLEST", "AGSAT", "AGMONO",
#                            "AGPOLI", "AGTRANS", "CALCIO", "FERRO",
#                            "SODIO", "MAGNESIO", "FOSFORO", "POTASSIO",
#                            "COBRE", "ZINCO", "VITA_RAE", "TIAMINA",
#                            "RIBOFLAVINA", "NIACINA", "PIRIDOXAMINA",
#                            "COBALAMINA", "VITD", "VITE", "VITC",
#                            "FOLATO", "PESO", "PESO_FINAL",
#                            "RENDA_TOTAL", "DIA_SEMANA", "DIA_ATIPICO"), dec=".")   
# 
# CONDICOES_VIDA <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/CONDICOES_VIDA.txt",
#            widths = c(2,4,1,9,2,1,2,1,6,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14,10),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE",
#                            "V6101", "V6102", "V6103", "V61041", "V61042",
#                            "V61043", "V61044", "V61045", "V61046", 
#                            "V61051", "V61052", "V61053", "V61054",
#                            "V61055", "V61056", "V61057", "V61058",
#                            "V61061", "V61062", "V61063", "V61064",
#                            "V61065", "V61066", "V61067", "V61068",
#                            "V61069", "V610610", "V610611", "V61071",
#                            "V61072", "V61073", "V6108", "V6109",
#                            "V6110", "V6111", "V6112", "V6113", "V6114",
#                            "V6115", "V6116", "V6117", "V6118", "V6119",
#                            "V6120", "V6121", "PESO", "PESO_FINAL",
#                            "RENDA_TOTAL"), dec=".")   
# 
# RESTRICAO_PRODUTOS_SERVICOS_SAUDE <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/RESTRICAO_PRODUTOS_SERVICOS_SAUDE.txt", 
#            widths = c(2,4,1,9,2,1,2,2,2,7,1,14,14,10),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC",
#                            "COD_INFORMANTE", "QUADRO", "SEQ","V9001",
#                            "V9013", "PESO", "PESO_FINAL", "RENDA_TOTAL"), dec=".")   
# 
# SERVICO_NAO_MONETARIO_POF2 <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/SERVICO_NAO_MONETARIO_POF2.txt",
#            widths = c(2,4,1,9,2,1,2,2,7,2,10,2,2,10,1,12,10,10,1,2,14,14,10,5),
#            na.strings = c(" "),
#            col.names  = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
#                            "SEQ", "V9001", "V9002", "V8000", "V9010",
#                            "V9011", "V1904", "V1905", "DEFLATOR",
#                            "V8000_DEFLA", "V1904_DEFLA", "COD_IMPUT_VALOR",
#                            "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
#                            "RENDA_TOTAL","V9004"), dec=".")   
# 
# SERVICO_NAO_MONETARIO_POF4 <- 
#   read.fwf("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/POF 2017/Dados_20210304/SERVICO_NAO_MONETARIO_POF4.txt",
#            widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2,2,1,1,12,10,1,2,14,14,10,5),
#            na.strings = c(" "),
#            col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
#                            "COD_UPA", "NUM_DOM", "NUM_UC", 
#                            "COD_INFORMANTE", "QUADRO", "SEQ",
#                            "V9001", "V9002", "V8000", "V9010", "V9011",
#                            "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
#                            "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
#                            "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004"), dec=".") 
# 
# saveRDS(CONSUMO_ALIMENTAR,                 "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Consumo_Alimentar.Rds")
# saveRDS(DESPESA_INDIVIDUAL,                "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Despesa_Individual.Rds")
# saveRDS(CADERNETA_COLETIVA,                "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Caderneta_Coletiva.Rds")
# saveRDS(INVENTARIO,                        "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Inventario.Rds")
# saveRDS(DESPESA_COLETIVA,                  "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Despesa_Coletiva.Rds")
# saveRDS(MORADOR,                           "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Morador.Rds")
# saveRDS(OUTROS_RENDIMENTOS,                "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Outros_Rendimentos.Rds")
# saveRDS(RENDIMENTO_TRABALHO,               "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Rendimento_Trabalho.Rds")
# saveRDS(SERVICO_NAO_MONETARIO_POF4,        "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Servicio_Nao_Monetario_POF4.Rds")
# saveRDS(CONDICOES_VIDA,                    "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Condicoes_Vida.Rds")
# saveRDS(ALUGUEL_ESTIMADO,                  "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Aluguel_Estimado.Rds")
# saveRDS(DOMICILIO,                         "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Domicilio.Rds")
# saveRDS(CARACTERISTICAS_DIETA,             "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Caracteristicas_Dieta.Rds")
# saveRDS(RESTRICAO_PRODUTOS_SERVICOS_SAUDE, "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Restricao_Produtos_Servicios_Saude.Rds")
# saveRDS(SERVICO_NAO_MONETARIO_POF2,        "../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Servicio_Nao_Monetario_POF2.Rds")

# 1.0 Load Data ####

consumo_alimentar_0   <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Consumo_Alimentar.Rds")                  # Nutrition
despesa_individual_0  <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Despesa_Individual.Rds")
caderneta_coletiva_0  <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Caderneta_Coletiva.Rds")
inventario_0          <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Inventario.Rds")                         # Information on Appliances
despesa_coletiva_0    <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Despesa_Coletiva.Rds")
morador_0             <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Morador.Rds")                            # Information on Household
outros_rendimentos_0  <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Outros_Rendimentos.Rds")                 # Other Income
rendimento_trabalho_0 <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Rendimento_Trabalho.Rds")                # Information on Work and Work Income
servicio_nao_4_0      <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Servicio_Nao_Monetario_POF4.Rds")        # Informal Services Expenditures
condicoes_vida_0      <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Condicoes_Vida.Rds")
aluguel_estimado_0    <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Aluguel_Estimado.Rds")                   # Imputed rents
domicilio_0           <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Domicilio.Rds")                          # Information on Housing
caracter_dieta_0      <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Caracteristicas_Dieta.Rds")
rest_prod_serv_0      <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Restricao_Produtos_Servicios_Saude.Rds") # 
servicio_nao_2_0      <- readRDS("../0_Data/1_Household Data/3_Brazil/1_Data_Raw/Servicio_Nao_Monetario_POF2.Rds")        # Informal Services Expenditures

# 2.  Transform Data ####


# 2.0 Incomes ####

outros_rendimentos_1 <- outros_rendimentos_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  filter(V8500 < 9999999)%>%
  filter(COD_IMPUT_VALOR == 0)%>%
  select(hh_id, V9001, V8500_DEFLA, V9011, QUADRO)%>%
  mutate(income = ifelse(QUADRO == 54, V8500_DEFLA*V9011, V8500_DEFLA))

# income government conditional or unconditional cash transfers
# income government other monetary income than cash transfers

Income_Type   <- distinct(outros_rendimentos_1, V9001)%>%
  mutate(V9001 = as.character(V9001))
Income_Type.2 <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Documentation/Cadastro de Produtos.xlsx")

colnames(Income_Type.2) <- c("Quadro", "V9001", "Name")

Income_Type <- left_join(Income_Type, Income_Type.2)%>%
  arrange(V9001)%>%
  mutate(Name = tolower(Name))
# write.xlsx(Income_Type, "../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Documentation/Income_Types.xlsx")

Income_Type <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Documentation/Income_Types.xlsx")%>%
  select(V9001, Category)%>%
  filter(!is.na(Category))

outros_rendimentos_2 <- outros_rendimentos_1 %>%
  mutate(V9001 = as.character(V9001))%>%
  left_join(Income_Type)%>%
  filter(!is.na(Category))%>%
  group_by(hh_id, Category)%>%
  summarise(income = sum(income))%>%
  ungroup()%>%
  pivot_wider(names_from = "Category", values_from = "income", values_fill = 0)%>%
  rename(inc_gov_cash = cash, inc_gov_monetary = other)

# 2.1 Household Information ####

# a) Household

morador_1 <- morador_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  rename(hh_weights = PESO_FINAL)%>%
  select(hh_id, everything())

morador_1.1 <- morador_1 %>%
  filter(COD_INFORMANTE == "1")%>%
  rename(age_hhh = V0403, sex_hhh = V0404, ethnicity = V0405, edu_hhh = V0425, edu_hhh.b = V0419)%>%
  select(hh_id, age_hhh, sex_hhh, ethnicity, edu_hhh, edu_hhh.b)%>%
  mutate(help = 0)%>%
  unite(edu_hhh.c, c(edu_hhh.b, help), remove = FALSE, sep = "")%>%
  mutate(edu_hhh = ifelse(is.na(edu_hhh), edu_hhh.c, edu_hhh))%>%
  select(-edu_hhh.c, -edu_hhh.b, -help)%>%
  mutate(edu_hhh = ifelse(edu_hhh == "NA0", NA, edu_hhh))

morador_1.2 <- morador_1 %>%
  mutate(adults   = ifelse(V0403 > 15,1,0),
         children = ifelse(V0403 < 16,1,0))%>%
  group_by(hh_id)%>%
  summarise(hh_size  = n(),
            adults   = sum(adults),
            children = sum(children))%>%
  ungroup()

morador_1.3 <- morador_1 %>%
  select(hh_id, UF, TIPO_SITUACAO_REG, ESTRATO_POF, hh_weights)%>%
  mutate(urban_01 = ifelse(TIPO_SITUACAO_REG == 1,1,0))%>%
  rename(province = UF, district = ESTRATO_POF)%>%
  select(hh_id, hh_weights, urban_01, province, district)%>%
  distinct()

domicilio_1 <- domicilio_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  rename(water = V0207, toilet = V0212)%>%
  mutate(Heating_Fuel = ifelse(V02101 == 1, "Electricity",
                               ifelse(V02102 == 1, "Gas",
                                      ifelse(V02103 == 1, "Solar",
                                             ifelse(V02104 == 1, "Firewood or Coal",
                                                    ifelse(V02105 == 1, "Other", NA))))))%>%
  mutate(electricity.access = ifelse(V02141 == 1,1,0))%>%
  mutate(Cooking_Fuel = ifelse(V02161 == 1, "LPG",
                               ifelse(V02162 == 1, "Firewood or Coal",
                                      ifelse(V02163 == 1, "Electricity",
                                             ifelse(V02164 == 1, "Other (Oil, Kerosene)", "Unknown")))))%>%
  select(hh_id, water, toilet, Heating_Fuel, Cooking_Fuel, electricity.access)

Cooking.Code <- distinct(domicilio_1, Cooking_Fuel)%>%
  arrange(Cooking_Fuel)%>%
  mutate(cooking_fuel = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Cooking.Code.csv")

Heating.Code <- distinct(domicilio_1, Heating_Fuel)%>%
  arrange(Heating_Fuel)%>%
  mutate(heating_fuel = 1:n())%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Heating.Code.csv")

domicilio_2 <- domicilio_1 %>%
  left_join(Cooking.Code)%>%
  left_join(Heating.Code)%>%
  select(-Heating_Fuel, - Cooking_Fuel)

# b) Persons

# REDIMENTO_TRABALHO on industry of household head

personas_1 <- rendimento_trabalho_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  filter(COD_INFORMANTE == "1")%>%
  filter(SUB_QUADRO == 1)%>%
  rename(ind_hhh = V53061)%>% # ind_hhh.b = V53061 would be occupation
  select(hh_id, ind_hhh)

# c) Appliances

# INVENTARIO

appliances_1 <- inventario_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  select(hh_id, V9001, V9005)%>%
  mutate(cooker.01          = ifelse(V9001 == 1400101,1,0),
         refrigerator.01    = ifelse(V9001 == 1400201 | V9001 == 1400301 | V9001 == 1400401,1,0),
         dishwasher.01      = ifelse(V9001 == 1400801,1,0),
         microwave.01       = ifelse(V9001 == 1400901,1,0),
         oven.01            = ifelse(V9001 == 1401001,1,0),
         washing_machine.01 = ifelse(V9001 == 1401201,1,0),
         tv.01              = ifelse(V9001 == 1401301 | V9001 == 1401401,1,0),
         radio.01           = ifelse(V9001 == 1401601,1,0),
         computer.01        = ifelse(V9001 == 1401901 | V9001 == 1402001,1,0),
         ac.01              = ifelse(V9001 == 1402101,1,0),
         fan.01             = ifelse(V9001 == 1402201,1,0),
         car.01             = ifelse(V9001 == 1403001,1,0),
         motorcycle.01      = ifelse(V9001 == 1403101,1,0))%>%
  group_by(hh_id)%>%
  summarise(cooker.01          = ifelse(sum(cooker.01)>0,1,0),
            refrigerator.01    = ifelse(sum(refrigerator.01)>0,1,0),
            dishwasher.01      = ifelse(sum(dishwasher.01)>0,1,0),
            microwave.01       = ifelse(sum(microwave.01)>0,1,0),
            oven.01            = ifelse(sum(oven.01)>0,1,0),
            washing_machine.01 = ifelse(sum(washing_machine.01)>0,1,0),
            tv.01              = ifelse(sum(tv.01)>0,1,0),
            radio.01           = ifelse(sum(radio.01)>0,1,0),
            computer.01        = ifelse(sum(computer.01)>0,1,0),
            ac.01              = ifelse(sum(ac.01)>0,1,0),
            fan.01             = ifelse(sum(fan.01)>0,1,0),
            car.01             = ifelse(sum(car.01)>0,1,0),
            motorcycle.01      = ifelse(sum(motorcycle.01)>0,1,0))%>%
  ungroup()

write_csv(appliances_1, "../0_Data/1_Household Data/3_Brazil/1_Data_Clean/appliances_0_1_Brazil.csv")

Appliance.Code <- appliances_1 %>%
  distinct(V9001)%>%
  arrange(V9001)%>%
  bind_cols(Appliance = c("Cooker", "Freezer", "Refrigerator", "Refrigerator", "Shower", "Water Filter", "Water Purifier",
                          "Dishwasher", "Microwave", "Oven", "Iron", "Washing Machine", "TV", "TV", "Sound", "Radio", "DVD", "Antenna",
                          "Computer", "Computer", "AC", "Fan", "Bed", "Bed", "Cabinet", "Cabinet", "Table", "Sofa", "Chair",
                          "Car", "Motorcycle", "Bike"))%>%
  write.xlsx(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Appliance.Codes.xlsx")


# d) Save Output

household_information_1 <- left_join(morador_1.3, morador_1.2)%>%
  left_join(morador_1.1)%>%
  left_join(domicilio_2)%>%
  left_join(personas_1)%>%
  left_join(outros_rendimentos_2)%>%
  mutate(inc_gov_cash     = ifelse(is.na(inc_gov_cash),0,inc_gov_cash),
         inc_gov_monetary = ifelse(is.na(inc_gov_monetary),0,inc_gov_monetary))

write_csv(household_information_1, "../0_Data/1_Household Data/3_Brazil/1_Data_Clean/household_information_Brazil.csv")

# 2.2 Expenditure Information ####

# Imputed rents - hence not considered
# aluguel_estimado_1 <- aluguel_estimado_0 %>%
#   unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)

despesa_individual_1 <- despesa_individual_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  rename(item_code = V9001, time_type = V9011, expenditures = V8000)%>%
  mutate(sp = ifelse(V9002 %in% c(7,8,9,10,11),1,0))%>%
  filter(COD_IMPUT_VALOR == 0)%>%
  select(hh_id, item_code, expenditures, time_type, sp, V8000_DEFLA, FATOR_ANUALIZACAO, QUADRO)%>%
  filter(expenditures <= 9999999)%>%
  mutate(expenditures_year    = ifelse(sp == 0 & is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO, 
                                       ifelse(sp == 0 & !is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO*time_type,0)),
         expenditures_sp_year = ifelse(sp == 1 & is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO, 
                                       ifelse(sp == 1 & !is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO*time_type,0)))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  mutate(type = "DI")

caderneta_coletiva_1 <- caderneta_coletiva_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  rename(item_code = V9001)%>%
  mutate(sp = ifelse(V9002 %in% c(7,8,9,10,11),1,0))%>%
  filter(COD_IMPUT_VALOR == 0)%>%
  filter(V8000 <= 9999999)%>%
  mutate(expenditures_year    = ifelse(sp == 0, V8000_DEFLA*FATOR_ANUALIZACAO,0),
         expenditures_sp_year = ifelse(sp == 1, V8000_DEFLA*FATOR_ANUALIZACAO,0))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  mutate(type = "CC")
  
despesa_coletiva_1   <- despesa_coletiva_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  filter(COD_IMPUT_VALOR == 0)%>%
  filter(V8000 <= 9999999)%>%
  mutate(sp = ifelse(V9002 %in% c(7,8,9,10,11),1,0))%>%
  rename(item_code = V9001, time_type = V9011)%>%
  mutate(V1904_DEFLA = ifelse(is.na(V1904_DEFLA),0,V1904_DEFLA))%>%
  mutate(V8000_DEFLA = V8000_DEFLA + V1904_DEFLA)%>%
  mutate(expenditures_year = ifelse(sp == 0 & !is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO*time_type,
                                    ifelse(sp == 0 & is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO,0)),
         expenditures_sp_year = ifelse(sp == 1 & !is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO*time_type,
                                       ifelse(sp == 1 & is.na(time_type), V8000_DEFLA*FATOR_ANUALIZACAO,0)))%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  mutate(type = "DC")

servicio_2.1 <- servicio_nao_2_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  filter(V8000 <= 9999999)%>%
  filter(COD_IMPUT_VALOR == 0)%>%
  mutate(V8000_DEFLA = ifelse(is.na(V1904_DEFLA),V8000_DEFLA, V8000_DEFLA+V1904_DEFLA))%>%
  mutate(expenditures_sp_year = ifelse(is.na(V9011), V8000_DEFLA*FATOR_ANUALIZACAO,
                                       ifelse(!is.na(V9011), V8000_DEFLA*FATOR_ANUALIZACAO*V9011,0)))%>%
  rename(item_code = V9001)%>%
  mutate(expenditures_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  mutate(type = "S2")
  
servicio_4.1 <- servicio_nao_4_0 %>%
  unite(hh_id, c(COD_UPA, NUM_DOM), sep = "", remove = FALSE)%>%
  filter(V8000 >= 9999999)%>%
  filter(COD_IMPUT_VALOR == 0)%>%
  mutate(expenditures_sp_year = ifelse(is.na(V9011), V8000_DEFLA*FATOR_ANUALIZACAO,
                                       ifelse(!is.na(V9011), V8000_DEFLA*FATOR_ANUALIZACAO*V9011,0)))%>%
  rename(item_code = V9001)%>%
  mutate(expenditures_year = 0)%>%
  select(hh_id, item_code, expenditures_year, expenditures_sp_year)%>%
  arrange(hh_id, item_code)%>%
  mutate(type = "S4")

expenditures_items <- bind_rows(despesa_individual_1, despesa_coletiva_1)%>%
  bind_rows(caderneta_coletiva_1)%>%
  bind_rows(servicio_2.1)%>%
  bind_rows(servicio_4.1)%>%
  arrange(hh_id, item_code)%>%
  mutate(item_code = as.character(item_code))%>%
  left_join(select(Item.Codes.final, item_code, item_code_new))%>%
  select(hh_id, item_code_new, expenditures_year, expenditures_sp_year, type)%>%
  arrange(hh_id, item_code_new)%>%
  # for now: aggregate payments per item code
  group_by(hh_id, item_code_new)%>%
  summarise(expenditures_year    = sum(expenditures_year),
            expenditures_sp_year = sum(expenditures_sp_year))%>%
  ungroup()%>%
  rename(item_code = item_code_new)%>%
  filter(!is.na(item_code))

#TBD on how to deal with duplicate information - should be okay, if this is repeated payments

write_csv(expenditures_items, "../0_Data/1_Household Data/3_Brazil/1_Data_Clean/expenditures_items_Brazil.csv")


# 2.3 Item Codes ####

Item.Codes.joint <- arrange(distinct(despesa_individual_1, item_code))%>%
  bind_rows(arrange(distinct(caderneta_coletiva_1, item_code)))%>%
  bind_rows(arrange(distinct(despesa_coletiva_1, item_code)))%>%
  bind_rows(arrange(distinct(servicio_2.1, item_code)))%>%
  bind_rows(arrange(distinct(servicio_4.1, item_code)))%>%
  arrange(item_code)%>%
  distinct(item_code)%>%
  mutate(item_code = as.character(item_code))

Item.Codes.1 <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Documentation/Cadastro de Produtos.xlsx")%>%
  rename(item_code = "CÓDIGO.DO.PRODUTO")

Item.Codes.0 <- left_join(Item.Codes.joint, Item.Codes.1, by = "item_code")%>%
  arrange(item_code)%>%
  rename(item_name = "DESCRIÇÃO.DO.PRODUTO")%>%
  mutate(item_name = tolower(item_name))%>%
  mutate(item_type = nchar(item_code))%>%
  mutate(Codigo    = ifelse(item_type == 6, str_sub(item_code,1,4),
                              ifelse(item_type == 7, str_sub(item_code,1,5), "FAIL")))

Item.Codes.2 <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Supplementary/Tradutor_Despesa_Geral.xlsx")%>%
  unite(Description, c(Descricao_3, Descricao_4, Descricao_5), sep = ", ", remove = FALSE)%>%
  select(Codigo, Description)%>%
  mutate(Codigo = as.character(Codigo),
         item_type = nchar(Codigo))

Item.Codes.3 <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Supplementary/Tradutor_Aquisicao_Alimentar.xlsx")%>%
  unite(Description_B, c(Descricao_2, Descricao_3), sep = ", ", remove = FALSE)%>%
  select(Codigo, Description_B)%>%
  mutate(Codigo = as.character(Codigo))

Item.Codes.4 <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Supplementary/Tradutor_Alimentacao.xlsx")%>%
  unite(Description_C, c(Descricao_2, Descricao_3), sep = ", ", remove = FALSE)%>%
  select(Codigo, Description_C)%>%
  mutate(Codigo = as.character(Codigo))

Item.Codes.final <- left_join(Item.Codes.0, Item.Codes.2, by = "Codigo")%>%
  left_join(Item.Codes.3, by = "Codigo")%>%
  left_join(Item.Codes.4, by = "Codigo")%>%
  arrange(item_code)%>%
  select(item_code, item_name, QUADRO, Description, Description_B, Description_C)%>%
  filter(Description != "Contribuições trabalhistas, NA, NA")%>%
  mutate(item_code_new = 1:n())%>%
  select(item_code_new, everything())

# write.xlsx(Item.Codes.final, "../0_Data/1_Household Data/3_brazil/3_Matching_Tables/Item_Codes_Description_Brazil_1.xlsx")



# 3. Codes ####

Ethnicity.Code <- distinct(morador_1.1, ethnicity)%>%
  arrange(ethnicity)%>%
  mutate(Ethnicity = c("Branca", "Preta", "Amarela", "Parda", "Indigena", "Sem declaracao"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Ethnicity.Code.csv")

Education.Code <- distinct(morador_1.1, edu_hhh)%>%
  mutate(edu_hhh = as.numeric(edu_hhh))%>%
  arrange(edu_hhh)%>%
  bind_cols(Education = c("Creche", "Pre-Escola", "Classe de Alfabetizacao", "Alfabetizcap de Jovens e Adultos", "Antigo Primario", "Antigo Ginasial (media 1 ciclo)",
                          "Regiular Do Ensino Fundamental Ou Do 1 GRAU", "Educacao de Jovens e Adultos - Eja do ensino fundamental ou supletivo do 1 grau",
                          "ANtigo cientifico, classico (medio 2 ciclo)", "Regular do ensino medio ou do 2 grau", "Educacao de jovens e adultos - eja do ensino medio ou supletivo do 2 grau",
                          "Superior - graduacao", "Especializadcao ne nivel superior", "Mestrado", "Doutorado",
                          "Classe de Alfabetizacao (Currently)", "Alfabetizcap de Jovens e Adultos (Currently)", "Antigo Primario (Currently)", "Antigo Ginasial (media 1 ciclo) (Currently)",
                          "Regiular Do Ensino Fundamental Ou Do 1 GRAU (Currently)", "Educacao de Jovens e Adultos - Eja do ensino fundamental ou supletivo do 1 grau (Currently)",
                          "ANtigo cientifico, classico (medio 2 ciclo) (Currently)", "Regular do ensino medio ou do 2 grau (Currently)", "Educacao de jovens e adultos - eja do ensino medio ou supletivo do 2 grau (Currently)", NA))%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Education.Code.csv")

#Spatial.Code <- read_delim("../0_Data/1_Household Data/3_Brazil/9_Documentation/Supplementary/Tradutor_Estratos.csv", delim =";")%>%
#  pivot_longer(INICIO:FINAL, names_to = "type", values_to = "district")
#District.Code <- distinct(morador_1.3, district)%>%
#  arrange(district)%>%
#  left_join(Spatial.Code, by = "district")%>%
#  write.xlsx(., "../0_Data/1_Household Data/3_Brazil/2_Codes/District.Code.xlsx")
    
District.Code <- read.xlsx("../0_Data/1_Household Data/3_Brazil/2_Codes/District.Code.xlsx")%>%
  select(-type)%>%
  unite(District, c(NOME, LOCAL), sep = "_", remove = FALSE)%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/District.Code.csv")

District.Code.2 <- District.Code %>%
  mutate(province = as.numeric(str_sub(district,1,2)))%>%
  select(province, NOME)%>%
  distinct()

Province.Code <- distinct(morador_1.3, province)%>%
  arrange(province)%>%
  left_join(District.Code.2)%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Province.Code.csv")

Gender.Code <- distinct(morador_1.1, sex_hhh)%>%
  mutate(Gender = c("Male", "Female"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Gender.Code.csv")

Water.Code <- distinct(domicilio_2, water)%>%
  arrange(water)%>%
  bind_cols(Water = c("General Distribution Network", "Deep or Artesian Well", "Shallow Well, Phreatic or Pipe", "Source or Spring", "Rainwater", "Other"))%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Water.Code.csv")

Toilet.Code <- distinct(domicilio_2, toilet)%>%
  arrange(toilet)%>%
  bind_cols(Toilet = c("Main Network", "Fossa not connected to network", "Ditch", "River, Lake or Sea", "Other", NA))%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Toilet.Code.csv")

Industry.Code.0 <- read.xlsx("../0_Data/1_Household Data/3_Brazil/9_Documentation/0_Documentation/Atividade CNAE Domiciliar 2.0.xlsx", startRow = 3)%>%
  rename(ind_hhh = Classe, Industry = 'Denominação')%>%
  select(ind_hhh, Industry)%>%
  filter(!is.na(ind_hhh))%>%
  mutate(ind_hhh = as.numeric(ind_hhh))

Industry.Code <- distinct(personas_1, ind_hhh)%>%
  arrange(ind_hhh)%>%
  left_join(Industry.Code.0)%>%
  write_csv(., "../0_Data/1_Household Data/3_Brazil/2_Codes/Industry.Code.csv")
