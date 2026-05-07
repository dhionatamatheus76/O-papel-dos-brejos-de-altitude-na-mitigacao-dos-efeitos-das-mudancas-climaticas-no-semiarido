# Dados da Serra das Almas
sa_chuvoso <- read.csv('CO2_SA_chuvoso.csv')
sa_seco <- read.csv('CO2_SA_seco.csv')
ent_sa_chuvoso <- read.csv('entorno_SA_chuvoso.csv')
ent_sa_seco <- read.csv('entorno_SA_seco.csv')

#Dados de Ubajara
ubj_chuvoso <- read.csv('CO2_UBJ_chuvoso.csv')
ubj_seco <- read.csv('CO2_UBJ_seco.csv')
ent_ubj_chuvoso <- read.csv('entorno_UBJ_chuvoso.csv')
ent_ubj_seco <- read.csv('entorno_UBJ_seco.csv')

#teste de normalidade
library(dplyr)

#UBJ
shapiro.test(ubj_chuvoso$CO2FLUX8) #não normal p-value = 1.874e-15
shapiro.test(ubj_seco$CO2FLUX8) #não normal p-value = 0.000407
shapiro.test(ent_ubj_chuvoso$CO2FLUX) #não normal p-value = 1.078e-06
shapiro.test(ent_sa_seco$CO2FLUX8) #não normal p-value = 4.194e-05

#SA
shapiro.test(sa_chuvoso$CO2FLUX8) #não normal p-value = 4.707e-06
shapiro.test(sa_seco$CO2FLUX8) #não normal p-value = 1.226e-05
shapiro.test(ent_sa_chuvoso$CO2FLUX8) #não normal p-value = 2.751e-08
shapiro.test(ent_ubj_seco$CO2FLUX8) #não normal p-value = 0.0006727

#comparação de ubajara com o entorno por perído
wilcox.test(ubj_chuvoso$CO2FLUX8,
            ent_ubj_chuvoso$CO2FLUX8) # p-value < 2.2e-16, há diferença
wilcox.test(ubj_seco$CO2FLUX8, 
            ent_ubj_seco$CO2FLUX8) #p-value = 6.82e-08, há diferença
wilcox.test(ubj_chuvoso$CO2FLUX8, 
            ubj_seco$CO2FLUX8) #p-value = 0.0004668 há diferença
#mediana para comparação
median(ubj_chuvoso$CO2FLUX8) # m =  0.1891681
median(ent_ubj_chuvoso$CO2FLUX) # m =  0.1602431
median(ubj_seco$CO2FLUX8) # m = 0.1570123
median(ent_ubj_seco$CO2FLUX8) # m = 0.1394041

#comparação da Serra das Almas com seu entorno por período
wilcox.test(sa_chuvoso$CO2FLUX8, 
            ent_sa_chuvoso$CO2FLUX8) # p-value = 0.1319, não há diferença
wilcox.test(sa_seco$CO2FLUX8, 
            ent_sa_seco$CO2FLUX8) # p-value = 2.495e-11, há diferença
wilcox.test(sa_chuvoso$CO2FLUX8, 
            sa_seco$CO2FLUX8) #p-value < 2.2e-16 há diferença

#mediana para comparação
median(sa_chuvoso$CO2FLUX8) # m= 0.1653048
median(sa_seco$CO2FLUX8) # m= 0.1125049
median(ent_sa_chuvoso$CO2FLUX8) # m= 0.1697855
median(ent_sa_seco$CO2FLUX8) # m= 0.09442156

#comparação UCs
wilcox.test(ubj_chuvoso$CO2FLUX8, 
            sa_chuvoso$CO2FLUX8) #há diferença, p-value = 9.099e-05
wilcox.test(ubj_seco$CO2FLUX8, 
            sa_seco$CO2FLUX8) #há diferença, p-value < 2.2e-16


#desvio padrão ubj
dp_ubjc <- sd(ubj_chuvoso$CO2FLUX8) #0.05669
dp_ubjs <- sd(ubj_seco$CO2FLUX8) #0.03042
dp_entubjc <- sd(ent_ubj_chuvoso$CO2FLUX) #0.48097
dp_entubjs <- sd(ent_ubj_seco$CO2FLUX8) #0.02952

#desvio padrão SA
dp_sac <- sd(sa_chuvoso$CO2FLUX8) #0.37163
dp_sas <- sd(sa_seco$CO2FLUX8) #0.01890
dp_entsac <- sd(ent_sa_chuvoso$CO2FLUX8) #0.37471
dp_entsas <- sd(ent_sa_seco$CO2FLUX8) #0.01410

#bloxplot ubj e entorno

library(ggplot2)
library(patchwork)
#construção do gráfico 
dados_ubj1 <- data.frame(
  Valor = c(ubj_chuvoso$CO2FLUX8, ent_ubj_chuvoso$CO2FLUX), 
  Area = c(
    rep('UC', length(ubj_chuvoso$CO2FLUX8)), 
    rep('Entorno', length(ent_ubj_chuvoso$CO2FLUX))
  )
)
View(dados_ubj1)
dados_ubj1$Area <- factor(dados_ubj1$Area, levels = c("UC", "Entorno"))
ggplot(dados_ubj1, aes(x = Area, y = Valor,  fill = Area)) + 
       geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.4) +
       theme_minimal() +
  scale_fill_manual(values = c("UC" = "#A8DCAB", "Entorno" = "#B3B3B3")) +
       labs(title = "PARNA de Ubajara x Entorno PARNA de Ubajara",
            subtitle = "Período Chuvoso (2013-2023)",
            x = "Área",
            y = "Índice de CO2flux")

#gráfico período seco
dados_ubj2 <- data.frame(
             Valor = c(ubj_seco$CO2FLUX8, ent_ubj_seco$CO2FLUX8),
             Area = c(
                      rep('UC',length(ubj_seco$CO2FLUX8)),
                      rep('Entorno',length(ent_ubj_seco$CO2FLUX8))
                      ))
dados_ubj2$Area <- factor(dados_ubj2$Area, levels = c("UC", "Entorno"))
ggplot(dados_ubj2, aes(x = Area, y = Valor,  fill = Area)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.4) +
  theme_minimal() +
  scale_fill_manual(values = c("UC" = "#E1F943", "Entorno" = "#B3B3B3")) +
  labs(title = "PARNA de Ubajara x Entorno PARNA de Ubajara",
       subtitle = "Período Seco (2013-2023)",
       x = "Área",
       y = "Índice de CO2flux")

#gráfico nos dois períodos 

dados_ubj3 <- data.frame(
              Valor = c(ubj_chuvoso$CO2FLUX8,  ubj_seco$CO2FLUX8),
              Area = c(
                rep('UC Chuvoso', length(ubj_chuvoso$CO2FLUX8)),
                rep('UC Seco', length(ubj_seco$CO2FLUX8))
              )
)
ggplot(dados_ubj3, aes(x = Area, y = Valor,  fill = Area)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.4) +
  theme_minimal() +
  scale_fill_manual(values = c("UC Seco" = "#E1F943", "UC Chuvoso" = "#A8DCAB")) +
  labs(title = "PARNA de Ubajara por período",
       subtitle = "(2013-2023)",
       x = "Área",
       y = "Índice de CO2flux")


#boxplot serra das almas e entorno

dados_sa <- data.frame(
            Valor = c(sa_chuvoso$CO2FLUX8, ent_sa_chuvoso$CO2FLUX8),
            Area = c(
              rep('UC', length(sa_chuvoso$CO2FLUX8)),
              rep('Entorno',  length(ent_sa_chuvoso$CO2FLUX8))
            )
)
dados_sa$Area <- factor(dados_sa$Area, levels = c("UC", "Entorno"))
ggplot(dados_sa, aes(x = Area, y = Valor,  fill = Area)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.4) +
  theme_minimal() +
  scale_fill_manual(values = c("UC" = "#fdb462", "Entorno" = "#80b1d3")) +
  labs(title = "RPPN Serra das Almas x Entorno RPPN Serra das Almas",
       subtitle = "Período Chuvoso (2013-2023)",
       x = "Área",
       y = "Índice de CO2flux")


dados_sa1 <-  data.frame(
              Valor = c(sa_seco$CO2FLUX8, ent_sa_seco$CO2FLUX8),
              Area = c(
                rep("UC", length(sa_seco$CO2FLUX8)),
                rep('Entorno', length(ent_sa_seco$CO2FLUX8))
              )
)

dados_sa1$Area <- factor(dados_sa1$Area, levels = c("UC", "Entorno"))
ggplot(dados_sa1, aes(x = Area, y = Valor,  fill = Area)) + 
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.4) +
  theme_minimal() +
  scale_fill_manual(values = c("UC" = "#DB9D00", "Entorno" = "#80b1d3")) +
  labs(title = "RPPN Serra das Almas x Entorno RPPN Serra das Almas",
       subtitle = "Período Seco (2013-2023)",
       x = "Área",
       y = "Índice de CO2flux")


dados_sa2 <- data.frame(
             Valor = c(sa_chuvoso$CO2FLUX8, sa_seco$CO2FLUX8),
             Area = c(
               rep('UC Chuvoso', length(sa_chuvoso$CO2FLUX8)),
               rep('UC Seco', length(sa_seco$CO2FLUX8))
             )
)

ggplot(dados_sa2, aes(x= Area, y= Valor, fill = Area)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA,  width = 0.4) +
  theme_minimal() + 
  scale_fill_manual(values = c('UC Chuvoso' = '#fdb462','UC Seco'= '#DB9D00'))+
  labs(title = 'RPPN Serra das Almas por Período',
       subtitle = '(2013-2023)',
       x = 'Área',
       y = 'Índice de CO2flux'
       )

#Comparação das duas unidades

dados <- data.frame(
         Valor = c(sa_chuvoso$CO2FLUX8, ubj_chuvoso$CO2FLUX8),
         Area = c(
           rep('SA Chuvoso', length(sa_chuvoso$CO2FLUX8)),
           rep('UBJ Chuvoso', length(ubj_chuvoso$CO2FLUX8))
         )
)

ggplot(dados, aes(x = Area, y = Valor, fill = Area))+
  geom_boxplot(alpha=0.6, outlier.shape = NA, width = 0.4) + 
  theme_minimal() +
  scale_fill_manual(values =  c('SA Chuvoso' = '#fdb462', 'UBJ Chuvoso'= '#A8DCAB'))+
  labs(title = 'RPPN Serra das Almas x PARNA de Ubajara',
       subtitle = 'Período Chuvoso (2013-2023)',
       x = 'Área',
       y = 'Índice de CO2flux')
       
       
dados1 <- data.frame(
  Valor = c(sa_seco$CO2FLUX8, ubj_seco$CO2FLUX8),
  Area = c(
    rep('SA Seco', length(sa_seco$CO2FLUX8)),
    rep('UBJ Seco', length(ubj_seco$CO2FLUX8))
  )
)

ggplot(dados1, aes(x = Area, y = Valor, fill = Area))+
  geom_boxplot(alpha=0.6, outlier.shape = NA, width = 0.4) + 
  theme_minimal() +
  scale_fill_manual(values =  c('SA Seco' = '#DB9D00', 'UBJ Seco'= '#E1F943'))+
  labs(title = 'RPPN Serra das Almas x PARNA de Ubajara',
       subtitle = 'Período Seco (2013-2023)',
       x = 'Área',
       y = 'Índice de CO2flux')       
       
