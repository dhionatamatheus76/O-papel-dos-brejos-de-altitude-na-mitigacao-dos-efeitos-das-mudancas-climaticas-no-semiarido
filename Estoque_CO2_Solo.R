## Analise estatística do estoque de carbono no solo Parna UBJ e RPPN SA
library(dplyr)

# Data Frame com dados amostrais

parna_ubj <- read.csv('Estoque_CO2_UBJ.csv')
ent_ubj <- read.csv('Estoque_CO2_Ent_UBJ.csv')
rppn_sa <- read.csv('Estoque_CO2_SA.csv')
ent_sa <- read.csv('Estoque_CO2_Ent_SA.csv')

#teste de normalidade Shapiro-Wilk 
shapiro.test(ent_ubj$Media_Carbono) #não normal (p = 9.04e-09)
shapiro.test(parna_ubj$Media_Carbono) #não normal (p=0.0006401)
shapiro.test(rppn_sa$Media_Carbono)#não normal (p= 4.11e-07)
shapiro.test(ent_sa$Media_Carbono) #não normal (p = 1.393e-07)

#dados das amostras
summary(parna_ubj$Media_Carbono)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#31.55   39.18   43.41   43.52   48.66   55.45 
summary(ent_ubj$Media_Carbono)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.73   31.23   38.00   37.62   42.73   52.18
summary(rppn_sa$Media_Carbono)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 29.55   32.82   35.27   36.08   39.20   45.18
summary(ent_sa$Media_Carbono)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 27.73   30.91   32.32   33.17   34.86   44.91 

# teste não parametrico (Wilcoxon)  

wilcox.test(parna_ubj$Media_Carbono,ent_ubj$Media_Carbono) # p-value < 2.2e-16
wilcox.test(rppn_sa$Media_Carbono,ent_sa$Media_Carbono)#p-value = 1.029e-13
wilcox.test(parna_ubj$Media_Carbono,rppn_sa$Media_Carbono) # p-value < 2.2e-16
wilcox.test(ent_sa$Media_Carbono, ent_ubj$Media_Carbono) # p-value = 2.708e-09



library(ggplot2)

library(ggplot2)

dados_ubjEC <- rbind(
  data.frame(
    Area = "PARNA UBJ",
    Carbono = parna_ubj$Media_Carbono
  ),
  data.frame(
    Area = "Entorno UBJ",
    Carbono = ent_ubj$Media_Carbono
  )
)

ggplot(dados_ubjEC,
       aes(x = Area,
           y = Carbono,
           fill = Area)) +
  
  geom_boxplot(
    width = 0.55,
    alpha = 0.45,
    colour = "black",
    linewidth = 0.7
  ) +
  
  scale_fill_manual(values = c(
    "PARNA UBJ"="#8B4513",
    "Entorno UBJ"="#D2B48C"
  )) +
  
  labs(
    title = "PARNA de Ubajara × Entorno",
    x = "Área",
    y = expression("Estoque de carbono (t/ha)")
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

dados_entEC <- rbind(
  data.frame(
    Area = "Entorno UBJ",
    Carbono = ent_ubj$Media_Carbono
  ),
  data.frame(
    Area = "Entorno SA",
    Carbono = ent_sa$Media_Carbono
  )
)

dados_saEC <- rbind(
  data.frame(
    Area = "RPPN SA",
    Carbono = rppn_sa$Media_Carbono
  ),
  data.frame(
    Area = "Entorno SA",
    Carbono = ent_sa$Media_Carbono
  )
)

ggplot(dados_saEC,
       aes(Area, Carbono, fill = Area)) +
  
  geom_boxplot(
    width = .55,
    alpha = .45,
    colour = "black",
    linewidth = .7
  ) +
  
  scale_fill_manual(values = c(
    "RPPN SA"="#2E8B57",
    "Entorno SA"="#66C2A5"
  )) +
  
  labs(
    title = "RPPN Serra das Almas × Entorno",
    x = "Área",
    y = expression("Estoque de carbono (t/ha)")
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

dados_ucEC <- rbind(
  data.frame(
    Area = "PARNA UBJ",
    Carbono = parna_ubj$Media_Carbono
  ),
  data.frame(
    Area = "RPPN SA",
    Carbono = rppn_sa$Media_Carbono
  )
)

ggplot(dados_ucEC,
       aes(Area, Carbono, fill = Area)) +
  
  geom_boxplot(
    width = .55,
    alpha = .45,
    colour = "black",
    linewidth = .7
  ) +
  
  scale_fill_manual(values = c(
    "PARNA UBJ"="#8B4513",
    "RPPN SA"="#2E8B57"
  )) +
  
  labs(
    title = "PARNA de Ubajara × RPPN Serra das Almas",
    x = "Área",
    y = expression("Estoque de carbono (t/ha)")
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggplot(dados_entEC,
       aes(x = Area,
           y = Carbono, fill = Area)) +
  
  geom_boxplot(
    width = 0.55,
    alpha = 0.45,
    colour = "black",
    linewidth = 0.7
  ) +
  scale_fill_manual(values = c(
    "Entorno UBJ" = "#D2B48C",
    "Entorno SA"  = "#66C2A5"
  )) +
  
  labs(
    title = "Entorno do PARNA de UBJ × da RPPN SA",
    x = "Área",
    y = expression("Estoque de carbono (t/ha)")
  ) +
  
  theme_minimal(base_size = 15)
  


