library(ggplot2)
library(openxlsx)
library(ggcorrplot)
library(dplyr)
library(skimr)
setwd("C:/Users/rafah/Desktop/rafah/Facultad/econ-pol/econ-pol/research/pol_comp")

ejes<-read.xlsx('encuesta.xlsx',sheet="ejes")

#ejes$tipo<-ifelse(crit_1$moral_ax>0 & crit_1$econ_ax>0,"Estatista Conservador",
                         #ifelse(crit_1$moral_ax>0 & crit_1$econ_ax<0,"Privatista Conservador",
                                 #ifelse(crit_1$moral_ax<0 & crit_1$econ_ax>0,"Estatista Progresista",
                                         #ifelse(crit_1$moral_ax<0 & crit_1$econ_ax<0,"Privatista Progresista","Inclasificable"))))

#frec_cat<-as.data.frame(table(ejes$tipo))
#colnames(frec_cat) <- c("Categoría", "Cantidad")

#ggplot(frec_cat, aes(x = "", y = Cantidad, fill = Categoría)) +
  #geom_bar(stat = "identity", width = 1) +
  #coord_polar(theta = "y") +  # Transformar a gráfico de torta
  #labs(title = "Distribución de individuos", fill = "Categorías") +
  #theme_void() +
  #scale_fill_manual(values = c("Estatista Conservador" = "blue", "Estatista Progresista" = "red", "Privatista Conservador" = "purple","Privatista Progresista"="green","Inclasificable"="grey"))


labels <- data.frame(label = c("Estatista","Privatista","Progresista","Conservador"),
                     x = c(0, 0, -8.5, 8.5),
                     y = c(8.5, -8.5, 0, 0)
)
labels$angle <- ifelse(labels$label %in% c("Progresista", "Conservador"), 90, 0)

c1 <- ggplot(ejes, aes(moral_ax,econ_ax)) +
  theme_minimal() +
  coord_cartesian(xlim = c(-8,8), ylim = c(-8,8)) +
  geom_rect(aes(xmin = -8, xmax = 0,
                ymin = 0  , ymax = 8),
            fill = "#CECECE", alpha=0.5) +
  geom_rect(aes(xmin = 0, xmax = 8,
                ymin = 0, ymax = 8 ),
            fill = "#E8E8E8") +
  geom_rect(aes(xmin = -8, xmax = 0,
                ymin = -8, ymax = 0),
            fill = "#E8E8E8") +
  geom_rect(aes(xmin =   0, xmax = 8,
                ymin = -8, ymax =  0),
            fill = "#CECECE")
# Create plot, plot labels and axis labels
p1 <- c1 +
  geom_point(size = 3, colour = "black") +
  geom_point(size = 3, shape = 21) + 
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle)) +
  labs(x = "", y = "")
plot(p1)

#¿a quién votó?
vote<-c1 +
  geom_point(aes(colour = vote), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("Javier Milei (La Libertad Avanza)" = "#C001F6", 
                                "Patricia Bullrich (Juntos por el Cambio)" = "#F9CA0D", 
                                "Sergio Massa (Unión por la Patria)" = "#0D03FA", 
                                "Juan Schiaretti (Hacemos por Nuestro País)" = "#4DFE06", 
                                "En blanco" = "white","MEDIO"="black"),
                     guide = "none")
plot(vote)

#cuadrante
categoria<-c1 +
  geom_point(aes(colour = categoria), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("CONSERVADORESTATISTA" = "blue", 
                                "CONSERVADORPRIVATISTA" = "purple", 
                                "PROGRESISTAESTATISTA" = "red", 
                                "PROGRESISTAPRIVATISTA" = "green", 
                                "MEDIO" ="black"),
                     guide = "none") 
plot(categoria)

#¿la política tradicional es responsable?
pol_trad<-c1 +
  geom_point(aes(colour = trad_pol), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De acuerdo" = "green", 
                                "En desacuerdo" = "red"),
                     guide = "none")
plot(pol_trad)

#¿es la falta de dolares responsable?
lack_dollar<-c1 +
  geom_point(aes(colour = lack_dollar), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De acuerdo" = "green", 
                                "En desacuerdo" = "red"),
                     guide = "none")
plot(lack_dollar)

#pagar con cierto grado de corrpución

corruption<-c1 +
  geom_point(aes(colour = corruption), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De acuerdo" = "green", 
                                "En desacuerdo" = "red"),
                     guide = "none")
plot(corruption)

#menos derechos pero mas seguridad
less_rights_seg<-c1 +
  geom_point(aes(colour = less_right_sec), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De acuerdo" = "green", 
                                "En desacuerdo" = "red"),
                     guide = "none")
plot(less_rights_seg)

#menos derechos pero mejor economía

less_right_eco<-c1 +
  geom_point(aes(colour = less_right_eco), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De acuerdo" = "green", 
                                "En desacuerdo" = "red"),
                     guide = "none")
plot(less_right_eco)

#menos derechos, mas estabilidad económica y crecimiento
stability<-c1 +
  geom_point(aes(colour = stability), size = 3, shape = 19) +  # Asignar color según la variable 'voto'
  geom_text(data = labels, color = "black", 
            aes(x = x, y = y, label = label, angle = angle))+
  labs(x = "", y = "") +
  scale_color_manual(values = c("De acuerdo" = "green", 
                                "En desacuerdo" = "red"),
                     guide = "none")
plot(stability)

#matriz de correlaciones

cor<-read.xlsx('encuesta.xlsx',sheet="corr")

corr <- cor %>% 
  select(qe1, qe2, qe3, qe4, qe5, 
         qe6, qe7, qe8, qm1, qm2, qm3, qm4,
         qm5, qm6, qm7) %>% 
  # calcular la matriz de correlación y redondear a un decimal
  cor(use = "pairwise") %>% 
  round(1)
ggcorrplot(corr, type = "lower", lab = T, show.legend = F)

help(ggcorrplot)


#transferencia de votos

library(ggalluvial)

vote<-read.xlsx('encuesta.xlsx',sheet="paso_general")

ggplot(data = vote,
       aes(axis1 = paso, axis2 = general,axis3=ballotage)) +
  geom_alluvium(aes(fill = paso),
                curve_type = "arctangent") +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("PASO","General", "Ballotage"),
                   expand = c(0.15, 0.05)) +
  scale_fill_manual(values=c("Grabois"="blue","Rodríguez Larreta"="yellow","Milei"="violet","Schiaretti"="green","Bullrich"="yellow","Massa"="blue")) +
  
  theme(legend.position = "none")

help(ggalluvial)
