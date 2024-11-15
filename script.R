library(readxl)
library(ggthemes)
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(joineR)
library(car)

micosis_l <- read_excel("data/micosis.xlsx")

micosis_a = micosis_l %>%
  spread(key = "mes", value = "long") %>% 
  rename( 'mes00'='0','mes01'='1' , 'mes02'='2'  , 
          'mes03' = '3', 'mes06'= '6', 'mes12'= '12' ) 
micosis_l$mes_factor = factor(micosis_l$mes)

### Perfiles individuales
ggplot(micosis_l, aes(x = mes, y = long, group = id, color = tto)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ tto) +
  #scale_x_continuous("Edad (a?os)", breaks = seq(8, 14, 2)) +
  #scale_y_continuous("Distancia") +
  #coord_cartesian(ylim = c(12, 32)) +
  theme_bw() +
  scale_color_brewer(palette = 'Set1') +
  labs(title = "Evoluci?n de?s del tiempo seg?n tto") +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 15))


# GRAFICO DE PERFILES PROMEDIO
ggplot(micosis_l, aes(x = mes, y = long, color = tto)) +
  stat_summary(fun = mean, geom = 'line', size = 1) + 
  stat_summary(fun = mean, geom = 'point', size = 2) +
  #scale_color_brewer(palette = 'Set1') +
  #coord_cartesian(ylim = c(12, 32)) +
  theme_light() +
  #scale_x_continuous("Edad (a?os)", breaks = seq(8, 14, 2)) +
  #scale_y_continuous("Distancia") +
  labs(title = "Evoluci?n de la distancia maxilar media a traves del tiempo seg?n sexo",
       color = "Sexo") +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 13),
        strip.text = element_text(size = 15))

ggplot(micosis_l, aes(x = mes_factor, y = long)) + 
  stat_summary(fun = mean, geom = 'line', color = 'cyan', size = 1) + 
  stat_summary(fun = mean, geom = 'point', color = 'cyan', size = 2) +
  geom_line(aes(x = mes, y = long, group = tto), 
            linewidth = 0.5, na.rm = T, color = 'grey50') +
  geom_point(size = 0.5, na.rm = T, color = 'grey50') +
  facet_wrap(~tto) +
  theme_light() 
  #scale_x_continuous("Semana", breaks = seq(0, 12, 1)) +
  #scale_y_continuous("Peso corporal (gr.)") +
  #, limits = c(14, 32), breaks = seq(12, 34, 2)) +
  #labs(title = "Evolución del peso de las ratas a través del tiempo según grupo") +
  theme(plot.title = element_text(size = 17))

#### BOXPLOTS ##########


ggplot(micosis_l, aes(x = mes_factor, y = long, fill = tto)) +
  geom_boxplot() +
  theme_light() +
  #scale_x_discrete("") +
  #scale_y_continuous("Distancia", limits = c(16,32), breaks = seq(16, 32, 2)) +
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "Distribución de la distancia maxilar según edad y sexo",
       fill = "Sexo") +
  theme(plot.title = element_text(size = 15),
        axis.title = element_text(size = 12),
        legend.position = 'bottom')


#------------------------------------------#
# 4. VARIOGRAMA
#------------------------------------------#  

long_medio <- lm(long ~ tto+club+sexo+mes, data = micosis_l)
micosis_l <- mutate(micosis_l,
                    residuos = long_medio$residuals,
                    sd_residuos = long_medio$residuals/sd(long_medio$residuals))
micosis_l_est = micosis_l %>% 
  group_by(sem, sexo) %>% 
  mutate(h.est = scale(h))



vgm <- variogram(micosis_l$id, micosis_l$mes, micosis_l$residuos)
vgm1 = data.frame(vgm$svar)

ggplot(data = vgm1, aes(x = vt, y = vv)) +
  geom_point(color = 'grey50') +
  stat_summary(fun = mean, geom = 'line', color = 'orangered', size = 1) + 
  geom_hline(yintercept = vgm$sigma2) +
  theme_light() +
  scale_x_continuous("Rezago", breaks = seq(1, 12, 1)) +
  scale_y_continuous("Variograma muestral", limits = c(0, 40), breaks = seq(0, 40, 5)) +
  labs(title = "Variograma muestral") +
  theme(plot.title = element_text(size = 17))

# hips.l.est = hips.l %>% 
#   group_by(sem, sexo) %>% 
#   mutate(h.est = scale(h))
