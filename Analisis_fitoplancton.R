
#Script para analizar los datos del fitoplancton 
# para el paper de Aquatic Sciences

# Como es más descriptivo solo sacaremos un par de PCA y 
# RDAs o CCAs. 


library(dplyr)
library(ggplot2)


# Cargamos los datos RAW ####

# fisicoquimicos 

nombres_embalses <- read.csv("codigo_embalses.csv", header = TRUE)


fisicoquimicos_raw <- read.csv("fisicoquimicos.csv", header = T)


# fito todas las sp 

fitoplancton_raw <- read.csv("fito todas las sp.csv", header = T)

#solo sp importantes 

sps_importantes_raw <- read.csv("fito sps importantes.csv", header = T)


# PCA de los embalses y sus valores fisicoquimicos ####

fisicoquimicos_raw <- fisicoquimicos_raw %>% 
  mutate_if(is.character, as.factor)

#agregamos la columna de tipo de embalse desde el df nombres_master

fisicoquimicos_raw <- fisicoquimicos_raw %>% 
  #agregamos los tipos de cada embalse
  left_join(nombres_embalses           #de este df
            %>% select(Embalse, Tipo), #filtramos solo las columnas que nos interesan
            by = "Embalse") %>%  #la columna clave en los dos df
  #convertimos los characteres en factores 
  mutate_if(is.character, as.factor) %>% 
  mutate(Tipo = as.factor(Tipo)) %>% 
  #colocamos tipo de embalse despues de fecha
  relocate(Tipo, .after = Fecha)



#Nos quedamos con las columnas que queremos 

fisicoquimicos_pca <- fisicoquimicos_raw %>% 
  dplyr::select(c(1,5:20))

#normalizamos los datos con log1p 
fisicoquimicos_pca <- fisicoquimicos_pca %>% 
  mutate_if(is.numeric, log1p)

#agregamos la columna de pH

fisicoquimicos_pca$pH <- cbind(fisicoquimicos_raw$pH)

colnames(fisicoquimicos_pca)



#hacemos PCA

library(factoextra)
library(FactoMineR)


#primero convertimos los codigos en nombres de las filas
#para que nos aparezcan en las graficas y no haya problemas posteriores

rownames(fisicoquimicos_pca) <- fisicoquimicos_pca$Code



#guardamos con la funcion del paquete 
pca_fisicos <-PCA(fisicoquimicos_pca[-1], #quitamos la columna de codigo
                  graph = T,
                  scale.unit = T)

#29.46% y 15.12%, explicacion de los dos primeros ejes

summary(pca_fisicos)

fviz_eig(pca_fisicos, addlabels = TRUE)
#grafica bonita de barplots de los valores de los eigenvalues


#grafica biplot

fviz_pca_biplot(pca_fisicos,
                repel = T,
                col.var = "black",
                labelsize = 5,
                arrowsize = 1,
                habillage = fisicoquimicos_raw$Tipo,
                addEllipses = F,
                mean.point = FALSE,
                title = "")+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 15, face = "bold", color = "black")) +
  xlab("PC1 (29.46%)") +
  ylab("PC2 (15.12%)")



#grafica individuos (en este caso embalses)
fviz_pca_ind(pca_fisicos,
             geom = "point",
             label = "all",
             repel = TRUE,
             col.ind = "black",
             labelsize = 6,
             habillage = fisicoquimicos_raw$Tipo,
             addEllipses = T,
             title = "") +
  theme_bw() +
  theme(axis.line = element_line(color = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        text = element_text(size = 15, face = "bold", color = "black")) +
  xlab("PC1 (29.46%)") +
  ylab("PC2 (15.12%)")


#crearemos un nuevo dataframe agroupando por embalse y sacando la media de cada variable

fisico_media <- fisicoquimicos_raw %>%
  group_by(Embalse) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#agregamos el tipo de embalse 

fisico_media <- fisico_media %>% 
  #agregamos los tipos de cada embalse
  left_join(nombres_embalses           #de este df
            %>% select(Embalse, Tipo), #filtramos solo las columnas que nos interesan
            by = "Embalse") %>%  #la columna clave en los dos df
  #convertimos los characteres en factores 
  mutate_if(is.character, as.factor) %>% 
  mutate(Tipo = as.factor(Tipo)) %>% 
  #colocamos tipo de embalse despues de fecha
  relocate(Tipo, .after = Fecha)
  

#quitamos las columnas que no necestiamos 

fisico_media_pca <- fisico_media %>% 
  dplyr::select(-c(2,3,20:22))

#normalizamos los datos con log1p 
fisico_media_pca <- fisico_media_pca %>% 
  mutate_if(is.numeric, log1p)

#agregamos la columna de pH

fisico_media_pca$pH <- cbind(fisico_media$pH)

colnames(fisico_media_pca)



library(tibble)
#convertimos la columna de embalse en rownames
fisico_media_pca <- fisico_media_pca %>%
  column_to_rownames(var = "Embalse")


#hacemos PCA
pca_fisicos_media <-PCA(fisico_media_pca,
                  graph = T,
                  scale.unit = T)

#39.05% y 17.05%, explicacion de los dos primeros ejes

summary(pca_fisicos_media)

fviz_eig(pca_fisicos_media, addlabels = TRUE)
#grafica bonita de barplots de los valores de los eigenvalues


colores_tipo <- c(
  "1"  = "#FF7970", # rosado claro
  "7"  = "#C49A00",  # mostaza dorado
  "9"  = "#53B400",  # verde manzana
  "10" = "#00C094",  # turquesa claro
  "11" = "#00B6EB",  # azul cielo
  "12" = "#A58AFF",  # lavanda/morado
  "13" = "#FB61D7")   # rosa fuerte

#grafica biplot

fviz_pca_biplot(pca_fisicos_media,
                repel = T,
                col.var = "black",
                labelsize = 7,
                arrowsize = 1,
                habillage = fisico_media$Tipo,
                addEllipses = F,
                mean.point = FALSE,
                title = "")+
  scale_color_manual(values = colores_tipo) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 20, face = "bold", color = "black")) +
  xlab("PC1 (39.05%)") +
  ylab("PC2 (17.05%)")



#grafica individuos (en este caso embalses)
fviz_pca_ind(pca_fisicos_media,
             geom = "text",
             label = "all",
             repel = TRUE,
             col.ind = "black",
             labelsize = 7,
             habillage = fisico_media$Tipo,
             addEllipses = F,
             title = "") +
  scale_color_manual(values = colores_tipo) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size = 20, face = "bold", color = "black")) +
  xlab("PC1 (39.05%)") +
  ylab("PC2 (17.05%)")



# Multivariante fito ####

#arreglamos los df para que este listo para el RDA o CCA

#necesitamos dos matrices

#matriz de especies 

cca_fito <- sps_importantes_raw %>% 
  dplyr::select(-c(1:4,6:8)) #quitamos las columnas que no son datos de abundancia

#transponemos las filas por columnas

cca_fito <- cca_fito %>% 
  pivot_longer(cols = -Species, 
               names_to = "Especie", 
               values_to = "Abundancia") %>%
  pivot_wider(names_from = Species,
              values_from = Abundancia) %>% 
  #renombramos la columna especie como Code
  rename(Code = Especie)


#hay un embalse que no tiene densidad en las especies dominantes
#entonces lo vamos a quitar 

cca_fito <- cca_fito %>% 
  filter(Code != "SAB2013")


#usamos la funcion decorana para ver si es mejor usar RDA o CCA
library(vegan)
decorana(cca_fito[-1])

#el valor del axis lenghts es de 4 para el primer eje, y 4, 3.7 y 3.5 para los restantes
#entonces un CCA es mejor

decorana(decostand(cca_fito[-1], method = "hellinger"))
#cambia si primero lo pasamos a hellinger? no, siguen siendo muy altos los valores

#matriz de variables ambientales

#ya que quitamos un embalse del fito hay que quitarselo tambien de los fisicoquimicos

fisicoquimicos_cca <- fisicoquimicos_pca %>% 
  filter(Code != "SAB2013")

#del resto ya lo teniamos listo

#transformamos a hellinger para evitar el problema del doble cero 
cca_fito_hellinger <- decostand(cca_fito[-1], method = "hellinger")


# Modelos base
mod0 <- cca(cca_fito_hellinger ~ 1, data = fisicoquimicos_cca[-1])    # modelo nulo
mod_full <- cca(cca_fito_hellinger ~ ., data = fisicoquimicos_cca[-1]) # modelo completo con todas las variables disponibles

# R2 ajustado del modelo completo (para limitar la búsqueda)
R2adj_full <- RsquareAdj(mod_full)$adj.r.squared
R2adj_full


#Ahora la selección forward (usa el R2 ajustado como tope y permutaciones):

set.seed(42)
step_mod <- ordiR2step(mod0,
                       scope = formula(mod_full),
                       direction = "both",
                       R2scope = T,
                       pstep = 1000,
                       trace = T,
                       permutations = 999)

# Resultado: modelo final seleccionado
step_mod
# Variables seleccionadas:
selected_vars <- attr(terms(step_mod), "term.labels")
selected_vars


#el modelo de arriba nos indica que solo necesitamos la clorofila,
#pero incluire más variables hasta que siga siendo signficante


# modelo final cca ####


cca_importants <- cca(cca_fito_hellinger ~ Chlorophyll + DO + pH + Conductivity +
                        Profundidad + Secchi + SS + Anoxia + Ntotal + Ptotal + 
                        Temperature,
                        data = fisicoquimicos_cca[-1])


summary(cca_importants, display = NULL)
# Partitioning of variance
# Constrained (proportion) 05.64%
# con los fisicoquimicos se explica el 37.09% de la varianza
# el eje uno explica el 49.07% de la varianza (del 31.16%), mientras que
# el eje dos explica el 20.17% de la varianza (del 31.16%)
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/rda-and-dbrda/


#checamos si es significate
anova.cca(cca_importants)
# is significant so yeeiii! 

anova(cca_importants, by = "term", permutations = 999)  # test por cada variable incluida

#igual nos dice que la unica que valdria es la clorofila



vif.cca(cca_importants)
# Si hay VIF muy altos (por ejemplo > 10 o >5), considerar retirar variables colineales.


#Visualizar el CCA ####

# Extraer scores con scaling = 1 (sitios y biplot)
sites <- scores(cca_importants, display = "sites", choices = 1:2, scaling = 1)
species <- scores(cca_importants, display = "species", choices = 1:2, scaling = 1)
env_bp <- scores(cca_importants, display = "bp", choices = 1:2, scaling = 1) # biplot (flechas)

# Convertir a data.frames
df_sites <- as.data.frame(sites[,1:2])
colnames(df_sites) <- c("CCA1","CCA2")
df_sites$Site <- rownames(sites)

df_species <- as.data.frame(species[,1:2])
colnames(df_species) <- c("CCA1","CCA2")
df_species$Species <- rownames(species)

df_env <- as.data.frame(env_bp[,1:2])
colnames(df_env) <- c("CCA1","CCA2")
df_env$Variable <- rownames(env_bp)

# Calcular factor de multiplicación para que las flechas ajusten a la escala de los sitios
site_max <- max(abs(df_sites[,c("CCA1","CCA2")]))
env_max  <- max(abs(df_env[,c("CCA1","CCA2")]))
arrow_mult <- 0.8 * site_max / env_max   # 0.8 es un ajuste visual; cambiar si hace falta

df_env_scaled <- df_env
df_env_scaled[,c("CCA1","CCA2")] <- df_env_scaled[,c("CCA1","CCA2")] * arrow_mult



library(scales)  # opcional para formatting

ggplot() +
  # puntos de los sitios
  geom_point(data = df_sites, aes(x = CCA1, y = CCA2,
                                  colour = grafica_cca$Trophic.State),
             size = 5, alpha = 1) +
  scale_color_manual(
    values = c("Blue", "Gold", "Darkgreen","Red"),
    name = "Trophic State") +
  # vectores ambientales (flechas)
  geom_segment(data = df_env_scaled,
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2),
               arrow = arrow(angle = 22.5, length = unit(0.25, "cm"), type = "closed"),
               color = "#1f78b4", size = 1.5) +
  geom_text_repel(data = df_env_scaled,
                  aes(x = CCA1, y = CCA2, label = Variable),
                  color = "#1f78b4", size = 7) +
  # especies (si quieres mostrarlas)
  geom_text_repel(data = df_species, aes(x = CCA1, y = CCA2, label = Species),
                  color = "darkred", size = 6, fontface = "italic", max.overlaps = 40) +
  # líneas eje
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  coord_cartesian(xlim = c(-4, 2), ylim = c(-4, 4)) +
  labs(x = paste0("CCA1 (", round(100 * cca_importants$CCA$eig[1] / sum(cca_importants$CCA$eig), 1), "%)"),
       y = paste0("CCA2 (", round(100 * cca_importants$CCA$eig[2] / sum(cca_importants$CCA$eig), 1), "%)"),
       title = "") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



grafica_cca <- fisicoquimicos_raw %>% 
  #quitamos el embalse de sab2013
  filter(Code != "SAB2013") %>% 
  #seleecionamos la columnas que queremos
  dplyr::select(c(1,4,24))

#reodernamos los niveles de Trophic state
grafica_cca$Trophic.State <- factor(grafica_cca$Trophic.State,
                                    levels = c("Oligotrophic", "Mesotrophic", "Eutrophic", "Hypereutrophic"))


#Tabla de fisicoquimicos para el paper ####

#opcion 1

library(tidyr)

fisico_stats_long <- fisicoquimicos_raw %>%
  group_by(Embalse) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -Embalse,
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|se)"
  )

#resultado
print(fisico_stats_long)


#opcion 2, mas compacta y lista para el paper

fisico_stats_paper <- fisicoquimicos_raw %>%
  group_by(Embalse) %>%
  summarise(
    n = sum(!is.na(pH)),  # puedes usar cualquier variable que siempre esté presente
    across(
      where(is.numeric),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        se   = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  # redondear y forzar a 2 decimales fijos
  mutate(across(ends_with("_mean"), ~formatC(.x, digits = 2, format = "f"))) %>%
  mutate(across(ends_with("_se"),   ~formatC(.x, digits = 2, format = "f"))) %>%
  # combinar media ± se
  rowwise() %>%
  mutate(
    across(
      ends_with("_mean"),
      ~ paste0(.x, " ± ", get(sub("_mean$", "_se", cur_column()))),
      .names = "{sub('_mean$', '', .col)}"
    )
  ) %>%
  # dejar solo columnas finales limpias
  select(Embalse, n, !ends_with(c("_mean","_se")))



#agregamos el nombre de cada embalse y exportamos a excel

fisico_stats_paper %>% 
  #agregamos los tipos de cada embalse
  left_join(nombres_embalses           #de este df
            %>% select(Embalse, Nombre), #filtramos solo las columnas que nos interesan
            by = "Embalse") %>%   #la columna clave en los dos df
  #colocamos nombre despues de embalse
  relocate(Nombre, .after = Embalse) %>% 
  write_clip()


#obtenemos el promedio, el min y max valor de cada variable y el code de donde se dio



# indval #### 

#para hacer el indval necesitamos dos matrices
#una con los datos de abundancia y otra con los grupos 

#matriz con los grupos o los estado troficos 

colnames(fisicoquimicos_raw)

fisicos_indval <- fisicoquimicos_raw %>% 
  dplyr::select(c(1,23)) %>% 
  filter(Code != "SAB2013") %>% 
  #arrange the code column 
  arrange(Code)


#ahora nos quedamos solo con el estado trofico
  
  indval_trophic <- fisicos_indval %>%
  dplyr::select(-1)


#ahora la df del phytoplankton 

# de cca_fito quitamos la primera columna

fito_indval <- cca_fito %>% 
  arrange(Code) %>% 
  dplyr::select(-1)


#hacemos el indval


library(indicspecies)

results.fito.indval <- multipatt(fito_indval,
                                        indval_trophic$Trophic.State,
                                        control = how(nperm=9999))

summary(results.fito.indval)
