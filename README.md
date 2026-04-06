# Análisis Multivariante de Fitoplancton en Embalses

Script en R para el paper de *Aquatic Sciences*. Analiza datos de fitoplancton y variables fisicoquímicas de embalses mediante PCA, CCA e IndVal.

## Descripción

Análisis descriptivo que incluye:

PCA de variables fisicoquímicas (individuales: 44.6% y promedios: 56.1% varianza PC1+PC2)

CCA fitoplancton-ambiente (37% varianza explicada, clorofila variable clave)

Tabla resumen de parámetros por embalse (media±SE)

Especies indicadoras por estado trófico (IndVal)

## Resultados

| Análisis         | Varianza PC1+PC2         | Variables principales |
|------------------|--------------------------|-----------------------|
| PCA Individual   | 44.6%                    | Todas fisicoquímicas  |
| PCA Promedios    | 56.1%                    | Log-normalizadas      |
| CCA Fitoplancton | 31.2% (5.6% constrained) | Clorofila, DO, pH     |

**[Espacio para PCA biplot individuales]**

**[biplot CCA fitoplancton-ambiente]**

**Especies indicadoras IndVal**:

| Estado | Especie | IndVal | p-valor |
|--------|---------|--------|---------|
| Hipereutrófico | *W. naegeliana* | 0.94 | <0.01 |
| Hipereutrófico | *Tetrastrum komarekii* | 0.713 | 0.03 |
| Eutrófico | *Chroococcus distans* | 0.5 | 0.02 |


Para más detalles, revisa comentarios en el código. ¡Contribuciones bienvenidas vía pull requests!
