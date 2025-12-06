library(gamlss)
library(gamlss.data)
library(gamlss.ggplots)
library(dplyr)
library(ggplot2)
library(psych)
library(xtable)

dados <- abdom

dados <- dados |>
    rename(y_circ_abdom = y, x_idade_gest = x)

dados |> head()

####################### Análise univariada

dados |>  describe()

# var. explicativa
dados |>
    ggplot(aes(y = x_idade_gest)) +
    geom_boxplot(fill = "lightgray") +
    theme_bw(base_size = 22)

dados |>
    ggplot(aes(x_idade_gest)) +
    geom_density(fill = "lightgray") +
    xlim(c(min(dados$x_idade_gest) - 10, max(dados$x_idade_gest) + 10)) +
    theme_bw(base_size = 22)

# var. resposta
dados |>
    ggplot(aes(y = y_circ_abdom)) +
    geom_boxplot(fill = "lightgray") +
    theme_bw(base_size = 22)

dados |>
    ggplot(aes(y_circ_abdom)) +
    geom_density(fill = "lightgray") +
    xlim(c(min(dados$y_circ_abdom) - 50, max(dados$y_circ_abdom) + 50)) +
    theme_bw(base_size = 22)

##########################################################################



####################### Analise multivariada

dados |>
    ggplot(aes(y_circ_abdom, x_idade_gest)) +
    geom_point() +
    theme_bw(base_size = 22)

# correlação
cor(dados, method = "spearman")

##########################################################################



####################### Ajuste
n_cpus <- detectCores()

distribuicoes <- fitDist(dados$y_circ_abdom, type = "realplus")
distribuicoes$fits

# Normal

## \mu
m_no <- gamlss(y_circ_abdom ~ x_idade_gest, family = NO, data = dados)

summary(m_no)

wp(m_no, ylim.all = 2)

checkMomentSK(m_no)

checkCentileSK(m_no)

checkCentileSK(m_no, type = "tail")



### termos quadraticos
m11_no <- dados |>
    mutate(x_2 = x_idade_gest ^ 2) |>
    gamlss(y_circ_abdom ~ x_idade_gest + x_2,
           family = NO, data = _)

summary(m11_no)
# problema na matriz vcov



## \mu e \sigma
m2_no <- gamlss(y_circ_abdom ~ x_idade_gest,
                sigma.fo = ~ x_idade_gest,
                data = dados, family = NO)
summary(m2_no)

wp(m_no, ylim.all = 2)

checkMomentSK(m_no)

checkCentileSK(m_no)

checkCentileSK(m_no, type = "tail")



### termos quadraticos
m22_no <- dados |>
    mutate(x_2 = x_idade_gest ^ 2) |>
    gamlss(y_circ_abdom ~ x_idade_gest + x_2,
           sigma.fo = ~ x_idade_gest + x_2,
           family = NO, data = _)

summary(m1_no)
# problema na matriz vcov

### splines
m3_no <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest),
                data = dados, family = NO,
                start.from = m2_no)

summary(m3_no)

wp(m3_no, ylim.all = 2)

checkMomentSK(m3_no)

checkCentileSK(m3_no)

checkCentileSK(m3_no, type = "tail")

# Gama
m_ga <- gamlss(y_circ_abdom ~ x_idade_gest,
               data = dados, family = GA)

summary(m_ga)

wp(m_ga, ylim.all = 2)
checkMomentSK(m_ga)
checkCentileSK(m_ga)
checkCentileSK(m_ga, type = "tail")
# problema com assimetria

m1_ga <- gamlss(y_circ_abdom ~ x_idade_gest,
                sigma.fo = ~ x_idade_gest,
                data = dados, family = GA, start.from = m_ga)

wp(m1_ga, ylim.all = 1)
checkMomentSK(m1_ga)
checkCentileSK(m1_ga)
checkCentileSK(m1_ga, type = "tail")
# Problema com a assimetria

m2_ga <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest),
                data = dados, family = GA, start.from = m1_ga)

wp(m2_ga, ylim.all = 2)
checkMomentSK(m2_ga)
checkCentileSK(m2_ga)
checkCentileSK(m2_ga, type = "tail")
# Problema com a curtose

m_gg <- gamlss(y_circ_abdom ~ x_idade_gest,
               family = GG, data = dados)

wp(m_gg, ylim.all = 1)
checkMomentSK(m_gg)
checkCentileSK(m_gg)
checkCentileSK(m_gg, type = "tail")

m1_gg <- gamlss(y_circ_abdom ~ x_idade_gest,
                sigma.fo = ~ x_idade_gest,
                nu.fo = ~ x_idade_gest,
                data = dados, family = GG, start.from = m_gg)

wp(m1_gg, ylim.all = 1)
checkMomentSK(m1_gg)
checkCentileSK(m1_gg)
checkCentileSK(m1_gg, type = "tail")

m2_gg <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest),
                nu.fo = ~ pb(x_idade_gest),
                data = dados, family = GG, start.from = m1_gg)

wp(m2_gg, ylim.all = 1)
checkMomentSK(m2_gg)
checkCentileSK(m2_gg)
checkCentileSK(m2_gg, type = "tail")

m0_no <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest), data = dados, family = NO)

choose_dist <- chooseDist(m0_no, parallel = "snow", ncpus = detectCores(), type = "realplus")

getOrder(choose_dist, 1)

# BCT
m1_bct <- gamlss(y_circ_abdom ~ x_idade_gest,
                 sigma.fo = ~ x_idade_gest,
                 family = BCT, data = dados)

summary(m1_bct)
wp(m1_bct, ylim.all = 1)
wp(m1_bct, xvar = x_idade_gest)
checkMomentSK(m1_bct)
checkCentileSK(m1_bct)
checkCentileSK(m1_bct, type = "tail")

m2_bct <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                 sigma.fo = ~ pb(x_idade_gest),
                 family = BCT, data = dados, start.from = m1_bct)
summary(m2_bct)
wp(m2_bct, ylim.all = 1)
checkMomentSK(m2_bct)
checkCentileSK(m2_bct)
checkCentileSK(m2_bct, type = "tail")

m3_bct <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                 sigma.fo = ~ pb(x_idade_gest),
                 tau.fo = ~ pb(x_idade_gest),
                 family = BCT, data = dados, start.from = m2_bct)
summary(m3_bct)
wp(m3_bct, ylim.all = 1)
checkMomentSK(m3_bct)
checkCentileSK(m3_bct)
checkCentileSK(m3_bct, type = "tail")

GAIC(m1_bct, m2_bct, m3_bct)

cbind(dados, ajuste = fitted(m1_bct)) |>
    ggplot(aes(x = x_idade_gest, y = y_circ_abdom)) +
    geom_point(shape = 1) +
    geom_line(aes(x = x_idade_gest, y = ajuste, color = "ajuste"), lwd = 3) +
    labs(colour = "Valores Ajustados") +
    scale_color_manual(values = c("ajuste" = "red"),
                       labels = c("ajuste" = "Curva de Ajuste"),
                       aesthetics = "color") +
    theme_bw(base_size = 20)

# IG
m1_ig <- gamlss(y_circ_abdom ~ x_idade_gest,
                sigma.fo = y_circ_abdom ~ x_idade_gest,
                data = dados, family = IG)
wp(m1_ig, ylim.all = 1)
checkMomentSK(m1_ig)
checkCentileSK(m1_ig)
checkCentileSK(m1_ig, type = "tail")

m2_ig <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest),
                data = dados, family = IG, start.from = m1_ig)
wp(m2_ig, ylim.all = 1)
checkMomentSK(m2_ig)
checkCentileSK(m2_ig)
checkCentileSK(m2_ig, type = "tail")

m3_ig <- gamlss(y_circ_abdom ~ poly(x_idade_gest, 3),
                sigma.fo = ~ poly(x_idade_gest, 3),
                data = dados, family = IG, start.from = m1_ig)
summary(m3_ig)
wp(m3_ig, ylim.all = 1)
checkMomentSK(m3_ig)
checkCentileSK(m3_ig)
checkCentileSK(m3_ig, type = "tail")



# GAF
m1_gaf <- gamlss(y_circ_abdom ~ x_idade_gest,
                 sigma.fo = y_circ_abdom ~ x_idade_gest,
                 data = dados, family = GAF, method = mixed(100, 400))
summary(m1_gaf)
wp(m1_gaf)


m1 <- gamlss(y ~ poly(x, 2),
             sigma.fo = ~ poly(x, 2),
             # nu.fo = ~ poly(x, 3),
             data = abdom, family = GAF,
             method = mixed(300, 600), c.crit = 0.00001)

summary(m1)
wp(m1, ylim.all = 2)
wp(m1, ylim.all = 2, xvar = x)

m2 <- gamlss(y ~ poly(x, 3),
             sigma.fo = ~ poly(x, 1),
             # nu.fo = ~ poly(x, 3),
             data = abdom, family = GAF,
             start.from = m1,
             method = mixed(1000, 20000), c.crit = 0.00001)

wp(m2, ylim.all = 1, xvar = x)

m3 <- gamlss(y ~ poly(x, 3),
             data = abdom, family = GAF,
             start.from = m2,
             method = mixed(1000, 40000), c.crit = 0.00001)

wp(m3, ylim.all = 2)
wp(m3, xvar = x)
summary(m3)
