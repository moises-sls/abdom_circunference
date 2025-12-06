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

wp(m1_ga, ylim.all = 2)
checkMomentSK(m1_ga)
checkCentileSK(m1_ga)
checkCentileSK(m1_ga, type = "tail")

m2_ga <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest),
                data = dados, family = GA, start.from = m1_ga)

wp(m2_ga, ylim.all = 2)
checkMomentSK(m2_ga)
checkCentileSK(m2_ga)
checkCentileSK(m2_ga, type = "tail")


1

































































# log SHASHo
gen.Family("SHASHo", "log")
m50 <- gamlss(y_circ_abdom ~ pb(x_idade_gest),
                sigma.fo = ~ pb(x_idade_gest),
                nu.fo = ~ pb(x_idade_gest),
                tau.fo = ~ pb(x_idade_gest),
                family = logSHASHo, data = dados,
                method = mixed(50, 100),
                control = gamlss.control(ncpus = n_cpus))

summary(m50)
wp(m50, ylim.all = 1)
