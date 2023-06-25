### Packages utilisés 
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(knitr)

### Importation des données
# Lecture du fichier Excel et stockage dans l'objet de données df
df <- read_excel(path = "C:/Users/Yassine/Desktop/Projects/Volatility.xlsx")

# Calcul du logarithme des rendements du cours de clôture (RMASI)
RMASI <- log(df[3] / lag(df[3]))

# Ajout de la colonne RMASI à l'objet de données df
df <- cbind(df, RMASI) 
names(df)[4] <- "RMASI"

# Calcul du logarithme des variations du nombre de nouveaux cas (Tcovid)
Tcovid = log(df[2] / lag(df[2]))
df <- cbind(df, Tcovid)
names(df)[5] <- "Tcovid"

# Renommer les colonnes existantes
names(df)[1] <- "Date"
names(df)[2] <- "New_cases"
names(df)[3] <- "Close"

# Remplacer les valeurs manquantes par zéro
df[is.na(df)] <- 0
data <- as.data.frame(df)

## Faut eviter les valeurs +infini et -infini
is_neg_inf <- is.infinite(data$Tcovid) & data$Tcovid < 0
is_pos_inf <- is.infinite(data$Tcovid) & data$Tcovid > 0

# Remplacer -Inf avec 0 et Inf avec 1
data$Tcovid[is_neg_inf] <- 0
data$Tcovid[is_pos_inf] <- 1

plot(x= data$Date, y = data$RMASI,
     col = "blue",
     main = "Rendement quotidien MASI entre Janvier 2020 et Juin 2020",
     type = "line")

plot(x= data$Date, y = data$New_cases,
     col = "red",
     main = "Evolution journalière des cas positifs (Covid-19) au Maroc",
     type = "line")

library(e1071) #test de Jarque-Berra
library(moments) #Skewness et Kurtosis

#Statistique descriptive
desc_stats_RMASI <- summary(data$RMASI)
desc_stats_Tcovid <- summary(data$Tcovid)

#skewness
skewness_RMASI <- skewness(data$RMASI)
skewness_Tcovid <- skewness(data$Tcovid)

#kurtosis
kurtosis_RMASI <- kurtosis(data$RMASI)
kurtosis_Tcovid <- kurtosis(data$Tcovid)

# Jarque Berra Test
jb_test_RMASI <- jarque.test(data$RMASI)
jb_test_Tcovid <- jarque.test(data$Tcovid)

#resumé des résultats sous forme d'un tableau
desc_stat <- data.frame(
  Variable = c("RMASI", "Tcovid"),
  Moyenne = c(desc_stats_RMASI[["Mean"]], desc_stats_Tcovid[["Mean"]]),
  Médiane = c(desc_stats_RMASI[["Median"]], desc_stats_Tcovid[["Median"]]),
  Maximum = c(desc_stats_RMASI[["Max."]], desc_stats_Tcovid[["Max."]]),
  Minimum = c(desc_stats_RMASI[["Min."]], desc_stats_Tcovid[["Min."]]),
  Std_dev = c(sd(data$RMASI), sd(data$Tcovid)),
  Skewness = c(skewness_RMASI, skewness_Tcovid),
  Kurtosis = c(kurtosis_RMASI, kurtosis_Tcovid),
  JB_p_value = c(jb_test_RMASI$p.value, jb_test_Tcovid$p.value),
  Observations = c(length(data$RMASI), length(data$Tcovid))
)
kable(t(desc_stat))

library(tseries)

# ADF et KPSS test pour RMASI
adf1 <- adf.test(data$RMASI, alternative = "stationary")
kpss1 <- kpss.test(data$RMASI)

# ADF et KPSS test pour Tcovid
adf2 <- adf.test(data$Tcovid, alternative = "stationary")
kpss2 <- kpss.test(data$Tcovid)

# Tableau des résultats
stationarity <- data.frame(
  Variable = c("RMASI", "Tcovid"),
  "Test ADF" = c(adf1$p.value, adf2$p.value),
  "Test KPSS" = c(kpss1$p.value, kpss2$p.value),
  "Stationnarité" = c("Stationnaire", "Stationnaire")
)

kable(stationarity)

library(lmtest)
library(vars)

var_data = cbind(data$RMASI, data$Tcovid)
colnames(var_data) <- c("RMASI", "TCOVID")

# Définition de la plage de lag
lag_range = 1:10

# Initialisation des vecteurs pour stocker les valeurs AIC et BIC
aic_values = numeric(length(lag_range))
bic_values = numeric(length(lag_range))

# Boucle pour ajuster les modèles VAR avec différents lags
# Calculer les valeurs AIC et BIC
for (i in lag_range) {
  var_fit <- VAR(var_data, p = i) # Ajustement du modèle VAR avec un lag de i
  aic_values[i] <- AIC(var_fit) # Stockage des valeurs AIC et BIC
  bic_values[i] <- BIC(var_fit)
}

# Détermination du lag optimal
optimal_lag_aic <- lag_range[which.min(aic_values)]
optimal_lag_bic <- lag_range[which.min(bic_values)]

# Tableau des résultats
optimal_lags <- data.frame(
  "Criterion" = c("AIC", "BIC"),
  "optimal lag" = c(optimal_lag_aic, optimal_lag_bic),
  "Criterion value" = c(aic_values[optimal_lag_aic], bic_values[optimal_lag_bic])
)
kable(optimal_lags)

Tcovid_granger = grangertest(data$RMASI ~ data$Tcovid, order = optimal_lag_aic)
RMASI_granger = grangertest(data$Tcovid ~ data$RMASI, order = optimal_lag_aic)

Causality_test_table <- data.frame(
  "null hypothesis" = c("TCOVID does not Granger cause RMASI", 
                        "RMASI does not Granger cause TCOVID"),
  
  "Optimal Lags" = c(optimal_lag_aic, optimal_lag_aic),
  "F Stat" = c(Tcovid_granger$F[2], RMASI_granger$F[2]),
  
  "Probability" = c(format(Tcovid_granger$`Pr(>F)`[2], digits = 4, scientific = TRUE), 
                    format(RMASI_granger$`Pr(>F)`[2], digits = 4, scientific = FALSE)),
  
  "significance" = c("significant at 1%", "non significant")
)
kable(t(Causality_test_table))


VAR <- VAR(var_data, p = 2)
summary(VAR)


arch_data <- data.frame(read_excel(path = "C:/Users/Yassine/Desktop/Projects/arch_data.xlsx"))
colnames(arch_data) <- c("Date", "Close", "DCOVID")

RMASI <- log(arch_data$Close / lag(arch_data$Close))
arch_data <- cbind(arch_data, RMASI)
names(arch_data)[4] <- "RMASI"
arch_data <- na.omit(arch_data)

plot_ret <- ggplot(arch_data, aes(x = arch_data$Date, y = arch_data$RMASI)) +
  geom_line(color = "blue") +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
  xlab("Date") +  # X-axis label
  ylab("Variation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Evolution du rendement journalier du MASI entre 2 janvier 2019 et 22 juin 2020")

print(plot_ret)


hist(arch_data$RMASI, breaks = 55, xlab = "", col = "10")


desc_stats_change <- summary(arch_data$RMASI)
skewness_change <- skewness(arch_data$RMASI)
kurtosis_change <- kurtosis(arch_data$RMASI)
jb_test_change <- jarque.test(arch_data$RMASI)

#resumé des résultats sous forme d'un tableau
desc_stat_change <- data.frame(
  Variable = "RMASI",
  Moyenne = desc_stats_change[["Mean"]],
  Médiane = desc_stats_change[["Median"]],
  Maximum = desc_stats_change[["Max."]],
  Minimum = desc_stats_change[["Min."]],
  Std_dev = sd(arch_data$RMASI),
  Skewness = skewness_change,
  Kurtosis = kurtosis_change,
  JB_p_value = jb_test_change$p.value,
  Observations = length(arch_data$RMASI)
)
kable(t(desc_stat_change))


library(gridExtra) # Package utilisé pour la combinaison des graphes

# ACF
acf_data <- acf(arch_data$RMASI, main = "ACF Correlogram", plot = FALSE)

# PACF
pacf_data <- pacf(arch_data$RMASI, main = "PACF Correlogram", plot = FALSE)

# Création du graphique de l'ACF + bandes de significance
acf_plot <- ggplot(data.frame(lag = acf_data$lag, acf = acf_data$acf), aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  geom_hline(yintercept = 1.96/sqrt(length(arch_data$RMASI)), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1.96/sqrt(length(arch_data$RMASI)), linetype = "dashed", color = "red") +
  labs(title = "ACF Correlogram") +
  xlab("Lags") +
  ylab("ACF")

# Création du graphique de PACF + bandes de significance
pacf_plot <- ggplot(data.frame(lag = pacf_data$lag, pacf = pacf_data$acf), aes(x = lag, y = pacf)) +
  geom_bar(stat = "identity", fill = "green", width = 0.5) +
  geom_hline(yintercept = 1.96/sqrt(length(arch_data$RMASI)), linetype = "dashed", color = "red") +
  geom_hline(yintercept = -1.96/sqrt(length(arch_data$RMASI)), linetype = "dashed", color = "red") +
  labs(title = "PACF Correlogram") +
  xlab("Lags") +
  ylab("PACF")

# Combiner les graphiques en un seul
combined_plot <- grid.arrange(acf_plot, pacf_plot, nrow = 1)


# ADF et KPSS test pour RMASI
adf <- adf.test(arch_data$RMASI)
kpss <- kpss.test(arch_data$RMASI)

# Tableau des résultats + interprétation
statio <- data.frame(
  Variable = "RMASI",
  "Test ADF" = adf$p.value,
  "Test KPSS" = kpss$p.value,
  "Stationnarité" = "Stationnaire"
)

kable(t(statio))


library(stats)
library(aTSA)
library(lmtest)

# AR model
ar_model = arima(arch_data$RMASI, order = c(1,0,0))
ar_const = ar_model$coef[2]
ar_coef = ar_model$coef[1]
ar_aic = ar_model$aic

# MA model
ma_model = arima(arch_data$RMASI, order = c(0,0,1))
ma_const = ma_model$coef[2]
ma_coef = ma_model$coef[1]
ma_aic = ma_model$aic

# ARMA(1,1) model
arma_model = arima(arch_data$RMASI, order = c(1,0,1))
arma_const = arma_model$coef[3]
arma_AR = arma_model$coef[1]
arma_MA = arma_model$coef[2]
arma_aic = arma_model$aic

ARMA <- data.frame(
  Model = c("AR(1)", "MA(1)", "ARMA(1,1)"),
  constantes = I(list(ar_const, ma_const, arma_const)),
  AR_term = I(list(ar_coef, "", arma_AR)),
  MA_term = I(list("", ma_coef, arma_MA)),
  AIC = c(ar_aic, ma_aic, arma_aic)
)
print(t(ARMA))


arch_test_AR = arch.test(ar_model, output = FALSE)
arch_test_MA = arch.test(ma_model, output = FALSE)
arch_test_ARMA = arch.test(arma_model, output = FALSE)

# Résultats de Lagrange Multiplier Test
arch_test_AR[,4:5] # Model AR(1)
arch_test_MA[,4:5] # model MA(1)
arch_test_ARMA[,4:5] # model ARMA(1,1)


library(rugarch)

# Conversion des données en TS
returns <- ts(arch_data$RMASI)

# creation d'un vecteur de la variable dichotomique
dummy <- ts(arch_data$DCOVID)

# Definition du model GARCH(1,1) avec variable dichotomique
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                               external.regressors = matrix(dummy)),
                         mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
                         distribution.model = "norm")

# Fit
garch_model <- ugarchfit(spec = garch_spec, data = returns)


print(garch_model)

