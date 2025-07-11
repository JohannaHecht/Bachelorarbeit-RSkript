rm(list = ls())
dev.off()
gc()
#Umfrage einlesen
df <- read.csv(file.choose(), sep = ";")
names(df)
head(df)

#Umkodierung von v_22, v_73 und v_97
df$v_22 <- df$v_22 - 5
df$v_73 <- df$v_73 - 5
df$v_97 <- df$v_97 - 5
table(df$v_22)
table(df$v_73)
table(df$v_97)

#Rosenberg-Items festlegen
rses_items <- c("v_12", "v_85", "v_86", "v_87", "v_88", "v_89", "v_90", "v_91", "v_92", "v_93")
#Negativ formulierte Items des Rosenberg
rses_invert <- c("v_85", "v_88", "v_89", "v_91", "v_92")
#SSEIT-Items festlegen
sseit_items <- c("v_13", "v_14", "v_15", "v_16", "v_17", "v_18", "v_19", "v_20",
                 "v_21", "v_22", "v_23", "v_24", "v_25", "v_26", "v_27", "v_28",
                 "v_29", "v_30", "v_31", "v_32", "v_33", "v_34", "v_35", "v_36",
                 "v_37", "v_38", "v_39", "v_40", "v_41", "v_42", "v_43", "v_44",
                 "v_45")
sseit_invert <- c("v_17", "v_40", "v_45")
ffqm_items <- c("v_46", "v_47", "v_48", "v_49", "v_50", "v_51", "v_52", "v_53",
                "v_54", "v_55", "v_56", "v_57", "v_58", "v_59", "v_60", "v_61",
                "v_62", "v_63", "v_64", "v_65", "v_66", "v_67", "v_68", "v_69",
                "v_70", "v_71", "v_72", "v_73", "v_74", "v_75", "v_76", "v_77",
                "v_78", "v_79", "v_80", "v_81", "v_82", "v_83", "v_84")
ffqm_invert <- c("v_48", "v_50", "v_53", "v_55", "v_58", "v_59",
                 "v_62", "v_63", "v_67", "v_68", "v_70", "v_73", "v_75", "v_79",
                 "v_80", "v_83", "v_84")

reverse_items <- function(df, vars, max_scale) {
  df[vars] <- lapply(df[vars], function(x) max_scale + 1 - x)
  return(df)
}

#Umkodierung
df <- reverse_items(df, rses_invert, 4)
df <- reverse_items(df, sseit_invert, 5)
df <- reverse_items(df, ffqm_invert, 5)


#Daten bereinigen
#Alter
df_clean <- df[df$v_112 >= 18 & df$v_112 <= 60, ]
#Deutschkenntnisse
df_clean <- df_clean[df_clean$v_111 <= 3, ]
#Unvollständige Antworten
df_clean <- df_clean[complete.cases(df_clean), ]
#Anzahl unvollständiger Zeilen anzeigen lassen
sum(!complete.cases(df_clean))

# Standardabweichung der Antworten auf RSES-Items
df_clean$sd_rses <- apply(df_clean[rses_items], 1, sd)
# Übersicht anzeigen
summary(df_clean$sd_rses)
hist(df_clean$sd_rses, main = "RSES: Standardabweichung pro Person", xlab = "SD", breaks = 10)
# Standardabweichung der SSEIT-Antworten
df_clean$sd_sseit <- apply(df_clean[sseit_items], 1, sd)
# Übersicht anzeigen
summary(df_clean$sd_sseit)
hist(df_clean$sd_sseit, main = "SSEIT: Standardabweichung pro Person", xlab = "SD", breaks = 10)
# Standardabweichung der FFMQ-Antworten
df_clean$sd_ffmq <- apply(df_clean[ffqm_items], 1, sd)
# Übersicht anzeigen
summary(df_clean$sd_ffmq)
hist(df_clean$sd_ffmq, main = "FFMQ: Standardabweichung pro Person", xlab = "SD", breaks = 10)
# Fälle markieren, bei denen SD < 0.3 (Straightlining-Verdacht)
df_clean$low_quality_rses  <- df_clean$sd_rses  < 0.3
df_clean$low_quality_sseit <- df_clean$sd_sseit < 0.3
df_clean$low_quality_ffmq  <- df_clean$sd_ffmq  < 0.3
# Nur Fälle behalten mit ausreichender Varianz
df_clean <- df_clean[!(df_clean$low_quality_rses |
                         df_clean$low_quality_sseit |
                         df_clean$low_quality_ffmq), ]
#Anteil gleicher Antworten
mode_prop <- function(x) {
  freq <- table(x)
  max(freq) / length(x)
}
#Anteil häufigster Antwort bei RSES
df_clean$mode_rses <- apply(df_clean[rses_items], 1, mode_prop)
#Übersicht & Histogramm
summary(df_clean$mode_rses)
hist(df_clean$mode_rses, main = "RSES: Anteil häufigster Antwort", xlab = "Anteil", breaks = 10)
#Anteil häufigster Antwort bei SSEIT
df_clean$mode_sseit <- apply(df_clean[sseit_items], 1, mode_prop)
# Übersicht & Histogramm
summary(df_clean$mode_sseit)
hist(df_clean$mode_sseit, main = "SSEIT: Anteil häufigster Antwort", xlab = "Anteil", breaks = 10)
# Anteil häufigster Antwort bei FFMQ
df_clean$mode_ffmq <- apply(df_clean[ffqm_items], 1, mode_prop)
# Übersicht & Histogramm
summary(df_clean$mode_ffmq)
hist(df_clean$mode_ffmq, main = "FFMQ: Anteil häufigster Antwort", xlab = "Anteil", breaks = 10)
df_clean$low_quality_rses2  <- df_clean$mode_rses  > 0.9
df_clean$low_quality_sseit2 <- df_clean$mode_sseit > 0.9
df_clean$low_quality_ffmq2  <- df_clean$mode_ffmq  > 0.9
# Ausschluss aller Fälle mit extrem einseitigen Antworten auf mindestens einer Skala
df_clean <- df_clean[!(df_clean$low_quality_rses2 |
                         df_clean$low_quality_sseit2 |
                         df_clean$low_quality_ffmq2), ]

#Vorraussetzungen prüfen für unabhängigen t-test

# Mittelwert der FFMQ-Items berechnen
df_clean$ffmq_mean <- rowMeans(df_clean[ffqm_items], na.rm = TRUE)
table(df_clean$Geschlecht)

# Q-Q-Plots zur visuellen Prüfung der Normalverteilung
# Q-Q-Plot für weiblich
qqnorm(df_clean$ffmq_mean[df_clean$Geschlecht == 1], main = "Q-Q-Plot: weiblich")
qqline(df_clean$ffmq_mean[df_clean$Geschlecht == 1])

# Q-Q-Plot für männlich
qqnorm(df_clean$ffmq_mean[df_clean$Geschlecht == 2], main = "Q-Q-Plot: männlich")
qqline(df_clean$ffmq_mean[df_clean$Geschlecht == 2])

#Ausreißer prüfen
max(df_clean$ffmq_mean[df_clean$Geschlecht == 2])
df_clean[df_clean$Geschlecht == 2 & df_clean$ffmq_mean == max(df_clean$ffmq_mean[df_clean$Geschlecht == 2]), ]

#bleibt im Datensatz

#Noramlverteilung ist erfüllt

#Varianzhomogenität prüfen
library(car)
leveneTest(ffmq_mean ~ as.factor(Geschlecht), data = df_clean)

#Varianzhomogenität ist erfüllt

#t-test durchführen
t.test(ffmq_mean ~ as.factor(Geschlecht), data = df_clean, var.equal = TRUE)


mean_frauen_sum <- mean(df_clean$ffmq_sum[df_clean$Geschlecht == 1], na.rm = TRUE)
mean_maenner_sum <- mean(df_clean$ffmq_sum[df_clean$Geschlecht == 2], na.rm = TRUE)

cat("Summenmittelwert FFMQ – Frauen:", round(mean_frauen_sum, 2), "\n")
cat("Summenmittelwert FFMQ – Männer:", round(mean_maenner_sum, 2), "\n")


#Vorrausetzungen Pearson-Korrelation

# Skalenwerte Achtsamkeit (FFMQ) und Selbstwert (RSES) berechnen
df_clean$ffmq_score <- rowMeans(df_clean[ffqm_items], na.rm = TRUE)
df_clean$rses_score <- rowMeans(df_clean[rses_items], na.rm = TRUE)

# Visualisierung: Scatterplot Achtsamkeit vs. Selbstwert
plot(df_clean$ffmq_score, df_clean$rses_score,
     xlab = "Achtsamkeit (FFMQ)",
     ylab = "Selbstwert (RSES)",
     main = "Scatterplot: Achtsamkeit vs. Selbstwert")
abline(lm(rses_score ~ ffmq_score, data = df_clean), col = "blue")

# Normalverteilungsprüfung mit Q-Q-Plots
# Q-Q-Plot für Achtsamkeit
qqnorm(df_clean$ffmq_score, main = "Q-Q-Plot: Achtsamkeit (FFMQ)")
qqline(df_clean$ffmq_score)

#Normalverteilung bei ffqm/Achtsamkeit erfüllt

# Q-Q-Plot für Selbstwert
qqnorm(df_clean$rses_score, main = "Q-Q-Plot: Selbstwert (RSES)")
qqline(df_clean$rses_score)

#Normalverteilung bei rses/Selbstwert erfüllt


#Pearson-Korrelation zwischen Achtsamkeit (FFMQ) und Selbstwert (RSES)
cor.test(df_clean$ffmq_score, df_clean$rses_score, method = "pearson")

#Achtsamkeit korreliert positiv mit Selbstwert


#Facetten Achtsamkeit benennen
# Beobachten (Observe)
observe_items <- c("v_46", "v_49", "v_51", "v_56", "v_60", "v_64", "v_70", "v_80")

# Beschreiben (Describe)
describe_items <- c("v_47", "v_52", "v_57", "v_61", "v_67", "v_72", "v_77", "v_82")

# Nicht-Reagieren (Nonreactivity)
nonreact_items <- c("v_54", "v_65", "v_69", "v_73", "v_79", "v_83", "v_84")

# Nicht-Bewerten (Nonjudging)
nonjudge_items <- c("v_48", "v_55", "v_59", "v_63", "v_68", "v_74", "v_78", "v_85") 

# Handeln mit Bewusstheit (Acting with Awareness)
awareness_items <- c("v_50", "v_53", "v_58", "v_62", "v_66", "v_71", "v_76", "v_81")


#Summenwerte der FFMQ-Facetten berechnen
df_clean$ffmq_observe_sum    <- rowSums(df_clean[observe_items], na.rm = TRUE)
df_clean$ffmq_describe_sum   <- rowSums(df_clean[describe_items], na.rm = TRUE)
df_clean$ffmq_nonreact_sum   <- rowSums(df_clean[nonreact_items], na.rm = TRUE)
df_clean$ffmq_nonjudge_sum   <- rowSums(df_clean[nonjudge_items], na.rm = TRUE)
df_clean$ffmq_awareness_sum  <- rowSums(df_clean[awareness_items], na.rm = TRUE)

#Mittelwerte der FFMQ-Facetten berechnen
df_clean$ffmq_observe_mean    <- rowMeans(df_clean[observe_items], na.rm = TRUE)
df_clean$ffmq_describe_mean   <- rowMeans(df_clean[describe_items], na.rm = TRUE)
df_clean$ffmq_nonreact_mean   <- rowMeans(df_clean[nonreact_items], na.rm = TRUE)
df_clean$ffmq_nonjudge_mean   <- rowMeans(df_clean[nonjudge_items], na.rm = TRUE)
df_clean$ffmq_awareness_mean  <- rowMeans(df_clean[awareness_items], na.rm = TRUE)


#Zusammenfassung aller Mittelwerte (Minimum, Median, Mean, Max usw.)
summary(df_clean[, c("ffmq_observe_mean", "ffmq_describe_mean", 
                     "ffmq_nonreact_mean", "ffmq_nonjudge_mean", 
                     "ffmq_awareness_mean")])

#Vorraussetzungen für Pearson-Korrelation für die Facetten

# Q-Q-Plots für Normalverteilung der FFMQ-Facetten-Beobachten
qqnorm(df_clean$ffmq_observe_mean, main = "Q-Q-Plot: Beobachten")
qqline(df_clean$ffmq_observe_mean)

# Q-Q-Plots für Normalverteilung der FFMQ-Facetten-Beschreiben
qqnorm(df_clean$ffmq_describe_mean, main = "Q-Q-Plot: Beschreiben")
qqline(df_clean$ffmq_describe_mean)

# Q-Q-Plots für Normalverteilung der FFMQ-Facetten-Nicht-Reagieren
qqnorm(df_clean$ffmq_nonreact_mean, main = "Q-Q-Plot: Nicht-Reagieren")
qqline(df_clean$ffmq_nonreact_mean)

# Q-Q-Plots für Normalverteilung der FFMQ-Facetten-Nicht-Beobachten
qqnorm(df_clean$ffmq_nonjudge_mean, main = "Q-Q-Plot: Nicht-Bewerten")
qqline(df_clean$ffmq_nonjudge_mean)

# Q-Q-Plots für Normalverteilung der FFMQ-Facetten-Handeln-mit-Bewusstheit
qqnorm(df_clean$ffmq_awareness_mean, main = "Q-Q-Plot: Handeln mit Bewusstheit")
qqline(df_clean$ffmq_awareness_mean)

# Beobachten und Selbstwert
plot(df_clean$ffmq_observe_mean, df_clean$rses_score,
     xlab = "Beobachten", ylab = "Selbstwert",
     main = "Scatterplot: Beobachten vs. Selbstwert")
abline(lm(rses_score ~ ffmq_observe_mean, data = df_clean), col = "blue")

# Beschreiben und Selbstwert
plot(df_clean$ffmq_describe_mean, df_clean$rses_score,
     xlab = "Beschreiben", ylab = "Selbstwert",
     main = "Scatterplot: Beschreiben vs. Selbstwert")
abline(lm(rses_score ~ ffmq_describe_mean, data = df_clean), col = "blue")

# Nicht-Reagieren und Selbstwert
plot(df_clean$ffmq_nonreact_mean, df_clean$rses_score,
     xlab = "Nicht-Reagieren", ylab = "Selbstwert",
     main = "Scatterplot: Nicht-Reagieren vs. Selbstwert")
abline(lm(rses_score ~ ffmq_nonreact_mean, data = df_clean), col = "blue")

# Nicht-Bewerten und Selbstwert
plot(df_clean$ffmq_nonjudge_mean, df_clean$rses_score,
     xlab = "Nicht-Bewerten", ylab = "Selbstwert",
     main = "Scatterplot: Nicht-Bewerten vs. Selbstwert")
abline(lm(rses_score ~ ffmq_nonjudge_mean, data = df_clean), col = "blue")

# Handeln mit Bewusstheit und Selbstwert
plot(df_clean$ffmq_awareness_mean, df_clean$rses_score,
     xlab = "Handeln mit Bewusstheit", ylab = "Selbstwert",
     main = "Scatterplot: Bewusstes Handeln vs. Selbstwert")
abline(lm(rses_score ~ ffmq_awareness_mean, data = df_clean), col = "blue")

#Linearität bei den Facetten mehr oder weniger gegeben

#Ausreißer identifizieren
boxplot(df_clean$ffmq_observe_mean, main = "Boxplot: Beobachten")
boxplot(df_clean$ffmq_describe_mean, main = "Boxplot: Beschreiben")
boxplot(df_clean$ffmq_nonreact_mean, main = "Boxplot: Nicht-Reagieren")
boxplot(df_clean$ffmq_nonjudge_mean, main = "Boxplot: Nicht-Bewerten")
boxplot(df_clean$ffmq_awareness_mean, main = "Boxplot: Handeln mit Bewusstheit")
boxplot(df_clean$rses_score, main = "Boxplot: Selbstwert (RSES)")

#Keine bedenklichen ausreißer

# z-Werte zur Identifikation von Ausreißern
z_observe   <- scale(df_clean$ffmq_observe_mean)
z_describe  <- scale(df_clean$ffmq_describe_mean)
z_nonreact  <- scale(df_clean$ffmq_nonreact_mean)
z_nonjudge  <- scale(df_clean$ffmq_nonjudge_mean)
z_awareness <- scale(df_clean$ffmq_awareness_mean)
z_rses      <- scale(df_clean$rses_score)

# Ausreißerpositionen anzeigen
which(abs(z_observe) > 3)
which(abs(z_describe) > 3)
which(abs(z_nonreact) > 3)
which(abs(z_nonjudge) > 3)
which(abs(z_awareness) > 3)
which(abs(z_rses) > 3)

#Einige Ausreißer auffällig


# Pearson-Korrelationen zwischen Achtsamkeits-Facetten und Selbstwert (mit Ausreißern)
cor.test(df_clean$ffmq_observe_mean, df_clean$rses_score, method = "pearson")
cor.test(df_clean$ffmq_describe_mean, df_clean$rses_score, method = "pearson")
cor.test(df_clean$ffmq_nonreact_mean, df_clean$rses_score, method = "pearson")
cor.test(df_clean$ffmq_nonjudge_mean, df_clean$rses_score, method = "pearson")
cor.test(df_clean$ffmq_awareness_mean, df_clean$rses_score, method = "pearson")

# Z-Werte berechnen
z_observe   <- scale(df_clean$ffmq_observe_mean)
z_describe  <- scale(df_clean$ffmq_describe_mean)
z_nonreact  <- scale(df_clean$ffmq_nonreact_mean)
z_nonjudge  <- scale(df_clean$ffmq_nonjudge_mean)
z_awareness <- scale(df_clean$ffmq_awareness_mean)
z_rses      <- scale(df_clean$rses_score)

# TRUE für alle, die innerhalb des Bereichs liegen
no_outliers <- abs(z_observe)   <= 3 &
  abs(z_describe)  <= 3 &
  abs(z_nonreact)  <= 3 &
  abs(z_nonjudge)  <= 3 &
  abs(z_awareness) <= 3 &
  abs(z_rses)      <= 3

#Datensatz filtern
df_no_outliers <- df_clean[no_outliers, ]


#Korrelation ohne Ausreißer (Sensitivitätstests)
cor.test(df_no_outliers$ffmq_observe_mean, df_no_outliers$rses_score, method = "pearson")
cor.test(df_no_outliers$ffmq_describe_mean, df_no_outliers$rses_score, method = "pearson")
cor.test(df_no_outliers$ffmq_nonreact_mean, df_no_outliers$rses_score, method = "pearson")
cor.test(df_no_outliers$ffmq_nonjudge_mean, df_no_outliers$rses_score, method = "pearson")
cor.test(df_no_outliers$ffmq_awareness_mean, df_no_outliers$rses_score, method = "pearson")

#Sensitivitätstest(da Normalverteilunf des RSES fragwürdig)

# Spearman-Korrelation: Gesamtscore Achtsamkeit vs. Selbstwert
cor.test(df_clean$ffmq_score, df_clean$rses_score, method = "spearman")

# Spearman-Korrelation Facetten
cor.test(df_clean$ffmq_observe_mean, df_clean$rses_score, method = "spearman")
cor.test(df_clean$ffmq_describe_mean, df_clean$rses_score, method = "spearman")
cor.test(df_clean$ffmq_nonreact_mean, df_clean$rses_score, method = "spearman")
cor.test(df_clean$ffmq_nonjudge_mean, df_clean$rses_score, method = "spearman")
cor.test(df_clean$ffmq_awareness_mean, df_clean$rses_score, method = "spearman")

#Moderatoranalyse EI

#Voraussetzungen prüfen für Multiple lineare Regression

#Prädiktoren zentrieren
df_clean$ffmq_z   <- scale(df_clean$ffmq_score, center = TRUE, scale = FALSE)
df_clean$sseit_score <- rowMeans(df_clean[sseit_items], na.rm = TRUE)
df_clean$sseit_z  <- scale(df_clean$sseit_score, center = TRUE, scale = FALSE)
df_clean$interact <- df_clean$ffmq_z * df_clean$sseit_z
model_mod <- lm(rses_score ~ ffmq_z + sseit_z + interact, data = df_clean)
summary(model_mod)

#lineare Zusammenhänge
plot(df_clean$ffmq_score, df_clean$rses_score, 
     main = "Achtsamkeit vs. Selbstwert", xlab = "FFMQ", ylab = "RSES")
abline(lm(rses_score ~ ffmq_score, data = df_clean), col = "blue")

plot(df_clean$sseit_score, df_clean$rses_score, 
     main = "Emotionale Intelligenz vs. Selbstwert", xlab = "SSEIT", ylab = "RSES")
abline(lm(rses_score ~ sseit_score, data = df_clean), col = "darkgreen")

# Histogramm der Residuen
hist(resid(model_mod), main = "Histogramm der Residuen", xlab = "Residuen")

# QQ-Plot
qqnorm(resid(model_mod))
qqline(resid(model_mod), col = "red")

#Homoskedastizität
plot(fitted(model_mod), resid(model_mod),
     xlab = "Vorhergesagte Werte", ylab = "Residuen",
     main = "Residuen vs. Vorhergesagte Werte")
abline(h = 0, col = "red")

# Paket für Bootstrapping herunter laden
install.packages("boot")  
library(boot)

# Bootstrapping-Funktion
mod_fun <- function(data, indices) {
  d <- data[indices, ]  # Bootstrap-Stichprobe
  model <- lm(rses_score ~ ffmq_z + sseit_z + interact, data = d)
  return(coef(model))  # Nur Regressionskoeffizienten
}

# Reproduzierbarkeit
set.seed(123)
boot_results <- boot(data = df_clean, statistic = mod_fun, R = 2000)

boot_results
boot.ci(boot_results, type = "perc", index = 4)
names(coef(model_mod))

install.packages("lmtest")  
library(lmtest)
bptest(model_mod)

#Multikollinearität
library(car)
vif(model_mod)

#Unabhängigkeit der Residuen
library(lmtest)
dwtest(model_mod)

# Interaktionsterm erzeugen
df_clean$interact <- df_clean$ffmq_z * df_clean$sseit_z

# Regression mit Interaktion
model_mod <- lm(rses_score ~ ffmq_z + sseit_z + interact, data = df_clean)
summary(model_mod)

# Modell 1: nur Hauptvariablen
mod_main <- lm(rses_score ~ ffmq_z + sseit_z, data = df_clean)

# Modell 2: Hauptvariablen + Interaktion
mod_interact <- lm(rses_score ~ ffmq_z + sseit_z + interact, data = df_clean)

# Vergleich der beiden Modelle
anova(mod_main, mod_interact)

#Sensitivitätstest wenn rses als nicht normalverteilt definiert wird
install.packages("MASS")
library(MASS)

# Robuste Moderatoranalyse mit rlm
model_robust <- rlm(rses_score ~ ffmq_z + sseit_z + interact, data = df_clean)
summary(model_robust)

library(MASS)

# Robustes Modell MIT Interaktion
model_robust <- rlm(rses_score ~ ffmq_z + sseit_z + interact, data = df_clean)

# Robustes Modell OHNE Interaktion
model_robust_base <- rlm(rses_score ~ ffmq_z + sseit_z, data = df_clean)

# Total Sum of Squares (TSS)
tss <- sum((df_clean$rses_score - mean(df_clean$rses_score))^2)

# Residual Sum of Squares (RSS)
rss_full <- sum(resid(model_robust)^2)
rss_base <- sum(resid(model_robust_base)^2)

# Pseudo-R² beider Modelle
pseudo_r2_full <- 1 - (rss_full / tss)
pseudo_r2_base <- 1 - (rss_base / tss)

# ΔR²
delta_r2 <- pseudo_r2_full - pseudo_r2_base

# Ausgabe
pseudo_r2_full
pseudo_r2_base
delta_r2

#Demografische Daten analysieren

#Altersspanne
range(df_clean$v_112, na.rm = TRUE)
# Mittelwert des Alters
mean(df_clean$v_112, na.rm = TRUE)

# Standardabweichung des Alters
sd(df_clean$v_112, na.rm = TRUE)

#Anzahl Männer und Frauen
table(factor(df_clean$Geschlecht, levels = c(1, 2), labels = c("weiblich", "männlich")))

#Bildungsgrad
bildungsgrad_labels <- c(
  "1" = "Kein Schulabschluss",
  "2" = "Hauptschule",
  "3" = "Mittlere Reife",
  "4" = "(Fach-)Abitur",
  "5" = "Bachelor",
  "6" = "Master",
  "7" = "Promotion"
)
table(factor(df_clean$Bildungsgrad, levels = 1:7, labels = bildungsgrad_labels))

#Beruflicher Status
beruf_labels <- c(
  "1" = "Erwerbstätig",
  "2" = "In Ausbildung",
  "3" = "Student*in",
  "4" = "Rentner*in",
  "5" = "Sonstiges")
table(factor(df_clean$Beruf, levels = 1:5, labels = beruf_labels))

#Familienstand
familienstand_labels <- c(
  "1" = "Ledig",
  "2" = "Verheiratet",
  "3" = "Geschieden",
  "4" = "Sonstiges"
)
table(factor(df_clean$Familienstand, levels = 1:4, labels = familienstand_labels))

#Psychische Erkrankungen
psy_labels <- c(
  "1" = "Ja, aktuell",
  "2" = "Ja, in der Vergangenheit",
  "3" = "Nein"
)
table(factor(df_clean$v_11, levels = 1:3, labels = psy_labels))

# Gesamtmittelwerte
mean(df_clean$rses_score, na.rm = TRUE)
mean(df_clean$ffmq_score, na.rm = TRUE)
mean(df_clean$sseit_score, na.rm = TRUE)

# Datenrahmen mit Mittelwerten erstellen
library(ggplot2)

#Cronbachs alpha
install.packages("psych")
library(psych)
# RSES
alpha(df_clean[rses_items])

# FFMQ
alpha(df_clean[ffqm_items])

# Facetten
alpha(df_clean[nonjudge_items])
alpha(df_clean[nonreact_items])
alpha(df_clean[observe_items])
alpha(df_clean[awareness_items])
alpha(df_clean[describe_items])

print(nonjudge_items)

# SSEIT – Emotionale Intelligenz
alpha(df_clean[sseit_items])


#Gesamtscore je Person RSES
df_clean$rses_sum <- rowSums(df_clean[rses_items], na.rm = TRUE)

#Mittelwert über alle Proband*innen
mean_rses_sum <- mean(df_clean$rses_sum, na.rm = TRUE)

print(paste("Gesamtsummenmittelwert RSES:", round(mean_rses_sum, 2)))

#Visualisieren
hist(df_clean$rses_sum,
     main = "Verteilung der Gesamtsummen der RSES",
     xlab = "RSES Gesamtscore",
     col = "skyblue", breaks = 10)

#Gesamtscore je Person FFQM
df_clean$ffmq_sum <- rowSums(df_clean[ffqm_items], na.rm = TRUE)

#Mittelwert über alle Proband*innen
mean_ffmq_sum <- mean(df_clean$ffmq_sum, na.rm = TRUE)

print(paste("Gesamtsummenmittelwert FFMQ:", round(mean_ffmq_sum, 2)))

#Visualisierung
hist(df_clean$ffmq_sum,
     main = "Verteilung der Gesamtsummen der FFMQ",
     xlab = "FFMQ Gesamtscore",
     col = "lightgreen", breaks = 10)

#Gesamtsumme je Person SSEIT
df_clean$sseit_sum <- rowSums(df_clean[sseit_items], na.rm = TRUE)

#Mittelwert über alle Personen SSEIT
mean_sseit_sum <- mean(df_clean$sseit_sum, na.rm = TRUE)

print(paste("Gesamtsummenmittelwert SSEIT:", round(mean_sseit_sum, 2)))

#Visualisierung
hist(df_clean$sseit_sum,
     main = "Verteilung der Gesamtsummen der SSEIT",
     xlab = "SSEIT Gesamtscore",
     col = "orange", breaks = 10)

#Standardabweichung
# RSES
sd_rses_sum <- sd(df_clean$rses_sum, na.rm = TRUE)
print(paste("SD RSES Gesamtscore:", round(sd_rses_sum, 3)))

# FFMQ – Achtsamkeit
sd_ffmq_sum <- sd(df_clean$ffmq_sum, na.rm = TRUE)
print(paste("SD FFMQ Gesamtscore:", round(sd_ffmq_sum, 3)))

# Beobachten (Observe)
df_clean$observe_sum <- rowSums(df_clean[observe_items], na.rm = TRUE)

# Beschreiben (Describe)
df_clean$describe_sum <- rowSums(df_clean[describe_items], na.rm = TRUE)

# Nicht-Reagieren (Nonreactivity)
df_clean$nonreact_sum <- rowSums(df_clean[nonreact_items], na.rm = TRUE)

# Nicht-Bewerten (Nonjudging)
df_clean$nonjudge_sum <- rowSums(df_clean[nonjudge_items], na.rm = TRUE)

# Handeln mit Bewusstheit (Awareness)
df_clean$aware_sum <- rowSums(df_clean[awareness_items], na.rm = TRUE)

# Beobachten
mean_observe_sum <- mean(df_clean$observe_sum, na.rm = TRUE)
sd_observe_sum <- sd(df_clean$observe_sum, na.rm = TRUE)

# Beschreiben
mean_describe_sum <- mean(df_clean$describe_sum, na.rm = TRUE)
sd_describe_sum <- sd(df_clean$describe_sum, na.rm = TRUE)

# Nicht-Reagieren
mean_nonreact_sum <- mean(df_clean$nonreact_sum, na.rm = TRUE)
sd_nonreact_sum <- sd(df_clean$nonreact_sum, na.rm = TRUE)

# Nicht-Bewerten
mean_nonjudge_sum <- mean(df_clean$nonjudge_sum, na.rm = TRUE)
sd_nonjudge_sum <- sd(df_clean$nonjudge_sum, na.rm = TRUE)

# Bewusstes Handeln
mean_aware_sum <- mean(df_clean$aware_sum, na.rm = TRUE)
sd_aware_sum <- sd(df_clean$aware_sum, na.rm = TRUE)

# Ausgabe
cat("Summenwerte der FFMQ-Facetten:\n")
cat("Beobachten       - M =", round(mean_observe_sum, 2), ", SD =", round(sd_observe_sum, 2), "\n")
cat("Beschreiben      - M =", round(mean_describe_sum, 2), ", SD =", round(sd_describe_sum, 2), "\n")
cat("Nicht-Reagieren  - M =", round(mean_nonreact_sum, 2), ", SD =", round(sd_nonreact_sum, 2), "\n")
cat("Nicht-Bewerten   - M =", round(mean_nonjudge_sum, 2), ", SD =", round(sd_nonjudge_sum, 2), "\n")
cat("Bewusstes Handeln- M =", round(mean_aware_sum, 2), ", SD =", round(sd_aware_sum, 2), "\n")


# SSEIT – Emotionale Intelligenz
sd_sseit_sum <- sd(df_clean$sseit_sum, na.rm = TRUE)
print(paste("SD SSEIT Gesamtscore:", round(sd_sseit_sum, 3)))


library(dplyr)
library(ggplot2)

# Sicherstellen, dass rses_sum existiert und keine NAs enthält
df_clean <- df_clean %>%
  filter(!is.na(rses_sum) & !is.na(sseit_sum) & !is.na(ffmq_sum))

# Selbstwert in drei Gruppen (Tertile) einteilen
df_clean <- df_clean %>%
  mutate(
    rses_group = cut(
      rses_sum,
      breaks = quantile(rses_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
    )
  )








#Graphen für Diskussion erstellen


library(dplyr)
library(ggplot2)

df_clean <- df_clean %>%
  filter(!is.na(rses_sum) & !is.na(sseit_sum) & !is.na(ffmq_sum))
# Selbstwert in drei Gruppen (Tertile) einteilen
df_clean <- df_clean %>%
  mutate(
    rses_group = cut(
      rses_sum,
      breaks = quantile(rses_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
 
      # Mittelwerte je EI-Wert und Selbstwertgruppe berechnen
      df_line <- df_clean %>%
        group_by(sseit_sum, rses_group) %>%
        summarise(ffmq_mean = mean(ffmq_sum, na.rm = TRUE), .groups = "drop")     
      
      
  
      library(dplyr)
      library(ggplot2)
      
      # Sicherstellen, dass benötigte Variablen vollständig sind
      df_plot <- df_clean %>%
        filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum))
      
      # Gruppierung des Selbstwerts (z. B. in Tertile)
      rses_tertiles <- quantile(df_plot$rses_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
      
      # Neue Gruppierungsvariable erstellen
      df_plot <- df_plot %>%
        mutate(
          rses_group = cut(
            rses_sum,
            breaks = rses_tertiles,
            include.lowest = TRUE,
            labels = c("niedrig", "mittel", "hoch")
          )
        )
      
      # Mittelwerte je Kombination aus EI und Selbstwertgruppe berechnen
      df_line <- df_plot %>%
        group_by(sseit_sum, rses_group) %>%
        summarise(ffmq_mean = mean(ffmq_sum), .groups = "drop")
      
      # Plot erstellen
      ggplot(df_line, aes(x = sseit_sum, y = ffmq_mean, color = rses_group)) +
        geom_line(size = 1.2) +
        labs(
          x = "Emotionale Intelligenz (SSEIT)",
          y = "Achtsamkeit (FFMQ)",
          color = "Selbstwert-Gruppe",
          title = "Achtsamkeit in Abhängigkeit von Emotionaler Intelligenz\nunterteilt nach Selbstwert"
        ) +
        scale_color_manual(values = c("blue", "gray40", "red")) +
        theme_minimal()
      library(ggplot2)
      library(dplyr)
      
      # Neue Version des Graphs des Dataframes mit Selbstwert-Gruppierung
      df_plot <- df_clean %>%
        filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
        mutate(
          rses_group = cut(
            rses_sum,
            breaks = quantile(rses_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
            include.lowest = TRUE,
            labels = c("niedrig", "mittel", "hoch")
          )
        )
      
      # Anderer Graph
      ggplot(df_plot, aes(x = sseit_sum, y = ffmq_sum, color = rses_group)) +
        geom_smooth(method = "loess", se = FALSE, span = 1) +
        labs(
          x = "Emotionale Intelligenz (SSEIT)",
          y = "Achtsamkeit (FFMQ)",
          color = "Selbstwert-Gruppe",
          title = "Geglättete Verläufe: Achtsamkeit in Abhängigkeit von EI\nunterteilt nach Selbstwert"
        ) +
        scale_color_manual(values = c("blue", "gray40", "red")) +
        theme_minimal()
      
      library(ggplot2)
      library(dplyr)
      
      # Neue Gruppierung nach emotionaler Intelligenz in Tertile
      df_plot <- df_clean %>%
        filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
        mutate(
          sseit_group = cut(
            sseit_sum,
            breaks = quantile(sseit_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
            include.lowest = TRUE,
            labels = c("niedrig", "mittel", "hoch")
          )
        )
      
      # Plot erstellen
      ggplot(df_plot, aes(x = ffmq_sum, y = rses_sum, color = sseit_group)) +
        geom_smooth(method = "loess", se = FALSE, span = 1) +
        labs(
          x = "Achtsamkeit (FFMQ)",
          y = "Selbstwert (RSES)",
          color = "Emotionale\nIntelligenz",
          title = ""
        ) +
        scale_color_manual(values = c("blue", "gray40", "red")) +
        theme_minimal()
      
      library(ggplot2)
library(dplyr)

# Gruppierung nach emotionaler Intelligenz (Tertile)
df_plot <- df_clean %>%
  filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
  mutate(
    sseit_group = cut(
      sseit_sum,
      breaks = quantile(sseit_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
    )
  )

# Plot mit Punkten + geglätteter Linie (loess)
ggplot(df_plot, aes(x = ffmq_sum, y = rses_sum, color = sseit_group)) +
  geom_point(alpha = 0.6, size = 2) +  # Punkte
  geom_smooth(method = "loess", se = FALSE, span = 1, size = 1.2) +  # Linien
  labs(
    x = "Achtsamkeit (FFMQ)",
    y = "Selbstwert (RSES)",
    color = "Emotionale\nIntelligenz",
    title = "Zusammenhang zwischen Achtsamkeit und Selbstwert\nnach emotionaler Intelligenz"
  ) +
  scale_color_manual(values = c("blue", "gray40", "red")) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Gruppierung nach emotionaler Intelligenz (Tertile)
df_plot <- df_clean %>%
  filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
  mutate(
    sseit_group = cut(
      sseit_sum,
      breaks = quantile(sseit_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
    )
  )

# Plot mit Punkten + geglätteter Linie (loess)
ggplot(df_plot, aes(x = ffmq_sum, y = rses_sum, color = sseit_group)) +
  geom_point(alpha = 0.6, size = 2) +  # Punkte
  geom_smooth(method = "loess", se = FALSE, span = 1, size = 1.2) +  # Linien
  labs(
    x = "Achtsamkeit (FFMQ)",
    y = "Selbstwert (RSES)",
    color = "Emotionale\nIntelligenz",
    title = "Zusammenhang zwischen Achtsamkeit und Selbstwert\nnach emotionaler Intelligenz"
  ) +
  scale_color_manual(values = c("blue", "gray40", "red")) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Gruppierung nach emotionaler Intelligenz in Tertile und mittlere Gruppe rausfiltern
df_plot <- df_clean %>%
  filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
  mutate(
    sseit_group = cut(
      sseit_sum,
      breaks = quantile(sseit_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
    )
  ) %>%
  filter(sseit_group != "mittel")  # Mittlere Gruppe ausschließen

# Plot mit Punkten und Linien (nur niedrig & hoch)
ggplot(df_plot, aes(x = ffmq_sum, y = rses_sum, color = sseit_group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 1, size = 1.2) +
  labs(
    x = "Achtsamkeit (FFMQ)",
    y = "Selbstwert (RSES)",
    color = "Emotionale\nIntelligenz",
    title = "Zusammenhang zwischen Achtsamkeit und Selbstwert\n(ohne mittlere EI-Gruppe)"
  ) +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Gruppierung nach emotionaler Intelligenz und nur "niedrig" behalten
df_plot <- df_clean %>%
  filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
  mutate(
    sseit_group = cut(
      sseit_sum,
      breaks = quantile(sseit_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
    )
  ) %>%
  filter(sseit_group == "niedrig")  # Nur niedrige EI-Gruppe

# Plot: nur "niedrig" zeigen
ggplot(df_plot, aes(x = ffmq_sum, y = rses_sum)) +
  geom_point(alpha = 0.6, size = 2, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, span = 1, size = 1.2, color = "blue") +
  labs(
    x = "Achtsamkeit (FFMQ)",
    y = "Selbstwert (RSES)",
    title = ""
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# Gruppierung nach emotionaler Intelligenz und nur "niedrig" behalten
df_plot <- df_clean %>%
  filter(!is.na(rses_sum), !is.na(sseit_sum), !is.na(ffmq_sum)) %>%
  mutate(
    sseit_group = cut(
      sseit_sum,
      breaks = quantile(sseit_sum, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("niedrig", "mittel", "hoch")
    )
  ) %>%
  filter(sseit_group == "niedrig")  # Nur niedrige EI-Gruppe

# Plot: nur Glättungslinie anzeigen, ohne Punkte
ggplot(df_plot, aes(x = ffmq_sum, y = rses_sum)) +
  geom_smooth(method = "loess", se = FALSE, span = 1, size = 1.2, color = "blue") +
  labs(
    x = "Achtsamkeit (FFMQ)",
    y = "Selbstwert (RSES)",
    title = ""
  ) +
  theme_minimal()

quantile(df_clean$sseit_sum, probs = c(0, 1/3), na.rm = TRUE)



#Psyschiche Erkrankung als Kovariate

# Nur Personen ohne psychische Erkrankung behalten (v_11 == 3)
df_filtered <- df_clean[df_clean$v_11 == 3, ]

# Pearson-Korrelation neu berechnen 
cor.test(df_filtered$ffmq_score, df_filtered$rses_score, method = "pearson")

# Beobachten (Observe)
cor.test(df_filtered$ffmq_observe_mean, df_filtered$rses_score, method = "pearson")

# Beschreiben (Describe)
cor.test(df_filtered$ffmq_describe_mean, df_filtered$rses_score, method = "pearson")

# Nicht-Reagieren (Nonreact)
cor.test(df_filtered$ffmq_nonreact_mean, df_filtered$rses_score, method = "pearson")

# Nicht-Werten (Nonjudge)
cor.test(df_filtered$ffmq_nonjudge_mean, df_filtered$rses_score, method = "pearson")

# Mit Aufmerksamkeit Handeln (Awareness)
cor.test(df_filtered$ffmq_awareness_mean, df_filtered$rses_score, method = "pearson")


# Mittelwert der Achtsamkeit je Geschlecht
t.test(ffmq_score ~ as.factor(Geschlecht), data = df_filtered, var.equal = TRUE)

# Zentrierte Prädiktoren berechnen
df_filtered$ffmq_z <- scale(df_filtered$ffmq_score, center = TRUE, scale = FALSE)
df_filtered$sseit_z <- scale(df_filtered$sseit_score, center = TRUE, scale = FALSE)

# Interaktionsterm berechnen
df_filtered$interact <- df_filtered$ffmq_z * df_filtered$sseit_z
# Multiple lineare Regression (Moderation)
model_mod_filtered <- lm(rses_score ~ ffmq_z + sseit_z + interact, data = df_filtered)

# Zusammenfassung der Ergebnisse
summary(model_mod_filtered)

# Modell ohne Interaktion
model_base <- lm(rses_score ~ ffmq_z + sseit_z, data = df_filtered)

# Modell mit Interaktion
model_full <- lm(rses_score ~ ffmq_z + sseit_z + interact, data = df_filtered)

# ΔR²
delta_r2 <- summary(model_full)$r.squared - summary(model_base)$r.squared
delta_r2


