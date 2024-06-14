---
title: "SAE Régression"
author: "Binh Minh TRAN"
date: "2024-05-17"
output:
  github_document:
    toc: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(rgl.useNULL = TRUE) # Suppress the separate window.
library(rgl)
library(knitr)
```

### **1.Comprendre l'ensemble de données**

Dans ce jeu de données, nous avons 234 voitures.
Voici la description des autres variables :

- cty et hwy donnent la consommation en carburant des voitures en miles par gallon respectivement pour la conduite en ville et sur l'autoroute.

- displ est la cylindrée du moteur en litres.

- cyl est le nombre de cylindres du moteur

- drv est le mode de transmission : traction avant (f comme front), propulsion (r comme rear) ou quatre roues motrices (4).

- modèle est le modèle de la voiture. Il y a 38 modèles, sélectionnés avec différentes versions entre 1999 et 2008.

- class est une variable catégorielle décrivant le « type » de voiture : deux places, SUV, compacte, etc.

**La variable cible à modéliser est cty.**

#### **1.1 Chargement du jeu de données.**

```{r dataset}

Consommations<-read.csv2(file = "Consommations.csv", header = TRUE, sep = ';', dec = ',', stringsAsFactors = TRUE)

#Transformation
Consommations$year<-as.factor(Consommations$year)
```
#### **1.2 Analyse Exploratoire des Données (EDA).**

```{r EDA}
head(Consommations)  #6 première observations
summary(Consommations)
str(Consommations)
```

#### **1.3 Gestion des données.**

```{r Clean}
is.null(Consommations)
#vérifier la valeur nulle
(col_has_na <- sapply(Consommations, function(x) any(is.na(x))))
```

#### **1.4 Total de voitures.**

```{r Total Voiture}
#Table de l'effectif des modèles de manufacturer
(Eff_Manu<-table(Consommations$manufacturer))
EffManuSort<-sort(Eff_Manu,decreasing = FALSE)
par(mar = c(5, 10, 4, 0.5) + 0.1)
barplot(EffManuSort,xlab="Nombre de voitures",main = "Nombre de modèles par fabricant", horiz = TRUE, xlim = c(0, 40),las=1,col="blue")
```

(*) Chaque modèle n'a pas qu'un seul type

### **2. La relation entre cty et hwy.**

```{r}
#la corrélation de coefficient
cor(Consommations$cty,Consommations$hwy)
```

```{r}
plot(cty~hwy,data=Consommations,col="blue",bg="blue",pch=21)
reglin<-lm(cty~hwy,data=Consommations)
abline(reglin, col="orange",lwd=3)
summary(reglin)
```

> **Commentaire**: Les points sont répartis uniformément autour de la ligne de régression, montrant une relation claire et stable entre les deux variables avec une dispersion modérée. La tendance croissante et la ligne de régression indiquent une forte corrélation positive (R=95%) entre la consommation de carburant en ville et sur autoroute.
> 
> **Conclusion** : Cela indique qu'il existe une relation linéaire forte et positive entre les deux variables, ce qui signifie que les véhicules ayant une haute efficacité de carburant sur autoroute tendent également à avoir une haute efficacité de carburant en ville.


```{r}
boxplot(Consommations$cty,Consommations$hwy,names=c("cty","hwy"),main="La quantité d'émissions polluantes en ville et sur autoroute",col=c("blue","darkgrey"))
```

(*) Miles par gallon.

(*) Plus le nombre de miles par gallon (MPG) est faible, plus la consommation de carbone est élevée.

> **Conclusion**: La consommation de carburant des voitures en milles par gallon a tendance à être plus faible en ville qu'en autoroute. Autrement dit, La consommation de carburant des voitures a tendance à être plus faible en autoroute qu'en ville.


### **3. Modélisation de la variable cty.**

3 modèles ont été utilisés pour prédire la variable `cty` :

##### Tableau des formules de Régression

| Modèle                     | Formule                           |
|----------------------------|-----------------------------------|
| **Régression linéaire**    | \( y = a + b \cdot x \)           |
| **Régression exponentielle** | \( y = b \cdot a^x \)            |
| **Régression logarithmique** | \( y = a \cdot \log(x) + b \)   |


Tout d'abord, il est nécessaire de déterminer la variable indépendante à inclure dans le modèle.

```{r}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(Consommations,upper.panel = panel.cor,col="blue")

```

> les variables qui sont fortement corrélée avec cty sont `displ` et `cyl`.

>  - `displ` est la cylindrée du moteur en litres. 
>  - `cyl` est le nombre de cylindres du moteur


#### **3.1 Le modèle linéaire.**

```{r}
#la corrélation de coéfficient
cor(Consommations$cty,Consommations$displ)
plot(cty~displ,data = Consommations,col="blue",bg="blue",pch=21,main="Ajustement linéaire")
reglinDispl<-lm(cty~displ,data = Consommations)
abline(reglinDispl,col="orange",lwd=3.5)
```

> **Commentaire** : Il existe une relation linéaire forte et négative entre la cylindrée et la consommation de carburant en ville. Cela signifie que les véhicules avec une plus grande cylindrée tendent à avoir une consommation de carburant plus élevée en ville.

#### **3.2 Le modèle exponentiel**

##### 3.2.1 Ajustement exponentiel avec modèle non-linéaire nls():

D'abord, il faut trouver les valeurs initiales de a et b pour le modèle non-linéaire `nls()`, afin d'atteindre plus rapidement le point de convergence. Ces valeurs de a et b ne sont pas encore le couple final qui minimise le RSS \( \sum_{i=1}^{n}(y_i - \hat{y}_i)^2 \), mais elles peuvent aider à accélérer le processus de convergence vers la solution optimale (où le RSS est minimisé).


```{r}
# Fonction pour essayer différentes valeurs initiales
finda_b<- function(data,formula, a_vals, b_vals) {
  for (a in a_vals) {
    for (b in b_vals) {
      try({
        model <- nls(formula, data = data, start = list(a = a, b = b))
        return(list(a=a,b=b))
      }, silent = TRUE)
    }
  }
  stop("aucune valeur favorable")
}

# Initialiser les valeurs initiales de a et b
a_vals <- seq(1, 3, by = 1)
b_vals <- seq(1, 3, by = 1)


resultat <- finda_b(Consommations, cty ~ b * a^(displ), a_vals, b_vals)
print(resultat)
```

On utilise la fonction `nls` (moindres carrés non linéaires) avec a et b récupérés du résultat de la fonction `finda_b` pour exécuter l'équation \( y = b \cdot a^x \). 

```{r}
(regExp <- nls(cty ~ b * a^(displ),data=Consommations, start = list(a = resultat$a, b = resultat$b)))
summary(regExp)
```

La fonction ajustée est : \( y = 30.08 \cdot 0.84^x \)

On doit après ordonner les valeurs de `displ` dans l'ordre croissant. Le but est d'éviter les discontinuités dans la courbe de prédiction car c'est un modèle non linéaire.

```{r}
# sorted_index renvoie la permutation des indices des éléments de `displ` qui les trierait dans l'ordre croissant
sorted_index <- order(Consommations$displ)
sorted_displ <- Consommations$displ[sorted_index]
sorted_cty <- Consommations$cty[sorted_index]
sorted_pred <- predict(regExp)[sorted_index]
```

```{r}
#trace un nuage de point
plot(cty ~ displ, data = Consommations, main = "Ajustement exponentielle", xlab = "displ", ylab = "cty", pch = 19, col = "blue")

#tracer une courbe de la fonction exponentielle
lines(sorted_displ, sorted_pred, col = "orange", lwd = 3.5)

```

##### 3.2.2 Ajustement exponentiel avec modèle linéaire:

Une fonction exponentielle dans la forme \( y = b \cdot a^x \):

Étape 1: Transformer l'équation en logarithme.

Logarithme népérien

\[
\begin{align*}
y &= b \cdot a^x \\
\log(y) &= \log(b \cdot a^x) \\
&= \log(b) + \log(a^x) \\
&= \log(b) + x \cdot \log(a)
\end{align*}
\]

Ainsi, nous obtenons une nouvelle équation linéaire avec \( \log(y) \) comme variable dépendante et \( x \) comme variable indépendante. 

```{r}
log_cty<-log(Consommations$cty)
plot(log_cty~displ,data = Consommations, main = "Ajustement linéaire exponentielle", xlab = "displ", ylab = "log_cty", col="blue",bg="blue",pch=21)
(linExp<-lm(log_cty~displ,data=Consommations))
abline(linExp,col="orange",lwd=3.5)
```

On obtient l'équation suivante: \( \log(y) = -0.158x + 3.343 \). À présent, la relation de corrélation est entre `log_cty` et `displ`. **Étant donné que la variable cible est `cty`, nous devons continuer à transformer la relation.**
 

Étape 2: Trouver la courbe de prédiction en fonction de la relation entre `cty` et `displ`. Le but est d'assurer que la variable cible soit `cty` et non `log_cty`.

On transforme l'équation obtenue :

\[ y = e^{-0.158 \cdot x + 3.343} \]


```{r}
# y stocke les predictions de y par apport à l'équation au dessus.
y<-exp(-0.158*Consommations$displ+3.343)

# ordonner les paires de valeurs des variables indépendantes et dépendantes afin d'assurer la continuité de la courbe.
sorted_index <- order(Consommations$displ)
sorted_displ <- Consommations$displ[sorted_index]
sorted_y<-y[sorted_index]

#tracer un graphique
plot(cty ~ displ, data = Consommations, main = "Ajustement exponentielle", xlab = "displ", ylab = "cty", pch = 19, col = "blue")
lines(sorted_displ,sorted_y,col="orange",lwd=3.5)
```
```{r}
# RSS 
sum((sorted_cty-sorted_y)^2)
# R^2
(cov(log_cty,Consommations$displ)/(sd(log_cty)*sd(Consommations$displ)))^2
```

> À partir du RSS de ce méthode on peut dire que la premiere methode (retrouve dans 4.2.1) est plus optimise car le RSS de la premiere methode(1358) > celle de la deuxieme methode (1390).

#### **3.3 Le modèle logarithmique.**

##### 3.3.1 Ajustement logarithmique avec modèle non-linéaire nls():

D'abord, on doit trouver les valeurs initiales de a et b pour le modèle non-linéaire `nls()`, afin d'atteindre plus rapidement le point de convergence. Ces valeurs de a et b ne sont pas encore le couple final qui minimise le RSS \( \sum_{i=1}^{n}(y_i - \hat{y}_i)^2 \), mais elles peuvent aider à accélérer le processus de convergence vers la solution optimale (où le RSS est minimisé).

```{r}
# Fonction pour essayer différentes valeurs initiales
finda_b<- function(data,formula, a_vals, b_vals) {
  for (a in a_vals) {
    for (b in b_vals) {
      try({
        model <- nls(formula, data = data, start = list(a = a, b = b))
        return(list(a=a,b=b))
      }, silent = TRUE)
    }
  }
  stop("pas de valeur favorable")
}

# Initialiser les valeurs initiales de a et b
a_vals <- seq(1, 3, by = 1)
b_vals <- seq(1, 3, by = 1)


resultat <- finda_b(Consommations, cty ~ a * log(displ) + b, a_vals, b_vals)
print(resultat)
```

```{r}
(RegLog <- nls(cty ~ a * log(displ) + b, data = Consommations, start = list(a = resultat$a, b = resultat$b)))
```

La fonction ajustée est : \( y = -9.369 \cdot log(x) +27.861 \)

```{r}
# sorted_index renvoie la permutation des indices des éléments de `displ` qui les trierait dans l'ordre croissant
sorted_index <- order(Consommations$displ)
sorted_displ <- Consommations$displ[sorted_index]
sorted_cty <- Consommations$cty[sorted_index]
sorted_pred <- predict(RegLog)[sorted_index]
```

```{r}
plot(Consommations$displ, Consommations$cty, main = "Ajustement logarithmique", xlab = "X", ylab = "Y", pch = 19,col="blue")
#tracer un courbe du fonction logarithme
lines(sorted_displ, sorted_pred, col = "orange", lwd = 3.5)
```

##### 3.3.2 Ajustement logarithmique avec modèle linéaire:

Une fonction logarithmique dans la forme \( y = a \cdot \log(x) + b \):

Étape 1: nous obtenons une nouvelle équation linéaire avec \( \log(x) \) comme variable indépendante  et \( y \) comme variable dépendante.

```{r log(x)}
#transform displ en log
log_displ<-log(Consommations$displ)
#trace un graphique
(ReglinLog<-lm(cty~log_displ,data=Consommations))
plot(cty~log_displ,data = Consommations,col="blue",main="Ajustement logarithme",bg="blue",pch=21)
abline(ReglinLog,col="orange",lwd=3.5)
```

On obtient l'équation suivante : \( \ y = -9.369 \cdot log(x) + 27.861 \). À présent, la relation de corrélation est entre `cty` et `log_displ`. 

Étape 2: Trouver la courbe de prédiction en fonction de la relation entre `cty` et `displ`.

On applique l'equation obtenue pour trouver la courbe de prediction.

```{r}
# sorted_index renvoie la permutation des indices des éléments de `displ` qui les trierait dans l'ordre croissant
sorted_index <- order(Consommations$displ)
sorted_displ <- Consommations$displ[sorted_index]
sorted_cty <- Consommations$cty[sorted_index]
sorted_pred_lin_log <- predict(ReglinLog)[sorted_index]

plot(cty~displ,data = Consommations,col="blue",main="Ajustement logarithme",bg="blue",pch=21)
lines(sorted_displ,sorted_pred_lin_log,col="orange",lwd=3.5)
```

```{r}
# RSS
sum((sorted_cty-sorted_pred_lin_log)^2)
```

> Les deux méthodes du modèle logarithmique renvoient un RSS de 1281. Cela signifie que l'on peut efficacement utiliser l'une des deux méthodes.

### **4. Évaluation de modèle**

Les critères d'évaluation utilisés sont les suivants : RSS, R², RMSE.

- RMSE (Root Mean Square Error): Erreur quadratique moyenne
  $$
  RMSE = \sqrt{\frac{\sum_{i=1}^{n}(y_i - \hat{y}_i)^2}{n}}
  $$

- RSS (Residual Sum of Squares): Somme des carrés des résidus
  $$
  RSS = \sum_{i=1}^{n}(y_i - \hat{y}_i)^2
  $$

- R² (Coefficient of Determination): Coefficient de détermination
  $$
  R^2 = 1 - \frac{RSS}{TSS}
  $$
  où $TSS = \sum_{i=1}^{n}(y_i - \bar{y})^2$ est la somme des carrés totaux.

#### **4.1 Évaluation du modèle linéaire**

```{r}
lin_model <- lm(cty ~ displ, data = Consommations)
(lin_rmse <- (sqrt(mean(resid(lin_model)^2)))) #Residus = prédiction - observation
(lin_rss<-(sum(resid(lin_model)^2)))
(lin_r2 <- summary(lin_model)$r.squared)
```

> **Conclusion**: R² ~ 65%, `displ` ne peut pas représenter la totalité de la variation de `cty`, seulement à 63%, le reste étant représenté par les résidus.

#### **4.2 Évaluation du modèle exponentiel **

```{r}
exp_model <- nlExp <- nls(cty ~ b * a^(displ),data=Consommations, start = list(a = 2, b = 1))
(exp_rmse <- (sqrt(mean(resid(exp_model)^2)))) #Residus = prédiction - observation
(exp_rss<-(sum(resid(exp_model)^2)))
(exp_r2 <- summary(linExp)$r.squared)
```

> **Conclusion** : residual sum-of-squares (RSS): 1358, en moyenne, la différence entre les valeurs prédites et observées est d'environ 1.72, mais il y a plusieurs aberrantes. `displ` ne peut pas représenter la totalité de la variation de `cty`, seulement à 67%

#### **4.3 Évaluation du modèle logarithme **

```{r}
log_model <- nlExp <- nls(cty ~ a * log(displ) + b,data=Consommations, start = list(a = 1, b = 1))
(log_rmse <- (sqrt(mean(resid(log_model)^2)))) #Residus = prédiction - observation
(log_rss<-(sum(resid(log_model)^2)))
(log_r2 <- summary(ReglinLog)$r.squared)
```

> **Conclusion** : R² explique qu'environ 70% de la variance de `cty` peut être expliquée par la variable `log_displ`, 30% étant représenté par les résidus.

### **5. Comparer les performances des modèles**

```{r 4 plot residual vs fitted}
#Représentation des résidus et des valeurs ajustées
par(mfrow=c(1,3))
par(mar=c(4.5,4.5,4.5,4.5))
#Lineaire
plot(fitted(reglinDispl),resid(reglinDispl),col="blue",bg="blue",pch=21,main = "Résiduels vs Ajustés (LIN)",xlab = "Ajustés", ylab = "Résiduels")
abline(h = 0, col = "orange",lwd=2)
#Exp
plot(fitted(regExp), resid(regExp), xlab = "Ajustés", ylab = "Résiduels", main = "Résiduels vs Ajustés (EXP)",col="blue",bg="blue",pch=21)
abline(h = 0, col = "orange",lwd=2)
#Log
plot(fitted(RegLog), resid(RegLog), xlab = "Ajustés", ylab = "Résiduels", main = "Résiduels vs Ajustés (LOG)",col="blue",bg="blue",pch=21)
abline(h = 0, col = "orange",lwd=2)
```

##### Tableau comparaison de l'efficacité des modèles:


| Modèle          | Régression Linéaire | Régression Exponentielle | Régression Logarithmique |
|-----------------|----------------------|---------------------------|---------------------------|
| RSS             | `r lin_rss`          | `r exp_rss`               | `r log_rss`               |
| RMSE            | `r lin_rmse`         | `r exp_rmse`              | `r log_rmse`              |
| \(R^2\)             | `r lin_r2`           | `r exp_r2`                | `r log_r2`                |


##### Critiques :

##### 1. Régression Logarithmique

- **Forces :** 
  - Ce modèle présente les meilleures performances sur les trois métriques (RSS, RMSE et R-carré).
  - Cela suggère qu'une relation logarithmique entre les variables indépendantes et dépendantes offre le meilleur ajustement pour les données données.
  
- **Faiblesses :**
  - L'applicabilité d'un modèle logarithmique dépend de la nature des données.
  - Si les données contiennent des valeurs nulles ou négatives, une transformation logarithmique peut ne pas être appropriée.

##### 2. Régression Exponentielle

- **Forces :** 
  - Le modèle de régression exponentielle fonctionne mieux que le modèle linéaire sur toutes les métriques.
  - Il peut être particulièrement adapté aux données présentant une croissance ou une décroissance exponentielle.

- **Faiblesses :**
  - Comme le modèle logarithmique, le modèle exponentiel peut ne pas être approprié pour tous les types de données.
  - Surtout si les données ne suivent pas une tendance exponentielle.

##### 3. Régression Linéaire

- **Forces :** 
  - La régression linéaire est le modèle le plus simple et le plus interprétable.
  - Il fonctionne de manière décente mais pas aussi bien que les autres modèles.

- **Faiblesses :**
  - Le RSS et le RMSE plus élevés, ainsi que la valeur R-carré plus faible, indiquent que le modèle linéaire ne s'ajuste pas aux données aussi bien que les autres modèles.
  - Il peut être trop simpliste pour capturer les schémas sous-jacents dans les données.


> **Conclusion** : Les trois modèles présentent des valeurs RSS assez similaires. Mais le modèle le plus efficace est le modèle logarithmique, avec le plus petit RMSE et le plus grand \( R^2 \). Sur la base des métriques fournies, le modèle de régression logarithmique est le plus approprié pour les données, suivi par la régression exponentielle et enfin par la régression linéaire. Cependant, le choix final du modèle doit également tenir compte du contexte des données, des hypothèses sous-jacentes de chaque modèle et de l'interprétabilité des résultats.

### **6. Propositions d'amélioration du modèle**

**6.1 Amélioration du modèle logarithmique**:

`hwy` est une autre variable qui est aussi fortement corrélée avec `cty` comme `displ`. Mais la relation correlation entre `hwy` et `displ` n'est pas vraiment forte.

Le but est d'utiliser les deux variables `hwy` et `displ` pour améliorer la précision de la prédiction de la variable `cty`.

Le modèle utilisé est maintenant : 
\[ y = a \cdot \log(x1+x2)+ b \]
avec \( x1 \) et \( x2 \) étant `displ` et `hwy`.


```{r}
# Fonction pour essayer différentes valeurs initiales
finda_b <- function(data, formula, a_vals, b_vals, c_vals) {
  for (a in a_vals) {
    for (b in b_vals) {
        try({
          model <- nls(formula, data = data, start = list(a = a, b = b))
          return(list(a = a, b = b))
        }, silent = TRUE)
      }
  }
  stop("pas de valeur favorable")
}

# Initialiser les valeurs initiales de a, b et c
a_vals <- seq(1, 10, by = 1)
b_vals <- seq(1, 10, by = 1)

resultat <- finda_b(Consommations, cty ~ a * log(displ+hwy) + b, a_vals, b_vals)
print(resultat)
```

```{r}
(RegLog <- nls(cty ~ a * log(displ+hwy) + b, data = Consommations, start = list(a = resultat$a, b = resultat$b)))
```

On obtient l'équation suivante: \( \ y = 20.72 \cdot log(x1+x2) -51 \). On a le RSS maintenant est 730.2 plus petit que le modele avec une seule variable independante `displ` est 1281.

```{r}
# sorted_index renvoie la permutation des indices des éléments de `displ` qui les trierait dans l'ordre croissant
Consommations_displ_hwy<-Consommations$displ+Consommations$hwy
sorted_index <- order(Consommations_displ_hwy)
sorted_displ_hwy <- Consommations_displ_hwy[sorted_index]
sorted_cty <- Consommations$cty[sorted_index]
sorted_pred <- predict(RegLog)[sorted_index]
```

```{r}
plot(Consommations_displ_hwy, Consommations$cty, main = "Ajustement logarithmique", xlab = "hwy + displ", ylab = "cty", pch = 19,col="blue")
#tracer un courbe du fonction logarithme
lines(sorted_displ_hwy, sorted_pred, col = "orange", lwd = 3.5)
```

(*) L'abscisse \( x \) est \( hwy + displ \).


```{r}
#residual vs fitted (modele amelioré)
plot(fitted(RegLog), resid(RegLog), xlab = "Ajustés", ylab = "Résiduels", main = "Résiduels vs Ajustés (LOG)",col="blue",bg="blue",pch=21)
abline(h = 0, col = "orange",lwd=2)
```


**6.2 Amélioration du modèle linéaire**:

On ajoute une autre variable `hwy`.

`hwy` est une autre variable qui est aussi fortement corrélée avec `cty` comme `displ`. Mais la relation correlation entre `hwy` et `displ` n'est pas vraiment forte.

Le but est d'utiliser les deux variables `hwy` et `displ` pour améliorer la précision de la prédiction de la variable `cty`.

Le modèle utilisé est maintenant : 
\[ y = a \cdot  x1 + c \cdot x2 + b \]
avec \( x1 \) et \( x2 \) étant `displ` et `hwy`.


```{r}

displ <- Consommations$displ
hwy <- Consommations$hwy
cty <- Consommations$cty

#modele lineaire
ReglinLog <- lm(cty ~ displ + hwy, data = Consommations)
summary(ReglinLog)
```

On obtient l'équation suivante : \[ \ y = -0.528 \cdot x1 + 0.595\cdot x2 + 4.736 \]

On prédit `cty` avec `displ(la cylindrée du moteur en litres) = 5` et `hwy(en miles par gallon pour la conduite sur l'autoroute) = 25` 

```{r}
predict(ReglinLog, newdata = data.frame(displ = 5, hwy = 25))
```

**Évaluation de modèle**:

```{r}
# Calculer les valeurs ajustées et les résidus
fitted_values <- fitted(ReglinLog)
residuals <- resid(ReglinLog)

# Tracer le graphique des résidus contre les valeurs ajustées
plot(fitted_values, residuals, main = "Graphique des résidus vs valeurs ajustées", xlab = "Valeurs ajustées", ylab = "Résidus",col="blue",pch=21,bg="blue")
abline(h = 0, col = "orange", lwd = 2)
```


```{r}
#RSS
(lin_rssOpti<-(sum(resid(ReglinLog)^2)))
#R^2
(lin_r2<-summary(ReglinLog)$r.squared)
```

```{r}
ModeleLinAvant <-lin_rss
ModeleLinApres <-lin_rssOpti
```

##### Tableau des RSS des Modèles Linéaires.

| RSS Avant Optimisation | RSS Après Optimisation |
|-------------------------------|-------------------------------|
| `r ModeleLinAvant`             | `r ModeleLinApres`             |


> Le modèle linéaire optimisé a réduit le RSS de ~ 5 fois par rapport au modèle linéaire précédent.
