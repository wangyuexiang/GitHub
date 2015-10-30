---
title: "SA_PM"
author: "WANG Yuexiang"
date: "14 septembre 2015"
output: html_documentation
---
<style type="text/css">

body, td {
   font-size: 12px;
}
code.r{
  font-size: 12px;
}
pre {
  font-size: 12px
}
</style>


## Analyse sur les trajets de Paul

Apres une analyse sur les trajets historiques de Paul, nous avons identifies deux types de trajets que nous pouvons proposer dans "Smart Alerts".
* **Aller-retour Domicile-travail:** 
  * Aller:  Frejus, vers Italie (78 fois)
  * Retour: Frejus, vers Aix (64 fois)
* **Aller-retour a l'aeroport:**
  * Aller:  Antibes P/V, vers Italie (15 fois)
  * Retour: Antibes P/V, vers Aix (15 fois)

Les parties suivantes visualisent ses trajets frequentes et la deuxieme types en detail.

### 1. Ses trajets frequentes en 2015
![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

* Code:
  * 25006002 = La Barque
  * 25006007 = Le Muy
  * 25006010 = Frejus, 
  * 25006012 = Antibes P/V
* Sens 
  * 1: en direction d'Italie
  * 2: en direction d'Aix

### 2. Ses trajets par Antibes P/V (peut-etre lies a l'aeroport)

* **15 Aller-retour a l'aeroport**
* **Pattern 1: Aller l'apres-midi ou le soir de la veille (8 fois)**
  * aller lundi soir et retour mardi soir (2 fois)
  * aller mardi soir et retour mercredi soir (6 fois)
* **Pattern 2: Aller matin tot et retour le meme soir (7 fois)**
  * lundi (2 fois), mercredi (3 fois), jeudi (1 fois), et vendredi (1 fois)

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 


![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
