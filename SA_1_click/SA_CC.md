---
title: "SA_CC"
author: "Yuexiang Wang"
date: "30 July, 2015"
output: html_document
---

# Models Results

## PM
![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) ![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) ![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png) ![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png) ![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png) 











## CC


```
## Joining by: c("Entr", "Sor")
```

```
## Source: local data frame [4 x 4]
## 
##       Entr      Sor  n           order
## 1 25004211 25004220 65  Retour: Lan-Av
## 2 25004220 25004211 63   Aller: Av-Lan
## 3 25006002 25006009 44  Aller: Bar-Fre
## 4 25006009 25006002 41 Retour: Fre-Bar
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png) ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-3.png) 


## Original points
![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## Result Time Interval by DOW
![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

## Result Geo-representation
![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

## Result Geo-representation for CC
![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


## Result of CC in details

```r
print(Ind, n = 24)
```

```
## Source: local data frame [49 x 7]
## 
##     ID      Ind1       Ind2       Ind3       Ind ModelDecade model
## 1   CC 0.6666667 0.33333333 0.34545455 0.5975758           0     1
## 2   CF 0.6764706 0.32352941 0.32653061 0.6111645           0     1
## 3   FC 0.4761905 0.52380952 0.55147059 0.3658964           0     1
## 4   FF 0.5555556 0.44444444 0.42500000 0.4705556           0     2
## 5   JA 0.3846154 0.61538462 0.61538462 0.2615385           0     1
## 6   JP 0.7586207 0.24137931 0.23255814 0.7121091           0     1
## 7   LF 0.3333333 0.66666667 0.66666667 0.2000000           0     2
## 8   MB 0.5000000 0.50000000 0.50819672 0.3983607           0     2
## 9   MC 0.9473684 0.05263158 0.04819277 0.9377299           0     1
## 10  MG 0.9210526 0.07894737 0.07738095 0.9055764           0     1
## 11  MK 0.8970588 0.10294118 0.11437908 0.8741830           0     1
## 12  MR 0.5937500 0.40625000 0.48913043 0.4959239           0     1
## 13  NP 0.8500000 0.15000000 0.14044944 0.8219101           0     1
## 14 PCO 0.8461538 0.15384615 0.14529915 0.8170940           0     1
## 15 PCU 0.8437500 0.15625000 0.15714286 0.8123214           0     1
## 16  PM 0.5400000 0.46000000 0.57861635 0.4242767           0     1
## 17  VM 0.7000000 0.30000000 0.30000000 0.6400000           0     2
## 18  CF 0.5000000 0.50000000 0.52173913 0.3956522           1     1
## 19  FC 0.4166667 0.58333333 0.59479554 0.2977076           1     1
## 20  FF 0.4000000 0.60000000 0.60000000 0.2800000           1     2
## 21  JA 0.2500000 0.75000000 0.72727273 0.1045455           1     1
## 22  JP 0.6666667 0.33333333 0.30769231 0.6051282           1     1
## 23  LF 0.7000000 0.30000000 0.27083333 0.6458333           1     2
## 24  MB 0.5555556 0.44444444 0.45569620 0.4644163           1     2
## .. ...       ...        ...        ...       ...         ...   ...
```

```r
Ind.result
```

```
## Error in eval(expr, envir, enclos): object 'Ind.result' not found
```

```r
print(result.final %>% filter(ID == "CC"), n = 100) 
```

```
## Source: local data frame [10 x 7]
## 
##    ID     Entr      Sor DOW      Tmin      Tmax noPsg
## 1  CC 25004211 25006009   1  8.001759  9.596959    26
## 2  CC 25006009 25004211   1 19.023189 20.693478    30
## 3  CC 25004211 25006009   2  8.001759  9.596959    26
## 4  CC 25006009 25004211   2 19.023189 20.693478    30
## 5  CC 25004211 25006009   3  8.001759  9.596959    26
## 6  CC 25006009 25004211   3 19.023189 20.693478    30
## 7  CC 25004211 25006009   4  8.001759  9.596959    26
## 8  CC 25006009 25004211   4 19.023189 20.693478    30
## 9  CC 25004211 25006009   5  8.001759  9.596959    26
## 10 CC 25006009 25004211   5 19.023189 20.693478    30
```
