ANALISIS FACTORIAL
================

# IMPORTAR LA BASE DE DATOS EN FORMA EXCEL

``` r
library(readxl)
datos<- read_excel("C:/Users/57313/Downloads/tabla excel (1).xlsx")
```

# TIPIFICACION O ESTANDARIZACION DE LAS VARIABLES

la tipificacion permite que todas las variables metricas gocen de una
misma unidad de medida estdistica

``` r
datost<- datos #crear una nueva base de datos 
datost<- scale(datost, center= T , scale= T)
datost<- as.data.frame(datost)
```

\#NORMALIDAD MULTIVARIANTE  
HO:normalidad multivariante H1:no normalidad multivariante confianza=95%
Alfa= 5%=0,05 P value &gt; alfa:no se rechaza a la HO (Normalidad) P
Value &lt; alfa:se rechaza a la HO (No normalidad)

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test         Statistic            p value Result
    ## 1 Mardia Skewness  70.8466451074404 0.0874081337394897    YES
    ## 2 Mardia Kurtosis -1.22592863485921  0.220225531962007    YES
    ## 3             MVN              <NA>               <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test   Variable Statistic   p value Normality
    ## 1 Shapiro-Wilk   nota 1      0.9491  0.1604      YES   
    ## 2 Shapiro-Wilk   nota 2      0.8105   1e-04      NO    
    ## 3 Shapiro-Wilk   nota 3      0.7299  <0.001      NO    
    ## 4 Shapiro-Wilk   horas       0.8419   4e-04      NO    
    ## 5 Shapiro-Wilk   nota 4      0.7634  <0.001      NO    
    ## 6 Shapiro-Wilk reprobados    0.8926  0.0056      NO    
    ## 
    ## $Descriptives
    ##             n          Mean Std.Dev      Median       Min       Max       25th
    ## nota 1     30 -4.672658e-16       1 -0.44391864 -2.005855 1.9400889 -0.6905401
    ## nota 2     30  3.469447e-18       1  0.05717908 -1.086403 2.3443424 -0.5146118
    ## nota 3     30 -1.442856e-16       1  0.77773345 -2.175685 0.7777335 -0.6989756
    ## horas      30 -2.657116e-15       1  0.22155814 -2.577071 0.9212154 -0.8279278
    ## nota 4     30  3.691347e-17       1 -0.57611730 -1.639718 1.0192845 -0.7533842
    ## reprobados 30 -6.112732e-17       1  0.01994916 -2.373950 2.4138484 -0.5785256
    ##                  75th        Skew   Kurtosis
    ## nota 1     0.95360300  0.12460484 -1.0271410
    ## nota 2     0.05717908  1.08203196  0.4958638
    ## nota 3     0.77773345 -0.86554413 -0.4821635
    ## horas      0.92121543 -0.93207876 -0.2745832
    ## nota 4     1.01928446 -0.06475535 -1.7671910
    ## reprobados 0.01994916  0.26507224  0.2901682

# MATRIZ DE CORRELACIONES

HO: Correlacion entre las variables=0 (no hay correacion) H1:
Correlacion diferente de 0 (si hay correlacion) cuando no se rechaza la
HO, no se aplica AFE. Se rechace HO, si para aplicar AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##            nota 1 nota 2 nota 3 horas nota 4 reprobados
    ## nota 1       1.00   0.28  -0.06 -0.03   0.24      -0.51
    ## nota 2       0.28   1.00   0.03  0.30   0.69      -0.48
    ## nota 3      -0.06   0.03   1.00  0.06  -0.13      -0.06
    ## horas       -0.03   0.30   0.06  1.00   0.60      -0.42
    ## nota 4       0.24   0.69  -0.13  0.60   1.00      -0.62
    ## reprobados  -0.51  -0.48  -0.06 -0.42  -0.62       1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##            nota 1 nota 2 nota 3 horas nota 4 reprobados
    ## nota 1       0.00   1.00   1.00  1.00   1.00       0.05
    ## nota 2       0.14   0.00   1.00  1.00   0.00       0.07
    ## nota 3       0.75   0.88   0.00  1.00   1.00       1.00
    ## horas        0.87   0.11   0.76  0.00   0.01       0.20
    ## nota 4       0.20   0.00   0.48  0.00   0.00       0.00
    ## reprobados   0.00   0.01   0.73  0.02   0.00       0.00
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7]) #se crea la matriz de correlaciones
correlaciones$r #matriz de correlaciones
```

    ##                 nota 1      nota 2      nota 3       horas     nota 4
    ## nota 1      1.00000000  0.27587247 -0.06103291 -0.03014676  0.2386892
    ## nota 2      0.27587247  1.00000000  0.02969849  0.29728420  0.6859247
    ## nota 3     -0.06103291  0.02969849  1.00000000  0.05866621 -0.1340451
    ## horas      -0.03014676  0.29728420  0.05866621  1.00000000  0.6003502
    ## nota 4      0.23868921  0.68592466 -0.13404511  0.60035025  1.0000000
    ## reprobados -0.50827565 -0.48498341 -0.06481003 -0.42329957 -0.6246491
    ##             reprobados
    ## nota 1     -0.50827565
    ## nota 2     -0.48498341
    ## nota 3     -0.06481003
    ## horas      -0.42329957
    ## nota 4     -0.62464913
    ## reprobados  1.00000000

``` r
r <-as.matrix(correlaciones$r)
```

P value &gt; alfa: no se rechaza HO estamos en esta situacion ,por lo
tanto no se aplica Analisis Factorial Exploratorio P value &lt; alfa: se
rechaza HO

# INDICADORES DE APLICABILIDAD DEL AFE(BONDAD DEL AJUSTE)

## CONTRASTE DE ESFERICIDAD DE BARTLETT

HO: las correlaciones teoricas entre cada par de variables es nulo H1:
Las correlaciones teoricas entre cada par de variables no es nula

``` r
dim(datost) #tamaño de la muestra=30 personas 
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n=30)
```

    ## $chisq
    ## [1] 57.05328
    ## 
    ## $p.value
    ## [1] 8.037563e-07
    ## 
    ## $df
    ## [1] 15

## MEDIDA DE ADECUACION MUESTRAL DE KAISER, MEYER Y OKLIN( KMO)

Estudia variable por variables,si son o no aceptadas en el modelo para
hacer AFE. (que variables elimino) Se mantiene una variable en el
modelo,si el KNO es igual o mayor a 0,7. se elimina una variable del
modelo,si el KNO es menor a 0,7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.61
    ## MSA for each item = 
    ##     nota 1     nota 2     nota 3      horas     nota 4 reprobados 
    ##       0.53       0.66       0.12       0.60       0.61       0.70

KMO=0.61 mediocre NOTA1=0.53 miserable NOTA2=0.66 mediocre NOTA3=0.12
inaceptable HORAS=0.60 miserable NOTAS4=0.61 miserable REPROBADOS=0.70
regular \# DETERMINACION DEL NUMERO DE FACTORES A EXTRAER \#\# metodos
de las componentes principales iteradas (o Ejes principales)

Este metodo de las ejes principales es de naturaliza no parametrica,es
decir, que se ocupa,cuando no hay normalidad multivariante;pero,tambien
es valido para modelos parametricos (normalidad multivariante)

``` r
fa.parallel(r, fm="pa", n.obs=30, ylabel = "Eigenvalues") 
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](exa_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

con el metodo de los ejes principales es 1 factor \#\# Metodo de las
componentes principales metodos parametrico,sirve solo para modelos con
normalidad multivariante.

``` r
fa.parallel(r, fm="pc", n.obs=30, ylabel = "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

![](exa_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

con el metodo de las componentes principales 1 factor \#\# Metodo de la
maxima verosimilitud con el metodo parametrico,sirve solo para modelos
con normalidad multivariante.

``` r
fa.parallel(r, fm="ml", n.obs=30, ylabel = "Eigenvalues")
```

![](exa_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

1 factor \#\# Metodo paralelo con interaciones metodo parametrico,sirve
solo para modelos con normalidad multivariante.

``` r
library(paran)
```

    ## Loading required package: MASS

``` r
paran(r, iterations= 100,graph= F)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 100 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.960819    3.779856      1.819037
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (1 components retained)

# METODOS DE EXTRACCION DE FACTORES

## METODO DE ANALISIS DE LOS COMPONENTES PRINCIPALES(ACP)

``` r
acp<- principal(r, nfactors=1, rotate= "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##              PC1     h2   u2 com
    ## nota 1      0.48 0.2310 0.77   1
    ## nota 2      0.77 0.5978 0.40   1
    ## nota 3     -0.02 0.0004 1.00   1
    ## horas       0.64 0.4063 0.59   1
    ## nota 4      0.90 0.8013 0.20   1
    ## reprobados -0.83 0.6967 0.30   1
    ## 
    ##                 PC1
    ## SS loadings    2.73
    ## Proportion Var 0.46
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.14 
    ## 
    ## Fit based upon off diagonal values = 0.86

PC1: cargas factoriales de cada variable. h2: Comunalidad(varianza comun
explicado). notas1 es explicado en un 23% por el factor extraido,notas 2
es explicada en un 59% por el factor extraido,notas3 por un 0,0%,horas
por un 40%,notas 4 por un 80% y reprobados por un 69%

mientras mas alta sea h2 es mejor el modelo. 0;1

u2: Especificacidad (varianza residuall o varianza no explicada) nota 1
no es explicada en un 76%,notas 2en 40%,notas 3en 99%,horas en 59%,nota
4 en 19% y mreprrobadosen 30%

Mientras mas pequeños sea u2 es mejor el modelo. 0;1

h2+u2=1 Comunidad+Especificada=1 varianza explicada+varianza no
explicada=1

SS loadings= 2.73(es la varianza explicada en valores absolutos o la
suma de los h2 )

proportion var=0.46 ( El % que la varianza explicada representa del
total)

Lo ideal es que proportion var sea lo mas cercano a 1.

RMSR= 0,14 (Raiz cuadrada media de los residuos) teoricamente un modelo
presenta una solcuion adecuada cuando el RSMR PC1: cargas

## METODOS DE LOS EJES PRINCIPALES O COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi<-fa(r, nfactors = 1, fm= "pa", rotate =  "none", n.obs= 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##              PA1     h2   u2 com
    ## nota 1      0.36 0.1282 0.87   1
    ## nota 2      0.68 0.4672 0.53   1
    ## nota 3     -0.02 0.0004 1.00   1
    ## horas       0.53 0.2768 0.72   1
    ## nota 4      0.93 0.8618 0.14   1
    ## reprobados -0.76 0.5754 0.42   1
    ## 
    ##                 PA1
    ## SS loadings    2.31
    ## Proportion Var 0.38
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  2.18 with Chi Square of  57.05
    ## The degrees of freedom for the model are 9  and the objective function was  0.59 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.11 
    ## The df corrected root mean square of the residuals is  0.14 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  10.19  with prob <  0.33 
    ## The total number of observations was  30  with Likelihood Chi Square =  15.15  with prob <  0.087 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.748
    ## RMSEA index =  0.147  and the 90 % confidence intervals are  0 0.284
    ## BIC =  -15.46
    ## Fit based upon off diagonal values = 0.92
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.96
    ## Multiple R square of scores with factors          0.92
    ## Minimum correlation of possible factor scores     0.85

## METODO DE MAXIMA VEROSIMILITUD

``` r
mve<- fa(r,nfactors=1, fm= "ml", rotate= "none", n.obs= 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##              ML1    h2    u2 com
    ## nota 1      0.24 0.059 0.941   1
    ## nota 2      0.69 0.475 0.525   1
    ## nota 3     -0.13 0.017 0.983   1
    ## horas       0.60 0.362 0.638   1
    ## nota 4      0.99 0.989 0.011   1
    ## reprobados -0.63 0.397 0.603   1
    ## 
    ##                 ML1
    ## SS loadings    2.30
    ## Proportion Var 0.38
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  2.18 with Chi Square of  57.05
    ## The degrees of freedom for the model are 9  and the objective function was  0.53 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.13 
    ## The df corrected root mean square of the residuals is  0.16 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  14.54  with prob <  0.1 
    ## The total number of observations was  30  with Likelihood Chi Square =  13.42  with prob <  0.14 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.819
    ## RMSEA index =  0.123  and the 90 % confidence intervals are  0 0.266
    ## BIC =  -17.2
    ## Fit based upon off diagonal values = 0.89
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   0.99
    ## Multiple R square of scores with factors          0.99
    ## Minimum correlation of possible factor scores     0.98

## PRESENTACION GRAFICA DE LOS FACTORES EXTRAIDOS

## metodo de analisis de los componentes principales (ACP)

\#\#Solo se grafica cuando hay 2 factores a extraer,con 1 factor no hay
grafica

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate = "none", scores = T)
acp1$scores
```

    ##               PC1
    ##  [1,] -1.94630322
    ##  [2,] -1.10865533
    ##  [3,] -1.00465879
    ##  [4,] -1.09138932
    ##  [5,] -1.56446328
    ##  [6,] -1.61298855
    ##  [7,] -1.29591163
    ##  [8,] -0.77423669
    ##  [9,] -0.52821162
    ## [10,]  0.04948628
    ## [11,] -0.15919513
    ## [12,] -0.14126950
    ## [13,]  0.02690536
    ## [14,] -0.31796696
    ## [15,] -0.16617080
    ## [16,] -0.92132075
    ## [17,]  1.20587758
    ## [18,]  0.47489618
    ## [19,]  0.47489618
    ## [20,]  0.47489618
    ## [21,]  0.47489618
    ## [22,]  0.89437140
    ## [23,]  1.03892229
    ## [24,]  0.84713175
    ## [25,]  1.49763850
    ## [26,]  1.43623220
    ## [27,]  1.19152060
    ## [28,]  0.41607805
    ## [29,]  1.18942141
    ## [30,]  0.93957143

``` r
puntacionesfactoriales_acp<- acp1$scores
puntacionesfactoriales_acp<- as.data.frame(puntacionesfactoriales_acp)
```

## METODO DE LAS COMPONENTES PRINCIPALES ITERADAS (CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm= "pa", rotate = "none", n.obs = 30,scores = "regression")
cpi1$scores
```

    ##              PA1
    ##  [1,] -1.2077626
    ##  [2,] -1.0887728
    ##  [3,] -1.0332670
    ##  [4,] -1.0493550
    ##  [5,] -1.6175944
    ##  [6,] -1.6670021
    ##  [7,] -1.3403600
    ##  [8,] -0.8760035
    ##  [9,] -0.4268972
    ## [10,] -0.3869611
    ## [11,] -0.5216200
    ## [12,] -0.4695464
    ## [13,] -0.3825708
    ## [14,] -0.2041037
    ## [15,] -0.4587918
    ## [16,] -0.3744929
    ## [17,]  1.4076009
    ## [18,]  0.7874783
    ## [19,]  0.7874783
    ## [20,]  0.7874783
    ## [21,]  0.7874783
    ## [22,]  0.9501189
    ## [23,]  0.9769323
    ## [24,]  0.9039641
    ## [25,]  1.0661259
    ## [26,]  0.9406346
    ## [27,]  1.0164615
    ## [28,]  0.8715313
    ## [29,]  1.0552144
    ## [30,]  0.7666039

``` r
puntfact_cpi<- cpi1$scores
puntfact_cpi<- as.data.frame(puntfact_cpi)
```

## METODOS DE LA MAXIMA VEROSIMILITUD

``` r
mvel<- fa(datost[,2:7], nfactors = 1, fm= "ml", rotate = "none", n.obs = 30,scores = "regression")
mvel$scores
```

    ##              ML1
    ##  [1,] -0.7954580
    ##  [2,] -0.7605610
    ##  [3,] -0.7593334
    ##  [4,] -0.7607090
    ##  [5,] -1.6332974
    ##  [6,] -1.6318116
    ##  [7,] -1.1915601
    ##  [8,] -1.1736350
    ##  [9,] -0.7550694
    ## [10,] -0.5546942
    ## [11,] -0.5615849
    ## [12,] -0.6488150
    ## [13,] -0.7279065
    ## [14,] -0.7469094
    ## [15,] -0.7287134
    ## [16,] -0.7593102
    ## [17,]  1.0240374
    ## [18,]  0.9971308
    ## [19,]  0.9971308
    ## [20,]  0.9971308
    ## [21,]  0.9971308
    ## [22,]  1.0186286
    ## [23,]  1.0209213
    ## [24,]  1.0065349
    ## [25,]  1.0369580
    ## [26,]  1.0379114
    ## [27,]  1.0272636
    ## [28,]  0.9926033
    ## [29,]  1.0238030
    ## [30,]  1.0121839

``` r
puntfact_mve<- mvel$scores
puntfact_mve<- as.data.frame(puntfact_mve)
```

# OBTENCION DE LOS FACTORES EXTRAIDOS

se trabaja con el metodo que el investigador decidio (ACP,CPI,MVE)

``` r
factor.scores(r, mve,method = "Trurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                     ML1
    ## nota 1      0.002788994
    ## nota 2      0.014119515
    ## nota 3     -0.001421934
    ## horas       0.010145759
    ## nota 4      0.970823383
    ## reprobados -0.011239657
    ## 
    ## $r.scores
    ##     ML1
    ## ML1   1
    ## 
    ## $R2
    ## [1] 0.9892455

## AGREGAR FACTOR EXTRAIDO ( PUNTACIONES FACTORIALES) EN EL DATA FRAME ORIGINAL

``` r
datos_puntaciones<- c(datos, puntacionesfactoriales_acp)
datos_puntaciones<- as.data.frame(datos_puntaciones)
```

# GUARDAR EL DATA FRAME “DATOS\_PUNTACIONES”

``` r
setwd("C:/Users/57313/Downloads") #Donde guardar tu archivo excel csv
write.table(datos_puntaciones, file = "encuesta.csv", sep = ";", row.names = F, dec =";")
```
