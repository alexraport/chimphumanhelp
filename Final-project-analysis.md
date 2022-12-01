Final project analysis
================
Eloise Pedersen and Alex Raport
2022-12-01

# Description of each variable in your preliminary analysis

Level 1: Response (repeated measure) Level 2: Individuals

Level 1 predictors: none Level 2 predictors: Age; species type (human or
chimpanzee)

Outcome: Individual helper/model selected (Bernoulli distribution marked
as 0 or 1)

``` r
head(data3)
```

    ## # A tibble: 6 × 6
    ##      ID Trial AgeGroup Species    TrialID RespHelp
    ##   <dbl> <dbl>    <dbl> <chr>        <dbl>    <dbl>
    ## 1     1     1        1 Chimpanzee       1        1
    ## 2     1     2        1 Chimpanzee       2        0
    ## 3     1     3        1 Chimpanzee       3        1
    ## 4     2     1        2 Chimpanzee       4        1
    ## 5     2     2        2 Chimpanzee       5        0
    ## 6     2     3        2 Chimpanzee       6        0

See the below graph for a visualization of the Bernoulli distribution:

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

# Model equations

**Human or Chimpanzee only**

\$\$
\$\$

**Combined Equation**

$$
  \begin{aligned}
    \text{Helper}_{ti} & = \beta_{0j} + \beta_{1i} \text{Species}_{ti} + e_{ti} \\
    \beta_{0i} & = \gamma_{00} + {\gamma_{01}} \text{AgeGroup}_{i} + u_{0i} \\
    \beta_{1i} & = \gamma_{10} + \gamma_{11} \text{AgeGroup}_{i} + u_{1i} \\
    \text{Helper}_{ti} & = \gamma_{00} + {\gamma_{01}} \text{AgeGroup}_{i} + u_{0i} + 
    \gamma_{10}\text{Species}_{ti} + \gamma_{11} \text{AgeGroup}_{i} * \text{Species}_{ti} + u_{1i} * \text{Species}_{ti} 
    + e_{ti}
  \end{aligned}
$$

# Your code for running the multilevel analyses

## Unconditional Model

``` r
#Human
set.seed(1000)
unconhum <- brm(RespHelp ~ (1 | ID),
                  data = data1,
                  family = bernoulli("logit"), file = "huicc")


#Chimp
unconchi <- brm(RespHelp ~ (1 | ID),
                  data = data2,
                  family = bernoulli("logit"), file = "chicc")

#Both
unconbo <- brm(RespHelp ~ (1 | ID),
                  data = data3,
                  family = bernoulli("logit"), file = "boicc")
```

## ICC

``` r
set.seed(1000)
performance::icc(unconhum)
```

    ## # Intraclass Correlation Coefficient
    ## 
    ##     Adjusted ICC: 0.075
    ##   Unadjusted ICC: 0.075

``` r
performance::icc(unconchi)
```

    ## # Intraclass Correlation Coefficient
    ## 
    ##     Adjusted ICC: 0.068
    ##   Unadjusted ICC: 0.068

``` r
performance::icc(unconbo)
```

    ## # Intraclass Correlation Coefficient
    ## 
    ##     Adjusted ICC: 0.099
    ##   Unadjusted ICC: 0.099

**ICC analysis**

One’s unique ID accounts for 7.5% if variation in humans, 6.8% of
variation in chimpanzees, and 10% of variation when species are combined

## Random Slopes

Is there a different relationship between age and helper chosen when
looking at individuals or overall population?

``` r
data3 %>%
    # randomly sample 16 individuals
    filter(ID %in% sample(unique(ID), 16)) %>%
    # Age on x-axis and RespHelp on y-axis
    ggplot(aes(x = AgeGroup, y = RespHelp)) +
    geom_jitter() + geom_boxplot()+
    # present data of different ids in different panels
    facet_wrap(~ID)
```

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
data3 %>%
    # randomly sample 16 schools
    filter(ID %in% sample(unique(ID), 16)) %>%
    # ses on x-axis and mathach on y-axis
    ggplot(aes(x = Species, y = RespHelp)) +
    # add points (half the original size)
    geom_jitter() + geom_boxplot()+
    # present data of different ids in different panels
    facet_wrap(~ID)
```

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Trials

``` r
trials <- glmer(
    RespHelp ~ Trial + (Trial | ID),
    data = data3,
   family = binomial("logit"))
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(trials)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: RespHelp ~ Trial + (Trial | ID)
    ##    Data: data3
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    418.0    436.5   -204.0    408.0      292 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1262 -0.8929 -0.7584  1.0124  1.2443 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  ID     (Intercept) 0.243724 0.49368       
    ##         Trial       0.001802 0.04245  -1.00
    ## Number of obs: 297, groups:  ID, 88
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.4730     0.3026  -1.563    0.118
    ## Trial         0.1423     0.1226   1.160    0.246
    ## 
    ## Correlation of Fixed Effects:
    ##       (Intr)
    ## Trial -0.906
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
plot_model(trials,
    type = "pred",
    show.data = TRUE, jitter = 0.02,
    title = "", dot.size = 0.5
)
```

    ## $Trial

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Human model for effect of Age on Helper chosen

``` r
#human only model

mhumgl<- glmer(RespHelp ~ AgeGroup + (AgeGroup | ID),
data = data1, family = binomial("logit"))
summary(mhumgl)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: RespHelp ~ AgeGroup + (AgeGroup | ID)
    ##    Data: data1
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    203.8    218.7    -96.9    193.8      142 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4142 -1.1402  0.7071  0.8771  0.8771 
    ## 
    ## Random effects:
    ##  Groups Name        Variance  Std.Dev.  Corr 
    ##  ID     (Intercept) 1.424e-13 3.774e-07      
    ##         AgeGroup    4.180e-14 2.045e-07 -1.00
    ## Number of obs: 147, groups:  ID, 38
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -0.1684     0.5418  -0.311    0.756
    ## AgeGroup      0.4308     0.3416   1.261    0.207
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## AgeGroup -0.949
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
mhum <- brm(RespHelp ~ AgeGroup + (AgeGroup | ID),
                 data = data1, family = bernoulli("logit"), file = "final1")

summary(mhum)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: RespHelp ~ AgeGroup + (AgeGroup | ID) 
    ##    Data: data1 (Number of observations: 147) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 38) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               0.40      0.32     0.02     1.23 1.00     1154
    ## sd(AgeGroup)                0.24      0.20     0.01     0.74 1.00     1265
    ## cor(Intercept,AgeGroup)    -0.27      0.58    -0.98     0.91 1.00     2098
    ##                         Tail_ESS
    ## sd(Intercept)               1189
    ## sd(AgeGroup)                1686
    ## cor(Intercept,AgeGroup)     2088
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.19      0.60    -1.42     1.00 1.00     3021     1790
    ## AgeGroup      0.46      0.39    -0.27     1.25 1.00     3130     2277
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
m1_plotsh <- plot_model(mhum,
    type = "pred",
    show.data = TRUE, jitter = 0.02,
    title = "", dot.size = 0.5
)
m1_plotsh
```

    ## $AgeGroup

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Chimp model for effect of Age on Helper chosen

``` r
#chimp only model

mchigl<- glmer(RespHelp ~ AgeGroup + (AgeGroup | ID),
data = data2, family = binomial("logit"))
summary(mchigl)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: RespHelp ~ AgeGroup + (AgeGroup | ID)
    ##    Data: data2
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    191.4    206.5    -90.7    181.4      145 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6766 -0.6385 -0.6025  1.4780  1.5662 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev. Corr 
    ##  ID     (Intercept) 0.125    0.3535        
    ##         AgeGroup    0.125    0.3535   -1.00
    ## Number of obs: 150, groups:  ID, 50
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) -0.78095    0.82942  -0.942    0.346
    ## AgeGroup    -0.06635    0.46014  -0.144    0.885
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## AgeGroup -0.971
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

``` r
mchimp <- brm(RespHelp ~ AgeGroup + (AgeGroup | ID),
                 data = data2, family = bernoulli("logit"), file = "final2")

summary(mchimp)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: RespHelp ~ AgeGroup + (AgeGroup | ID) 
    ##    Data: data2 (Number of observations: 150) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 50) 
    ##                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## sd(Intercept)               0.62      0.50     0.03     1.87 1.00     1580
    ## sd(AgeGroup)                0.37      0.28     0.01     1.06 1.00      868
    ## cor(Intercept,AgeGroup)    -0.26      0.58    -0.98     0.91 1.00     1312
    ##                         Tail_ESS
    ## sd(Intercept)               1486
    ## sd(AgeGroup)                1193
    ## cor(Intercept,AgeGroup)     1493
    ## 
    ## Population-Level Effects: 
    ##           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept    -0.88      0.99    -2.92     0.97 1.00     4118     2139
    ## AgeGroup     -0.06      0.54    -1.10     1.03 1.00     3684     2254
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
m1_plotsc <- plot_model(mchimp,
    type = "pred",
    show.data = TRUE, jitter = 0.02,
    title = "", dot.size = 0.5
)
m1_plotsc
```

    ## $AgeGroup

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Combined model for effect of Age Group and Species on Helper chosen

``` r
mcombgl<- glmer(RespHelp ~ AgeGroup + Species + AgeGroup * Species + (Species + AgeGroup || ID),
data = data3, family = binomial("logit"))
summary(mcombgl)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: RespHelp ~ AgeGroup + Species + AgeGroup * Species + (Species +  
    ##     AgeGroup || ID)
    ##    Data: data3
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    398.1    431.4   -190.1    380.1      288 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.4145 -0.6971 -0.6487  0.8771  1.5221 
    ## 
    ## Random effects:
    ##  Groups Name              Variance  Std.Dev.  Corr
    ##  ID     (Intercept)       1.947e-07 0.0004413     
    ##  ID.1   SpeciesChimpanzee 2.572e-02 0.1603888     
    ##         SpeciesHuman      2.462e-07 0.0004962 0.64
    ##  ID.2   AgeGroup          2.298e-08 0.0001516     
    ## Number of obs: 297, groups:  ID, 88
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)            -0.5503     0.6273  -0.877    0.380
    ## AgeGroup               -0.1461     0.3685  -0.397    0.692
    ## SpeciesHuman            0.3813     0.8289   0.460    0.646
    ## AgeGroup:SpeciesHuman   0.5775     0.5025   1.149    0.250
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) AgeGrp SpcsHm
    ## AgeGroup    -0.954              
    ## SpeciesHumn -0.757  0.722       
    ## AgGrp:SpcsH  0.699 -0.733 -0.951
    ## optimizer (Nelder_Mead) convergence code: 0 (OK)
    ## Model failed to converge with max|grad| = 0.0128351 (tol = 0.002, component 1)

``` r
combomodel <- brm(
    RespHelp ~ AgeGroup + Species + AgeGroup * Species + (Species + AgeGroup || ID),
    data = data3,
    family = bernoulli("logit"),
    file = "final3",
    seed = 1541)

summary(combomodel)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: RespHelp ~ AgeGroup + Species + AgeGroup * Species + (Species + AgeGroup || ID) 
    ##    Data: data3 (Number of observations: 297) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Group-Level Effects: 
    ## ~ID (Number of levels: 88) 
    ##                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)        0.28      0.20     0.01     0.73 1.00     1725     2196
    ## sd(SpeciesHuman)     0.29      0.23     0.01     0.82 1.00     2317     1864
    ## sd(AgeGroup)         0.15      0.11     0.01     0.41 1.00     2029     2482
    ## 
    ## Population-Level Effects: 
    ##                       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS
    ## Intercept                -0.56      0.67    -1.89     0.74 1.00     4843
    ## AgeGroup                 -0.17      0.39    -0.94     0.60 1.00     4855
    ## SpeciesHuman              0.37      0.92    -1.44     2.21 1.00     4875
    ## AgeGroup:SpeciesHuman     0.64      0.57    -0.50     1.74 1.00     4737
    ##                       Tail_ESS
    ## Intercept                 3618
    ## AgeGroup                  3676
    ## SpeciesHuman              3150
    ## AgeGroup:SpeciesHuman     3459
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

``` r
m1_plotsb <- plot_model(combomodel,
    type = "pred",
    show.data = TRUE, jitter = 0.02,
    title = "", dot.size = 0.5
)
```

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->![](Final-project-analysis_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
cuteplot <- combomodel %>%
    augment(data = data3) %>%
    ggplot(aes(
      x = AgeGroup, y = RespHelp, group = factor(ID),
      color = factor(Species)  # use `species` for coloring lines
    )) + geom_jitter() +
    labs(y = "Predicted Help", color = "Species") + stat_summary(aes(x = AgeGroup, y = RespHelp,
                     fill = factor(AgeGroup)),
                 fun = mean,
                 geom = "point",
                 color = "black", 
                 shape = 4, 
                 size = 2.5)

cuteplot + scale_color_manual(values = c("#006699", "#00CCFF"))
```

![](Final-project-analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# A table summarizing model results

``` r
msummary(
    list( "Human" = mhum, "Chimp" = mchimp, "Combined" = combomodel),
    statistic = "[{conf.low}, {conf.high}]",
    shape = effect + term ~ model,   
    coef_rename = c("b_Intercept" = "(Intercept)",
                    "b_AgeGroup" = "Age Group",
                    "b_SpeciesHuman" = "Species",
                    "b_AgeGroup × SpeciesHuman" = "Age Group × Species",
                    "sd_ID__Intercept" = "SD (ID)",
                    "sd_ID__AgeGroup" = "SD (Age Group)"),
                    "cor_ID__Intercept__AgeGroup" = "Cor (Age Group) ",
                    "sd_ID__SpeciesHuman"  = "SD (Species)",
    gof_map = c("nobs"),
    metrics = "none"
)
```

|        |                                 |       Human       |       Chimp       |     Combined      |
|:-------|:--------------------------------|:-----------------:|:-----------------:|:-----------------:|
| fixed  | (Intercept)                     |      -0.191       |      -0.865       |      -0.548       |
|        |                                 | \[-1.415, 1.002\] | \[-2.923, 0.970\] | \[-1.890, 0.735\] |
|        | Age Group                       |       0.458       |      -0.064       |      -0.165       |
|        |                                 | \[-0.274, 1.254\] | \[-1.104, 1.032\] | \[-0.940, 0.599\] |
|        | Species                         |                   |                   |       0.371       |
|        |                                 |                   |                   | \[-1.437, 2.212\] |
|        | Age Group:SpeciesHuman          |                   |                   |       0.636       |
|        |                                 |                   |                   | \[-0.497, 1.741\] |
| random | SD (ID)                         |       0.325       |       0.503       |       0.246       |
|        |                                 | \[0.016, 1.227\]  | \[0.026, 1.870\]  | \[0.013, 0.731\]  |
|        | SD (Age Group)                  |       0.195       |       0.320       |       0.128       |
|        |                                 | \[0.009, 0.738\]  | \[0.014, 1.062\]  | \[0.006, 0.415\]  |
|        | cor_ID\_\_Intercept\_\_AgeGroup |      -0.411       |      -0.395       |                   |
|        |                                 | \[-0.983, 0.906\] | \[-0.983, 0.914\] |                   |
|        | sd_ID\_\_SpeciesHuman           |                   |                   |       0.243       |
|        |                                 |                   |                   | \[0.009, 0.822\]  |
|        | Num.Obs.                        |        147        |        150        |        297        |

# A short paragraph summarizing the findings from your model addressing your research questions.

## Calculations

Because it’s a logistic model, the coefficient refers to an increase or
decrease in the log odds of the model being chosen, we need to convert
this to probability:

**Human**

Log odds of age group 1 in the human only data to select model 1 (high
rank/mastery) is $\gamma_{00} = −0.200$ $\gamma_{01} = 0.472$ so log
odds for age group 2 to select model 1 is 0.272

plogis(-0.200) = 0.45 = probability age group 1 chooses model 1
plogis(0.272) = 0.56 = probability age group 2 chooses model 1

Probability ratio (age 2/age 1) = 1.24 = effect of being age group 2 in
selecting mastery

**Chimpanzee**

Log odds of age group 1 in the chimp only data to select model 0 (high
payoff/outcome) is $\gamma_{00} = -0.647$ $\gamma_{01} = -0.126$ so log
odds for age group 2 to select model 0 is -0.773

plogis(-0.647) = 0.34 = probability age group 1 chooses model 1
plogis(-0.773) = 0.32 = probability age group 2 chooses model 1

Probability ratio (age 2/age 1) = 0.94 = effect of being age group 2

**Combo**

Log odds of age group 1 in the combined data to select model 1 (high
payoff/outcome) is $\gamma_{00} = -0.560$ $\gamma_{01} = -0.161$ so log
odds for age group 2 to select model 0 is -0.721

plogis(-0.560) = 0.36 = probability age group 1 chooses model 1
plogis(-0.721) = 0.33 = probability age group 2 chooses model 1

Probability ratio (age 2/age 1) = 0.91 = effect of being age group 2

**Species effect**

Log odds of humans in the combined data to select model 1 (high
payoff/outcome) is $\gamma_{00} = -0.560$ $\gamma_{01} = 0.362$ so log
odds for chimps to select model 0 is -0.198

plogis(-0.560) = 0.36 = probability humans choose model 1 plogis(-0.198)
= 0.45 = probability chimps choose model 1

Probability ratio (chimp/human) = 1.25 = effect of being a chimp

## Interpretations

Our research is asking 2 main questions. The first is given the choice
of two individuals from which to seek help (outcome 0 vs mastery 1),
does age influence the decision for humans and chimpanzees? Our second,
is if species type (human or chimpanzee) affects this choice. In
answering the first question we initially looked at chimpanzees and
humans separately. Chimpanzees in the older age group were 0.96 times
more likely to select an outcome model compared to their younger
counterparts. Similarly, human children in the older age group were 1.29
times more likely to select an outcome model compared to younger
children. When looking at an age comparison of humans and chimpanzees
together, those in the older age groups were 1.076923 times more likely
to select the outcome model. In answering our second question, we found
that as a species, chimpanzees are 1.62 times more likely to select an
outcome model over human children.
