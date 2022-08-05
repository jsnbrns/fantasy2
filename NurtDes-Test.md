NurtDes Corr Compare
================
Jason Burns
2022-08-05

``` r
library(tidyverse)
library(tidyselect)
library(haven)
library(lavaan)
library(semTools)
library(knitr)
library(kableExtra)
library(psych)
library(cocor)
library(apaTables)
library(broom)

data<-read.csv("NurtDes_S2_data_j_06.17.22.csv")
```

# Discriminant validity corr matrix

*correlations with CNCMS competition, nurturance, and IMS commitment.
Benjamini & Hochberg corrected p-values.*

``` r
discrim_corr <- data %>% 
  select(
    #variables of interest
    desire,
    #comparison vars 
    finance, ls_plan, clean, h_c_maint, cc_dev, meals, p_logistics, hh_admin 
  ) %>%
  corr.test(
    x = .,
    method = "pearson", 
    adjust = "none")

'%!in%' <- function(x,y)!('%in%'(x,y))

discrim_r <- discrim_corr$r %>% 
  as_tibble(rownames = "vars")  %>% 
  select(vars, desire) %>% 
  filter(vars %!in% c("desire")) %>% 
  pivot_longer(
    cols = contains("desire"),
    values_to = "r",
    names_to = "corr_with"
  )

discrim_p <- discrim_corr$p %>% 
  as_tibble(rownames = "vars")  %>% 
  select(vars, desire) %>% 
  filter(vars %!in% c("desire")) %>% 
#  mutate(
#    across(.cols = where(is.numeric),
#           .fns = ~round(., digits = 3)
#           )
#  ) %>% 
  pivot_longer(
    cols = contains("desire"),
    values_to = "p",
    names_to = "corr_with"
  )

vars<-c("finance", "ls_plan", "clean", "h_c_maint", "cc_dev", "meals", "p_logistics", "hh_admin" )
corr_with<-rep(c("desire"), times=8)
n<-rep(c(396), times=8)
discrim_n <- data.frame(vars, corr_with, n)

discrim_full <- 
  full_join(discrim_r, discrim_n, by = c("vars", "corr_with")) %>% 
  full_join(., discrim_p, by = c("vars", "corr_with")) %>% 
  mutate(
    p.adj = p.adjust(p = p, method = "BH")
  ) 

discrim_full %>% 
  kable(format = "html", escape = F, digits = 3) %>% 
  kable_styling("striped", full_width = T)
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
vars
</th>
<th style="text-align:left;">
corr_with
</th>
<th style="text-align:right;">
r
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
p
</th>
<th style="text-align:right;">
p.adj
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.002
</td>
<td style="text-align:right;">
0.002
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
clean
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.002
</td>
<td style="text-align:right;">
0.002
</td>
</tr>
<tr>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
meals
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
<tr>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:left;">
desire
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
396
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.000
</td>
</tr>
</tbody>
</table>

# My code:

``` r
##Function: flatten the correlation matrix

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#Flatten and remove desire
matrix<-flattenCorrMatrix(discrim_corr$r, discrim_corr$p) %>% filter(row != "desire") %>% rename(var1=column, var2=row, var1var2=cor) %>% subset(select=-p)

#Get just the desire ~ target var values
v1<-discrim_r[1:8,-2]

#Merge the matrices to create one big one
matrix2<-merge(matrix, v1, by.x = "var1", by.y = "vars", all.x = TRUE)%>% rename (var1des=r)
#Merge again
matrix3<-merge(matrix2, v1, by.x = "var2", by.y = "vars", all.x = TRUE)%>% rename (var2des=r)%>% 
  mutate(
    Var1vsVar2 = paste(var1, "vs", var2, sep = "")
  ) 

#t values for test
r.test.out.t<-matrix3 %>%
   as_tibble() %>% 
  split(.$Var1vsVar2) %>% 
  map(
    .f = ~ with(data = .,
                r.test(
                  r12 = var1des, #corr between IMS and target
                  r13 = var2des, #corr between nurt and target
                  r23 = var1var2, #corr between IMS and nurt
                  n = 396 #samplesize
                  )
                )
        ) %>% 
  map_df("t") %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Var1vsVar2",
    values_to = "Williams' t"
    )

#p values for test
r.test.out.p<-matrix3 %>%
   as_tibble() %>% 
  split(.$Var1vsVar2) %>% 
  map(
    .f = ~ with(data = .,
                r.test(
                  r12 = var1des, #corr between Variable 1 and sexual desire
                  r13 = var2des, #corr between Variable 2 and sexual desire
                  r23 = var1var2, #corr between variable 1 and variable 2
                  n = 396 #samplesize
                  )
                )
        ) %>% 
  map_df("p") %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Var1vsVar2",
    values_to = "p"
    )

desire_cor <- matrix3 %>% 
  full_join(x = ., y = r.test.out.t, by = "Var1vsVar2") %>% 
  full_join(x = ., y = r.test.out.p, by = "Var1vsVar2") %>% 
  mutate(
    p.adj = p.adjust(p = p, method = "BH")
  )

desire_cor %>% 
  arrange(desc(abs(`Williams' t`))) %>% #sort so that significant differences are easy to see. 
  kable(format = "html", escape = F, digits = 3) %>% 
  kable_styling("striped", full_width = T)
```

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
var2
</th>
<th style="text-align:left;">
var1
</th>
<th style="text-align:right;">
var1var2
</th>
<th style="text-align:right;">
var1des
</th>
<th style="text-align:right;">
var2des
</th>
<th style="text-align:left;">
Var1vsVar2
</th>
<th style="text-align:right;">
Williams’ t
</th>
<th style="text-align:right;">
p
</th>
<th style="text-align:right;">
p.adj
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:right;">
0.350
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
p_logisticsvsfinance
</td>
<td style="text-align:right;">
-3.535
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.006
</td>
</tr>
<tr>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:right;">
0.306
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:left;">
p_logisticsvsh_c\_maint
</td>
<td style="text-align:right;">
-3.446
</td>
<td style="text-align:right;">
0.001
</td>
<td style="text-align:right;">
0.006
</td>
</tr>
<tr>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.542
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:left;">
hh_adminvsp_logistics
</td>
<td style="text-align:right;">
3.432
</td>
<td style="text-align:right;">
0.001
</td>
<td style="text-align:right;">
0.006
</td>
</tr>
<tr>
<td style="text-align:left;">
clean
</td>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:right;">
0.415
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:left;">
p_logisticsvsclean
</td>
<td style="text-align:right;">
-2.727
</td>
<td style="text-align:right;">
0.007
</td>
<td style="text-align:right;">
0.035
</td>
</tr>
<tr>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:right;">
0.195
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:left;">
cc_devvsh_c\_maint
</td>
<td style="text-align:right;">
-2.697
</td>
<td style="text-align:right;">
0.007
</td>
<td style="text-align:right;">
0.035
</td>
</tr>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:right;">
0.203
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
cc_devvsfinance
</td>
<td style="text-align:right;">
-2.687
</td>
<td style="text-align:right;">
0.008
</td>
<td style="text-align:right;">
0.035
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:right;">
0.514
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:left;">
p_logisticsvsls_plan
</td>
<td style="text-align:right;">
-2.548
</td>
<td style="text-align:right;">
0.011
</td>
<td style="text-align:right;">
0.045
</td>
</tr>
<tr>
<td style="text-align:left;">
meals
</td>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:right;">
0.453
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:left;">
p_logisticsvsmeals
</td>
<td style="text-align:right;">
-2.479
</td>
<td style="text-align:right;">
0.014
</td>
<td style="text-align:right;">
0.048
</td>
</tr>
<tr>
<td style="text-align:left;">
clean
</td>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:right;">
0.423
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:left;">
cc_devvsclean
</td>
<td style="text-align:right;">
-2.160
</td>
<td style="text-align:right;">
0.031
</td>
<td style="text-align:right;">
0.098
</td>
</tr>
<tr>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.181
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:left;">
hh_adminvscc_dev
</td>
<td style="text-align:right;">
2.076
</td>
<td style="text-align:right;">
0.039
</td>
<td style="text-align:right;">
0.108
</td>
</tr>
<tr>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:left;">
meals
</td>
<td style="text-align:right;">
0.425
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:left;">
mealsvscc_dev
</td>
<td style="text-align:right;">
1.834
</td>
<td style="text-align:right;">
0.067
</td>
<td style="text-align:right;">
0.172
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:right;">
0.344
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:left;">
cc_devvsls_plan
</td>
<td style="text-align:right;">
-1.651
</td>
<td style="text-align:right;">
0.100
</td>
<td style="text-align:right;">
0.232
</td>
</tr>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:right;">
0.335
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
ls_planvsfinance
</td>
<td style="text-align:right;">
-1.282
</td>
<td style="text-align:right;">
0.200
</td>
<td style="text-align:right;">
0.432
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:right;">
0.160
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:left;">
h_c\_maintvsls_plan
</td>
<td style="text-align:right;">
1.165
</td>
<td style="text-align:right;">
0.245
</td>
<td style="text-align:right;">
0.490
</td>
</tr>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
meals
</td>
<td style="text-align:right;">
0.149
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
mealsvsfinance
</td>
<td style="text-align:right;">
-1.075
</td>
<td style="text-align:right;">
0.283
</td>
<td style="text-align:right;">
0.512
</td>
</tr>
<tr>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:left;">
meals
</td>
<td style="text-align:right;">
0.075
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:left;">
mealsvsh_c\_maint
</td>
<td style="text-align:right;">
-1.054
</td>
<td style="text-align:right;">
0.293
</td>
<td style="text-align:right;">
0.512
</td>
</tr>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.724
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
hh_adminvsfinance
</td>
<td style="text-align:right;">
-0.973
</td>
<td style="text-align:right;">
0.331
</td>
<td style="text-align:right;">
0.545
</td>
</tr>
<tr>
<td style="text-align:left;">
clean
</td>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:right;">
0.204
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:left;">
h_c\_maintvsclean
</td>
<td style="text-align:right;">
0.857
</td>
<td style="text-align:right;">
0.392
</td>
<td style="text-align:right;">
0.610
</td>
</tr>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
clean
</td>
<td style="text-align:right;">
0.057
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
cleanvsfinance
</td>
<td style="text-align:right;">
-0.767
</td>
<td style="text-align:right;">
0.444
</td>
<td style="text-align:right;">
0.647
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.480
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:left;">
hh_adminvsls_plan
</td>
<td style="text-align:right;">
0.737
</td>
<td style="text-align:right;">
0.462
</td>
<td style="text-align:right;">
0.647
</td>
</tr>
<tr>
<td style="text-align:left;">
cc_dev
</td>
<td style="text-align:left;">
p_logistics
</td>
<td style="text-align:right;">
0.603
</td>
<td style="text-align:right;">
-0.345
</td>
<td style="text-align:right;">
-0.316
</td>
<td style="text-align:left;">
p_logisticsvscc_dev
</td>
<td style="text-align:right;">
-0.689
</td>
<td style="text-align:right;">
0.491
</td>
<td style="text-align:right;">
0.655
</td>
</tr>
<tr>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.305
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:left;">
hh_adminvsh_c\_maint
</td>
<td style="text-align:right;">
-0.639
</td>
<td style="text-align:right;">
0.523
</td>
<td style="text-align:right;">
0.666
</td>
</tr>
<tr>
<td style="text-align:left;">
meals
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.095
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:left;">
hh_adminvsmeals
</td>
<td style="text-align:right;">
0.503
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
0.749
</td>
</tr>
<tr>
<td style="text-align:left;">
clean
</td>
<td style="text-align:left;">
meals
</td>
<td style="text-align:right;">
0.617
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:left;">
mealsvsclean
</td>
<td style="text-align:right;">
-0.398
</td>
<td style="text-align:right;">
0.691
</td>
<td style="text-align:right;">
0.806
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
clean
</td>
<td style="text-align:right;">
0.240
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:left;">
cleanvsls_plan
</td>
<td style="text-align:right;">
0.346
</td>
<td style="text-align:right;">
0.729
</td>
<td style="text-align:right;">
0.817
</td>
</tr>
<tr>
<td style="text-align:left;">
clean
</td>
<td style="text-align:left;">
hh_admin
</td>
<td style="text-align:right;">
0.054
</td>
<td style="text-align:right;">
-0.189
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:left;">
hh_adminvsclean
</td>
<td style="text-align:right;">
0.238
</td>
<td style="text-align:right;">
0.812
</td>
<td style="text-align:right;">
0.875
</td>
</tr>
<tr>
<td style="text-align:left;">
ls_plan
</td>
<td style="text-align:left;">
meals
</td>
<td style="text-align:right;">
0.293
</td>
<td style="text-align:right;">
-0.222
</td>
<td style="text-align:right;">
-0.226
</td>
<td style="text-align:left;">
mealsvsls_plan
</td>
<td style="text-align:right;">
0.065
</td>
<td style="text-align:right;">
0.948
</td>
<td style="text-align:right;">
0.982
</td>
</tr>
<tr>
<td style="text-align:left;">
finance
</td>
<td style="text-align:left;">
h_c\_maint
</td>
<td style="text-align:right;">
0.184
</td>
<td style="text-align:right;">
-0.152
</td>
<td style="text-align:right;">
-0.153
</td>
<td style="text-align:left;">
h_c\_maintvsfinance
</td>
<td style="text-align:right;">
0.023
</td>
<td style="text-align:right;">
0.982
</td>
<td style="text-align:right;">
0.982
</td>
</tr>
</tbody>
</table>
