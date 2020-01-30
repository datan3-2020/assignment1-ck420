Statistical assignment 1
================
Connor Kowalewski (680011991) Candidate Number: 084707
30/01/2020

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code below.

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages ----------------- tidyverse 1.3.0 --

    ## v ggplot2 3.2.1     v purrr   0.3.3
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts -------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
Data <- read.csv(file.choose (), sep = "\t")
```

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
Data <- select(Data, pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data <- filter(Data, h_memorig == 1)
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>
.

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.
2)  Recode sex into a character vector with the values “male” or
    “female”.
3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
        mutate(EU = ifelse(h_eumem == 1, 1,
                    ifelse(h_eumem == 2, 0, NA)))
Data <- Data %>%
        mutate(sex = ifelse(h_sex_dv == 1, "Male", 
                     ifelse(h_sex_dv == 2, "Female", NA))) 
        
Data <- Data %>%
        mutate(agegr = case_when(
          between(h_age_dv, 16, 25)~"16 to 25",
          between(h_age_dv, 26, 40)~"26 to 40",
          between(h_age_dv, 41, 55)~"41 to 55",
          between(h_age_dv, 56, 70)~"56 to 70",
          h_age_dv >= 70~"Over 70"))
```

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
        count(EU) %>%
        mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 3 x 3
    ##      EU     n  perc
    ##   <dbl> <int> <dbl>
    ## 1     0  9338  40.7
    ## 2     1 11118  48.4
    ## 3    NA  2501  10.9

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

This is an interesting result because it suggests more respondents
supported remain at 48.4% compared with those who supported leave at
40.7% (rounded to 1dp). This would appear to contradict the result of
the 2016 referendum where those who voted leave won. Obviusly though we
must take into account the limitations that this is a much smaller
population compared to those who took part in the referendum and there
is also a sizeable amount of NAs at 2501 which constitues 10.9% (rounded
to 1dp) of the entire sample.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
  group_by(sex) %>%
     count(EU) %>%
        mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 7 x 4
    ## # Groups:   sex [3]
    ##   sex       EU     n  perc
    ##   <chr>  <dbl> <int> <dbl>
    ## 1 Female     0  4859  38.9
    ## 2 Female     1  6371  51.0
    ## 3 Female    NA  1256  10.1
    ## 4 Male       0  4479  42.8
    ## 5 Male       1  4746  45.3
    ## 6 Male      NA  1245  11.9
    ## 7 <NA>       1     1 100

``` r
Data %>%
  group_by(agegr) %>%
     count(EU) %>%
        mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 15 x 4
    ## # Groups:   agegr [5]
    ##    agegr       EU     n  perc
    ##    <chr>    <dbl> <int> <dbl>
    ##  1 16 to 25     0   763 26.4 
    ##  2 16 to 25     1  1749 60.6 
    ##  3 16 to 25    NA   373 12.9 
    ##  4 26 to 40     0  1508 34.4 
    ##  5 26 to 40     1  2440 55.7 
    ##  6 26 to 40    NA   436  9.95
    ##  7 41 to 55     0  2509 40.8 
    ##  8 41 to 55     1  3013 49.0 
    ##  9 41 to 55    NA   628 10.2 
    ## 10 56 to 70     0  2737 46.1 
    ## 11 56 to 70     1  2646 44.5 
    ## 12 56 to 70    NA   558  9.39
    ## 13 Over 70      0  1821 50.6 
    ## 14 Over 70      1  1270 35.3 
    ## 15 Over 70     NA   506 14.1

Write a couple of sentences interpreting your results.

(Results were rounded to 1dp for interpretation)

SEX We can see that roughly 38.9% (4859) of women supported leaving the
EU compared with 51% (6371) supporting remain. This suggests females
were more in support of remaining than leaving, though around 10% of
responses are recorded as NA which is something to bare in mind,
alongside the small sample size. Men on the otherhand seem to be much
more evenly split only just favouring for remain at 45.3% (4746)
compared with 42.8% (4479) leaving. Again though there is a high number
of NAs at 11.9%. All in all though it appears both sexes supported,
though to varying degrees, remain over leave.

Age When we look at age we can see a very clear pattern in that the
younger age groups will overwhelmingly seem to favour remaining whilst
the older age groups will tend to prefer leave. In the 16 to 25 for
example, 60.6% (1749) are remainer compared with just 26.4% (763)
identifying as leave and the sizeable rest being NAs (12.9%, 373).
Compare this with the over 70s category where 50.6% preferred leave and
only 35.3% favoured remain, and you see the pattern at its most
strongest. As with all age groups though the issue of a high amount of
NAs remains.
