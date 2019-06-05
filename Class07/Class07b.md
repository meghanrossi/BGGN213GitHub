Class07\_2
================

``` r
source("http://tinyurl.com/rescale-R")
```

    ## Warning in file(filename, "r", encoding = encoding): "internal" method
    ## cannot handle https redirection to: 'https://bioboot.github.io/bggn213_f17/
    ## class-material/rescale.R'

    ## Warning in file(filename, "r", encoding = encoding): "internal" method
    ## failed, so trying "libcurl"

``` r
x <- df1$IDs
y <- df2$IDs
x[x %in% y]
```

    ## [1] "gene2" "gene3"

``` r
y[y %in% x]
```

    ## [1] "gene2" "gene3"

``` r
gene_intersect2(df1, df2)
```

    ##     IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
    ## 2 gene2   1                               -2
    ## 3 gene3   1                                1

``` r
# if (!requireNamespace("BiocManager"))
#     install.packages("BiocManager")
# BiocManager::install()
```

browseVignettes(package = "dplyr")
