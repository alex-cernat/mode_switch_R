# Understanding the patterns of mode switching in longitudinal studies

This is the syntax used to produce the peer review paper:
Cernat, A. and Sakshaug, W. J. (2021). Understanding the patterns of mode switching in longitudinal studies. *Survey Research Methods*.

## Overview of the syntax

The `master.R` file cleans the data, exports it to mplus, imports it and creates the tables and figures. In the `mplus` folder you can find the mplus models estimated with the results. The `Results.Rmd` and `Results.html` present the two tables in the paper.

The data used were the waves 4 to 10 of the Understanding Society Innovation Panel which can be downloaded from the UK Data Archive: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6849. Please note that more recent versions of the data might be slightly different to the version we have used.

You can find the info of the packages used bellow.


## Info on the session

```
R version 3.6.2 (2019-12-12)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252 
[2] LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets 
[6] methods   base     

other attached packages:
 [1] viridis_0.5.1         viridisLite_0.3.0    
 [3] ggalluvial_0.11.1     broom_0.7.4          
 [5] texreg_1.36.23        ggpubr_0.4.0         
 [7] plotly_4.9.2          poLCA_1.4.1          
 [9] MASS_7.3-51.4         scatterplot3d_0.3-41 
[11] MplusAutomation_0.7-3 reshape2_1.4.3       
[13] rmarkdown_2.1         lme4_1.1-21          
[15] Matrix_1.2-18         haven_2.2.0          
[17] ggthemes_4.2.0        forcats_0.4.0        
[19] stringr_1.4.0         dplyr_1.0.4          
[21] purrr_0.3.3           readr_1.3.1          
[23] tidyr_1.0.2           tibble_3.0.6         
[25] ggplot2_3.2.1         tidyverse_1.3.0      

loaded via a namespace (and not attached):
 [1] minqa_1.2.4             colorspace_1.4-1       
 [3] ggsignif_0.6.0          ellipsis_0.3.0         
 [5] rio_0.5.16              fs_1.3.1               
 [7] rstudioapi_0.11         lubridate_1.7.4        
 [9] xml2_1.2.2              splines_3.6.2          
[11] robustbase_0.93-5       knitr_1.28             
[13] jsonlite_1.6.1          nloptr_1.2.1           
[15] packrat_0.5.0           dbplyr_1.4.2           
[17] shiny_1.4.0             compiler_3.6.2         
[19] httr_1.4.1              backports_1.1.5        
[21] assertthat_0.2.1        fastmap_1.0.1          
[23] lazyeval_0.2.2          cli_2.3.0              
[25] later_1.0.0             htmltools_0.4.0        
[27] tools_3.6.2             coda_0.19-3            
[29] gtable_0.3.0            glue_1.4.2             
[31] Rcpp_1.0.3              carData_3.0-3          
[33] cellranger_1.1.0        vctrs_0.3.6            
[35] nlme_3.1-142            crosstalk_1.0.0        
[37] xfun_0.12               proto_1.0.0            
[39] openxlsx_4.1.4          rvest_0.3.5            
[41] mime_0.9                miniUI_0.1.1.1         
[43] lifecycle_0.2.0         rstatix_0.7.0          
[45] DEoptimR_1.0-8          scales_1.1.0           
[47] hms_0.5.3               promises_1.1.0         
[49] parallel_3.6.2          yaml_2.2.1             
[51] curl_4.3                gridExtra_2.3          
[53] pander_0.6.3            qpcR_1.4-1             
[55] stringi_1.4.6           boot_1.3-23            
[57] zip_2.0.4               manipulateWidget_0.10.0
[59] rlang_0.4.10            pkgconfig_2.0.3        
[61] rgl_0.100.47            evaluate_0.14          
[63] lattice_0.20-38         htmlwidgets_1.5.1      
[65] tidyselect_1.1.0        plyr_1.8.5             
[67] magrittr_1.5            R6_2.4.1               
[69] generics_0.1.0          DBI_1.1.0              
[71] gsubfn_0.7              pillar_1.4.3           
[73] foreign_0.8-72          withr_2.4.1            
[75] abind_1.4-5             modelr_0.1.5           
[77] crayon_1.3.4            car_3.0-6              
[79] grid_3.6.2              readxl_1.3.1           
[81] minpack.lm_1.2-1        data.table_1.12.8      
[83] reprex_0.3.0            digest_0.6.24          
[85] webshot_0.5.2           xtable_1.8-4           
[87] httpuv_1.5.2            munsell_0.5.0 
```
