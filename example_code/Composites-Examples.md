Script to Make Composites Example
================
Becky Neufeld
2025-04-27

This R script is an example of R code that I’ve used on longitudinal
survey data to automate the process of making composite variables from
item-level data.

Composite variables are means or sums of different items from the
survey. For example, the composite variable “Rejection Sensitivity” is
the mean of the three survey items: (1) “In your graduate program, to
what extent do you expect that people might expect less of you because
of your background (race, gender, and other social identities)?”; (2)
“In your graduate program, to what extent do you expect that others will
treat you differently because of your background (race, gender, and
other social identities)?”; and (3) “In your graduate program, to what
extent do you expect to be treated fairly?”.

The script is used to automate the process of making composite variables
by setting up the desired composite logic, selecting patterns in the
item names and putting the relevant items together to form the
composites.

## Load relevant packages

``` r
library(tidyverse)
library(reshape)
library(haven)
library(readr)
library(random)
```

## Function used to generate fake data

The original survey data is confidential, so these functions were run
offline on that dataset to make a fake dataset with similar structure
for this example script.

``` r
generate_random_strings <- function(n, length = 10) {
   random_string <- function() {
     paste0(sample(c(0:9, letters, LETTERS), length, replace = TRUE), collapse = "")
   }
   replicate(n, random_string())
 }
 
 dataset[] <- lapply(dataset, function(column) {
   if (is.numeric(column)) {
     return(runif(length(column), min = 0, max = 100))  # Replace with random numbers
   } else if (is.character(column)) {
     return(generate_random_strings(length(column)))  # Replace with random strings
   } else if (is.factor(column)) {
     return(sample(levels(column), length(column), replace = TRUE))  # Replace with random factor levels
   } else if (inherits(column, "Date")) {
     return(as.Date(sample(16000:19000, length(column), replace = TRUE), origin = "1970-01-01"))  # Replace with random dates
   } else {
     return(column)
   }
 })
```

# Read in Fake Data

To run this script, the “fake” data must be downloaded from github and
added to the working directory.

``` r
GraSP_Long <- read_csv("Grasp_Long_Github_Fake.csv")
```

    ## New names:
    ## Rows: 2720 Columns: 536
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," dbl
    ## (536): ...1, ...2, X, ExternalReference_Intake, Status, Progress, Durati...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`
    ## • `...1` -> `...2`

# Functions to Make the Composites

create_mean_variables creates composite variables that are means of the
items from “dataset” that contain “patterns” strings in their names.

``` r
create_mean_variables <- function(patterns, dataset) {
  for (pattern in patterns) {
    selected_columns <- grepl(paste0("^", pattern), colnames(dataset), ignore.case = TRUE)
    selected_variables <- dataset[, selected_columns, drop = FALSE]
    mean_variable <- paste0(pattern, "_mean")
    dataset[[mean_variable]] <- apply(selected_variables, 1, function(x) mean(as.numeric(x), na.rm = TRUE))
    
    included_variables <- colnames(selected_variables)
    message(paste0("Mean calculation for pattern '", pattern, "' includes the following variables: ", paste(included_variables, collapse = ", ")))
  }
  
  return(dataset)
}
```

create_sum_variables creates composite variables that are sums of the
items from “dataset” that contain “patterns” strings in their names.

``` r
create_sum_variables <- function(patterns, dataset) {
  for (pattern in patterns) {
    selected_columns <- grepl(paste0("^", pattern), colnames(dataset), ignore.case = TRUE)
    selected_variables <- dataset[, selected_columns, drop = FALSE]
    sum_variable <- paste0(pattern, "_sum")
    dataset[[sum_variable]] <- apply(selected_variables, 1, function(x) sum(as.numeric(x), na.rm = TRUE))
    
    included_variables <- colnames(selected_variables)
    message(paste0("Mean calculation for pattern '", pattern, "' includes the following variables: ", paste(included_variables, collapse = ", ")))
  }
  
  return(dataset)
}
```

check_pattern_values checks the values of the survey items and displays
relevant survey issues.

``` r
check_pattern_values <- function(patterns, dataset, threshold) {
  variables_exceeding_threshold <- character()  # Store variables that exceed the threshold
  
  for (pattern in patterns) {
    selected_columns <- grepl(pattern, colnames(dataset), ignore.case = TRUE)
    selected_variables <- dataset[, selected_columns, drop = FALSE]
    
    exceeding_variables <- colnames(selected_variables)[sapply(selected_variables, function(x) any(as.numeric(x) > threshold, na.rm = TRUE))]
    variables_exceeding_threshold <- c(variables_exceeding_threshold, exceeding_variables)
  }
  
  if (length(variables_exceeding_threshold) > 0) {
    print("Variables exceeding the threshold of 7:")
    print(variables_exceeding_threshold)
  } else {
    print("No variables exceed the threshold of 7.")
  }
}
```

## Write out what patterns identify the composites

Here, we entered manually the patterns that defined each composite, for
example “DeptDiv” for the composite of Dept_Div1, Dept_Div2, and
Dept_Div3. Automated pattern matching was not used due to
inconsistencies in the variable naming, e.g., some variables have \_1,
others have .1, and some don’t have any delimiters before the number.

``` r
unique_variables <- unique(names(GraSP_Long)[-1])

patterns <- c("DeptDiv", "DeptComp", "GA_comp", "GA_agency", "GA_relation", "RS", "DeptFit", "Phd_Goals_int_comm", "Phd_Goals_int_agen", "Phd_Goals_ext_comm", "Phd_Goals_ext_agen", "SEff", "Eng",  "AffEng", "BehEng", "Belong", "scienceID", "AcaID", "stemID", "Commit_Grad", "Commit_field", "SWB", "FGP_RoleModel", 
              
              "FGP_Trust", "BFGP_Cares", "CFGP_Cares8", "AFGP_Freq", "BFGP_Freq", "AFGP_TradSup_AcaChal", "BFGP_TradSup_AcaChal", "AFGP_TradSup_Res", "BFGP_TradSup_Res", "AFGP_TradSup_Life", "BFGP_TradSup_Life",
              
              "F_Trust", "F_Cares", "BF_Freq", "AF_Freq", "CF_Freq", "AF_TradSup_AcaChal", "BF_TradSup_AcaChal","AF_TradSup_Res", "BF_TradSup_Res", "AF_TradSup_Life", "BF_TradSup_Life",
              
              
               "P_RoleModel", "P_normal", "P_Trust", "AP_Freq", "BP_Freq", "CP_Freq", "AP_TradSup_AcaChal", "BP_TradSup_AcaChal",
               
          "P_resp", "DeptClimate", "Microagg", "Cope_Denial", "Cope_Diseng", "Cope_Vent", "Cope_Reframe", "Cope_Emosup", "Cope_Instrsup", "Cope_Selfblame", "Cope_Subuse", "Cope_Plan", "Cope_Active", "Cope_Distract",
          
          "esteem", "WLBalance", "SF_Cares", "FGP_feedback", "F_RoleModel", "F_feedback"
          
         )
```

## Fix a data entry mistake that was made where not all decline to state answers were coded as “NA”

Check for value Issues (these scales should not have 8 in them).

``` r
threshold <- 7
check_pattern_values(patterns, GraSP_Long, threshold)
```

    ## [1] "Variables exceeding the threshold of 7:"
    ##   [1] "DeptDiv_1"               "DeptDiv_2"              
    ##   [3] "DeptDiv_3"               "DeptComp_1"             
    ##   [5] "DeptComp_2"              "DeptComp_3"             
    ##   [7] "DeptComp_4"              "DeptComp_5"             
    ##   [9] "GA_comp1"                "GA_comp2"               
    ##  [11] "GA_agency1"              "GA_agency2"             
    ##  [13] "GA_relation1"            "GA_relation2"           
    ##  [15] "RS_1"                    "RS_2"                   
    ##  [17] "RS_3"                    "P_R_E_Years_Research"   
    ##  [19] "P_R_E_Honors_Project"    "E_I_Masters"            
    ##  [21] "D_First_Gen"             "Cope_Instrsup1"         
    ##  [23] "Cope_Instrsup2"          "SecondMentorSup"        
    ##  [25] "Extraversion_mean"       "DeptFit_1"              
    ##  [27] "DeptFit_2"               "Phd_Goals_int_comm_1"   
    ##  [29] "Phd_Goals_int_comm_2"    "Phd_Goals_int_comm_3"   
    ##  [31] "Phd_Goals_int_agen_4"    "Phd_Goals_int_agen_5"   
    ##  [33] "Phd_Goals_int_agen_6"    "Phd_Goals_ext_comm_7"   
    ##  [35] "Phd_Goals_ext_comm_8"    "Phd_Goals_ext_agen_9"   
    ##  [37] "Phd_Goals_ext_agen_10"   "Phd_Goals_ext_agen_11"  
    ##  [39] "Phd_Goals_ext_agen_12"   "Phd_Goals_ext_agen_13"  
    ##  [41] "SEff1"                   "SEff2"                  
    ##  [43] "SEff3"                   "SEff4"                  
    ##  [45] "SEff5"                   "Eng1"                   
    ##  [47] "Eng2"                    "Eng3"                   
    ##  [49] "AffEng1"                 "AffEng2"                
    ##  [51] "BehEng1"                 "BehEng2"                
    ##  [53] "D_English"               "Cope_Diseng1"           
    ##  [55] "Cope_Diseng2"            "AffEng1"                
    ##  [57] "AffEng2"                 "BehEng1"                
    ##  [59] "BehEng2"                 "Belong2"                
    ##  [61] "Belong3"                 "Belong4"                
    ##  [63] "Belong5"                 "Belong6"                
    ##  [65] "Belong7"                 "Belong8"                
    ##  [67] "Belong9"                 "Belong10"               
    ##  [69] "Belong11"                "Belong12"               
    ##  [71] "Belong13"                "Belong14"               
    ##  [73] "Belong15"                "Belong16"               
    ##  [75] "Belong17"                "Belong18"               
    ##  [77] "Belong19"                "Belong20"               
    ##  [79] "Belong21"                "scienceID1"             
    ##  [81] "scienceID2"              "scienceID3"             
    ##  [83] "scienceID4"              "AcaID1"                 
    ##  [85] "AcaID2"                  "AcaID3"                 
    ##  [87] "AcaID4"                  "stemID1"                
    ##  [89] "stemID2"                 "stemID3"                
    ##  [91] "stemID4"                 "Commit_Grad1"           
    ##  [93] "Commit_Grad2"            "Commit_field1"          
    ##  [95] "Commit_field2"           "SWB1"                   
    ##  [97] "SWB2"                    "SWB3"                   
    ##  [99] "SWB4"                    "SWB5"                   
    ## [101] "FGP_RoleModel1"          "FGP_RoleModel2"         
    ## [103] "FGP_RoleModel3"          "FGP_Trust1"             
    ## [105] "FGP_Trust2"              "FGP_Trust3"             
    ## [107] "BFGP_Cares1"             "BFGP_Cares5"            
    ## [109] "BFGP_Cares6"             "BFGP_Cares8"            
    ## [111] "CFGP_Cares8"             "AFGP_Freq_Help"         
    ## [113] "AFGP_Freq_Emo"           "AFGP_Freq_GoodNews"     
    ## [115] "AFGP_Freq_AcaAch"        "BFGP_Freq_Setback"      
    ## [117] "BFGP_Freq_Results"       "BFGP_Freq_Ideas"        
    ## [119] "AFGP_TradSup_AcaChal_1"  "AFGP_TradSup_AcaChal_2" 
    ## [121] "AFGP_TradSup_AcaChal_3"  "BFGP_TradSup_AcaChal_6" 
    ## [123] "BFGP_TradSup_AcaChal_7"  "BFGP_TradSup_AcaChal_10"
    ## [125] "AFGP_TradSup_Res_1"      "AFGP_TradSup_Res_2"     
    ## [127] "AFGP_TradSup_Res_3"      "BFGP_TradSup_Res_6"     
    ## [129] "BFGP_TradSup_Res_7"      "BFGP_TradSup_Res_9"     
    ## [131] "BFGP_TradSup_Res_10"     "AFGP_TradSup_Life_1"    
    ## [133] "AFGP_TradSup_Life_2"     "AFGP_TradSup_Life_4"    
    ## [135] "BFGP_TradSup_Life_7"     "BFGP_TradSup_Life_9"    
    ## [137] "F_Trust1"                "F_Trust2"               
    ## [139] "F_Trust3"                "NE_F_Cares1"            
    ## [141] "F_Cares2"                "F_Cares3"               
    ## [143] "F_Cares4"                "F_Cares5"               
    ## [145] "F_Cares6"                "F_Cares7"               
    ## [147] "F_Cares8"                "F_Cares9"               
    ## [149] "SF_Cares5"               "SF_Cares6"              
    ## [151] "BF_Freq_Help"            "BF_Freq_AcaAch"         
    ## [153] "AF_Freq_Emo"             "AF_Freq_GoodNews"       
    ## [155] "CF_Freq_Setback"         "CF_Freq_Results"        
    ## [157] "CF_Freq_Ideas"           "AF_TradSup_AcaChal_1"   
    ## [159] "AF_TradSup_AcaChal_2"    "AF_TradSup_AcaChal_3"   
    ## [161] "BF_TradSup_AcaChal_6"    "BF_TradSup_AcaChal_7"   
    ## [163] "BF_TradSup_AcaChal_9"    "AF_TradSup_Res_1"       
    ## [165] "AF_TradSup_Res_2"        "AF_TradSup_Res_3"       
    ## [167] "BF_TradSup_Res_6"        "BF_TradSup_Res_7"       
    ## [169] "AF_TradSup_Life_2"       "AF_TradSup_Life_3"      
    ## [171] "BF_TradSup_Life_6"       "BF_TradSup_Life_7"      
    ## [173] "BF_TradSup_Life_9"       "BF_TradSup_Life_10"     
    ## [175] "FGP_RoleModel1"          "FGP_RoleModel2"         
    ## [177] "FGP_RoleModel3"          "P_RoleModel1"           
    ## [179] "P_RoleModel2"            "P_normal1"              
    ## [181] "P_normal2"               "P_normal3"              
    ## [183] "FGP_Trust1"              "FGP_Trust2"             
    ## [185] "FGP_Trust3"              "P_Trust1"               
    ## [187] "P_Trust2"                "P_Trust3"               
    ## [189] "AP_Freq_Emo"             "AP_Freq_LifeStress"     
    ## [191] "BP_Freq_Help"            "BP_Freq_AcaChal"        
    ## [193] "CP_Freq_Ideas"           "CP_Freq_Results"        
    ## [195] "AP_TradSup_AcaChal1_1"   "AP_TradSup_AcaChal1_2"  
    ## [197] "BP_TradSup_AcaChal1_3"   "BP_TradSup_AcaChal1_4"  
    ## [199] "BP_TradSup_AcaChal1_5"   "BP_TradSup_AcaChal1_6"  
    ## [201] "P_resp1"                 "P_resp2"                
    ## [203] "P_resp3"                 "BP_resp4"               
    ## [205] "P_resp5"                 "BP_resp6"               
    ## [207] "BP_resp7"                "P_resp8"                
    ## [209] "DeptClimate_1"           "DeptClimate_2"          
    ## [211] "DeptClimate_3"           "DeptClimate_4"          
    ## [213] "DeptClimate_5"           "DeptClimate_6"          
    ## [215] "DeptClimate_7"           "DeptClimate_8"          
    ## [217] "DeptClimate_9"           "DeptClimate_10"         
    ## [219] "Microagg_1"              "Microagg_2"             
    ## [221] "Microagg_3"              "Microagg_4"             
    ## [223] "Microagg_5"              "Microagg_6"             
    ## [225] "Microagg_7"              "Microagg_8"             
    ## [227] "Microagg_9"              "Microagg_10"            
    ## [229] "Cope_Denial1"            "Cope_Denial2"           
    ## [231] "Cope_Diseng1"            "Cope_Diseng2"           
    ## [233] "Cope_Vent1"              "Cope_Vent2"             
    ## [235] "Cope_Reframe1"           "Cope_Reframe2"          
    ## [237] "Cope_Emosup1"            "Cope_Emosup2"           
    ## [239] "Cope_Instrsup1"          "Cope_Instrsup2"         
    ## [241] "Cope_Selfblame1"         "Cope_Selfblame2"        
    ## [243] "Cope_Subuse1"            "Cope_Subuse2"           
    ## [245] "Cope_Plan1"              "Cope_Plan2"             
    ## [247] "Cope_Active1"            "Cope_Active2"           
    ## [249] "Cope_Distract1"          "Cope_Distract2"         
    ## [251] "esteem1"                 "esteem2"                
    ## [253] "esteem3"                 "esteem4"                
    ## [255] "esteem5"                 "esteem6"                
    ## [257] "WLBalance1"              "WLBalance2"             
    ## [259] "WLBalance3"              "WLBalance4"             
    ## [261] "WLBalance5"              "SF_Cares5"              
    ## [263] "SF_Cares6"               "FGP_feedback1"          
    ## [265] "FGP_feedback2"           "FGP_feedback3"          
    ## [267] "FGP_feedback4"           "FGP_feedback5"          
    ## [269] "FGP_feedback6"           "FGP_feedback7"          
    ## [271] "FGP_feedback8"           "FGP_feedback9"          
    ## [273] "F_RoleModel1"            "F_RoleModel2"           
    ## [275] "F_RoleModel3"            "F_feedback1"            
    ## [277] "F_feedback2"             "F_feedback3"            
    ## [279] "F_feedback4"             "F_feedback5"            
    ## [281] "F_feedback6"             "F_feedback7"            
    ## [283] "F_feedback8"             "F_feedback9"

Specify the variables to be modified (8 needs to change to NA).

``` r
variables_to_modify <- c(
  "AFGP_TradSup_AcaChal_1", "AFGP_TradSup_AcaChal_2", "AFGP_TradSup_AcaChal_3",
  "BFGP_TradSup_AcaChal_6", "BFGP_TradSup_AcaChal_7", "BFGP_TradSup_AcaChal_10",
  "AFGP_TradSup_Res_1", "AFGP_TradSup_Res_2", "AFGP_TradSup_Res_3",
  "BFGP_TradSup_Res_6", "BFGP_TradSup_Res_7", "BFGP_TradSup_Res_9",
  "AFGP_TradSup_Life_1", "AFGP_TradSup_Life_2", "AFGP_TradSup_Life_4",
  "BFGP_TradSup_Life_7", "BFGP_TradSup_Life_9",
  "AF_TradSup_AcaChal_1", "AF_TradSup_AcaChal_2", "AF_TradSup_AcaChal_3",
  "BF_TradSup_AcaChal_6", "BF_TradSup_AcaChal_7", "BF_TradSup_AcaChal_9",
  "AF_TradSup_Res_1", "AF_TradSup_Res_2", "AF_TradSup_Res_3",
  "BF_TradSup_Res_6", "BF_TradSup_Res_7",
  "AF_TradSup_Life_2", "AF_TradSup_Life_3",
  "BF_TradSup_Life_6", "BF_TradSup_Life_7", "BF_TradSup_Life_9",
  "AP_TradSup_AcaChal1_1", "AP_TradSup_AcaChal1_2",
  "BP_TradSup_AcaChal1_3", "BP_TradSup_AcaChal1_4", "BP_TradSup_AcaChal1_5",
  "BP_TradSup_AcaChal1_6", "DeptClimate_1", "DeptClimate_2", "DeptClimate_3",
  "DeptClimate_4", "DeptClimate_5", "DeptClimate_6", "DeptClimate_7",
  "DeptClimate_9", "DeptClimate_10", "Microagg_3", "Microagg_5",
  "Microagg_6", "Microagg_9"
)
```

Specify the variables to be modified (8 needs to change to NA).

``` r
for (var in variables_to_modify) {
  GraSP_Long[[var]][GraSP_Long[[var]] == 8] <- NA
}
```

# Run the functions to make the composite variables

``` r
GraSP_Composites <- create_mean_variables(patterns, GraSP_Long)
```

    ## Mean calculation for pattern 'DeptDiv' includes the following variables: DeptDiv_1, DeptDiv_2, DeptDiv_3

    ## Mean calculation for pattern 'DeptComp' includes the following variables: DeptComp_1, DeptComp_2, DeptComp_3, DeptComp_4, DeptComp_5

    ## Mean calculation for pattern 'GA_comp' includes the following variables: GA_comp1, GA_comp2

    ## Mean calculation for pattern 'GA_agency' includes the following variables: GA_agency1, GA_agency2

    ## Mean calculation for pattern 'GA_relation' includes the following variables: GA_relation1, GA_relation2

    ## Mean calculation for pattern 'RS' includes the following variables: RS_1, RS_2, RS_3

    ## Mean calculation for pattern 'DeptFit' includes the following variables: DeptFit_1, DeptFit_2

    ## Mean calculation for pattern 'Phd_Goals_int_comm' includes the following variables: Phd_Goals_int_comm_1, Phd_Goals_int_comm_2, Phd_Goals_int_comm_3

    ## Mean calculation for pattern 'Phd_Goals_int_agen' includes the following variables: Phd_Goals_int_agen_4, Phd_Goals_int_agen_5, Phd_Goals_int_agen_6

    ## Mean calculation for pattern 'Phd_Goals_ext_comm' includes the following variables: Phd_Goals_ext_comm_7, Phd_Goals_ext_comm_8

    ## Mean calculation for pattern 'Phd_Goals_ext_agen' includes the following variables: Phd_Goals_ext_agen_9, Phd_Goals_ext_agen_10, Phd_Goals_ext_agen_11, Phd_Goals_ext_agen_12, Phd_Goals_ext_agen_13

    ## Mean calculation for pattern 'SEff' includes the following variables: SEff1, SEff2, SEff3, SEff4, SEff5

    ## Mean calculation for pattern 'Eng' includes the following variables: Eng1, Eng2, Eng3

    ## Mean calculation for pattern 'AffEng' includes the following variables: AffEng1, AffEng2

    ## Mean calculation for pattern 'BehEng' includes the following variables: BehEng1, BehEng2

    ## Mean calculation for pattern 'Belong' includes the following variables: Belong2, Belong3, Belong4, Belong5, Belong6, Belong7, Belong8, Belong9, Belong10, Belong11, Belong12, Belong13, Belong14, Belong15, Belong16, Belong17, Belong18, Belong19, Belong20, Belong21

    ## Mean calculation for pattern 'scienceID' includes the following variables: scienceID1, scienceID2, scienceID3, scienceID4

    ## Mean calculation for pattern 'AcaID' includes the following variables: AcaID1, AcaID2, AcaID3, AcaID4

    ## Mean calculation for pattern 'stemID' includes the following variables: stemID1, stemID2, stemID3, stemID4

    ## Mean calculation for pattern 'Commit_Grad' includes the following variables: Commit_Grad1, Commit_Grad2

    ## Mean calculation for pattern 'Commit_field' includes the following variables: Commit_field1, Commit_field2

    ## Mean calculation for pattern 'SWB' includes the following variables: SWB1, SWB2, SWB3, SWB4, SWB5

    ## Mean calculation for pattern 'FGP_RoleModel' includes the following variables: FGP_RoleModel1, FGP_RoleModel2, FGP_RoleModel3

    ## Mean calculation for pattern 'FGP_Trust' includes the following variables: FGP_Trust1, FGP_Trust2, FGP_Trust3

    ## Mean calculation for pattern 'BFGP_Cares' includes the following variables: BFGP_Cares1, BFGP_Cares5, BFGP_Cares6, BFGP_Cares8

    ## Mean calculation for pattern 'CFGP_Cares8' includes the following variables: CFGP_Cares8

    ## Mean calculation for pattern 'AFGP_Freq' includes the following variables: AFGP_Freq_Help, AFGP_Freq_Emo, AFGP_Freq_GoodNews, AFGP_Freq_AcaAch

    ## Mean calculation for pattern 'BFGP_Freq' includes the following variables: BFGP_Freq_Setback, BFGP_Freq_Results, BFGP_Freq_Ideas

    ## Mean calculation for pattern 'AFGP_TradSup_AcaChal' includes the following variables: AFGP_TradSup_AcaChal_1, AFGP_TradSup_AcaChal_2, AFGP_TradSup_AcaChal_3

    ## Mean calculation for pattern 'BFGP_TradSup_AcaChal' includes the following variables: BFGP_TradSup_AcaChal_6, BFGP_TradSup_AcaChal_7, BFGP_TradSup_AcaChal_10

    ## Mean calculation for pattern 'AFGP_TradSup_Res' includes the following variables: AFGP_TradSup_Res_1, AFGP_TradSup_Res_2, AFGP_TradSup_Res_3

    ## Mean calculation for pattern 'BFGP_TradSup_Res' includes the following variables: BFGP_TradSup_Res_6, BFGP_TradSup_Res_7, BFGP_TradSup_Res_9, BFGP_TradSup_Res_10

    ## Mean calculation for pattern 'AFGP_TradSup_Life' includes the following variables: AFGP_TradSup_Life_1, AFGP_TradSup_Life_2, AFGP_TradSup_Life_4

    ## Mean calculation for pattern 'BFGP_TradSup_Life' includes the following variables: BFGP_TradSup_Life_7, BFGP_TradSup_Life_9

    ## Mean calculation for pattern 'F_Trust' includes the following variables: F_Trust1, F_Trust2, F_Trust3

    ## Mean calculation for pattern 'F_Cares' includes the following variables: F_Cares2, F_Cares3, F_Cares4, F_Cares5, F_Cares6, F_Cares7, F_Cares8, F_Cares9

    ## Mean calculation for pattern 'BF_Freq' includes the following variables: BF_Freq_Help, BF_Freq_AcaAch

    ## Mean calculation for pattern 'AF_Freq' includes the following variables: AF_Freq_Emo, AF_Freq_GoodNews

    ## Mean calculation for pattern 'CF_Freq' includes the following variables: CF_Freq_Setback, CF_Freq_Results, CF_Freq_Ideas

    ## Mean calculation for pattern 'AF_TradSup_AcaChal' includes the following variables: AF_TradSup_AcaChal_1, AF_TradSup_AcaChal_2, AF_TradSup_AcaChal_3

    ## Mean calculation for pattern 'BF_TradSup_AcaChal' includes the following variables: BF_TradSup_AcaChal_6, BF_TradSup_AcaChal_7, BF_TradSup_AcaChal_9

    ## Mean calculation for pattern 'AF_TradSup_Res' includes the following variables: AF_TradSup_Res_1, AF_TradSup_Res_2, AF_TradSup_Res_3

    ## Mean calculation for pattern 'BF_TradSup_Res' includes the following variables: BF_TradSup_Res_6, BF_TradSup_Res_7

    ## Mean calculation for pattern 'AF_TradSup_Life' includes the following variables: AF_TradSup_Life_2, AF_TradSup_Life_3

    ## Mean calculation for pattern 'BF_TradSup_Life' includes the following variables: BF_TradSup_Life_6, BF_TradSup_Life_7, BF_TradSup_Life_9, BF_TradSup_Life_10

    ## Mean calculation for pattern 'P_RoleModel' includes the following variables: P_RoleModel1, P_RoleModel2

    ## Mean calculation for pattern 'P_normal' includes the following variables: P_normal1, P_normal2, P_normal3

    ## Mean calculation for pattern 'P_Trust' includes the following variables: P_Trust1, P_Trust2, P_Trust3

    ## Mean calculation for pattern 'AP_Freq' includes the following variables: AP_Freq_Emo, AP_Freq_LifeStress

    ## Mean calculation for pattern 'BP_Freq' includes the following variables: BP_Freq_Help, BP_Freq_AcaChal

    ## Mean calculation for pattern 'CP_Freq' includes the following variables: CP_Freq_Ideas, CP_Freq_Results

    ## Mean calculation for pattern 'AP_TradSup_AcaChal' includes the following variables: AP_TradSup_AcaChal1_1, AP_TradSup_AcaChal1_2

    ## Mean calculation for pattern 'BP_TradSup_AcaChal' includes the following variables: BP_TradSup_AcaChal1_3, BP_TradSup_AcaChal1_4, BP_TradSup_AcaChal1_5, BP_TradSup_AcaChal1_6

    ## Mean calculation for pattern 'P_resp' includes the following variables: P_resp1, P_resp2, P_resp3, P_resp5, P_resp8

    ## Mean calculation for pattern 'DeptClimate' includes the following variables: DeptClimate_1, DeptClimate_2, DeptClimate_3, DeptClimate_4, DeptClimate_5, DeptClimate_6, DeptClimate_7, DeptClimate_8, DeptClimate_9, DeptClimate_10

    ## Mean calculation for pattern 'Microagg' includes the following variables: Microagg_1, Microagg_2, Microagg_3, Microagg_4, Microagg_5, Microagg_6, Microagg_7, Microagg_8, Microagg_9, Microagg_10

    ## Mean calculation for pattern 'Cope_Denial' includes the following variables: Cope_Denial1, Cope_Denial2

    ## Mean calculation for pattern 'Cope_Diseng' includes the following variables: Cope_Diseng1, Cope_Diseng2

    ## Mean calculation for pattern 'Cope_Vent' includes the following variables: Cope_Vent1, Cope_Vent2

    ## Mean calculation for pattern 'Cope_Reframe' includes the following variables: Cope_Reframe1, Cope_Reframe2

    ## Mean calculation for pattern 'Cope_Emosup' includes the following variables: Cope_Emosup1, Cope_Emosup2

    ## Mean calculation for pattern 'Cope_Instrsup' includes the following variables: Cope_Instrsup1, Cope_Instrsup2

    ## Mean calculation for pattern 'Cope_Selfblame' includes the following variables: Cope_Selfblame1, Cope_Selfblame2

    ## Mean calculation for pattern 'Cope_Subuse' includes the following variables: Cope_Subuse1, Cope_Subuse2

    ## Mean calculation for pattern 'Cope_Plan' includes the following variables: Cope_Plan1, Cope_Plan2

    ## Mean calculation for pattern 'Cope_Active' includes the following variables: Cope_Active1, Cope_Active2

    ## Mean calculation for pattern 'Cope_Distract' includes the following variables: Cope_Distract1, Cope_Distract2

    ## Mean calculation for pattern 'esteem' includes the following variables: esteem1, esteem2, esteem3, esteem4, esteem5, esteem6

    ## Mean calculation for pattern 'WLBalance' includes the following variables: WLBalance1, WLBalance2, WLBalance3, WLBalance4, WLBalance5

    ## Mean calculation for pattern 'SF_Cares' includes the following variables: SF_Cares5, SF_Cares6

    ## Mean calculation for pattern 'FGP_feedback' includes the following variables: FGP_feedback1, FGP_feedback2, FGP_feedback3, FGP_feedback4, FGP_feedback5, FGP_feedback6, FGP_feedback7, FGP_feedback8, FGP_feedback9

    ## Mean calculation for pattern 'F_RoleModel' includes the following variables: F_RoleModel1, F_RoleModel2, F_RoleModel3

    ## Mean calculation for pattern 'F_feedback' includes the following variables: F_feedback1, F_feedback2, F_feedback3, F_feedback4, F_feedback5, F_feedback6, F_feedback7, F_feedback8, F_feedback9

``` r
patterns <- c("Stress")
GraSP_Composites_Long <- create_sum_variables(patterns, GraSP_Composites)
```

    ## Mean calculation for pattern 'Stress' includes the following variables: Stress1, Stress2, Stress3, Stress4, Stress5, Stress6, Stress7, Stress8, Stress9, Stress10
