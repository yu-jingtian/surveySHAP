# surveySHAP

**surveySHAP** provides SHAP-based interpretation tools for machine-learning models
applied to survey data, with a focus on categorical covariates and continuous or
ordinal policy preference outcomes.

The package implements exact **TreeSHAP** using xgboost’s native implementation
and is designed for reproducible, model-based interpretation of survey responses.

## 

### SHAP values (basic idea)

For each observation $`i`$ and each feature $`j`$, the model prediction can be written as  
```math
\hat y_i = \phi_0 + \sum_j \phi_{ij},
```
where $`\phi_0`$ is the baseline prediction (the average model output), and $`\phi_{ij}`$ is the SHAP value of feature $`j`$ for observation $`i`$.

-$`\phi_{ij} > 0`$: feature$`j`$ pushes the prediction **upward**  
-$`\phi_{ij} < 0`$: feature$`j`$ pushes the prediction **downward**  
-$`|\phi_{ij}|`$: the **strength** of that push  

---

### Strength and direction in `surveySHAP`

In this project, we summarize SHAP values across individuals in two ways:

- **One-way (main) effects**: contributions from a single feature (e.g. *partisan*, *gun ownership*).
- **Two-way (interaction) effects**: joint contributions from a pair of features (e.g. *gun ownership × partisan*).

For both one-way and two-way effects:

- **Strength** measures *how large* the effect is, defined as the average magnitude of SHAP values across individuals.
- **Direction** measures *which way* the effect tends to push predictions, based on the sign of SHAP values aggregated across individuals.

Intuitively, strength answers *“how important is this factor?”*, while direction answers *“does this factor tend to increase or decrease the outcome?”*.


---

## Installation

You can install the development version directly from GitHub using **devtools**:

```r
# install devtools if needed
install.packages("devtools")

# install surveySHAP from GitHub
devtools::install_github("yu-jingtian/surveySHAP")
```

Then load the package:

```r
library(surveySHAP)
```

---

## Included data

The package includes a **processed survey dataset** used for examples and testing.

```r
data(survey_data)
str(survey_data)
```

### Dataset overview

- **Rows:** ~60,000 respondents  
- **Variables:**
  - `gun_control` – numeric policy preference score  
    (higher values indicate more restrictive / prohibitive positions)
  - `gun_own` – household gun ownership status
  - `partisan` – partisan identification
  - `race` – self-reported race
  - `gender` – gender identity
  - `college` – college education status

The dataset is already cleaned and harmonized.  
No additional preprocessing is required before modeling.

---

## Basic usage

Run the full XGBoost + SHAP workflow with a single call:

```r
shap_rslt <- run_survey_shap(survey_data)
```

This function:
1. Builds a full one-hot encoded sparse design matrix (no dropped reference levels)
2. Fits an XGBoost regression model
3. Computes main-effect SHAP values
4. Computes interaction SHAP values on a subsample
5. Aggregates results to interpretable group-level summaries

---

## Output structure

`run_survey_shap()` returns a named list with the following elements:

### model
The fitted `xgb.Booster` object used for all SHAP calculations.

### design
A list containing the cleaned modeling data, sparse design matrix, outcome vector,
and one-hot feature names.

### shap_main
Main-effect SHAP values computed via native TreeSHAP (`predcontrib = TRUE`),
including feature-level SHAP values and baseline predictions.

### strength_main_group
Group-level importance summary based on mean absolute SHAP values.

### direction_main_active
Directional (“active”) SHAP effects for each categorical variable level.

### shap_interaction
Interaction SHAP values computed via `predinteraction = TRUE` on a subsample.

### strength_interaction_group
Group-level interaction strength summary across variable pairs.

### direction_interaction_active
Directional interaction effects for active–active feature pairs.

---

## Example results

Below we report summary results produced by:

```r
shap_rslt <- run_survey_shap(survey_data)
summarize_shap_rslt(
  shap_rslt,
  top_n = 5,
  min_n_main = 100,
  min_n_interaction = 100
)
```

---

### One-way SHAP strength (group level)

| group     | strength    |
|-----------|-------------|
| partisan  | 0.18185593  |
| gun_own   | 0.08811271  |
| gender    | 0.03016185  |
| college   | 0.01778079  |
| race      | 0.01162914  |

---

### Two-way SHAP strength (group pairs)

| pair                    | strength     |
|-------------------------|--------------|
| gun_own × partisan      | 0.0191926630 |
| partisan × race         | 0.0129670057 |
| gender × partisan       | 0.0106519856 |
| college × partisan      | 0.0086285716 |
| gender × gun_own        | 0.0040878162 |
| gun_own × race          | 0.0039177700 |
| college × gun_own       | 0.0034441383 |
| college × race          | 0.0014474342 |
| gender × race           | 0.0012783710 |
| college × gender        | 0.0008779862 |

---

### Sample-size filtering

- **One-way direction:** kept 16 / 16 levels (`min_n_main = 100`)
- **Two-way direction:** kept 69 / 99 pairs (`min_n_interaction = 100`)

---

### One-way SHAP direction (TOP 5)

| feature  | level               | shap_active | n     |
|----------|---------------------|-------------|-------|
| partisan | Dem.                | 0.13755627  | 18956 |
| gun_own  | No guns in HH        | 0.05423058  | 26412 |
| gender   | Other               | 0.04566492  | 199   |
| gun_own  | HH owns (not self)  | 0.03959752  | 5137  |
| race     | Asian               | 0.01701959  | 1296  |

---

### One-way SHAP direction (BOTTOM 5)

| feature  | level      | shap_active  | n     |
|----------|------------|--------------|-------|
| partisan | Rep.       | -0.089594014 | 12665 |
| gun_own  | Not sure   | -0.082315312 | 2112  |
| gender   | Men        | -0.023295154 | 19425 |
| gun_own  | Own a gun  | -0.020335748 | 10737 |
| race     | Hipanic    | -0.005021199 | 3275  |

---

### Two-way SHAP direction (active–active, TOP 5)

| feature_i | level_i            | feature_j | level_j | direction_active | n_active |
|-----------|--------------------|-----------|---------|------------------|----------|
| gun_own   | Not sure           | partisan  | Rep.    | 0.008531859      | 103      |
| gun_own   | No guns in HH      | partisan  | Ind.    | 0.006711430      | 808      |
| gun_own   | HH owns (not self) | partisan  | Ind.    | 0.005844191      | 145      |
| partisan  | Ind.               | race      | Black   | 0.005452007      | 113      |
| partisan  | Dem.               | race      | White   | 0.004115614      | 1469     |

---

### Two-way SHAP direction (active–active, BOTTOM 5)

| feature_i | level_i            | feature_j | level_j | direction_active | n_active |
|-----------|--------------------|-----------|---------|------------------|----------|
| partisan  | Dem.               | race      | Black   | -0.013514628     | 434      |
| gun_own   | No guns in HH      | partisan  | Dem.    | -0.009700744     | 1555     |
| gun_own   | HH owns (not self) | partisan  | Dem.    | -0.006499079     | 243      |
| gun_own   | No guns in HH      | race      | Black   | -0.006345862     | 402      |
| gun_own   | HH owns (not self) | gender    | Men     | -0.005851881     | 118      |

---

## Bootstrap example results
```r
set.seed(123)

# Use a small subset to keep this fast
dat <- survey_data
dat <- dat[sample(nrow(dat), 1000), ]

boot_obj <- bootstrap_survey_shap(
  data = dat,
  B = 50,
  seed = 123,
  parallel = TRUE,
  n_cores = 5,
  run_args = list(interaction_subsample_n = 500),
  verbose = FALSE
)

# Strength summaries (group + group-pairs)
sum_strength <- summarise_bootstrap_strength(boot_obj, conf = 0.95, top_k = 5)

# Direction summaries (main + interaction)
sum_dir <- summarise_bootstrap_direction(boot_obj, conf = 0.95, top_k = 5)
```

---

### One-way SHAP strength

```r
top_oneway <- sum_strength$oneway[order(-sum_strength$oneway$estimate), ][1:5, ]
```
| key       | estimate   | mean_boot | sd_boot   | ci_lo     | ci_hi     | present_prob | topk_prob |
|-----------|------------|-----------|-----------|-----------|-----------|--------------|-----------|
| partisan  | 0.19731979 | 0.20154446| 0.01258249| 0.18310503| 0.22609376| 1            | 1.00      |
| gun_own   | 0.07602144 | 0.07567311| 0.01027066| 0.05800629| 0.09529334| 1            | 1.00      |
| college   | 0.02588084 | 0.02934282| 0.00749308| 0.01787321| 0.04363296| 1            | 1.00      |
| race      | 0.02331752 | 0.02629609| 0.00464206| 0.01923797| 0.03776716| 1            | 1.00      |
| gender    | 0.02289166 | 0.02283906| 0.00572754| 0.01442756| 0.03508973| 1            | 1.00      |

---

### Two-way SHAP strength

```r
top_twoway <- sum_strength$twoway[order(-sum_strength$twoway$estimate), ][1:5, ]
```
| key                  | estimate   | mean_boot | sd_boot   | ci_lo     | ci_hi     | present_prob | topk_prob |
|----------------------|------------|-----------|-----------|-----------|-----------|--------------|-----------|
| gun_own × partisan   | 0.02819071 | 0.03139412| 0.00511443| 0.02257495| 0.04030891| 1            | 1.00      |
| partisan × race      | 0.01895790 | 0.02124341| 0.00385540| 0.01287349| 0.02761454| 1            | 0.96      |
| gender × gun_own     | 0.01338963 | 0.01428737| 0.00352367| 0.00896940| 0.02178179| 1            | 0.72      |
| college × partisan   | 0.01327816 | 0.01554670| 0.00352778| 0.00985452| 0.02365714| 1            | 0.82      |
| college × gun_own    | 0.01322679 | 0.01487810| 0.00326608| 0.01012085| 0.02197130| 1            | 0.84      |


---

### One-way SHAP direction

```r
dir_main_stable <- subset(
  sum_dir$main,
  present_prob >= 0.9 & (ci_lo > 0 | ci_hi < 0)
)
dir_main_stable <- dir_main_stable[order(-abs(dir_main_stable$estimate)), , drop = FALSE]
dir_main_stable <- head(dir_main_stable, 10)
```
| key                  | estimate    | mean_boot | sd_boot   | ci_lo      | ci_hi      | present_prob | top_pos_prob | top_neg_prob |
|----------------------|-------------|-----------|-----------|------------|------------|--------------|--------------|--------------|
| partisanDem.         |  0.15245436 |  0.16527562| 0.01945332|  0.12836963|  0.19895309| 1            | 1.00         | 0.00         |
| gun_ownNot sure      | -0.09689371 | -0.09687674| 0.03965131| -0.18359957| -0.03804548| 1            | 0.00         | 1.00         |
| partisanRep.         | -0.09341339 | -0.07653176| 0.01764064| -0.11813798| -0.04585943| 1            | 0.00         | 1.00         |
| gun_ownOwn a gun     | -0.05569325 | -0.05670274| 0.01848282| -0.09716583| -0.02759577| 1            | 0.00         | 1.00         |
| gun_ownNo guns in HH |  0.02983432 |  0.02824734| 0.01118576|  0.01212338|  0.05210482| 1            | 1.00         | 0.00         |
| collegeCollege       |  0.02092146 |  0.02206780| 0.00880451|  0.00282710|  0.03674581| 1            | 0.88         | 0.00         |
| collegeNon-college   | -0.00377092 | -0.00456371| 0.00258925| -0.00989098| -0.00102858| 1            | 0.00         | 0.02         |

---

### Two-way SHAP direction

```r
dir_int_stable <- subset(
  sum_dir$interaction,
  present_prob >= 0.8 & (ci_lo > 0 | ci_hi < 0)
)
dir_int_stable <- dir_int_stable[order(-abs(dir_int_stable$estimate)), , drop = FALSE]
dir_int_stable <- head(dir_int_stable, 10)
```
| key                                   | estimate     | mean_boot | sd_boot   | ci_lo        | ci_hi        | present_prob | top_pos_prob | top_neg_prob |
|---------------------------------------|--------------|-----------|-----------|--------------|--------------|--------------|--------------|--------------|
| partisanDem. × raceHispanic           | -0.01955365  | -0.02360109| 0.01205667| -0.04737807  | -0.00539031  | 1            | 0.00         | 0.70         |
| gun_ownOwn a gun × collegeCollege     |  0.00957188  |  0.00942894| 0.00474600|  0.00003639  |  0.01842166  | 1            | 0.14         | 0.00         |
| gun_ownNo guns in HH × partisanDem.   | -0.00882481  | -0.00797391| 0.00511382| -0.01921442  | -0.00058516  | 1            | 0.00         | 0.14         |
| partisanRep. × collegeCollege         | -0.00695158  | -0.00639419| 0.00355739| -0.01312093  | -0.00024062  | 1            | 0.00         | 0.00         |
| partisanDem. × raceWhite              |  0.00607532  |  0.00619627| 0.00178135|  0.00314827  |  0.00906379  | 1            | 0.00         | 0.00         |
| gun_ownNo guns in HH × genderMen      |  0.00522616  |  0.00501308| 0.00314126|  0.00002314  |  0.01207465  | 1            | 0.02         | 0.00         |
| gun_ownOwn a gun × collegeNon-college | -0.00284606  | -0.00244580| 0.00179395| -0.00683363  | -0.00016673  | 1            | 0.00         | 0.00         |
| partisanInd. × raceWhite              | -0.00210003  | -0.00231195| 0.00120560| -0.00476464  | -0.00001013  | 1            | 0.00         | 0.00         |


## License

MIT License.
