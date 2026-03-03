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

## Dataset Description

- **Rows:** 43,950 respondents (complete cases)
- **Variables:**

  - `gun_control` – count of restrictive gun policy responses (0–6). Larger values indicate more restrictive policy preferences.

  - `gun_own` – household gun ownership status

  - `partisan` – partisan identification (Democrat, Republican, Independent)

  - `race` – self-reported race

  - `gender` – gender identity

  - `educ` – highest level of education completed (used to derive `college`)

  - `rucc` – Rural–Urban Continuum Code (1 = most urban, 9 = most rural; used to derive `metro`)

  - `college` – discretized education (Non-college vs College)

  - `metro` – discretized RUCC (Big-metro for rucc==1 vs Non-metro)

  - `weight` – survey weight constructed from `commonweight`,  
    recommended for characterizing opinions and behaviors of adult Americans

The dataset is already cleaned and harmonized.  
No additional preprocessing is required before modeling.

---

## Basic usage

Run the full XGBoost + SHAP workflow with a single call:

```r
shap_rslt <- run_survey_shap(survey_data)
```

**Notes (updated outcome + weights):**

- The outcome `gun_control` is a 0–6 index (sum of 6 binary items). The default XGBoost fit treats it as a numeric response and fits a regression model (`objective = "reg:squarederror"`).
- SHAP values are therefore additive on the **logit scale** of the implied proportion. Positive SHAP values increase the predicted restrictiveness propensity; negative values decrease it.
- If the `weight` column is present, the model is trained with sample weights and all SHAP summaries (strength/direction) use weighted averages.


This function:
1. Builds a full one-hot encoded sparse design matrix (no dropped reference levels)
2. Fits an XGBoost regression model for the numeric outcome `gun_control`
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
  top_n = 5
)
```

---

## One-way Strength (Group)

| group     | strength |
|-----------|----------|
| partisan  | 1.09459930 |
| gun_own   | 0.55330738 |
| gender    | 0.20380765 |
| race      | 0.09216795 |
| college   | 0.07918583 |
| metro     | 0.06817119 |

---

## Two-way Strength (Group Pairs)

| pair | strength |
|------|----------|
| gun_own × partisan | 0.131543169 |
| partisan × race | 0.093237177 |
| gender × partisan | 0.062001247 |
| college × partisan | 0.052573639 |
| gun_own × race | 0.028549165 |
| gender × gun_own | 0.025923215 |
| college × gun_own | 0.020552467 |
| metro × partisan | 0.019790181 |
| gun_own × metro | 0.017661906 |
| gender × metro | 0.017114340 |
| gender × race | 0.014590894 |
| college × race | 0.011475818 |
| metro × race | 0.006491058 |
| college × gender | 0.006170314 |
| college × metro | 0.005905234 |

---

## Filters

- **One-way direction:** kept 10/10 (min_n_eff_main = 100)  
- **Two-way direction:** kept 76/76 (min_n_eff_interaction = 100)

---

## One-way Direction — TOP (Positive)

| feature | level | shap_active | n_raw | w_sum | n_eff |
|----------|--------|------------|-------|--------|--------|
| partisan | Dem. | 0.99686468 | 18742 | 16181.668 | 9179.5301 |
| race | Asian | 0.11097614 | 1287 | 2240.387 | 578.7991 |
| race | Black | 0.08634984 | 5006 | 5072.257 | 2251.3052 |
| gender | Woman | 0.08212956 | 24533 | 23660.215 | 12330.7889 |
| partisan | Ind. | 0.02416784 | 12640 | 13240.672 | 5853.1530 |

---

## One-way Direction — BOTTOM (Negative)

| feature | level | shap_active | n_raw | w_sum | n_eff |
|----------|--------|------------|-------|--------|--------|
| partisan | Rep. | -0.40470581 | 12568 | 14527.661 | 6517.462 |
| gender | Man | -0.14844891 | 19224 | 20067.494 | 8959.348 |
| race | Hipanic | -0.03824870 | 3257 | 3301.204 | 1163.115 |
| race | White | -0.01700461 | 34400 | 33336.152 | 18102.143 |
| college | College | 0.01346564 | 21944 | 21612.953 | 12680.470 |

---

## Two-way Direction — TOP (Positive)

| group_i | level_i | group_j | level_j | direction | n_raw | w_sum | n_eff |
|---------|---------|----------|-----------|-------|--------|--------|
| partisan | Rep. | gender | Woman | 0.03421657 | 750 | 831.1541 | 449.0228 |
| partisan | Dem. | race | White | 0.03194428 | 1478 | 1138.4415 | 855.1395 |
| partisan | Rep. | college | Non-college | 0.02559270 | 813 | 925.5654 | 384.5527 |
| race | Black | college | Non-college | 0.02346006 | 362 | 387.7252 | 153.7135 |
| gun_own | Own a gun | partisan | Rep. | 0.02337812 | 493 | 532.5715 | 273.0698 |

---

## Two-way Direction — BOTTOM (Negative)

| group_i | level_i | group_j | level_j | direction | n_raw | w_sum | n_eff |
|---------|----------|---------|----------|-----------|-------|--------|--------|
| partisan | Dem. | race | Black | -0.09550737 | 435 | 391.1704 | 232.6852 |
| gun_own | No guns in HH | partisan | Dem. | -0.08392455 | 1574 | 1338.3460 | 749.7547 |
| gun_own | HH owns (not self) | partisan | Dem. | -0.06954201 | 243 | 209.9412 | 136.1001 |
| gun_own | No guns in HH | race | Black | -0.03119176 | 427 | 442.5392 | 190.7885 |
| partisan | Dem. | college | Non-college | -0.02578396 | 1028 | 858.3718 | 402.1146 |



---

## Bootstrap example results
```r
# main strength
out1 <- boot_strength(data = dat, feature = "partisan", B = 30, seed = 1,
                      parallel = TRUE, n_cores = 8)
```
|   estimate| mean_boot  |  sd_boot  |   ci_lo |   ci_hi | present_prob|
|-----------|------------|-----------|---------|---------|-------------|
| 0.1820362 |0.1863809| 0.00565656| 0.1786726| 0.1981759   |         1|

```r
# interaction strength
out2 <- boot_strength(dat, feature = c("gun_own", "partisan"), B = 30, seed = 1,
                      parallel = TRUE, n_cores = 8)
```

|   estimate| mean_boot  |  sd_boot  |   ci_lo |   ci_hi | present_prob|
|-----------|------------|-----------|---------|---------|-------------|
| 0.01949454 |0.02080623 |0.0009580798 |0.01910883 |0.02286928     |  1|

```r
# main direction
out3 <- boot_direction(dat, feature = "partisan", level = "Dem.", B = 30, seed = 1,
                       parallel = TRUE, n_cores = 8)
```
|   estimate| mean_boot  |  sd_boot  |   ci_lo |   ci_hi | present_prob|
|-----------|------------|-----------|---------|---------|-------------|
| 0.1449576| 0.1499139| 0.0112035| 0.1267271| 0.1662842|            1|

```r
# interaction direction
out4 <- boot_direction(dat,
                       feature = c("gun_own", "partisan"),
                       level   = c("No guns in HH", "Dem."),
                       B = 30, seed = 1,
                       parallel = TRUE, n_cores = 8)
```
|   estimate| mean_boot  |  sd_boot  |   ci_lo |   ci_hi | present_prob|
|-----------|------------|-----------|---------|---------|-------------|
| -0.01175205| -0.01158054| 0.001876561| -0.01421952| -0.007755951|   1|


## License

MIT License.
