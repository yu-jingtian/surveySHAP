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
# main strength
out1 <- boot_strength(data = dat, feature = "partisan", B = 30, seed = 1,
                      parallel = TRUE, n_cores = 8)
```
|   estimate mean_boot  |  sd_boot  |   ci_lo  |   ci_hi | present_prob|
|-----------|--------------------|-----------|---------|------------------|
| 0.1820362 |0.1863809| 0.00565656| 0.1786726| 0.1981759   |         1|

```r
> # interaction strength
> out2 <- boot_strength(dat, feature = c("gun_own", "partisan"), B = 30, seed = 1,
+                       parallel = TRUE, n_cores = 8)
> out2$summary
    estimate  mean_boot      sd_boot      ci_lo      ci_hi present_prob
1 0.01949454 0.02080623 0.0009580798 0.01910883 0.02286928            1
> 
> # main direction
> out3 <- boot_direction(dat, feature = "partisan", level = "Dem.", B = 30, seed = 1,
+                        parallel = TRUE, n_cores = 8)
> out3$summary
   estimate mean_boot   sd_boot     ci_lo     ci_hi present_prob
1 0.1449576 0.1499139 0.0112035 0.1267271 0.1662842            1
> 
> # interaction direction
> out4 <- boot_direction(dat,
+                        feature = c("gun_own", "partisan"),
+                        level   = c("No guns in HH", "Dem."),
+                        B = 30, seed = 1,
+                        parallel = TRUE, n_cores = 8)
> out4$summary
     estimate   mean_boot     sd_boot       ci_lo        ci_hi present_prob
1 -0.01175205 -0.01158054 0.001876561 -0.01421952 -0.007755951            1
```

## License

MIT License.
