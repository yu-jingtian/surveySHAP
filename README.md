# surveySHAP

**surveySHAP** provides SHAP-based interpretation tools for machine-learning models
applied to survey data, with a focus on categorical covariates and continuous or
ordinal policy preference outcomes.

The package implements exact **TreeSHAP** using xgboost’s native implementation
and is designed for reproducible, model-based interpretation of survey responses.

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

| level              | shap_active | n     | onehot                    |
|--------------------|-------------|-------|---------------------------|
| Dem.               | 0.14985639  | 18956 | partisanDem.              |
| No guns in HH      | 0.05013081  | 26412 | gun_ownNo guns in HH      |
| Other              | 0.04683239  | 199   | genderOther               |
| HH owns (not self) | 0.03358670  | 5137  | gun_ownHH owns (not self) |
| Asian              | 0.01674442  | 1296  | raceAsian                 |

---

### One-way SHAP direction (BOTTOM 5)

| level     | shap_active  | n     | onehot           |
|-----------|--------------|-------|------------------|
| Not sure  | -0.090879910 | 2112  | gun_ownNot sure  |
| Rep.      | -0.069700253 | 12665 | partisanRep.     |
| Own a gun | -0.026172638 | 10737 | gun_ownOwn a gun |
| Men       | -0.023176405 | 19425 | genderMen        |
| Hipanic   | -0.005866928 | 3275  | raceHipanic      |

---

### Two-way SHAP direction (active–active, TOP 5)

| feat_i                    | feat_j       | group_i | group_j | direction_active | n_active |
|---------------------------|--------------|---------|---------|------------------|----------|
| gun_ownNot sure           | partisanRep. | gun_own | partisan| 0.005862512      | 103      |
| gun_ownNo guns in HH      | partisanInd. | gun_own | partisan| 0.005718919      | 808      |
| partisanInd.              | raceBlack    | partisan| race    | 0.005082146      | 113      |
| partisanDem.              | raceWhite    | partisan| race    | 0.004441996      | 1469     |
| gun_ownHH owns (not self) | partisanInd. | gun_own | partisan| 0.004430534      | 145      |

---

### Two-way SHAP direction (active–active, BOTTOM 5)

| feat_i                    | feat_j         | group_i | group_j | direction_active | n_active |
|---------------------------|----------------|---------|---------|------------------|----------|
| partisanDem.              | raceBlack      | partisan| race    | -0.012890889     | 434      |
| gun_ownNo guns in HH      | partisanDem.   | gun_own | partisan| -0.008899130     | 1555     |
| gun_ownHH owns (not self) | partisanDem.   | gun_own | partisan| -0.006641536     | 243      |
| gun_ownNo guns in HH      | raceBlack      | gun_own | race    | -0.005707449     | 402      |
| partisanRep.              | collegeCollege | partisan| college | -0.004965699     | 590      |


## License

MIT License.
