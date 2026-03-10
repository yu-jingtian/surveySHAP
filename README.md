# surveySHAP

`surveySHAP` provides SHAP-based interpretation tools for four survey-model backends:

- `lm`
- `glm` (binomial with `cbind(gun_control, 6 - gun_control)`)
- `xgb_numeric`
- `xgb_logistic`

The package is designed for categorical survey covariates, full one-hot coding, and group-level summaries of both main effects and two-way interactions.

## SHAP setup for the four models

The package uses the following SHAP configuration:

- **LM** and **XGBoost numeric** use SHAP on the **count scale**.
- **GLM** and **XGBoost logistic** use SHAP on the **logit scale**.
- Interactions are allowed **only between variable groups**.
- For `lm` and `glm`, SHAP is computed analytically:
  - main SHAP: `phi_j = beta_j (x_j - mu_j)`
  - interaction SHAP: `phi_jk = beta_jk (x_j x_k - mu_jk)`
- For xgboost, native TreeSHAP is used for both main and interaction values.

## Installation

```r
install.packages("devtools")
devtools::install_github("yu-jingtian/surveySHAP")
```

## Included data

The package includes `survey_data`, a processed survey dataset with 43,950 complete cases.

Main variables:

- `gun_control`: count outcome from 0 to 6
- `gun_own`
- `partisan`
- `race`
- `gender`
- `educ` (used to derive `college`)
- `rucc` (used to derive `metro`)
- `weight`

The modeling groups are:

- `gun_own`
- `partisan`
- `race`
- `gender`
- `college`
- `metro`

## Basic usage

```r
library(surveySHAP)
data(survey_data)

res <- run_survey_shap(
  data = survey_data,
  model = "glm",
  output_dir = "surveyshap_output"
)
```

This writes four CSV files:

- `main_strength.csv`
- `main_direction.csv`
- `interaction_strength.csv`
- `interaction_direction.csv`

## Output tables

### Main strength

Columns:

- `group`
- `strength`

### Main direction

Columns:

- `feature`
- `level`
- `direction`
- `n_raw`
- `w_sum`
- `n_eff`

### Interaction strength

Columns:

- `feature1`
- `feature2`
- `strength`

### Interaction direction

Columns:

- `feature1`
- `feature2`
- `level1`
- `level2`
- `direction`
- `n_raw`
- `w_sum`
- `n_eff`

## Bootstrap examples

```r
boot_strength(
  survey_data,
  target = "main",
  feature = "partisan",
  model = "xgb_numeric",
  B = 50
)

boot_direction(
  survey_data,
  target = "interaction",
  feature = c("gun_own", "partisan"),
  level = c("Own a gun", "Republican"),
  model = "glm",
  B = 50
)
```
