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
devtools::install_github("YOUR_GITHUB_USERNAME/surveySHAP")
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

## License

MIT License.
