# PSM With Gain Score

This repository contains the R scripts used to study how self‑reported EdTech use relates to middle‑school achievement gains (Korean, Math, English). The workflow combines propensity score matching (PSM) with gain‑score difference‑in‑differences (DID) models and supporting assumption checks.

## Repository Structure

| File | Purpose |
| --- | --- |
| `dt_cleaning.R` | Builds analysis-ready student × school data sets (`mk_final_df`, `mm_final_df`, `me_final_df`) by merging teacher, student, parent, and school panels, deriving covariates, and exporting `m_*_final_2.csv`. |
| `dt_analysis.R` | Performs the main PSM + DID workflow for each subject: balance diagnostics, logit score estimation, nearest-neighbor matching (caliper 0.25), ATT estimation on matched samples, and result storage (`result_k`, `result_m`, `result_e`). |
| `assumption_check.R` | Tests the common-trends assumption by adding Grade 1 outcomes, re-estimating pre-trends, and bootstrapping coefficient ratios for each subject. Requires `matched_df` from `dt_analysis.R`. |
| `dt_sensitivity.R` | Rebuilds the merged data sets with alternative treatment coding/NA handling, exporting `m_*_final_3.csv` for robustness work. |
| `analysis_sensitivity.R` | Re-runs the full PSM + DID pipeline on the sensitivity data (`*_final_df_345`) and stores the robustness ATT objects (`result_sense_k`, etc.). |

## Prerequisites

1. **Software**: R ≥ 4.1.
2. **Data**: Load the raw SELS 2020 teacher (`m_t_3`), student (`m_s_1`, `m_s_2`, `m_s_3`), parent (`m_p_2`), and school (`m_sc_3`) frames into the R session before sourcing the scripts. Paths for `.csv` exports inside the scripts are currently commented for Google Drive but can point anywhere.
3. **Packages**: Install once with

   ```r
   install.packages(c("haven", "dplyr", "MatchIt", "lme4", "boot"))
   ```

## Running the Workflow

1. **Data preparation**  
   - Launch R in the repository root.  
   - Ensure the raw data frames (e.g., `m_t_3`, `m_s_2`) exist in the global environment.  
   - Run `source("dt_cleaning.R")`. This script:
     - Aggregates teacher EdTech usage to the school level (counts treated teachers, mean experience, environment indices).
     - Builds student/parent feature sets (ICT competency, SRL scales, digital device averages, EdTech attitudes, household income proxies).
     - Merges school and student data, computes gain scores (`did_*`), and writes the merged `.csv` files used downstream.

2. **Main analysis**  
   - `source("dt_analysis.R")` to:
     - Filter out rows with missing covariates.
     - Run pre-match balance `t.test` diagnostics.
     - Fit a logit propensity model with socio-economic, SRL, and digital environment covariates plus teacher-level EdTech use.
     - Match treated vs. control students via nearest-neighbor (`MatchIt::matchit`) using the estimated propensity scores and a 0.25 caliper.
     - Re-check balance within the matched samples, then estimate ATT via OLS of gain scores on treatment indicators.  
   - Capture or save `result_k`, `result_m`, `result_e` as needed; each object is an `lm` fit you can inspect with `summary()` or `broom::tidy()`.

3. **Assumption diagnostics**  
   - Keep the `matched_df` object from the main analysis in memory.  
   - Load earlier-wave achievement (`m_s_1_subset`) and run `source("assumption_check.R")`. The script regresses Grade 2 and Grade 3 scores on Grade 1 scores plus treatment, then bootstraps the ratio of pre/post slope coefficients to ensure similar pre-trends between treated and control groups.

4. **Sensitivity analysis**  
   - `source("dt_sensitivity.R")` to rebuild the merged files with alternative treatment coding (e.g., explicit handling of neutral EdTech responses) and re-export them as `*_final_df_345`.  
   - `source("analysis_sensitivity.R")` to repeat the entire PSM + DID workflow on those sensitivity data sets. Inspect `result_sense_k`, `result_sense_m`, and `result_sense_e` for robustness.

## Tips & Notes

- **Data availability**: The repository stores only the scripts; raw SELS data must be obtained under the appropriate data-use agreement and loaded manually. The commented `read.csv` lines illustrate expected file names.
- **Plots**: Both analysis scripts call `plot()` on the `MatchIt` objects for quick visual balance checks. These display in the active R graphics device.
- **Modularity**: If you want to run a single subject, subset the relevant data frame (`mk_final_df`, etc.) before the balance checks and matching loops, but keep the matching formula identical to maintain comparability.
- **Extending outputs**: To export ATT tables or balance statistics, wrap the `matchit` summaries in `summary(match_obj)$sum.matched` or use `MatchIt::love.plot`.

With the data loaded and dependencies installed, following the steps above will reproduce both the primary and sensitivity estimations of the PSM with gain-score approach.
