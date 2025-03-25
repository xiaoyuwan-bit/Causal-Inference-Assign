Causal -Inference Assignment code
# CSI 2024 Major Assignment 1 – Code Output Summary

##  Question 1: Estimating ACE (Average Causal Effect)

### ➔ Unadjusted Models (`Y ~ A`)
- Linear regression run on `data01` to `data04`
- All show **statistically significant positive effects** of `A` on `Y`  
- Example:  
  - `data01`: **ACE ≈ 1.14**, 95% CI: [1.01, 1.27], p < 0.001  
  - Strong evidence of effect without adjustment

### ➔ Adjusted Models (`Y ~ A + Z`)
- Controlling for `Z` changes conclusions:
  - `data01`: **ACE becomes non-significant**, suggesting **confounding by Z**
  - Other datasets: effect of `A` remains significant but is **weakened**
  - Example:  
    - `data03`: **ACE ≈ 0.56**, 95% CI: [0.47, 0.65]

---

## Question 2: Nighttime ICU Discharge and Mortality

###  Descriptive Statistics
- Exposure groups differ in key covariates (e.g., SAPS 3, admission reasons)
- Standardized differences indicate **imbalance**:
  - Example: Admission reason **std diff ≈ 18.24%**

###  Propensity Score Model
- Logistic regression fitted to estimate the probability of nighttime discharge  
- Propensity scores show **good overlap** between exposure groups  
- Inverse Probability Weights (**IPW**) computed for adjustment

---

###  Covariate Balance After Weighting
- Weighted standardized differences show **improved balance**
  - Admission reason: **18.24% → −0.4%**
  - SAPS 3: **−0.73%** after IPW
  - Other variables similarly well-balanced

---

##  Regression Models

### ➔ Unadjusted Logistic Model
- OR for nighttime ICU discharge on hospital death:
  - 95% CI: **[0.90, 1.80]** → Not statistically significant

### ➔ Adjusted Model
- Adjusted for SAPS 3 and covariates
- OR 95% CI: **[0.69, 1.46]** → Still **not significant**

### ➔ IPW Model
- Weighted logistic regression using IPW
- OR 95% CI: **[0.74, 1.50]** → No significant effect

---

##  E-Value Analysis
- E-value for observed association and lower bound: **1**
- Interpretation:
  - No robust evidence of causal association
  - Effect could be due to unmeasured confounding

---

##  Conclusion

- **No significant causal effect** of nighttime ICU discharge on hospital mortality was detected  
- Results are consistent across:
  - Unadjusted regression  
  - Covariate-adjusted regression  
  - Propensity score-weighted analysis (IPW)
- Propensity score model successfully **balanced covariates**

