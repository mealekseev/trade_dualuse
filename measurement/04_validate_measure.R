### Validate measure

# Step 1. Validate against policy
source("measurement/validation/run_policy.R")
source("measurement/validation/run_counts.R")

# Step 2. Validate against trade
source("measurement/validation/run_trade.R")

# Appendix. Run robustness
source("measurement/validation/run_policy_robustness.R")
source("measurement/validation/run_decompositions.R")

