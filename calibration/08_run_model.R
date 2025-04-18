### Build input-output networks with U.S., China, and the Rest of the World

# Step 1. Estimate conflict prize weights
system("julia ./calibration/model/est_beta.jl")
source("calibration/model/output_beta.R")

# Step 2. Run counterfactuals
system("julia ./calibration/model/run_scenarios.jl")
source("calibration/model/output_beta.R")

