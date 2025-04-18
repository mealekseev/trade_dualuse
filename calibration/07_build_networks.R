### Build input-output networks with U.S., China, and the Rest of the World

# Step 1. Collapse China's revenues from CSMAR
source("calibration/iotables/build_csmar.R")
source("calibration/iotables/clean_io_chn.R")

# Step 2. Construct input-output tables for the U.S., China, and the Rest of the World
system("conda run -n trade_defense python -m calibration.iotables.build_trade")
system("conda run -n trade_defense python -m calibration.iotables.build_io_chn")
system("conda run -n trade_defense python -m calibration.iotables.build_iotables_naics4")

# Step 3. Build networks for counterfactuals
system("conda run -n trade_defense python -m calibration.iotables.build_alliances_naics4")
system("conda run -n trade_defense python -m calibration.iotables.build_reshipping_naics4")
system("conda run -n trade_defense python -m calibration.iotables.build_reshipping_naics4_chn")

