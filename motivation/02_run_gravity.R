### Run gravity across blocs

# Step 1. Construct trade data
source("motivation/gravity/build_trade_hs.R")
source("motivation/gravity/build_trade_sitc.R")

# Step 2. Run first stage
source("motivation/gravity/run_gravity_hs.R")
source("motivation/gravity/run_gravity_sitc.R")

# Step 3. Make gravity plots
source("motivation/gravity/plot_gravity.R")

# Appendix Step 1. Plot two lines in gravity
source("motivation/gravity/plot_gravity_twolines.R")

# Appendix Step 2. Build event studies
source("motivation/events/build_wars.R")
source("motivation/events/run_wars.R")
source("motivation/events/plot_wars.R")
