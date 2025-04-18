### Assemble dual-use masterfile and plot statistics

# Step 1. Convert pdf tables with critical goods lists
system("conda run -n trade_defense python -m motivation.dualuse.build_pdf_tables")

# Step 2. Collapse trade and industrial policy counts
source("motivation/dualuse/build_gta_policy.R")

# Step 3. Collapse 2015-2019 trade by good
source("motivation/dualuse/build_trade.R")

# Step 4. Create dual-use masterfile
source("motivation/dualuse/build_dualuse.R")

# Step 5. Make plots
source("motivation/dualuse/plot_dualuse.R")

# Appendix Step 1. Plot security classifications
source("motivation/dualuse/plot_security.R")

# Appendix Step 2. Plot associated industrial policies
source("motivation/dualuse/plot_gta.R")
