### Assemble measure

# Step 1. Construct government contracts
system("conda run -n trade_defense python -m measurement.contracts.build_defense")

# Step 2. Build centrality
system("conda run -n trade_defense python -m measurement.centrality.build_io_usa")
system("conda run -n trade_defense python -m measurement.centrality.build_centrality_usa")

# Step 3. Output summary statistics
source("measurement/centrality/plot_centrality.R")


## Appendix
# Step 1. Assemble historical military procurement and plot anecdotal examples
system("conda run -n trade_defense python -m measurement.history.download_history")
system("conda run -n trade_defense python -m measurement.history.build_history")
system("conda run -n trade_defense python -m measurement.history.build_centrality_hist")
source("measurement/history/plot_history.R")

# Step 2. Output positions of dual-use goods in networks
source("measurement/centrality/plot_positions.R")

# Step 3. Output industry and elasticity pltos
source("measurement/centrality/plot_elasticity.R")


