### The script runs files that generates crosswalks

# Step 1. Build crosswalks
system("conda run -n trade_defense python -m crosswalks.build_crosswalks")

# Step 2. Additional crosswalks for China
source("crosswalks/build_crosswalks_cn.R")

