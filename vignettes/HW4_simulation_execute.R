# store some useful information
date_of_run <- Sys.time()
session_info <- devtools::session_info()

# run simulation
library(UWBiost561)
res = UWBiost561::simulation(10, c(0.5, 0.9))

# save results
save(res, date_of_run, session_info, file = "~/HW4_simulation.RData")

print("done")
