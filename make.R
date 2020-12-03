###########################
#### Load source files ####
###########################

source("R/packages.R")  # loads packages
source("R/download_data.R")
source("R/power.R")
source("R/GAM.R")
source("R/figures.R")
source("R/plan.R")      # creates the drake plan

# Tell the drake targets to fork up to 4 callr processes.
#future::plan(future.callr::callr)

#extrafont::loadfonts("win")

drake::make(
  plan
  #parallelism = "future",
  #jobs = 2
)
