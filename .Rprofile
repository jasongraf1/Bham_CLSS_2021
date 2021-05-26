## .Rprofile

## Set some default options. Comment our as desired.
options(papersize = "a4")
options(continue = "... ")
options(scipen = 7)
options(help_type = "html")
options(stringsAsFactors = FALSE)

## Create project directories if they don't already exist
.First <- function() {
  dir.create(here::here("cache"), showWarnings = F)
  dir.create(here::here("data_processed"), showWarnings = F)
  dir.create(here::here("data_raw"), showWarnings = F)
  dir.create(here::here("docs"), showWarnings = F)
  dir.create(here::here("figures"), showWarnings = F)
  dir.create(here::here("markdown"), showWarnings = F)
  dir.create(here::here("model_output"), showWarnings = F)
  dir.create(here::here("scripts_R"), showWarnings = F)
  # dir.create(here::here("scripts_python"), showWarnings = F)
  # dir.create(here::here("scripts_stan"), showWarnings = F)

  cat("\fWelcome to your R-Project:", basename(getwd()), "\n")
}

# packages to be automatically loaded when project starts. Uncomment as desired
pkgs <- c(
  "tidyverse", # for data wrangling, etc.
  "here", # for paths
  # ## for plotting
  # "gridExtra",
  # "scales",
  # "patchwork",
  # "ggpubr",
  # "ggsci",
  # "RColorBrewer"
)

## Install all necessary packages, if not already installed
if(interactive()){
  # utils::install.packages(pkgs[!(pkgs %in% utils::installed.packages()[, "Package"])])
  ## Load packages
  invisible(lapply(pkgs, library, character.only = TRUE))
  rm(pkgs)
}

## Write the session information and history to file at the end of session.
.Last <- function(){
  if(interactive()){
    top_msg <- paste0(
      "Session Info for VADIS CUP project\n",
      "Author: Jason Grafmiller\n",
      "Last modified: ", Sys.time(), "\n\n")
    writeLines(utils::capture.output(cat(top_msg), utils::sessionInfo()), "sessionInfo.txt")
    rm(top_msg)

    hist_file <- Sys.getenv("R_HISTFILE")
    if(hist_file == "") hist_file <- "~/.RHistory"
    savehistory(hist_file)
  }
  cat("\nGoodbye at ", date(), "\n")
}