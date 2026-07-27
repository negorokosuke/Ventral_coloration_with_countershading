############################################################
## 00_packages.R
## Load packages and initialize project
############################################################

## ---------- Packages ----------

library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)

library(mgcv)
library(gratia)

library(lme4)
library(lmerTest)

library(emmeans)

library(cowplot)
library(data.table)

library(here)

library(tidyverse)

library(MuMIn)
library(ggeffects)

library(patchwork)
library(viridis)

library(MASS)
library(purrr)
library(fuzzyjoin)
library(grid)

## ---------- Reproducibility ----------

set.seed(123)

options(stringsAsFactors = FALSE)

