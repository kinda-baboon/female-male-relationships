#!/usr/bin/env Rscript

# behavior.file = file.path('data','alldata_dump_2020-11-16.csv')

behavior.file = '~/Dropbox/kindaR/output/kinda4.RData'

n.cores = 0

# If not set, sets n.cores to detected cores
if (!exists('n.cores') || !n.cores) n.cores = ifelse('future' %in% rownames(installed.packages()),future::availableCores(),parallel::detectCores(logical=FALSE))
