#!/usr/bin/env Rscript

arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
packages.to.install <- c("getopt", "stringr", "data.table")

for(p in packages.to.install) {
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        suppressPackageStartupMessages(
            library(p, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE)
        )
    }
}

params = matrix(c(
    'help', 'h', 0, "logical",
    'data_dir', 'i', 2, "character"
), ncol=4, byrow=TRUE)

opt = getopt(params)
if (!is.null(opt$help)) {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
}

if(!is.character(opt$data_dir)) {
    cat(paste(getopt(params, command = basename(arg0), usage = TRUE)))
    q(status=1)
}

process_red_errors <- function(Dir) {
    File <- sprintf("%s/errors.csv", Dir)
    if (file.exists(File)) {
        df <- read.csv(File)
        total <- sum(df$count)

        df_r <- df[df$operation == "readonly_red", ]
        total_r <- sum(df_r$count)

        df_w <- df[df$operation == "writeonly_red", ]
        total_w <- sum(df_w$count)

        df$avg_count <- (df$count / total) * 100
        if(nrow(df_r) > 0) {
            df$avg_count_r <- (df_r$count / total_r) * 100
        }

        if (nrow(df_w)) {
            df$avg_count_w <- (df_w$count / total_w) * 100
        }

        options(width=90)
        print(df)
    }
}

process_red_errors(opt$data_dir)
