#!/usr/bin/env Rscript

# Parse the --file= argument out of command line args and
# determine where base directory is
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
packages.to.install <- c("getopt", "stringr")

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
    'verbose', 'v', 0, "logical",
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

verbose <- if (!is.null(opt$verbose)) {
    1
} else {
    0
}

get_client_threads <- function(Dir) {
    per_machine <- as.integer(str_extract(str_extract(Dir, "t[_=][0-9]+"), "[0-9]+"))
    num_clusters <- as.integer(str_extract(str_extract(Dir, "cl[_=][0-9]+"), "[0-9]+"))
    client_machines <- as.integer(str_extract(str_extract(Dir, "cm[_=][0-9]+"), "[0-9]+"))

    # If directory contains client machines info, use it
    # otherwise assume it's the same as database machines
    if (!is.na(client_machines)) {
        total <- per_machine * client_machines * num_clusters
    } else {
        db_machines <- as.integer(str_extract(str_extract(Dir, "dm[_=][0-9]+"), "[0-9]+"))
        total <- per_machine * db_machines * num_clusters
    }

    return(data.frame(per_machine, total))
}

latency_for_file <- function(File) {
    mean <- 0
    median <- 0
    if (file.exists(File)) {
        latencies <- read.csv(File)
        mean <- mean(latencies$mean) / 1000
        median <- mean(latencies$median) / 1000
    }

    return(data.frame(mean, median))
}

get_total_data <- function(Dir) {
    summary <- read.csv(sprintf("%s/summary.csv", Dir))

    # Remove first row, as it is usually inflated
    summary <- summary[-c(1), ]

    # All committed operations
    max_commit <- max(summary$successful)
    max_commit_row <- summary[c(which(summary$successful == max_commit)), ]
    max_commit_w <- max_commit / max_commit_row$window

    # Get the median of committed operations
    median_commit <- median(summary$successful)
    median_window <- median(summary$window)
    median_commit_w <- median_commit / median_window

    r <- latency_for_file(sprintf("%s/readonly-blue_latencies.csv", Dir))
    blue_mean_latency_ronly <- r$mean
    blue_median_latency_ronly <- r$median

    r <- latency_for_file(sprintf("%s/writeonly-blue_latencies.csv", Dir))
    blue_mean_latency_wonly <- r$mean
    blue_median_latency_wonly <- r$median

    r <- latency_for_file(sprintf("%s/read-write-blue_latencies.csv", Dir))
    blue_mean_latency_rw <- r$mean
    blue_median_latency_rw <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red_latencies.csv", Dir))
    red_mean_latency_ronly <- r$mean
    red_median_latency_ronly <- r$median

    r <- latency_for_file(sprintf("%s/writeonly-red_latencies.csv", Dir))
    red_mean_latency_wonly <- r$mean
    red_median_latency_wonly <- r$median

    r <- latency_for_file(sprintf("%s/read-write-red_latencies.csv", Dir))
    red_mean_latency_rw <- r$mean
    red_median_latency_rw <- r$median

    # sprintf("%s/readonly-blue-barrier_latencies.csv", Dir)
    # sprintf("%s/writeonly-blue-barrier_latencies.csv", Dir)
    # sprintf("%s/read-write-blue-barrier_latencies.csv", Dir)

    return(data.frame(max_commit_w,
                      median_commit_w,
                      blue_mean_latency_ronly,
                      blue_mean_latency_wonly,
                      blue_mean_latency_rw,
                      blue_median_latency_ronly,
                      blue_median_latency_wonly,
                      blue_median_latency_rw,
                      red_mean_latency_ronly,
                      red_mean_latency_wonly,
                      red_mean_latency_rw,
                      red_median_latency_ronly,
                      red_median_latency_wonly,
                      red_median_latency_rw))
}

format_data <- function(Dir, Data) {
    format_decimal <- function(string, ..., withoutZeros = FALSE) {
        return(formatC(string, format="f", big.mark=",", drop0trailing=withoutZeros))
    }

    thread_info <- get_client_threads(Dir)

    # if (verbose) {
    #     cat(sprintf("\nThreads: %s (%s)\n",
    #             format_decimal(thread_info$per_machine, withoutZeros=TRUE),
    #             format_decimal(thread_info$total, withoutZeros=TRUE)))

    #     cat(sprintf("Max Thr: %s\n", format_decimal(Data$max_commit_w)))
    #     cat(sprintf("Median Thr: %s\n", format_decimal(Data$median_commit_w)))

    #     cat("\nNo Barrier\n")
    #     cat(sprintf("Ronly Mean / Median Ms: %f / %f\n", Data$mean_latency_ronly, Data$median_latency_ronly))
    #     cat(sprintf("Wonly Mean / Median Ms: %f / %f\n", Data$mean_latency_wonly, Data$median_latency_wonly))
    #     cat(sprintf("RW Mean /Median Ms: %f / %f\n", Data$mean_latency_rw, Data$median_latency_rw))
    # }

    cat("threads,throughput,throughput_med,ronly_mean,wonly_mean,rw_mean,ronly_median,wonly_median,rw_median,red_ronly_mean,red_wonly_mean,red_rw_mean,red_ronly_median,red_wonly_median,red_rw_median\n")
    cat(sprintf(
        "%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f \n",
        thread_info$per_machine,
        Data$max_commit_w,
        Data$median_commit_w,
        Data$blue_mean_latency_ronly,
        Data$blue_mean_latency_wonly,
        Data$blue_mean_latency_rw,
        Data$blue_median_latency_ronly,
        Data$blue_median_latency_wonly,
        Data$blue_median_latency_rw,
        Data$red_mean_latency_ronly,
        Data$red_mean_latency_wonly,
        Data$red_mean_latency_rw,
        Data$red_median_latency_ronly,
        Data$red_median_latency_wonly,
        Data$red_median_latency_rw
    ))
}

input_dir <- opt$data_dir
data <- get_total_data(input_dir)
format_data(input_dir, data)
