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

get_client_threads <- function(Dir) {
    per_machine <- as.integer(str_extract(str_extract(Dir, "t=[0-9]+"), "[0-9]+"))
    num_clusters <- as.integer(str_extract(str_extract(Dir, "cl=[0-9]+"), "[0-9]+"))
    client_machines <- as.integer(str_extract(str_extract(Dir, "cm=[0-9]+"), "[0-9]+"))

    # If directory contains client machines info, use it
    # otherwise assume it's the same as database machines
    if (!is.na(client_machines)) {
        total <- per_machine * client_machines * num_clusters
    } else {
        db_machines <- as.integer(str_extract(str_extract(Dir, "dm=[0-9]+"), "[0-9]+"))
        total <- per_machine * db_machines * num_clusters
    }

    return(data.frame(per_machine, total))
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

    ronly_file <- sprintf("%s/readonly-blue_latencies.csv", Dir)
    wonly_file <- sprintf("%s/writeonly-blue_latencies.csv", Dir)
    rw_file <- sprintf("%s/read-write-blue_latencies.csv", Dir)

    mean_latency_ronly <- 0
    mean_latency_wonly <- 0
    mean_latency_rw <- 0
    median_latency_ronly <- 0
    median_latency_wonly <- 0
    median_latency_rw <- 0

    if (file.exists(ronly_file)) {
        latencies_ronly <- read.csv(ronly_file)
        mean_latency_ronly <- mean(latencies_ronly$mean) / 1000
        median_latency_ronly <- mean(latencies_ronly$median) / 1000
    }

    if (file.exists(wonly_file)) {
        latencies_wonly <- read.csv(wonly_file)
        mean_latency_wonly <- mean(latencies_wonly$mean) / 1000
        median_latency_wonly <- mean(latencies_wonly$median) / 1000
    }

    if (file.exists(rw_file)) {
        latencies_rw <- read.csv(rw_file)
        mean_latency_rw <- mean(latencies_rw$mean) / 1000
        median_latency_rw <- mean(latencies_rw$median) / 1000
    }

    return(data.frame(max_commit_w,
                      median_commit_w,
                      mean_latency_ronly,
                      mean_latency_wonly,
                      mean_latency_rw,
                      median_latency_ronly,
                      median_latency_wonly,
                      median_latency_rw))
}

format_data <- function(Dir, Data) {
    format_decimal <- function(string, ..., withoutZeros = FALSE) {
        return(formatC(string, format="f", big.mark=",", drop0trailing=withoutZeros))
    }

    thread_info <- get_client_threads(Dir)

    format <- sprintf("|%s (%s)|%s|%s|%s|%s|%s|%s|%s|%s|\n",
        "Threads",
        "Total",
        "Max Thr",
        "Median Thr",
        "Ronly Ms",
        "Wonly Ms",
        "R/W",
        "Ronly median",
        "Wonly median",
        "R/W median"
    )

    cat(format)

    format <- sprintf(
        "|%s (%s)|%s|%s|%f|%f|%f|%f|%f|%f|\n",
        format_decimal(thread_info$per_machine, withoutZeros=TRUE),
        format_decimal(thread_info$total, withoutZeros=TRUE),
        format_decimal(Data$max_commit_w),
        format_decimal(Data$median_commit_w),
        Data$mean_latency_ronly,
        Data$mean_latency_wonly,
        Data$mean_latency_rw,
        Data$median_latency_ronly,
        Data$median_latency_wonly,
        Data$median_latency_rw
    )

    cat(format)
}

input_dir <- opt$data_dir
data <- get_total_data(input_dir)
format_data(input_dir, data)
