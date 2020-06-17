#!/usr/bin/env Rscript

# Parse the --file= argument out of command line args and
# determine where base directory is
arg0 <- sub("--file=(.*)", "\\1", grep("--file=", commandArgs(), value = TRUE))
packages.to.install <- c("getopt", "stringr")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        library(p, character.only=TRUE)
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

    latencies_rw <- read.csv(sprintf("%s/readwrite_track_latencies.csv", Dir))
    latencies_ronly <- read.csv(sprintf("%s/readonly_latencies.csv", Dir))
    latencies_rw_commit <- read.csv(sprintf("%s/readwrite_track_commit_latencies.csv", Dir))

    # Remove first row, as it is usually inflated
    summary <- summary[-c(1), ]

    # All issued operations
    max_total <- max(summary$total)
    max_total_row <- summary[c(which(summary$total == max_total)), ]
    max_total_w <- max_total / max_total_row$window

    # All committed operations
    max_commit <- max(summary$successful)
    max_commit_row <- summary[c(which(summary$successful == max_commit)), ]
    max_commit_w <- max_commit / max_commit_row$window

    # Get the median of committed operations
    median_commit <- median(summary$successful)
    median_window <- median(summary$window)
    median_commit_w <- median_commit / median_window
    commit_ratio <- (sum(summary$successful) / sum(summary$total))

    mean_latency_rw <- mean(latencies_rw$mean) / 1000
    mean_latency_ronly <- mean(latencies_ronly$mean) / 1000
    mean_latency_rw_commit <- mean(latencies_rw_commit$mean) / 1000


    return(data.frame(max_total_w,
                      max_commit_w,
                      median_commit_w,
                      commit_ratio,
                      mean_latency_rw,
                      mean_latency_rw_commit,
                      mean_latency_ronly))
}

format_data <- function(Dir, Data) {
    format_decimal <- function(string, ..., withoutZeros = FALSE) {
        return(formatC(string, format="f", big.mark=",", drop0trailing=withoutZeros))
    }

    protocol_name <- toupper(str_extract(str_extract(Dir, "prot=[a-z]+"), "[a-z]+$"))
    thread_info <- get_client_threads(Dir)
    num_partitions <- as.integer(str_extract(str_extract(Dir, "ring=[0-9]+"), "[0-9]+"))
    versions <- str_extract(str_extract(Dir, "vsn=[0-9]+"), "[0-9]+")

    if (protocol_name == "RC") {
        # If read committed, don't include commit ratio, it's always 1
        format <- sprintf(
            "|%s|%s (%s)|%s|%f|%f|%f|\n",
            protocol_name,
            format_decimal(thread_info$per_machine, withoutZeros=TRUE),
            format_decimal(thread_info$total, withoutZeros=TRUE),
            format_decimal(Data$max_total_w),
            Data$mean_latency_ronly,
            Data$mean_latency_rw,
            Data$mean_latency_rw_commit
        )
    } else {
        format <- sprintf(
            "|%s|%s (%s)|%s|%s|%f|%f|%f|%f|\n",
            protocol_name,
            format_decimal(thread_info$per_machine, withoutZeros=TRUE),
            format_decimal(thread_info$total, withoutZeros=TRUE),
            format_decimal(Data$max_total_w),
            format_decimal(Data$max_commit_w),
            Data$mean_latency_ronly,
            Data$mean_latency_rw,
            Data$mean_latency_rw_commit,
            Data$commit_ratio
        )
    }

    cat(format)
}

input_dir <- opt$data_dir
data <- get_total_data(input_dir)
format_data(input_dir, data)
