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
    'header', 'p', 0, "logical",
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

print.headers <- if(!is.null(opt$header)) {
    0
} else {
    1
}

format_decimal <- function(string, ..., withoutZeros = FALSE) {
    return(formatC(string, format="f", big.mark=",", drop0trailing=withoutZeros))
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

get_summary_data <- function(Dir) {
    summary <- read.csv(sprintf("%s/summary.csv", Dir))
    abort_ratio <- sum(summary$failed) / sum(summary$total)

    # Remove first row, as it is usually inflated
    summary <- summary[-c(1), ]

    # All operations
    max_total <- max(summary$total)
    max_total_row <- summary[c(which(summary$total == max_total)), ]
    if (nrow(max_total_row) > 1) {
        max_total_row <- max_total_row[1, ]
    }
    max_total <- max_total / max_total_row$window

    # All committed operations
    max_commit <- max(summary$successful)
    max_commit_row <- summary[c(which(summary$successful == max_commit)), ]
    if (nrow(max_commit_row) > 1) {
        max_commit_row <- max_commit_row[1, ]
    }
    max_commit <- max_commit / max_commit_row$window

    # Get the median of committed operations
    median_commit <- median(summary$successful)
    median_window <- median(summary$window)
    median_commit <- median_commit / median_window

    return(data.frame(max_total, max_commit, median_commit, abort_ratio))
}

get_default_latencies <- function(Dir) {
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

    return(data.frame(blue_mean_latency_ronly,
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

get_red_track_latencies <- function(Dir) {
    r <- latency_for_file(sprintf("%s/readonly-red-track_latencies.csv", Dir))
    overall_mean <- r$mean
    overall_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_start_latencies.csv", Dir))
    start_mean <- r$mean
    start_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_read_latencies.csv", Dir))
    read_mean <- r$mean
    read_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_prepare_latencies.csv", Dir))
    prepare_mean <- r$mean
    prepare_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_accept_latencies.csv", Dir))
    accept_mean <- r$mean
    accept_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_commit_latencies.csv", Dir))
    commit_mean <- r$mean
    commit_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_coordinator_commit_latencies.csv", Dir))
    commit_coord_mean <- r$mean
    commit_coord_median <- r$median

    r <- latency_for_file(sprintf("%s/readonly-red-track_coordinator_commit_barrier_latencies.csv", Dir))
    commit_coord_barrier_mean <- r$mean
    commit_coord_barrier_median <- r$median

    return(data.frame(overall_mean, overall_median,
                      start_mean, start_median,
                      read_mean, read_median,
                      prepare_mean, prepare_median,
                      accept_mean, accept_median,
                      commit_mean, commit_median,
                      commit_coord_mean, commit_coord_median,
                      commit_coord_barrier_mean, commit_coord_barrier_median))
}

get_blue_track_latencies <- function(Dir) {
    r <- latency_for_file(sprintf("%s/read-write-blue-track_latencies.csv", Dir))
    overall_mean <- r$mean
    overall_median <- r$median

    r <- latency_for_file(sprintf("%s/read-write-blue-track_start_latencies.csv", Dir))
    start_mean <- r$mean
    start_median <- r$median

    r <- latency_for_file(sprintf("%s/read-write-blue-track_read_latencies.csv", Dir))
    read_mean <- r$mean
    read_median <- r$median

    r <- latency_for_file(sprintf("%s/read-write-blue-track_update_latencies.csv", Dir))
    update_mean <- r$mean
    update_median <- r$median

    r <- latency_for_file(sprintf("%s/read-write-blue-track_commit_latencies.csv", Dir))
    commit_mean <- r$mean
    commit_median <- r$median

    return(data.frame(overall_mean, overall_median,
                      start_mean, start_median,
                      read_mean, read_median,
                      update_mean, update_median,
                      commit_mean, commit_median))
}

get_blue_bypass_latencies <- function(Dir) {
    r <- latency_for_file(sprintf("%s/readonly-blue_latencies.csv", Dir))
    read_mean <- r$mean
    read_median <- r$median

   r <- latency_for_file(sprintf("%s/readonly-blue-bypass_latencies.csv", Dir))
   read_bypass_mean <- r$mean
   read_bypass_median <- r$median

    r <- latency_for_file(sprintf("%s/writeonly-blue_latencies.csv", Dir))
    update_mean <- r$mean
    update_median <- r$median

    r <- latency_for_file(sprintf("%s/writeonly-blue-bypass_latencies.csv", Dir))
    update_bypass_mean <- r$mean
    update_bypass_median <- r$median

    return(data.frame(read_mean, read_median,
                      read_bypass_mean, read_bypass_median,
                      update_mean, update_median,
                      update_bypass_mean, update_bypass_median))
}

process_default_data <- function(Dir) {
    thread_info <- get_client_threads(Dir)
    throughput_df <- get_summary_data(Dir)
    latency_df <- get_default_latencies(Dir)

    headers <- c(
        "threads",
        "total_throughput",
        "throughput",
        "throughput_med",
        "reads_mean",
        "reads_median",
        "updates_mean",
        "updates_median",
        "mixed_mean",
        "mixed_median",
        "red_reads_mean",
        "red_reads_median",
        "red_updates_mean",
        "red_updates_median",
        "red_mixed_mean",
        "red_mixed_median",
        "abort_ratio"
    )

    row_format <- "%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n"
    row_data <- sprintf(row_format,
                        thread_info$per_machine,
                        throughput_df$max_total,
                        throughput_df$max_commit,
                        throughput_df$median_commit,
                        latency_df$blue_mean_latency_ronly,
                        latency_df$blue_median_latency_ronly,
                        latency_df$blue_mean_latency_wonly,
                        latency_df$blue_median_latency_wonly,
                        latency_df$blue_mean_latency_rw,
                        latency_df$blue_median_latency_rw,
                        latency_df$red_mean_latency_ronly,
                        latency_df$red_median_latency_ronly,
                        latency_df$red_mean_latency_wonly,
                        latency_df$red_median_latency_wonly,
                        latency_df$red_mean_latency_rw,
                        latency_df$red_median_latency_rw,
                        throughput_df$abort_ratio)

    if(print.headers) {
        cat(sprintf("%s\n", paste(headers, collapse=",")))
    }
    cat(row_data)
}

process_red_track_data <- function(Dir) {
    thread_info <- get_client_threads(Dir)
    throughput_df <- get_summary_data(Dir)
    latency_df <- get_red_track_latencies(Dir)

    headers <- c(
        "threads",
        "throughput",
        "overall_mean",
        "overall_median",
        "start_mean",
        "start_median",
        "read_mean",
        "read_median",
        "prepare_mean",
        "prepare_median",
        "accept_mean",
        "accept_median",
        "commit_mean",
        "commit_median",
        "commit_coord_barrier_mean",
        "commit_coord_barrier_median",
        "commit_coord_mean",
        "commit_coord_median"
    )

    row_format <- "%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n"
    row_data <- sprintf(row_format,
                        thread_info$per_machine,
                        throughput_df$max_commit,
                        latency_df$overall_mean, latency_df$overall_median,
                        latency_df$start_mean, latency_df$start_median,
                        latency_df$read_mean, latency_df$read_median,
                        latency_df$prepare_mean, latency_df$prepare_median,
                        latency_df$accept_mean, latency_df$accept_median,
                        latency_df$commit_mean, latency_df$commit_median,
                        latency_df$commit_coord_barrier_mean, latency_df$commit_coord_barrier_median,
                        latency_df$commit_coord_mean, latency_df$commit_coord_median)

    if(print.headers) {
        cat(sprintf("%s\n", paste(headers, collapse=",")))
    }
    cat(row_data)
}

process_blue_track_data <- function(Dir) {
    thread_info <- get_client_threads(Dir)
    throughput_df <- get_summary_data(Dir)
    latency_df <- get_blue_track_latencies(Dir)

    headers <- c(
        "threads",
        "throughput",
        "overall_mean",
        "overall_median",
        "start_mean",
        "start_median",
        "read_mean",
        "read_median",
        "update_mean",
        "update_median",
        "commit_mean",
        "commit_median"
    )

    row_format <- "%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n"
    row_data <- sprintf(row_format,
                        thread_info$per_machine,
                        throughput_df$max_commit,
                        latency_df$overall_mean, latency_df$overall_median,
                        latency_df$start_mean, latency_df$start_median,
                        latency_df$read_mean, latency_df$read_median,
                        latency_df$update_mean, latency_df$update_median,
                        latency_df$commit_mean, latency_df$commit_median)

    if(print.headers) {
        cat(sprintf("%s\n", paste(headers, collapse=",")))
    }
    cat(row_data)
}

process_bypass_data <- function(Dir) {
    thread_info <- get_client_threads(Dir)
    throughput_df <- get_summary_data(Dir)
    latency_df <- get_blue_bypass_latencies(Dir)

    headers <- c(
        "threads",
        "total_throughput",
        "throughput",
        "throughput_med",
        "reads_mean",
        "reads_median",
        "updates_mean",
        "updates_median",
        "reads_bypass_mean",
        "reads_bypass_median",
        "updates_bypass_mean",
        "updates_bypass_median",
        "abort_ratio"
    )

    row_format <- "%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n"
    row_data <- sprintf(row_format,
                        thread_info$per_machine,
                        throughput_df$max_total,
                        throughput_df$max_commit,
                        throughput_df$median_commit,
                        latency_df$read_mean,
                        latency_df$read_median,
                        latency_df$update_mean,
                        latency_df$update_median,
                        latency_df$read_bypass_mean,
                        latency_df$read_bypass_median,
                        latency_df$update_bypass_mean,
                        latency_df$update_bypass_median,
                        throughput_df$abort_ratio)

    if(print.headers) {
        cat(sprintf("%s\n", paste(headers, collapse=",")))
    }
    cat(row_data)
}

# process_default_data(opt$data_dir)
process_bypass_data(opt$data_dir)