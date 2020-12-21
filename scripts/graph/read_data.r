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
    'data_dir', 'i', 2, "character",
    'rubis', 'r', 0, "logical"
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

rubis <- if (!is.null(opt$rubis)) {
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
    n <- 0
    mean <- 0
    median <- 0
    if (file.exists(File)) {
        latencies <- read.csv(File)
        # We don't want to add extra zeroes if there were no operations in a specific window
        latencies <- latencies[latencies$n != 0, ]
        n <- nrow(latencies)

        # If there are no windows with operations, return zero (caller with deal with this)
        if (n == 0) {
            return(data.frame(n, mean, median))
        }

        mean <- mean(latencies$mean) / 1000
        median <- mean(latencies$median) / 1000
    }

    return(data.frame(n, mean, median))
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

get_rubis_latencies <- function(Dir) {
    r <- latency_for_file(sprintf("%s/register-user_latencies.csv", Dir))
    mean_register_user <- r$mean
    median_register_user <- r$median

    r <- latency_for_file(sprintf("%s/browse-categories_latencies.csv", Dir))
    mean_browse_categories <- r$mean
    median_browse_categories <- r$median

    r <- latency_for_file(sprintf("%s/search-items-in-category_latencies.csv", Dir))
    mean_search_items_in_category <- r$mean
    median_search_items_in_category <- r$median

    r <- latency_for_file(sprintf("%s/browse-regions_latencies.csv", Dir))
    mean_browse_regions <- r$mean
    median_browse_regions <- r$median

    r <- latency_for_file(sprintf("%s/browse-categories-in-region_latencies.csv", Dir))
    mean_browse_categories_in_region <- r$mean
    median_browse_categories_in_region <- r$median

    r <- latency_for_file(sprintf("%s/search-items-in-region_latencies.csv", Dir))
    mean_search_items_in_region <- r$mean
    median_search_items_in_region <- r$median

    r <- latency_for_file(sprintf("%s/view-item_latencies.csv", Dir))
    mean_view_item <- r$mean
    median_view_item <- r$median

    r <- latency_for_file(sprintf("%s/view-user-info_latencies.csv", Dir))
    mean_view_user_info <- r$mean
    median_view_user_info <- r$median

    r <- latency_for_file(sprintf("%s/view-bid-history_latencies.csv", Dir))
    mean_view_bid_history <- r$mean
    median_view_bid_history <- r$median

    r <- latency_for_file(sprintf("%s/buy-now_latencies.csv", Dir))
    mean_buy_now <- r$mean
    median_buy_now <- r$median

    r <- latency_for_file(sprintf("%s/store-buy-now_latencies.csv", Dir))
    mean_store_buy_now <- r$mean
    median_store_buy_now <- r$median

    r <- latency_for_file(sprintf("%s/put-bid_latencies.csv", Dir))
    mean_put_bid <- r$mean
    median_put_bid <- r$median

    r <- latency_for_file(sprintf("%s/store-bid_latencies.csv", Dir))
    mean_store_bid <- r$mean
    median_store_bid <- r$median

    r <- latency_for_file(sprintf("%s/put-comment_latencies.csv", Dir))
    mean_put_comment <- r$mean
    median_put_comment <- r$median

    r <- latency_for_file(sprintf("%s/store-comment_latencies.csv", Dir))
    mean_store_comment <- r$mean
    median_store_comment <- r$median

    r <- latency_for_file(sprintf("%s/select-category-to-sell-item_latencies.csv", Dir))
    mean_select_category_to_sell_item <- r$mean
    median_select_category_to_sell_item <- r$median

    r <- latency_for_file(sprintf("%s/register-item_latencies.csv", Dir))
    mean_register_item <- r$mean
    median_register_item <- r$median

    r <- latency_for_file(sprintf("%s/about-me_latencies.csv", Dir))
    mean_about_me <- r$mean
    median_about_me <- r$median

    r <- latency_for_file(sprintf("%s/get-auctions-ready-for-close_latencies.csv", Dir))
    mean_get_auctions_ready_for_close <- r$mean
    median_get_auctions_ready_for_close <- r$median

    r <- latency_for_file(sprintf("%s/close-auction_latencies.csv", Dir))
    mean_close_auction <- r$mean
    median_close_auction <- r$median

    all_mean <- mean(c(mean_register_user,
                       mean_browse_categories,
                       mean_search_items_in_category,
                       mean_browse_regions,
                       mean_browse_categories_in_region,
                       mean_search_items_in_region,
                       mean_view_item,
                       mean_view_user_info,
                       mean_view_bid_history,
                       mean_buy_now,
                       mean_store_buy_now,
                       mean_put_bid,
                       mean_store_bid,
                       mean_put_comment,
                       mean_store_comment,
                       mean_select_category_to_sell_item,
                       mean_register_item,
                       mean_about_me,
                       mean_get_auctions_ready_for_close,
                       mean_close_auction))

    all_median <- mean(c(median_register_user,
                         median_browse_categories,
                         median_search_items_in_category,
                         median_browse_regions,
                         median_browse_categories_in_region,
                         median_search_items_in_region,
                         median_view_item,
                         median_view_user_info,
                         median_view_bid_history,
                         median_buy_now,
                         median_store_buy_now,
                         median_put_bid,
                         median_store_bid,
                         median_put_comment,
                         median_store_comment,
                         median_select_category_to_sell_item,
                         median_register_item,
                         median_about_me,
                         median_get_auctions_ready_for_close,
                         median_close_auction))

    return(data.frame(
        all_mean,
        all_median,
        mean_register_user,
        mean_browse_categories,
        mean_search_items_in_category,
        mean_browse_regions,
        mean_browse_categories_in_region,
        mean_search_items_in_region,
        mean_view_item,
        mean_view_user_info,
        mean_view_bid_history,
        mean_buy_now,
        mean_store_buy_now,
        mean_put_bid,
        mean_store_bid,
        mean_put_comment,
        mean_store_comment,
        mean_select_category_to_sell_item,
        mean_register_item,
        mean_about_me,
        mean_get_auctions_ready_for_close,
        mean_close_auction,
        median_register_user,
        median_browse_categories,
        median_search_items_in_category,
        median_browse_regions,
        median_browse_categories_in_region,
        median_search_items_in_region,
        median_view_item,
        median_view_user_info,
        median_view_bid_history,
        median_buy_now,
        median_store_buy_now,
        median_put_bid,
        median_store_bid,
        median_put_comment,
        median_store_comment,
        median_select_category_to_sell_item,
        median_register_item,
        median_about_me,
        median_get_auctions_ready_for_close,
        median_close_auction
    ))
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

process_rubis_data_summary <- function(Dir) {
    thread_info <- get_client_threads(Dir)
    throughput_df <- get_summary_data(Dir)
    latency_df <- get_rubis_latencies(Dir)

    headers <- c("threads", "total_throughput", "all_mean", "all_median")
    row_format <- "%s,%f,%f,%f\n"
    row_data <- sprintf(row_format,
                        thread_info$per_machine,
                        throughput_df$max_total,
                        latency_df$all_mean,
                        latency_df$all_median)
    if(print.headers) {
        cat(sprintf("%s\n", paste(headers, collapse=",")))
    }
    cat(row_data)
}

process_rubis_data <- function(Dir) {
    thread_info <- get_client_threads(Dir)
    throughput_df <- get_summary_data(Dir)
    latency_df <- get_rubis_latencies(Dir)

    headers <- c("threads",
                 "total_throughput",
                 "throughput",
                 "throughput_med",
                 "all_mean",
                 "all_median",
                 "register_user_mean",
                 "browse_categories_mean",
                 "search_items_in_category_mean",
                 "browse_regions_mean",
                 "browse_categories_in_region_mean",
                 "search_items_in_region_mean",
                 "view_item_mean",
                 "view_user_info_mean",
                 "view_bid_history_mean",
                 "buy_now_mean",
                 "store_buy_now_mean",
                 "put_bid_mean",
                 "store_bid_mean",
                 "put_comment_mean",
                 "store_comment_mean",
                 "select_category_to_sell_item_mean",
                 "register_item_mean",
                 "about_me_mean",
                 "get_auctions_ready_for_close_mean",
                 "close_auction_mean",
                 "register_user_median",
                 "browse_categories_median",
                 "search_items_in_category_median",
                 "browse_regions_median",
                 "browse_categories_in_region_median",
                 "search_items_in_region_median",
                 "view_item_median",
                 "view_user_info_median",
                 "view_bid_history_median",
                 "buy_now_median",
                 "store_buy_now_median",
                 "put_bid_median",
                 "store_bid_median",
                 "put_comment_median",
                 "store_comment_median",
                 "select_category_to_sell_item_median",
                 "register_item_median",
                 "about_me_median",
                 "get_auctions_ready_for_close_median",
                 "close_auction_median",
                 "abort_ratio")
    row_format <- "%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n"
    row_data <- sprintf(row_format,
                        thread_info$per_machine,
                        throughput_df$max_total,
                        throughput_df$max_commit,
                        throughput_df$median_commit,
                        latency_df$all_mean,
                        latency_df$all_median,
                        latency_df$mean_register_user,
                        latency_df$mean_browse_categories,
                        latency_df$mean_search_items_in_category,
                        latency_df$mean_browse_regions,
                        latency_df$mean_browse_categories_in_region,
                        latency_df$mean_search_items_in_region,
                        latency_df$mean_view_item,
                        latency_df$mean_view_user_info,
                        latency_df$mean_view_bid_history,
                        latency_df$mean_buy_now,
                        latency_df$mean_store_buy_now,
                        latency_df$mean_put_bid,
                        latency_df$mean_store_bid,
                        latency_df$mean_put_comment,
                        latency_df$mean_store_comment,
                        latency_df$mean_select_category_to_sell_item,
                        latency_df$mean_register_item,
                        latency_df$mean_about_me,
                        latency_df$mean_get_auctions_ready_for_close,
                        latency_df$mean_close_auction,
                        latency_df$median_register_user,
                        latency_df$median_browse_categories,
                        latency_df$median_search_items_in_category,
                        latency_df$median_browse_regions,
                        latency_df$median_browse_categories_in_region,
                        latency_df$median_search_items_in_region,
                        latency_df$median_view_item,
                        latency_df$median_view_user_info,
                        latency_df$median_view_bid_history,
                        latency_df$median_buy_now,
                        latency_df$median_store_buy_now,
                        latency_df$median_put_bid,
                        latency_df$median_store_bid,
                        latency_df$median_put_comment,
                        latency_df$median_store_comment,
                        latency_df$median_select_category_to_sell_item,
                        latency_df$median_register_item,
                        latency_df$median_about_me,
                        latency_df$median_get_auctions_ready_for_close,
                        latency_df$median_close_auction,
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

if (rubis) {
    process_rubis_data(opt$data_dir)
    # process_rubis_data_summary(opt$data_dir)
} else {
    process_default_data(opt$data_dir)
    # process_bypass_data(opt$data_dir)
}
