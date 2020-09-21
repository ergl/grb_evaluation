#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                         "grid",
                         "getopt",
                         "proto",
                         "gridExtra",
                         "ggplot2",
                         "scales")

for(p in packages.to.install) {
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        library(p, character.only=TRUE)
    }
}

format_thousand_comma <- function(x) {
    return(format(x/1000, big.mark = ",", scientific = FALSE))
}

plot_theme <- theme_minimal(base_size=10) +
    theme(plot.title = element_text(size=9, margin=margin(0,0,10,0)),
          plot.margin = margin(15,20,15,0),

          axis.title.x = element_text(size=9, margin=margin(10,0,0,0)),
          axis.title.y = element_text(size=9, margin=margin(0,10,0,10)),
          axis.text.x =  element_text(size=7, margin=margin(10,0,0,0)),
          axis.text.y =  element_text(size=7, margin=margin(0,8,0,10)),
          axis.line =    element_line(color="black", size=0.5),
          axis.ticks =   element_line(color="black"),

          panel.grid.minor = element_line(colour="#EBEBEB", size=0.25),
          panel.grid.major = element_line(colour="#EBEBEB", size=0.25),
          panel.spacing =    unit(1, "lines"),

          legend.position = "none")

# df <- read.csv("../../red_coord/latencies.csv")
# df_readonly <- df[df$exp == "reads", ]
# df_updates <- df[df$exp == "updates", ]
# group_by <- "replicas"
# labels <- c("Replica C", "Replica B", "Leader", "Replica A")
# reads_title_text <- "Reads only, 1 object, 20ms RTT"
# updates_title_text <- "Updates only, 1 object, 20ms RTT"
# throughput_y_coord <- coord_cartesian(ylim=c(0,35000))
# throughput_y_scale <- scale_y_continuous(breaks=seq(0, 1000000, by=2000),
#                                            labels=format_thousand_comma,
#                                            expand=c(0,0))

# latency_y_coord <- coord_cartesian(ylim=c(0,45))
# latency_y_scale <- scale_y_continuous(breaks=seq(0, 50, by=2),
#                                       expand=c(0,0),
#                                       sec.axis = dup_axis())

df <- read.csv("../../red_coord/red_high_latency/results.csv")
df <- df[df$threads == 250, ]
df_readonly <- df[df$exp == "reads", ]
df_updates <- df[df$exp == "updates", ]
group_by <- "threads"
labels <- c("Ireland", "Oregon", "Sydney", "N. Virginia (Leader)")
reads_title_text <- "Reads only, 1 object"
updates_title_text <- "Updates only, 1 object"
throughput_y_coord <- coord_cartesian(ylim=c(0,10000))
throughput_y_scale <- scale_y_continuous(breaks=seq(0, 1000000, by=500),
                                         labels=format_thousand_comma,
                                         expand=c(0,0))

latency_y_coord <- coord_cartesian(ylim=c(0,400))
latency_y_scale <- scale_y_continuous(breaks=seq(0, 400, by=10),
                                      expand=c(0,0),
                                      sec.axis = dup_axis())

throughput_plot <- ggplot(df_readonly, aes(x=factor(cluster), y=throughput,
                                           group=.data[[group_by]], fill=.data[[group_by]])) +
    geom_bar(stat='identity', position="dodge") +
    throughput_y_scale +
    throughput_y_coord +
    scale_x_discrete(labels=labels) +
    labs(title=reads_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

throughput_writes_plot <- ggplot(df_updates, aes(x=factor(cluster), y=throughput,
                                                 group=.data[[group_by]], fill=.data[[group_by]])) +
    geom_bar(stat='identity', position="dodge") +
    throughput_y_scale +
    throughput_y_coord +
    scale_x_discrete(labels=labels) +
    labs(title=updates_title_text, x = "Replicas", y = "Max. Throughput (Ktps)") +
    plot_theme

latency_plot <- ggplot(df_readonly, aes(x=factor(cluster), y=red_ronly_median,
                                        group=.data[[group_by]], fill=.data[[group_by]])) +
    geom_bar(stat='identity', position="dodge") +
    latency_y_scale +
    latency_y_coord +
    scale_x_discrete(labels=labels) +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

latency_writes_plot <- ggplot(df_updates, aes(x=factor(cluster), y=red_wonly_median,
                                              group=.data[[group_by]], fill=.data[[group_by]])) +
    geom_bar(stat='identity', position="dodge") +
    latency_y_scale +
    latency_y_coord +
    scale_x_discrete(labels=labels) +
    labs(x = "Replicas", y = "Median Latency (ms)") +
    plot_theme

reads <- grid.arrange(throughput_plot,
                      latency_plot,
                      nrow=1)

updates <- grid.arrange(throughput_writes_plot,
                        latency_writes_plot,
                        nrow=1)

combined <- grid.arrange(reads, updates, ncol=1)

ggsave(filename = "./sites_latency.pdf",
       plot = combined,
       device = "pdf",
       width = 12,
       height = 10,
       dpi = 300)
