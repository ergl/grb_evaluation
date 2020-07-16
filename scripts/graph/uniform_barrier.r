#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                         "grid",
                         "getopt",
                         "proto",
                         "gridExtra",
                         "reshape2",
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
    theme(plot.title = element_blank(),
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

          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size=7),
          legend.text = element_text(size=6),
          legend.box.background = element_rect(color="black", fill="white"))

df <- read.csv("../../uniform_replication/barrier/general.csv")
df_replication <- df[df$exp == "vary_repl", ]
# df_melted <- melt(df_replication, id='replication')
# df_replication_latencies <- df_melted[
#     df_melted$variable %in% c("ronly_median", "wonly_median", "ronly_barrier_median", "wonly_barrier_median")
#     ,
# ]

replication_plot <- ggplot(df_replication,
                           aes(x=factor(replication), y=throughput, group=1, color=1)) +

    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=2, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,325000)) +
    labs(x = "Replication Interval", y = "Throughput (Ktps)") +
    plot_theme

# readonly_plot <- ggplot(df_replication_latencies, aes(x=factor(replication), y=value, group=variable, color=variable)) +
# readonly_plot <- ggplot(df_replication, aes(x=factor(replication))) +

#     geom_point(aes(y=ronly_median), color=1) +
#     geom_line(aes(y=ronly_median), group=1, color=1) +

#     geom_point(aes(y=wonly_median), color=2) +
#     geom_line(aes(y=wonly_median), group=2, color=2) +

#     geom_point(aes(y=ronly_barrier_median), color=3) +
#     geom_line(aes(y=ronly_barrier_median), group=3, color=3) +

#     geom_point(aes(y=wonly_barrier_median), color=4) +
#     geom_line(aes(y=wonly_barrier_median), group=4, color=4) +

#     geom_vline(xintercept=2, size=0.5, color="#807F80") +

#     scale_y_log10() +
#     annotation_logticks(sides="l") +
#     labs(x = "Replication Interval", y = "Mean Readonly Latency (ms)") +
#     plot_theme

ronly <- ggplot(df_replication, aes(x=factor(replication), y=ronly_median)) +

    geom_point(size=1.5) +
    geom_line(group=1) +

    geom_vline(xintercept=2, size=0.5, color="#807F80") +

    scale_y_continuous(breaks=seq(0, 1, by=0.10),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,1)) +

    labs(x = "Replication Interval", y = "Mean Readonly Latency (ms)") +
    plot_theme

ronly_barrier <- ggplot(df_replication, aes(x=factor(replication), y=ronly_barrier_median)) +

    geom_point(size=1.5) +
    geom_line(group=1) +

    geom_vline(xintercept=2, size=0.5, color="#807F80") +

    scale_y_log10() +

    labs(x = "Replication Interval (Post-commit uniform barrier)", y = "Mean Readonly Latency (ms)") +
    plot_theme

wonly <- ggplot(df_replication, aes(x=factor(replication), y=wonly_median)) +

    geom_point(size=1.5) +
    geom_line(group=1) +

    geom_vline(xintercept=2, size=0.5, color="#807F80") +

    scale_y_continuous(breaks=seq(0, 1, by=0.10),
                       expand=c(0,0)) +

    coord_cartesian(ylim=c(0,1)) +

    labs(x = "Replication Interval", y = "Mean Update-only Latency (ms)") +
    plot_theme

wonly_barrier <- ggplot(df_replication, aes(x=factor(replication), y=wonly_barrier_median)) +

    geom_point(size=1.5) +
    geom_line(group=1) +

    geom_vline(xintercept=2, size=0.5, color="#807F80") +

    scale_y_log10() +

    labs(x = "Replication Interval (Post-commit uniform barrier)", y = "Mean Update-only Latency (ms)") +
    plot_theme

combined <- grid.arrange(replication_plot + theme(legend.position = "none"),
                         grid.arrange(grid.arrange(ronly, wonly, nrow=1),
                                      grid.arrange(ronly_barrier, wonly_barrier, nrow=1)), nrow=1)

ggsave(filename = "./uniform_barrier_bench.png",
       plot = combined,
       device = "png",
       width = 20,
       height = 10,
       dpi = 300)
