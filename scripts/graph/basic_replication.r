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
          legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.box.background = element_rect(color="white", fill="white"))

df <- read.csv("../../basic_replication_test/dynamic/general.csv")

df_keys <- df[df$exp == "keys", ]
df_replicas <- df[df$exp == "replicas", ]
df_fraction <- df[df$exp == "fraction", ]
df_partitions <- df[df$exp == "partitions", ]

partition_plot <- ggplot(df_partitions, aes(x=factor(partitions), y=throughput, group=1)) +
    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=4, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,325000)) +
    labs(x = "Partitions", y = "Throughput (Ktps)") +
    plot_theme

replica_plot <- ggplot(df_replicas, aes(x=factor(replicas), y=throughput, group=1)) +
    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=2, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,325000)) +
    labs(x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

keys_plot <- ggplot(df_keys, aes(x=factor(keys), y=throughput, group=1)) +
    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=3, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,500000)) +
    labs(x = "Operations", y = "Throughput (Ktps)") +
    plot_theme

fraction_plot <- ggplot(df_fraction, aes(x=factor(fraction), y=throughput, group=1)) +
    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=2, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,325000)) +
    labs(x = "Update Transactions (%)", y = "Throughput (Ktps)") +
    plot_theme

combined <- grid.arrange(partition_plot, replica_plot, keys_plot, fraction_plot, ncol=1)

ggsave(filename = "./dynamic_bench.png",
       plot = combined,
       device = "png",
       width = 5,
       height = 16,
       dpi = 300)
