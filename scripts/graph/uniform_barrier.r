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
    theme(plot.title = element_text(size=12, margin=margin(0,0,10,0)),
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

df <- read.csv("../../uniform_barrier/results.csv")
df_write <- df[df$exp == "write_only", ]
df <- df[df$exp == "write_barrier", ]

throughput_plot <- ggplot(df, aes(x=factor(replication), y=throughput,
                                  group=exp, color=exp)) +
    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=5000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,100000)) +
    labs(x = "Replication Interval", y = "Throughput (Ktps)") +
    plot_theme

write_throughput_plot <- ggplot(df_write, aes(x=factor(replication), y=throughput,
                                              group=exp, color=exp)) +
    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=10000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,200000)) +
    labs(x = "Replication Interval", y = "Throughput (Ktps)") +
    plot_theme

latencies_plot <- ggplot(df, aes(x=factor(replication), y=wonly_barrier_median,
                                 group=exp, color=exp)) +
    geom_point(size=1.5) +
    geom_line() +
    scale_y_log10() +
    labs(x = "Replication Interval", y = "Median Latency (ms)") +
    plot_theme

write_latencies_plot <- ggplot(df_write, aes(x=factor(replication), y=wonly_median,
                                 group=exp, color=exp)) +
    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 3, by=0.25), expand=c(0,0)) +
    coord_cartesian(ylim=c(0,3)) +
    labs(x = "Replication Interval", y = "Median Latency (ms)") +
    plot_theme

combined_barrier <- grid.arrange(throughput_plot + theme(legend.position = "none"),
                                 latencies_plot + theme(legend.position = "none"),
                                 nrow=1)

combined_write <- grid.arrange(write_throughput_plot + theme(legend.position = "none"),
                               write_latencies_plot + theme(legend.position = "none"),
                               nrow=1)

combined <- grid.arrange(combined_barrier,
                         combined_write,
                         ncol=1)

ggsave(filename = "./barrier.pdf",
       plot = combined,
       device = "pdf",
       width = 15,
       height = 10,
       dpi = 300)
