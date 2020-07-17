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


ronly_legend_params <- scale_colour_manual(name="",
                                           labels=c("No Barrier", "Post-commit barrier"),
                                           breaks=c("ronly_median", "ronly_barrier_median"),
                                           values=c("red", "blue"))

wonly_legend_params <- scale_colour_manual(name="",
                                           labels=c("No Barrier", "Post-commit barrier"),
                                           breaks=c("wonly_median", "wonly_barrier_median"),
                                           values=c("red", "blue"))

df <- read.csv("../../uniform_replication/barrier/general.csv")
df_replication <- df[df$exp == "vary_repl_fast", ]

df_replication_latencies_ronly <- melt(df_replication,
                                        id.vars = 'replication',
                                        measure.vars = c("ronly_median",
                                                         "ronly_barrier_median"))

df_replication_latencies_wonly <- melt(df_replication,
                                        id.vars = 'replication',
                                        measure.vars = c("wonly_median",
                                                         "wonly_barrier_median"))

replication_plot <- ggplot(df_replication,
                           aes(x=factor(replication), y=throughput, group=exp, color=exp)) +

    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=2, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,325000)) +
    labs(x = "Replication Interval", y = "Throughput (Ktps)") +
    plot_theme

ronly_latencies_plot <- ggplot(df_replication_latencies_ronly,
                               aes(x=factor(replication), y=value, color=variable, group=variable)) +
    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=2, size=0.5, color="#807F80") +
    scale_y_continuous(breaks=seq(0, 1, by=0.1), expand=c(0,0)) +
    coord_cartesian(ylim=c(0,0.61)) +
    ronly_legend_params +
    labs(title = "Read-only latencies", x = "Replication Interval", y = "Median Latency (ms)") +
    plot_theme

wonly_latencies_plot <- ggplot(df_replication_latencies_wonly,
                               aes(x=factor(replication), y=value, color=variable, group=variable)) +
    geom_point(size=1.5) +
    geom_line() +
    geom_vline(xintercept=2, size=0.5, color="#807F80") +
    scale_y_log10() +
    wonly_legend_params +
    labs(title = "Update latencies", x = "Replication Interval", y = "Median Latency (ms)") +
    plot_theme

get_legend <- function(arg_plot) {
    tmp <- ggplot_gtable(ggplot_build(arg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

combined_legend <- get_legend(ronly_latencies_plot)

combined <- grid.arrange(replication_plot + theme(legend.position = "none"),
                         grid.arrange(ronly_latencies_plot + theme(legend.position = "none"),
                                      wonly_latencies_plot + theme(legend.position = "none"),
                                      nrow=1),
                         combined_legend,
                         ncol=1,
                         heights=c(0.8,0.8,0.1))

ggsave(filename = "./uniform_barrier_bench.pdf",
       plot = combined,
       device = "pdf",
       width = 9,
       height = 11,
       dpi = 300)
