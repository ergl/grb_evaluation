#!/usr/bin/env Rscript

packages.to.install <- c("lemon",
                         "plyr",
                         "grid",
                         "getopt",
                         "proto",
                         "gridExtra",
                         "ggplot2",
                         "reshape2",
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

          axis.title.x = element_text(size=9, margin=margin(10,0,-15,0)),
          axis.title.y = element_text(size=9, margin=margin(0,0,0,10)),
          axis.text.x =  element_text(size=7, margin=margin(10,0,0,0)),
          axis.text.y =  element_text(size=7, margin=margin(0,8,0,10)),
          axis.line =    element_line(color="black", size=0.5),
          axis.ticks =   element_line(color="black"),

          panel.grid.minor = element_line(colour="#EBEBEB", size=0.25),
          panel.grid.major = element_line(colour="#EBEBEB", size=0.25),
          panel.spacing =    unit(1, "lines"),

          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size=8),
          legend.text = element_text(size=7),
          legend.box.background = element_rect(color="black", fill="white"))

legend_labels <- scale_colour_manual(
    name="Latencies",
    labels=c(`overall_median` = "Transaction Latency",
             `commit_median` = "Commit Latency"),
    values=c(`overall_median` = "red",
                `commit_median` = "blue")
)

df_combined <- read.csv("../../red_coord_2/progression_results.csv")
df_each <- read.csv("../../red_coord_2/progression_latencies.csv")
title_text <- "Redblue (100% red), R=1, 20ms RTT"

melted <- melt(df_combined,
               id.vars=c('client_threads', 'throughput'),
               measure.vars=c('overall_median', 'commit_median'))

combined_plot <- ggplot(melted, aes(x=throughput, y=value, colour=variable, group=variable)) +
    geom_point(size=1.5) +
    geom_path() +
    scale_x_continuous(breaks=seq(0, 80000, by=5000),
                       labels=format_thousand_comma) +
    scale_y_continuous(breaks=seq(0,50,by=2), expand=c(0,0)) +
    coord_cartesian(xlim=c(0,80000), ylim=c(0,40)) +
    geom_hline(yintercept=27.5, size=1, color="#807F80") +
    labs(title=title_text, x = "Throughput (ktps)", y = "Median Latency (ms)") +
    legend_labels +
    plot_theme

melted <- melt(df_each,
               id.vars=c('cluster', 'throughput'),
               measure.vars=c('overall_median', 'commit_median'))

melted$cluster <- factor(melted$cluster, levels = c('virginia', 'oregon', 'ireland', 'california'))
each_plot <- ggplot(melted, aes(x=throughput, y=value, colour=variable, group=variable)) +
    geom_point(size=1.5) +
    geom_path() +
    geom_hline(yintercept=20, size=0.5, color="#807F80") +
    geom_hline(yintercept=30, size=0.5, color="#807F80") +
    scale_x_continuous(breaks=seq(0, 30000, by=5000),
                       labels=format_thousand_comma) +
    scale_y_continuous(breaks=seq(0,50,by=2), expand=c(0,0), sec.axis = dup_axis(name="")) +
    coord_cartesian(xlim=c(0,20000), ylim=c(0,40)) +
    facet_rep_wrap(~cluster, nrow=2, ncol=2, scales="free_x", labeller=labeller(
        cluster = c(
            `virginia` = "Leader",
            `oregon` = "Replica A",
            `ireland` = "Replica B",
            `california` = "Replica C"
        )
    )) +
    labs(title=title_text, x = "Throughput (ktps)", y = "Median Latency (ms)") +
    legend_labels +
    plot_theme

get_legend <- function(arg_plot) {
    tmp <- ggplot_gtable(ggplot_build(arg_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

combined_legend <- get_legend(combined_plot)

both <- grid.arrange(combined_plot + theme(legend.position="none"),
                     each_plot + theme(legend.position="none"),
                     nrow=1)

final <- grid.arrange(both, combined_legend, ncol=1, heights=c(0.9, 0.1))

ggsave(filename = "./red_progression.pdf",
       plot = final,
       device = "pdf",
       width = 15,
       height = 10,
       dpi = 300)
