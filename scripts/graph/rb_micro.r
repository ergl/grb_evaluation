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

          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_text(size=7),
          legend.text = element_text(size=6),
          legend.box.background = element_rect(color="black", fill="white"))


df_updates_ratio <- read.csv("../../microbenchmarks/update_ratio_results.csv")
df_strong_ratio <- read.csv("../../microbenchmarks/strong_ratio_results.csv")

updates_ratio <- ggplot(df_updates_ratio,
                     aes(x=factor(update_p),
                         y=throughput,
                         fill=factor(keys_read))) +

    geom_bar(stat='identity', position="dodge") +
    scale_y_continuous(breaks=seq(0,600000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    scale_fill_manual(name="Keys read/updated",
                      labels=c(`1` = "1", `3` = "3"),
                      values=c(`1` = "#F2818F", `3` = "#1C5BD0")) +
    coord_cartesian(ylim=c(0,600000)) +
    labs(title="Redblue, 3DCs (20ms RTT), 100% blue",
         x="% of Update transactions",
         y="Max. Throughput (Ktps)") +
    plot_theme

strong_ratio <- ggplot(df_strong_ratio[df_strong_ratio$clusters == "all", ],
                     aes(x=factor(blue),
                         y=throughput,
                         fill=factor(keys_read))) +

    geom_bar(stat='identity', position="dodge") +
    scale_y_continuous(breaks=seq(0, 400000, by=20000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    scale_fill_manual(name="Keys read/updated",
                      labels=c(`1` = "1", `3` = "3"),
                      values=c(`1` = "#F2818F", `3` = "#1C5BD0")) +
    coord_cartesian(ylim=c(0,400000)) +
    labs(title="Redblue, 3DCs (20ms RTT), 20% updates",
         x="% of blue transactions",
         y="Max. Throughput (Ktps)") +
    plot_theme

ggsave(filename = "./rb_micro_update_ratio.pdf",
       device="pdf",
       plot=updates_ratio,
       width=5,
       height=5,
       dpi=300)

ggsave(filename = "./rb_micro_strong_ratio.pdf",
       device="pdf",
       plot=strong_ratio,
       width=5,
       height=5,
       dpi=300)
