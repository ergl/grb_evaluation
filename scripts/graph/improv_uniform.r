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

# legend_title <- "Replication"
# legend_labels <- c("Basic Replication",
#                    "Basic Replication (projection)",
#                    "Uniform Replication",
#                    "Uniform Replication (projection)",
#                    "Uniform Replication (improved)",
#                    "Uniform Replication (improved, projection)",)

# legend_breaks <- c("pvc", "pvc_linear", "socket", "socket_linear", "shackle_linear")
# legend_values <- c("red", "blue", "green")

# legend_params <- scale_colour_manual(name=legend_title,
#                                      breaks=legend_breaks,
#                                      labels=legend_labels,
#                                      values=legend_values)

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

df <- read.csv("../../validate_improved_replication/general.csv")

df_replicas <- df[df$exp == "replicas", ]
df_replicas$transport <- factor(
    df_replicas$transport,
    levels=c(levels(df_replicas$transport), "shackle_linear", "socket_linear", "pvc_linear")
)

df_shackle_base <- subset(df_replicas, transport == "shackle" & replicas == 1)$throughput
shackle_a <- df_replicas[1, ]
shackle_a$transport <- "shackle_linear"
shackle_a$throughput <- df_shackle_base
shackle_b <- df_replicas[2, ]
shackle_b$transport <- "shackle_linear"
shackle_b$throughput <- df_shackle_base * 2
shackle_c <- df_replicas[3, ]
shackle_c$transport <- "shackle_linear"
shackle_c$throughput <- df_shackle_base * 3
df_replicas[nrow(df_replicas) + 1, ] <- shackle_a
df_replicas[nrow(df_replicas) + 1, ] <- shackle_b
df_replicas[nrow(df_replicas) + 1, ] <- shackle_c

df_socket_base <- subset(df_replicas, transport == "socket" & replicas == 1)$throughput
shackle_a <- df_replicas[4, ]
shackle_a$transport <- "socket_linear"
shackle_a$throughput <- df_socket_base
shackle_b <- df_replicas[5, ]
shackle_b$transport <- "socket_linear"
shackle_b$throughput <- df_socket_base * 2
shackle_c <- df_replicas[6, ]
shackle_c$transport <- "socket_linear"
shackle_c$throughput <- df_socket_base * 3
df_replicas[nrow(df_replicas) + 1, ] <- shackle_a
df_replicas[nrow(df_replicas) + 1, ] <- shackle_b
df_replicas[nrow(df_replicas) + 1, ] <- shackle_c

df_pvc_base <- subset(df_replicas, transport == "pvc" & replicas == 1)$throughput
shackle_a <- df_replicas[7, ]
shackle_a$transport <- "pvc_linear"
shackle_a$throughput <- df_pvc_base
shackle_b <- df_replicas[8, ]
shackle_b$transport <- "pvc_linear"
shackle_b$throughput <- df_pvc_base * 2
shackle_c <- df_replicas[9, ]
shackle_c$transport <- "pvc_linear"
shackle_c$throughput <- df_pvc_base * 3
df_replicas[nrow(df_replicas) + 1, ] <- shackle_a
df_replicas[nrow(df_replicas) + 1, ] <- shackle_b
df_replicas[nrow(df_replicas) + 1, ] <- shackle_c

replica_plot <- ggplot(df_replicas[df_replicas$transport %in% c("shackle", "socket", "pvc"), ],
                      aes(x=replicas, y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("shackle", "socket", "pvc"),
        labels=c("New Uniform Repl.", "Old Uniform Repl.", "Basic Repl."),
        values=c("red", "blue", "orange")
    ) +
    labs(title="Replication Comparison", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot1 <- ggplot(df_replicas[df_replicas$transport %in% c("shackle", "shackle_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("shackle", "shackle_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="New Uniform Replication", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot2 <- ggplot(df_replicas[df_replicas$transport %in% c("socket", "socket_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("socket", "socket_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="Old Uniform Replication", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

replica_plot3 <- ggplot(df_replicas[df_replicas$transport %in% c("pvc", "pvc_linear"), ],
                       aes(x=factor(replicas), y=throughput, group=transport, color=transport)) +

    geom_point(size=1.5) +
    geom_line() +
    scale_y_continuous(breaks=seq(0, 1000000, by=25000),
                       labels=format_thousand_comma,
                       expand=c(0,0)) +
    coord_cartesian(ylim=c(0,400000)) +
    scale_colour_manual(
        name="",
        breaks=c("pvc", "pvc_linear"),
        labels=c("Experimental", "Expected"),
        values=c("red", "blue")
    ) +
    labs(title="Basic Replication", x = "Replicas", y = "Throughput (Ktps)") +
    plot_theme

combined <- grid.arrange(grid.arrange(replica_plot,
                                      replica_plot1, nrow=1),
                         grid.arrange(replica_plot2,
                                      replica_plot3, nrow=1),
                         ncol=1)

ggsave(filename = "./uniform_bench_improved.pdf",
       plot = combined,
       device = "pdf",
       width = 10,
       height = 10,
       dpi = 300)
