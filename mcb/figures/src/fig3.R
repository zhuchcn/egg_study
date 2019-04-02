setwd(dirname(parent.frame(n = 2)$ofile))
source("global.R")
load("../data/data.rda")

# Figure 3
# Box plot of choline, betaine, and TMAO values before and after egg and white
p1 = my_boxplot(data$bga$data, "Choline", data$bga$lm, show_legend = FALSE)
p2 = my_boxplot(data$bga$data, "Betaine", data$bga$lm, show_legend = FALSE)
p3 = my_boxplot(data$bga$data, "TMAO", data$bga$lm, show_legend = FALSE)

df = data.frame(
    x = as.numeric(data$bga$data$conc_table["Choline", ]),
    y = as.numeric(data$bga$data$conc_table["TMAO", ]),
    subject = data$bga$data$sample_table$Subject
)

cor = cor.test(df$x, df$y)

p4 = ggscatterplot(
    df,
    x = "x",
    y = "y",
    color = "subject",
    color.pal = pal_npg()(10)
)+
    annotate(geom = "text", x = 130000, y = 12000, size = text.size, 
             label = "R = " %+% round(cor$estimate, 3) %+% "\nP = " %+% round(cor$p.value, 3))+
    labs(x = "Choline", y = "TMAO") +
    theme_scatter()+
    theme(legend.position = "none")

c(p1, p2, p3) %<-% align_plots(p1, p2, p3, align = "hv", axis = "tblr")

p = plot_grid(p1, p2, p3, p4, labels = c("A", "B", "C", "D"), nrow = 2,
          label_size = title.size)

ggsave("../png/fig3.png", p, width = 18, height = 16, units = "in", dpi = 300)

