
### Boxplot AnDis Rmd

tiff(filename = "output/Figures/BoxPlots.tiff", res = 400, units = "cm",
     compression = "lzw", width = 15, height = 15)
AlldataSetQuant3 %>% filter(Trait != "ComprFilotaxia") %>%
  ggplot(mapping = aes(y = Value2, x = Method)) + theme_bw() +
  geom_boxplot(fill = "green") + facet_wrap(~Trait, scales = "free_y", ncol = 4) + ylab(NULL) +
  theme(axis.text.x = element_text(angle = 320, hjust = 0.1,
                                   colour = c("blue", "red", rep("green", 2), "black")))
dev.off()


#### Barplots AndDis Rmd

tiff(filename = "output/Figures/BarPlots.tiff", res = 400, units = "cm",
     compression = "lzw", width = 15, height = 25)
AlldataSetQuali3 %>% filter(Trait != "AnguloRamif", !is.na(Score)) %>%
  ggplot(aes(y = N, x = Method, fill = Score)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Trait, ncol = 4, scales = "fixed") + ylab(NULL) +
  scale_fill_viridis_d() + theme_bw() +
  theme(axis.text.x = element_text(angle = 320, hjust = 0.1,
                                   colour = c("blue", "red", rep("green", 2), "black")),
        legend.position = "bottom")
dev.off()



tiff(filename = "output/Figures/BarPlotHabCaule.tiff", res = 400, units = "cm",
     compression = "lzw", width = 15, height = 10)
AlldataSetQuali3 %>% filter(Trait == "AnguloRamif", !is.na(Score)) %>%
  ggplot(aes(y = N, x = Method, fill = Score)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~Trait, ncol = 4, scales = "fixed") + ylab(NULL) +
  scale_fill_viridis_d(guide = guide_legend(ncol = 2)) + theme_bw() +
  theme(axis.text.x = element_text(angle = 320, hjust = 0.1,
                                   colour = c("blue", "red", rep("green", 2), "black")))
dev.off()


### Kappa Correlogram DivGen Rmd
tiff(filename = "output/Figures/KappaGraph.tiff", res = 400, units = "cm",
     compression = "lzw", width = 12, height = 12)
corrplot::corrplot(Kappa,cl.pos = "n", tl.col = c(rep("red", times = 3),
                                                  rep("green", times = 9))) |>
  corrplot::corrRect(c(1, 4, 8, 9, 10, 12)) |>
  corrplot::corrRect(namesMat = r)
dev.off()


### Venn Diagrams DivGen Rmd
tiff(filename = "output/Figures/VennDiagram.tiff", res = 400, units = "cm",
     compression = "lzw", width = 20, height = 25)
ggarrange(VD1, VD3, VD2, ncol = 1, labels = "AUTO", heights = c(2,1.3,2))
dev.off()


### PCA accumulated variances
tiff(filename = "output/Figures/PCAVar.tiff", res = 400, units = "cm",
     compression = "lzw", width = 10, height = 8)
barplot(PercAc[1:15], main = "Variance explained by PCA",
        ylab = "Cumulative variance (%)", xlab = "Number of retained PCs",
        col = c("gray"), ylim = c(0, 100))
dev.off()


#### PCA Scatter plot PC1, 2, 3, and 4
tiff(filename = "output/Figures/PCAScatterPlot.tiff", res = 400, units = "cm",
     compression = "lzw", width = 20, height = 16)
ggarrange(PC12, PC34, common.legend = T, legend = "right", ncol = 1, labels = "AUTO")
dev.off()
