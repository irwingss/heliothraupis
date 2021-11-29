library(tidyverse)
data("airquality")
names(airquality)
lm(Temp ~ Ozone+I(Ozone^2), data=airquality) |> summary()
plot(Temp ~ Ozone, data=airquality)


library(tidyverse)
library(sjPlot)
cuadratico <- lm(Temp ~ Ozone+I(Ozone^2), data=airquality) 
plot_model(cuadratico, type = "pred", show.data = T)$Ozone+
  theme_bw()+
  labs(title="", x="Ozono", y="Temperatura Promedio Anual")

install.packages("cranlogs")
cranlogs::cran_downloads(packages = "HistData")

63.8614538 + 0.4896669*50 + -0.0023198*50^2
predict(cuadratico, data.frame(Ozone=100))


# Grafico con ggeffects::ggpredict
df2 <- ggeffects::ggpredict(cuadratico, terms = c("Ozone"))
df2

png("figs/graficación.png", width = 18, height = 12, units = "cm", res=600)
ggplot(data=df2, aes(x, predicted)) + 
  scale_x_continuous(expand=c(0,0))+
  coord_cartesian(clip = "off")+
  geom_point(data=airquality, aes(x=Ozone, y=Temp), color="#5f00db", alpha=0.5, size=3)+
  geom_line(color="#15b2db", lwd=1) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill="#15b2db", alpha=0.1) +
  labs(y="Temperatura promedio anual (°C)", x="Ozone (ppm)")+
  theme_minimal()
dev.off()


# install.packages("GGally")
library(GGally)
p <- ggpairs(iris, aes(color = Species), upper = list(continuous = wrap("cor", size = 2)))+ theme_bw()
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_size_manual(values = 0.1) +
      scale_fill_manual(values=c("#15b2db","#5f00db","#f72585")) +
      scale_color_manual(values=c("#15b2db","#5f00db","#f72585"))  
  }
}
p


library(ggridges)
data("iris")
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)+
  theme_classic2()



library(ggradar)
library(tidyverse)
library(scales)

mtcars_radar <- mtcars %>% 
  as_tibble(rownames = "group") %>% 
  mutate_at(vars(-group), rescale) %>% 
  tail(4) %>% 
  select(1:10)

ggradar(mtcars_radar)+theme(legend.position = "none")+
  scale_color_manual(values = c("#15b2db","#17004a","#5f00db","#b5179e","#f72585"))


data("iris")
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_gradient2(low="#f72585", mid="#15b2db", high = "#5f00db", guide = "colourbar",
                        name = "Tail probability")+
  theme_classic2()



#ECDF
library(tidyverse)
iris %>% filter(Species=="versicolor") %>% 
ggplot(aes(Petal.Length)) + stat_ecdf(geom = "step")

