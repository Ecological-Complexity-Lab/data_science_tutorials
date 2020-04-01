## @knitr load_libs
library(tidyverse)
library(bipartite)
## @knitr END

# This code is not within a chunk so it will not show
plot(1:10)

## @knitr plot_prices
ggplot(diamonds, aes(x=cut, y=price))+geom_boxplot()
## @knitr END