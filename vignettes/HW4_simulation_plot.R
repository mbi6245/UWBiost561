library(tidyverse)
load("HW4_simulation.Rdata")
res = data.frame(
  names = c("alpha1_success", "alpha1_failure", "alpha1_incomplete", "alpha2_success", "alpha2_failure", "alpha2_incomplete"),
  count = c(length(which(res[, 1] == 1)), length(which(res[, 1] == 0)), sum(is.na(res[, 1])), length(which(res[, 2] == 1)), length(which(res[, 2] == 0)), sum(is.na(res[, 2])))
)
plot_res = ggplot(data = res, mapping = aes(x = names, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "barplots of successful, unsuccessful, and incomplete runs \n for alpha = 0.5 and 0.9")

png("HW4_simulation.png")
print(plot_res)
dev.off()
