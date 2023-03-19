library(readxl)
yf_log <- read_excel("C:/Users/david/OneDrive/Yanquiel-Fernandez-log.xlsx")

lg_k <- .257
pad <- 48

yf_padk <- (yf_log$Roll_K + pad*lg_k) / (yf_log$Roll_PA + pad)

yf2 <- data.frame(PA = rep(yf_log$Roll_PA,2),
                  K_Per = c(yf_log$Roll_Kper, yf_padk),
                  type = c(rep("Actual",length(yf_padk)),
                           rep("Padded", length(yf_padk))))

library(ggplot2)

ggplot(yf2, aes(x = PA, y = K_Per, group = type, color = type)) +
  geom_line() +
  geom_vline(xintercept = 48, linetype = 2) +
  labs(title = "Yanquiel Fernandez K% vs Padded K%", x = "PA", y = "K_per", color = "Type") +
  scale_color_manual(values = c("Actual" = "blue", "Padded" = "red"))
