# plot for results from SimCorMultRes
library(ggplot2)
library(reshape2)
library(ggpubr)

results_qs = data.frame(
  method = rep(c("GEE", "RFboruta", "RFrecursive", "lassoGEE"), 9),

  k = rep(c(10,10,10,10, 20,20,20,20, 30,30,30,30), times = 3),
  rho = rep(c(0.2, 0.5, 0.8), each = 12),
  
  # method = rep(c("GEE", "RFboruta", "RFrecursive", "lassoGEE"), 6),
  # k = rep(c(10,10,10,10, 20,20,20,20, 30,30,30,30), times = 2),
  # rho = rep(c(0.2, 0.8), each = 12),
  
  mean_accuracy = c(
    0.88, 0.745, 0.715, 0.861,  #k=10,rho=0.2
    0.8655, 0.885, 0.7785, 0.9245,#k=20,rho=0.2
    0.851, 0.916, 0.8213333, 0.9326667,#k=30,rho=0.2
    
    0.86, 0.75, 0.736, 0.875,#k=10,rho=0.5
    0.876, 0.8705, 0.794, 0.9115, #k=20,rho=0.5
    0.8546667, 0.9123333, 0.823, 0.907, #k=30,rho=0.5
    
    0.889, 0.768, 0.713, 0.913, #k=10,rho=0.8
    0.884, 0.8745, 0.786, 0.894, #k=20,rho=0.8
    0.8903333, 0.9056667, 0.8043333, 0.8696667 #k=30,rho=0.8
  ),
  mean_recall = c(
    0.8904881, 0.9632997, 0.7252857, 0.9534762,  #k=10,rho=0.2
    0.7029325, 0.9548333, 0.632, 0.9089762,#k=20,rho=0.2
    0.5542556, 0.8906667, 0.5507446, 0.8194762, #k=30,rho=0.2
    
    0.8736587, 0.9776667, 0.763381, 0.9203704,#k=10,rho=0.5
    0.7392302, 0.9143333, 0.6404286, 0.8338492,#k=20,rho=0.5
    0.5660532, 0.8932143, 0.5575054, 0.7109722, #k=30,rho=0.5
    
    0.9262143, 0.9791246, 0.738, 0.8998452, #k=10,rho=0.8
    0.7558968, 0.9183333, 0.6324762, 0.7401566, #k=20,rho=0.8
    0.6622657, 0.8725, 0.5289459, 0.5962361 #k=30,rho=0.8
  ),
  mean_precision = c(
    0.886, 0.516, 0.788, 0.768,  #k=10,rho=0.2
    0.886, 0.58, 0.774, 0.79,#k=20,rho=0.2
    0.874, 0.58, 0.704, 0.802,#k=30,rho=0.2
    
    0.866, 0.518, 0.752, 0.834,#k=10,rho=0.5
    0.862, 0.548, 0.732, 0.848, #k=20,rho=0.5
    0.862, 0.562, 0.712, 0.838, #k=30,rho=0.5
    
    0.86, 0.552, 0.76, 0.948, #k=10,rho=0.8
    0.864, 0.556, 0.75, 0.96, #k=20,rho=0.8
    0.842, 0.528, 0.704, 0.924 #k=30,rho=0.8
  ),
  mean_fpr = c(
    0.126, 0.026, 0.358, 0.046,  #k=10,rho=0.2
    0.1413333, 0.01333333, 0.22, 0.03066667,#k=20,rho=0.2
    0.1536, 0.0168, 0.1552, 0.0412,#k=30,rho=0.2
    
    0.146, 0.018, 0.28, 0.084,#k=10,rho=0.5
    0.1193333, 0.022, 0.1853333, 0.06733333,#k=20,rho=0.5
    0.1468, 0.0176, 0.1548, 0.0792, #k=30,rho=0.5
    
    0.082, 0.016, 0.334, 0.122, #k=10,rho=0.8
    0.1093333, 0.01933333, 0.202, 0.128, #k=20,rho=0.8
    0.1, 0.0188, 0.1756, 0.1412 #k=30,rho=0.8
  )
)

finalre = read.csv("/Users/joycelin/Documents/UW/Course/STAT571/Project/output/simulation_glmm_part2_results_wobglmm.csv")
finalre$method = ifelse(finalre$method == "fdr", "Adjusted p-value by FDR",
                        ifelse(finalre$method == "holm", "Adjusted p-value by Holm's",
                               ifelse(finalre$method == "glmmAIC", "AIC forward on GLMM",
                                      finalre$method)))
resplot = rbind(finalre[,-c(1,2)], results_qs)
resplot[,-3] = apply(resplot[,-3], 2, as.numeric)

p1 <- ggplot(resplot,
       aes(x = k, y = mean_accuracy, group = factor(method), color = factor(method))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ rho, labeller = label_both) +
  labs(x = "k", y = "Mean Accuracy", color = "Method")

p2 <- ggplot(resplot,
       aes(x = k, y = mean_recall, group = factor(method), color = factor(method))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ rho, labeller = label_both) +
  labs(x = "k", y = "Mean Recall", color = "Method")

p3 <- ggplot(resplot,
       aes(x = k, y = mean_precision, group = factor(method), color = factor(method))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ rho, labeller = label_both) +
  labs(x = "k", y = "Mean Precision", color = "Method")

p4 <- ggplot(resplot,
             aes(x = k, y = mean_fpr, group = factor(method), color = factor(method))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ rho, labeller = label_both) +
  labs(x = "k", y = "Mean FPR", color = "Method")

ggarrange(p1, p2, p3, p4, ncol = 1, common.legend = TRUE, legend = "right")


resplot[, 4:7] = round(resplot[, 4:7], 3)
resplot = resplot[order(resplot$k, resplot$rho),]

write.csv(resplot, "/Users/joycelin/Documents/UW/Course/STAT571/Project/output/sim_res.csv", row.names = FALSE)


test = read.csv("/Users/joycelin/Documents/UW/Course/STAT571/Project/output/bglmm_time_res.csv")[, -1]

library(ggplot2)
ggplot(test, aes(x=p, y=x/60)) +
  geom_line() +
  geom_point() +
  labs(x = "number of covariates in the mdoel", y = "minutes take to fit a model")


# ggplot(results, aes(x = k, y = mean_accuracy, color = method, linetype = method, group = interaction(method, rho))) +
#   geom_line() +
#   geom_point() +
#   geom_text(data = subset(results, k == 30), aes(label = rho, x = k, y = mean_accuracy), hjust = -0.1, vjust = -0.5, size = 1.6, color = "black") +
#   labs(x = "k", y = "Mean Accuracy", color = "Method", linetype = "Method", title = "Mean Accuracy vs. k by Method and rho") +
#   theme_bw() +
#   theme(legend.position = "right")
# 
# 
# ggplot(results, aes(x = k, y = mean_recall, color = method, linetype = method, group = interaction(method, rho))) +
#   geom_line() +
#   geom_point() +
#   geom_text(data = subset(results, k == 10), aes(label = rho, x = k, y = mean_recall), hjust = -0.1, vjust = -0.3, size = 1.6, color = "black") +
#   labs(x = "k", y = "Mean Recall", color = "Method", linetype = "Method", title = "Mean Recall vs. k by Method and rho") +
#   theme_bw() +
#   theme(legend.position = "right")
# 
# 
# ggplot(results, aes(x = k, y = mean_precision, color = method, linetype = method, group = interaction(method, rho))) +
#   geom_line() +
#   geom_point() +
#   geom_text(data = subset(results, k == 10), aes(label = rho, x = k, y = mean_precision), hjust = -0.1, vjust = -0.5, size = 1.6, color = "black") +
#   labs(x = "k", y = "Mean Precision", color = "Method", linetype = "Method", title = "Mean Precision vs. k by Method and rho") +
#   theme_bw() +
#   theme(legend.position = "right")
# 
# 
# ggplot(results, aes(x = k, y = mean_fpr, color = method, linetype = method, group = interaction(method, rho))) +
#   geom_line() +
#   geom_point() +
#   geom_text(data = subset(results, k == 20), aes(label = rho, x = k, y = mean_fpr), hjust = -0.1, vjust = -0.5, size = 1.6, color = "black") +
#   labs(x = "k", y = "Mean FPR", color = "Method", linetype = "Method", title = "Mean FPR vs. k by Method and rho") +
#   theme_bw() +
#   theme(legend.position = "right")