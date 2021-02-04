"Mice iteration 2 takes 9.36329621473948 min"
"KNN iteration 2 takes 11.5834366520246 min"
"PCA iteration 2 takes 11.8936738967896 secs"
--
"Mice iteration 3 takes 14.3706520477931 min"
"KNN iteration 3 takes 11.8964193979899 min"
"PCA iteration 3 takes 12.9073429107666 secs"
--
"Mice iteration 4 takes 19.2369340658188 min"
"KNN iteration 4 takes 11.660524614652 min"
"PCA iteration 4 takes 16.1599969863892 secs"
--
"Mice iteration 5 takes 23.5015643358231 min"
"KNN iteration 5 takes 12.0548743327459 min "
"PCA iteration 5 takes 25.8708338737488 sec"
--
"Mice iteration 6 takes 27.8894051353137 mins"
"KNN iteration 6 takes 10.847679066658 mins"
"PCA iteration 6 takes 18.1653599739075 secs"
--
"Mice iteration 7 takes 30.5502681493759 mins"
"KNN iteration 7 takes 10.7397585352262 mins"
"PCA iteration 7 takes 30.8290278911591 sec"
--
"Mice iteration 8 takes 34.8877088665962 mins"
"KNN iteration 8 takes 10.728579334418 mins "
"PCA iteration 8 takes 32.4753880500793 secs"
--
"Mice iteration 9 takes 39.3392865180969 min"
"KNN iteration 9 takes 11.7248629490534 min"
"PCA iteration 9 takes 29.1194779872894 secs"
--
"Mice iteration 10 takes 50.5044936180115 min"
"KNN iteration 10 takes 12.793088499705 min"
"PCA iteration 10 takes 24.6836729049683 sec"



times <- NULL
times$mice <- c(9.36329621473948, 14.3706520477931, 19.2369340658188, 23.5015643358231, 27.8894051353137, 30.5502681493759, 34.8877088665962, 39.3392865180969, 50.5044936180115)
times$knn <- c(11.5834366520246, 11.8964193979899, 11.660524614652, 12.0548743327459, 10.847679066658, 10.7397585352262, 10.728579334418, 11.7248629490534, 12.793088499705)
times$pca <- c(11.8936738967896, 12.9073429107666, 16.1599969863892, 25.8708338737488, 18.1653599739075, 30.8290278911591, 32.4753880500793, 29.1194779872894, 24.6836729049683)
times$mice <- times$mice *60
times$knn <- times$knn*60
plot(times$mice)
plot(times$knn)
plot(times$pca)
times <- as.data.frame(times)
colors <- c("knn" = "red", "mice" = "blue", "pca" = "green")
ggplot(times, aes(x=(2:10))) +
  geom_line(aes(y=knn, color = "knn"), size = 1.15) +
  geom_line(aes(y=mice, color = "mice"), size = 1.15) +
  geom_line(aes(y=pca, color = "pca"), size = 1.15) +
  labs(y="Time in seconds", x= "Number of components") +
  theme(legend.position="right") +
  scale_color_manual(values = colors)
