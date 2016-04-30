library(ggplot2)


exam <- read.delim("Exam Anxiety.dat", header=T)
str(exam)

myColors <- c("black", "grey")

plot <- ggplot(exam, aes(Anxiety, Exam, colour=Gender, fill=Gender))
plot +  theme_bw() + geom_point() + stat_smooth(method="lm") + 
  scale_colour_manual(values = myColors) + 
  scale_fill_manual(values = myColors)
