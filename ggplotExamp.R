
load(ggplot2)
df <-read.csv("C:\\Users\\gerardo\\Documents\\Lab_documents\\bact_odor\\iproh.txt")

ggplot(df, aes(x = RTime, y = Intensity))   +     geom_point() + geom_point(data = df, aes(y = Intensity),   colour = "blue", size = 1)

ggplot(df, aes(x = RTime, y = Intensity)) + geom_line(data = df, colour="blue") + scale_y_continuous(limits = c(-300, 3500)) + scale_x_continuous(limits = c(0, 25))
