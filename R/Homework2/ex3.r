#Ho: mean(before) = mean (after)
#H1: mean(before) > mean (after)

#NormRazpredeleni

data = read.csv("/home/petko/Workspace/FMI-Semester-5/R/Homework2/mouse.txt", header = TRUE, sep = ";", dec = ".")
before = data[,2]
after = data[,3]
t.test(before,after,paired=T,alternative = "greater")
#p-value = 0.0372 < 0.05 -> othv. hip i mozhem da priemem alternativata