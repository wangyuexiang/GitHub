args <- commandArgs(trailingOnly = TRUE)

d1 <- as.Date(args[1])
d2 <- as.Date(args[2])

print("d1 DOW: " )
print(as.POSIXlt(d1)$wday)
