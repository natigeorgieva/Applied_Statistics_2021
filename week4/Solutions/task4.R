data(waves,package = "HSAUR")
??waves
str(waves)
edit(waves)
t.test(waves$method1,waves$method2,paired = TRUE)
ks.test(waves$method1,waves$method2)
cor.test(~method1 + method2, data = waves)
