library(BDSA)

##Muhammad BUdhi Salmanjannah
##5025201084
##Probstat B

##no. 1

#inisialisasi data atau variable
selisih = c(22, 20, 3, 13, 20, 18, 11, 16, 23)

#a
sdeviasi = sd(selisih)
cat("Standar Deviasi : ")
sdeviasi

#b
#variable yang dibutuhkan
avg = mean(selisih)
miu = 0
n = 9
student_t_test = ((avg - miu) / (sdeviasi / sqrt(n)))

pvalue = 2*pt(student_t*1, df=n-1, lower.tail = FALSE)
cat("nilai t(p value) : ")
pvalue

#c
#uji 2 populasi karena ada kadar saturasi O2 sebelum dan sesudah
oksigen_sebelum = c(78, 75, 67,77, 70, 72, 78, 74, 77)
oksigen_sesudah = c(100, 95, 70, 90, 90, 90, 89, 90, 100)

t.test(x=oksigen_sebelum, y=oksigen_sesudah,
       alternative = "two.sided",
       paired = FALSE, var.equal = TRUE,
       conf.level = 0.95)
#dari hasil yang didapatkan, dapat dilihat bahwa tidak ada pengaruh, peluang 95% intervalnya hasilnya minus
#-23.035747
#-9.408698
