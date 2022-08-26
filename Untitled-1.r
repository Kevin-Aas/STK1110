A = 0
for (i in 1:10000) {
    u1=runif(1); u2=runif(1)
    if(u1<.6 && u2<.7){
        A=A+1
    }
}
print(A)