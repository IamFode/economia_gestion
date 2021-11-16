# función de estimación de la MCO

def beta1_0(x,y):
    sumnum, sumden = 0,0
    mediax, mediay = sum(x)/len(x), sum(y)/len(y)
    for i in range(len(x)):
        sumnum += (x[i] - mediax) * (y[i] - mediay)
        sumden += (x[i]-mediax)**2
    beta1 = sumnum / sumden
    beta0 = mediay - beta1*mediax
    return "beta 1: {} \nbeta 0: {}".format(beta1,beta0) 

x = list(map(float,input("Introducir vector x: ").strip().split()))
y = list(map(float,input("Introducir vector y: ").strip().split()))

print(beta1_0(x,y))
