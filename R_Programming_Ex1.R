library(Ryacas)
#Function Limit
#No 1
Limit((1-cos(x))/x,"x",0) 

#No 2
h <- Sym("h")
Limit(((2*((-3+h)^2))-18)/h,h,0)

#No 3
t<- Sym("t")
Limit((t-(sqrt(3*t+4)))/(4-t),t,4)

#Differentiation
#Code untuk soal nomor 1
f = expression((sqrt(x)) * (x+1))
D(f,'x')

#Code untuk soal nomor 2
f = expression(((2*x^2)-3)/(sqrt(x)))
D(f,'x')

#Code untuk soal nomor 3
f = expression((x-1)/(x+1))
D(f,'x')

#Code untuk soal nomor 4
#A
f = expression((sqrt(x^3)) * ((x^3)+1))
D(f,'x')

#B
f = expression(((2*x^2)-3)/(sqrt(x^2)))
D(f,'x')

#C
f = expression((2*x^2-1)/(2*x^2+1))
D(f,'x')


#Integration
#No 1
x<-Sym("x")
Integrate((2*x^3),x,0,3)

#No 2
x<-Sym("x")
Integrate((1-(5*x^4)),x,-1,2)

#No 3
x<-Sym("x")
Integrate((x^4)-(3*x^2)+5,x,-2,2)

#No 4
x<-Sym("x")
Integrate((x^2)+(1/((2*x^1)/2)),x,1,4)


#No 5
x<-Sym("x")
Integrate(x*(2-3*x)^2,x,0,2)
