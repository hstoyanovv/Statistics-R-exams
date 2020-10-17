#izpita s Aids2

#1a
sum(Aids$age < 20);

#1b
i = order(Aids2$diag)
Aids2[i[1:5],]

#1v
men = Aids2[Aids2$sex == 'M',];
men.blood = sum(men$T.categ == 'blood');
p = men.blood/length(men[,1]); # 0.02069717

#1g
barplot(prop.table(table(status,state), 2), legend = T);

#zad2
prop.test(c(p[2,1],p[2,2]), c(sum(Aids2$sex == 'F'), sum(Aids2$sex == 'M')), alternative = "less");
#poneje e proporciq i vzimam murtvite jeni i muje i gi supostqvam s obshtiq broi i alternativa

#zad3
x = Aids2[Aids2$status == 'D',];
y = (x$death - x$diag)/365 + x$age
# datata na koqto sa umreli - datata na koqto sa diagnosticirani /365 + godinite kogato sa diagnostirirani.
wilcox.test(y,mu = 38);
#p-value = 27% moje da priemem truvdenieto.


#zad4
x = rchisq(100,10);
hist(x, probability = T);
curve(dchisq(x,10), add = T);

#zad5
#kak zavisi tegloto na surceto ot obshtoto teglo
men.cats = cats[cats$Sex == 'M',];
x= men.cats$Bwt;
y = men.cats$Hwt;
l = lm(y ~ x);
plot(x,y);
abline(l);
s = summary(l);
#p.value e < 2,2e-16, malko e toest ima vruzka , a i Multiple R-squared e 62%.

#vqrno li e che pri kotki po-tejki s 1kg , surceto e po - tejko s 5gr
#h0 : beta1 = 5
#h1 : beta1 != 5
beta1 = s$coefficients[2,1]
se = s$coefficients[2,2]
t = (beta1 - 5)/se
p = 2*pt(t,df = 95)
#p = 0.04.5 => h1 othvurlqme hipotezata;

#Postroite 95% doveritelen interval za surceto na kotka s teglo 2.6kg
d = data.frame(x = 2.6);
predict.lm(l,d,interval = "confidence",conf.level = 0.95);

#zad6
data = read.csv("E:/Data1.txt", header = FALSE); # header e false shtoto imah nqkuv X pred purvto chislo
x = data$V1;
shapiro.test(x); # p-valueto e 81% => normalno razpredelni sa
y = cut(x,breaks = c(0,2.5,5,7.5,10)); # razdelqme stoinostite po intervali da vidim dali sa ravnomerno rapredeleni v int [0,10]
m = table(y); # pravq tablica sus suotvetstiq kolko ot chislata v koi interval vlizat.
chisq.test(m) # p-valueto mnogo malko => othvurlqme hipotezata da sa ravnomerno razpredeleni v tozi interval.




