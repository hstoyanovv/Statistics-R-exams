#zad1
f = function(x)
{
  res = 0;
  
  for(i in 1:x)
  {
    res = res + 100*i^3/(9 + 4*(x^4));
  }
  
  return(res);
}

f1 = function(vec)
{
  res = rep(0,6);
  
  for(i in 1:length(vec))
  {
    res[i] = f(vec[i]);
  }
  
  return(res);
}

#zad2
#a
i = order(survey$Height, decreasing = T)
highest.students = survey[i[1:8],1]

#b
sum(survey$Sex == 'Female' & survey$Pulse > 75, na.rm = T)

#v
mens.height = survey$Height[survey$Sex == 'Male']
mens.height = mens.height[!is.na(mens.height)] # izbirame samo tiq koito imat visochina mahame na
shapiro.test(mens.height) # => normalno razpredeleni sa i polzvame t.test
t.test(mens.height,conf.level = 0.97);
#=> 177.0353 180.6167 - tvurdime che 97% sredno ot horata sa v tozi interval.

#g
women.pulse = survey$Pulse[survey$Sex == 'Female'];
women.pulse = women.pulse[!is.na(women.pulse)];

men.pulse = survey$Pulse[survey$Sex == 'Male'];
men.pulse = men.pulse[!is.na(men.pulse)];

shapiro.test(men.pulse);
shapiro.test(women.pulse);# => normalno razpredeleni sa moje da polzvame t.test
t.test(women.pulse,men.pulse,alternative = "greater"); # = >pvalue = 12% priemame hipotezata che sa ednakvi othvurlqme tvurdenieto

#d
l = lm(survey$Pulse ~ survey$Exer);
summary(l) # dava ni malko p -value 3% toest imat lineina zavisimost po mejdu si

#zad3
rain = c(8.2, 6.4, 12.8, 7.6, 13.8, 16.1, 12.4, 12.8, 14.3, 13.5, 8.4, 7.3);
production = c(17.2,17.6,19.6,19.8,20.0,23.5,20.2,20.2,22.1,21.2,18.9,17.1);
l = lm(production ~ rain);
summary(l);
d = data.frame(rain = c(15));
predict.lm(l,d,interval = "confidence", conf.level = 0.97);
#fit mi dava ochakvanoto za 15 sm/m2 koeto e 21.75151, a doveritelniq interval za 97% e lwr i upr.
#drug variant za ochakvanoto   15*s$coefficients[2,1] + s$coefficients[1,1]

#zad4
#h0 : prepodavat ednakvo, nezavisimi sa noviq stil ne vliqe
#h1 : razlichno vliqe
m = matrix(c(28, 42, 15, 23, 35, 17),nrow = 2, byrow = T);
chisq.test(m);
#priemame hipotezata , che prepodavat ednakvo zashtoto p-value e 73%

#zad5
data = read.csv("E:/Data1.txt", header = FALSE); # header e false shtoto imah nqkuv X pred purvto chislo
x = data$V1;
shapiro.test(x); # p-valueto e 81% => normalno razpredelni sa
y = cut(x,breaks = c(0,2.5,5,7.5,10)); # razdelqme stoinostite po intervali da vidim dali sa ravnomerno rapredeleni v int [0,10]
m = table(y); # pravq tablica sus suotvetstiq kolko ot chislata v koi interval vlizat.
chisq.test(m) # p-valueto mnogo malko => othvurlqme hipotezata da sa ravnomerno razpredeleni v tozi interval.