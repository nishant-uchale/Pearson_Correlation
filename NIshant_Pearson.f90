
program pearson
implicit none
integer::i,day(336),time(336)
real::r_whole,r_night,r_day,atemp(336),temp(336),rh(336),rh1(336)
open(100,file='ARTH_30m.dat')
open(200,file='output.dat')
do i=1,336
read(100,*)day(i),time(i),atemp(i),rh(i)
enddo
call correlation(atemp,rh,r_whole)
write(200,*)"Value of r for whole data set=",r_whole
do i=1,336
if(time(i).ge.2000.or.time(i).le.500) then
temp(i)=atemp(i)
rh1(i)=rh(i)
else 
 atemp(i)=-9999.0
 rh(i)=-9999.0
endif
end do
call correlation(temp,rh1,r_night)
write(200,*)"Value of r for night time=",r_night
do i=1,336
if(time(i).ge.900.and.time(i).le.1600) then
temp(i)=atemp(i)
rh1(i)=rh(i)
else
 atemp(i)=-9999.0
 rh(i)=-9999.0
endif
end do
call correlation(temp,rh1,r_day)
write(200,*)"Value of r for day time=",r_day

contains
subroutine correlation(temp,rh1,r)
implicit none
integer::i,count
real::temp(336),rh1(336),x_bar,y_bar,sumx,sumy,xy,num,s1,sum_s1,s2,sum_s2,denom,r
count=0;x_bar=0;y_bar=0;sumx=0;sumy=0;xy=0;s1=0;sum_s1=0;s2=0;sum_s2=0

do i=1,336
if (temp(i).ne.-9999.0 .and.rh1(i).ne.-9999.0)then

sumx=sumx+temp(i)
sumy=sumy+rh1(i)
count=count+1

end if
end do

x_bar=sumx/count
y_bar=sumy/count

num=0
do i=1,336
if (temp(i).ne.-9999.0.and.rh1(i).ne.-9999.0)then
num=num+(temp(i)-x_bar)*(rh1(i)-y_bar)
sum_s1=sum_s1+(temp(i)-x_bar)**2
sum_s2=sum_s2+(rh1(i)-y_bar)**2
end if
end do
denom=sqrt(sum_s1)*sqrt(sum_s2)
r=num/denom
end subroutine correlation
end program pearson





