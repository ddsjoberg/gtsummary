subroutine lo0(x,y,w,n,d,p,nvmax,span,degree,match,nef,dof,s,var,
		beta,iv,liv,lv,v,iwork,work)
integer n,d,p,nvmax,degree,match(*),nef,liv,lv,iv(liv),iwork(*)
double precision x(n,d),y(n),w(n),span,dof,s(n),var(n),v(lv),work(*)
double precision beta(p+1)
#work should be nef*(p+d+8) + 2*p + n +8 
integer qrank
	call lo1(x,y,w,n,d,p,nvmax,span,degree,match,nef,0,dof,s,var,beta,
#	xin,win,sqwin,sqwini,
	work(1),work(nef*d+1),work(nef*(d+1)+2),work(nef*(d+2)+2),
#	xqr,qrank,qpivot,qraux,	
	work(nef*(d+3)+2),qrank,iwork(1),work(nef*(p+d+4)+3+p),
	iv,liv,lv,v,	
	work(nef*(p+d+4)+4+2*p) )
return
end
	
subroutine lo1(x,y,w,n,d,p,nvmax,span,degree,match,nef,nit,dof,s,var,beta,
	xin,win,sqwin,sqwini,xqr,qrank,qpivot,qraux,
	iv,liv,lv,v,	
	work)
integer n,d,p,nvmax,degree,match(*),nef,nit,qrank,qpivot(p+1)
integer iv(liv),liv,lv
double precision x(n,d),y(n),w(n),span,dof,s(n),var(n),beta(p+1),
	xin(nef,d),win(nef+1),sqwin(nef),sqwini(nef),xqr(nef,p+1),
	qraux(p+1),v(lv),
	work(*)
#work should have size n +4*(nef+1)
call lo2(x,y,w,n,d,p,nvmax,span,degree,match,nef,nit,dof,s,var,beta,
	xin,win,sqwin,sqwini,xqr,qrank,qpivot,qraux,
	iv,liv,lv,v,	
	work(1),work(nef+2),work(2*nef+3),work(3*nef+4))
return
end



subroutine lo2(x,y,w,n,d,p,nvmax,span,degree,match,nef,nit,dof,s,var,beta,
	xin,win,sqwin,sqwini,xqr,qrank,qpivot,qraux,
	iv,liv,lv,v,	
	levout,sout,yin,work)
integer n,d,p,nvmax,degree,match(*),nef,nit,qrank,qpivot(p+1)
integer iv(liv),liv,lv
double precision x(n,d),y(n),w(n),span,dof,s(n),var(n),beta(p+1),
	xin(nef,d),win(nef+1),sqwin(nef),sqwini(nef),xqr(nef,p+1),
	qraux(p+1),v(lv),
	levout(nef+1), sout(nef+1),yin(nef+1),work(*)
#work should be length n
double precision junk, onedm7
integer job, info
logical setLf, ifvar
job=110;info=1
ifvar=.true.
onedm7=1d-7
if(nit<=1){
	call pck(n,nef,match,w,win)
	do i=1,nef{
		if(win(i)>0d0){
			sqwin(i)=dsqrt(win(i))
			sqwini(i)=1d0/sqwin(i)
		}
		else{
			sqwin(i)=1d-5
			sqwini(i)=1d5
			}
		}
	do i=1,n{
		k=match(i)
		if(k<=nef){
			do j=1,d
				xin(k,j)=x(i,j)
			for(j=d+1;j<=p;j=j+1)
				xqr(k,j+1)=x(i,j)
			
			}
		}
	do i=1,nef{
		xqr(i,1)=sqwin(i)
		do j=1,d
			xqr(i,j+1)=xin(i,j)*sqwin(i)
		for(j=d+2;j<=p+1;j=j+1)
				xqr(i,j)=xqr(i,j)*sqwin(i)
		}
	for(j=1;j<=p+1;j=j+1)
		qpivot(j)=j
	call dqrdca(xqr,nef,nef,p+1,qraux,qpivot,work,qrank,onedm7)
	setLf = (nit==1)
	call lowesd(106,iv,liv,lv,v,d,nef,span,degree,nvmax,setLf)
	v(2)=span/5d0
	}
do i=1,n
	work(i)=y(i)*w(i)
call pck(n,nef,match,work,yin)
	do i=1,nef
		yin(i)=yin(i)*sqwini(i)*sqwini(i)


if(nit<=1)call lowesb(xin,yin,win,levout,ifvar,iv,liv,lv,v)
	else call lowesr(yin,iv,liv,lv,v)
call lowese(iv,liv,lv,v,nef,xin,sout)

#now remove the parametric piece
do i=1,nef
	sout(i)=sout(i)*sqwin(i)
call dqrsl(xqr,nef,nef,qrank,qraux,sout,work(1),work(1),beta,
		sout,work(1),job,info)
#####dqrsl(x,ldx,n,k,qraux,y,qy,qty,b,rsd,xb,job,info)
do i=1,nef
	sout(i)=sout(i)*sqwini(i)

#now clean up 

if(nit<=1){
#get rid of the parametric component of the leverage
	job=10000
	for(j=1;j<=p+1;j=j+1){
		do i=1,nef
			work(i)=0d0
		work(j)=1d0
		call dqrsl(xqr,nef,nef,qrank,qraux,work,var,junk,junk,
			junk,junk,job,info)
		do i=1,nef
			levout(i)=levout(i) - var(i)**2
		}
	dof=0d0
	do i=1,nef {
		if(win(i)>0d0) {
			levout(i)=levout(i)/win(i)
			}
		else {levout(i)=0d0}
		}
	do i=1,nef {dof=dof+levout(i)*win(i)}
	call unpck(n,nef,match,levout,var)
	for(j=1;j<=p+1;j=j+1){work(j)=beta(j)}
	for(j=1;j<=p+1;j=j+1){beta(qpivot(j))=work(j)}
	}
call unpck(n,nef,match,sout,s)
return
end
subroutine pck(n,p,match,x,xbar)
integer match(n),p,n
double precision x(n),xbar(n)
	do i=1,p
		xbar(i)=0d0
	do i=1,n
		xbar(match(i))=xbar(match(i))+x(i)
return
end

subroutine suff(n,p,match,x,y,w,xbar,ybar,wbar,work)
integer match(n),p,n
double precision x(n),xbar(n),y(n),ybar(n),w(n),wbar(n),work(n)
call pck(n,p,match,w,wbar)
do i=1,n
	xbar(match(i))=x(i)
do i=1,n
	work(i)=y(i)*w(i)
call pck(n,p,match,work,ybar)
	do i=1,p{
		if(wbar(i)>0d0) 
			ybar(i)=ybar(i)/wbar(i) 
		else ybar(i)=0d0

	}
return
end
subroutine unpck(n,p,match,xbar,x)
integer match(n),p,n
double precision x(n),xbar(p+1)
if(p<n)xbar(p+1)=0d0
do i = 1,n
	x(i)=xbar(match(i))
return
end
double precision function dwrss(n,y,eta,w)
integer n
double precision y(n),w(n),wtot,wsum,work,eta(n)
wsum=0d0
wtot=0d0
do i = 1,n{
	work=y(i)-eta(i)
	wsum=wsum+w(i)*work*work
	wtot=wtot+w(i)
}
if (wtot > 0d0) {dwrss=wsum/wtot} else {dwrss=0d0}
return
end
