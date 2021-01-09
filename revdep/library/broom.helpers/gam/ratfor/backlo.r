subroutine baklo(x,y,w,npetc,wddnfl,spatol,match,
		etal,s,eta,beta,var,dof,
		qr,qraux,qpivot,effect,iv,v,iwork,work)
implicit double precision(a-h,o-z)
integer n,p,q,nit,maxit,qrank
integer npetc(7),wddnfl(*),match(*),qpivot(*),iv(*),iwork(*)
##integer which(q),dwhich(q),degree(q),nef(q),liv(q),lv(q),nvmax(q)
double precision x(*),y(*),w(*),spatol(*),
	etal(*),s(*),eta(*),beta(*),var(*),dof(*),
	qr(*),qraux(*),v(*),effect(*),work(*)
#work size: 4*n + sum( nef(k)*(pj+dj+4)+5+3*pj )	+5*n
#          = 9*n + sum( nef(k)*(pj+dj+4)+5+3*pj )

n=npetc(1)
p=npetc(2)
q=npetc(3)
maxit=npetc(5)
qrank=npetc(6)
call baklo0(x,n,p,y,w,q,wddnfl(1),wddnfl(q+1),wddnfl(2*q+1),
            spatol(1),wddnfl(3*q+1),dof,match,wddnfl(4*q+1),
            etal,s,eta,beta,var,spatol(q+1),
            nit,maxit,qr,qraux,qrank,qpivot,effect,
            work(1),work(n+1),work(2*n+1),work(3*n+1),
            iv,wddnfl(5*q+1),wddnfl(6*q+1),v,wddnfl(7*q+1),
            iwork(1),work(4*n+1))
npetc(4)=nit
npetc(6)=qrank
return
end

subroutine baklo0(x,n,p,y,w,q,which,dwhich,pwhich,span,degree,dof,match,nef,
			etal,s,eta,beta,var,tol,nit,maxit,
			qr,qraux,qrank,qpivot,effect,z,old,sqwt,sqwti,
			iv,liv,lv,v,nvmax,iwork,work)
implicit double precision(a-h,o-z)
integer n,p,q,which(q),dwhich(q),pwhich(q),degree(q),match(n,q),nef(q),nit,
		maxit,qrank,qpivot(p),iv(*),liv(q),lv(q),nvmax(q),iwork(q)
double precision x(n,p),y(n),w(n),span(q),dof(q),
			etal(n),s(n,q),eta(n),beta(p),var(n,q),tol,
			qr(n,p),qraux(p),v(*),effect(n),work(*)
#work should be sum( nef(k)*(pj+dj+4)+5+3*pj )	+5*n
double precision z(*),old(*),dwrss,ratio
double precision sqwt(n),sqwti(n)
logical anyzwt
double precision deltaf, normf,onedm7
integer job,info,slv,sliv,iw,j,dj,pj
onedm7=1d-7
job=1101;info=1
if(q==0)maxit=1 
ratio=1d0
# fix up sqy's for weighted problems.
anyzwt=.false.
do i=1,n{
	if(w(i)>0d0){
		sqwt(i)=dsqrt(w(i))
		sqwti(i)=1d0/sqwt(i)
	}
	else{
		sqwt(i)=0d0
		sqwti(i)=0d0
		anyzwt=.true.
		}
	}
# if qrank > 0 then qr etc contain the qr decomposition
# else baklo computes it. 
if(qrank==0){
	do i=1,n{
		do j=1,p{
			qr(i,j)=x(i,j)*sqwt(i)
			}
		}
	do j=1,p{qpivot(j)=j}
	call dqrdca(qr,n,n,p,qraux,qpivot,work,qrank,onedm7)
	}
do i=1,n{
	eta(i)=0d0
	for(j=1;j<=q;j=j+1){
		eta(i)=eta(i)+s(i,j)
		}
	}
nit=0
while ((ratio > tol )&(nit < maxit)){
	# first the linear fit
	deltaf=0d0
	nit=nit+1
	do i=1,n{
		z(i)=(y(i)-eta(i))*sqwt(i)
		old(i)=etal(i)
	}
#	call dqrsl1(qr,dq,qraux,qrank,sqz,one,work(1),etal,two,three)
#job=1101 -- computes fits, effects and beta
	call dqrsl(qr,n,n,qrank,qraux,z,work(1),effect(1),beta,
		work(1),etal,job,info)

# now unsqrt the fits
#Note: we dont have to fix up the zero weights till the end, since their fits
#are always immaterial to the computation
	do i=1,n{
		etal(i)=etal(i)*sqwti(i)
		}
	# now a single non-linear backfitting loop 
	sliv=1
	slv=1
	iw=5*n+1
	for(k=1;k<=q;k=k+1){

		j=which(k)
		dj=dwhich(k)
		pj=pwhich(k)
		do i=1,n{
			old(i)=s(i,k)
			z(i)=y(i)-etal(i)-eta(i)+old(i)
		}
call lo1(x(1,j),z,w,n,dj,pj,nvmax(k),span(k),degree(k),match(1,k),
		nef(k),nit,dof(k),s(1,k),var(1,k),work(iw),
#	xin,win
	work(iw+pj+1),work(iw+nef(k)*dj+pj+1),
#	sqwin,sqwini,
	work(iw+nef(k)*(dj+1)+pj+2),work(iw + nef(k)*(dj+2)+pj+2),
#	xqr,qrank,	
	work(iw+nef(k)*(dj+3)+pj+2),work(iw+nef(k)*(pj+dj+4)+pj+2),
#	qpivot,qraux,	
#	work(iw+nef(k)*(pj+dj+4)+pj+3),work(iw+nef(k)*(pj+dj+4)+4+2*pj),
	iwork(1),work(iw+nef(k)*(pj+dj+4)+4+2*pj),
	iv(sliv),liv(k),lv(k),v(slv),
	work(1) )
#work should be sum( nef(k)*(pj+dj+4)+5+3*pj )	+5*n
# In the call above I give lo1 pieces of work to use for storing
# the  qr decomposition, and it gets the same undisturbed portion
# each time. The fact that it is given a double work word for qrank
# is irrelevant but convenient; it still stores the integer qrank there.
# I do this because there is a partition like this for each lo() term
# in the model, and the number of them is variable
		sliv=sliv+liv(k)
		slv=slv+lv(k)
		iw=iw+nef(k)*(pj+dj+4)+5+3*pj
		do i=1,n{
			eta(i)=eta(i)+s(i,k)-old(i)
			}
		deltaf=deltaf+dwrss(n,old,s(1,k),w)
		}
	normf=0d0
	do i=1,n{
		normf=normf+w(i)*eta(i)*eta(i)
		}
	if(normf>0d0){
		ratio=dsqrt(deltaf/normf)
		}
	 else {ratio = 0d0}
	}
#now package up the results
do j=1,p {work(j)=beta(j)}
do j=1,p {beta(qpivot(j))=work(j)}
if(anyzwt){
	do i=1,n {
		if(w(i) <= 0d0){
			etal(i)=0d0
			do j=1,p{
				etal(i)=etal(i)+beta(j)*x(i,j)
				}
			}
		}
	}
		
do i=1,n
	eta(i)=eta(i)+etal(i)
	

return
end
