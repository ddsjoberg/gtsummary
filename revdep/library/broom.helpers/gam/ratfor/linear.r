subroutine dqrls(x,dx,pivot,qraux,y,dy,beta,res,qt,tol,scrtch,rank)
integer pivot(*),dx(2),dy(2),rank
double precision x(*), qraux(*), y(*), beta(*),res(*),qt(*),tol(*),
	scrtch(*)

integer n,p,q,kn,kp,k,info

n=dx(1); p=dx(2); q=dy(2)
call dqrdca(x,n,n,p,qraux,pivot,scrtch,rank,tol(1))

kn=1; kp=1
if(rank>0)for(k=1;k<=q;k=k+1){
	call dqrsl(x,n,n,rank,qraux,y(kn),scrtch,qt(kn),beta(kp),
		res(kn),scrtch,00110,info)
	kn = kn+n; kp=kp+p
}
return
end

#apply the qr decomposition to do various jobs
subroutine dqrsl1(qr,dq,qra,rank,y,k,qy,qb,job,info)
double precision qr(*),qra(*),y(*),qy(*),qb(*); integer dq(2),job,k,rank
integer n,kn,kb,j
double precision ourqty(1), ourqy(1), ourb(1), ourrsd(1), ourxb(1)
ourqty(1) = 0d0
ourqy(1) = 0d0
ourb(1) = 0d0
ourrsd(1) = 0d0
ourxb(1) = 0d0
n = dq(1)
kn = 1; kb = 1
switch(job) {
case 10000: #qy
for(j=0; j<k; j = j+1) {
	call dqrsl(qr,dq(1),dq(1),rank,qra,y(kn),qy(kn),ourqty,ourb,ourrsd,ourxb,job,info)
	kn = kn +n
}
case 1000: #qty
for(j=0; j<k; j = j+1) {
        call dqrsl(qr,dq(1),dq(1),rank,qra,y(kn),ourqy,qy(kn),ourb,ourrsd,ourxb,job,info)
	kn = kn +n
}
case 100: #coefs
for(j=0; j<k; j = j+1) {
	call dqrsl(qr,dq(1),dq(1),rank,qra,y(kn),ourqy,qy(kn),qb(kb),ourrsd,ourxb,job,info)
	kn = kn +n; kb = kb +rank
}
case 10: #residuals
for(j=0; j<k; j = j+1) {
	call dqrsl(qr,dq(1),dq(1),rank,qra,y(kn),ourqy,qy(kn),ourb,qb(kn),ourxb,job,info)
	kn = kn +n
}
case 1: #fitted
for(j=0; j<k; j = j+1) {
	call dqrsl(qr,dq(1),dq(1),rank,qra,y(kn),ourqy,qy(kn),ourb,ourrsd,qb(kn),job,info)
	kn = kn +n
}
default:
	info = -1
}
return
end

subroutine dqr(x,dx,pivot,qraux,tol,scrtch,rank)
integer pivot(*),dx(2),rank
double precision x(*), qraux(*), tol(*), scrtch(*)

integer n,p

n=dx(1); p=dx(2);
call dqrdca(x,n,n,p,qraux,pivot,scrtch,rank,tol(1))
return
end


# qr decomposition, modified from linpack routines to give stable
# ordering and rank estimation
subroutine dqrdca(x,ldx,n,p,qraux,jpvt,work,rank,eps)
integer ldx,n,p,rank
integer jpvt(*)
double precision x(ldx,*),qraux(*),work(*),eps
integer j,jj,jp,l,lup,curpvt
double precision dnrm2,tt
double precision ddot,nrmxl,t,ww
do j=1,p {
	qraux(j) = dnrm2(n,x(1,j),1)
	work(j) = qraux(j); work(j+p) =  qraux(j)
}
l=1; lup = min0(n,p); curpvt = p
while(l<=lup) {
	qraux(l) = 0.0d0
	nrmxl = dnrm2(n-l+1,x(l,l),1)
	t = work(l+p); if(t > 0.)t = nrmxl/t
	if(t < eps){
		call dshift(x,ldx,n,l,curpvt)
		jp = jpvt(l); t=qraux(l); tt=work(l); ww = work(l+p)
		for(j=l+1; j<=curpvt; j=j+1){
			jj=j-1
			jpvt(jj)=jpvt(j); qraux(jj)=qraux(j)
			work(jj)=work(j); work(jj+p) = work(j+p)
		}
		jpvt(curpvt)=jp; qraux(curpvt)=t;
		work(curpvt)=tt; work(curpvt+p) = ww
		curpvt=curpvt-1; if(lup>curpvt)lup=curpvt
	}
	else {
		if(l==n)break
		if (x(l,l)!=0.0d0)
			nrmxl = dsign(nrmxl,x(l,l))
		call dscal(n-l+1,1.0d0/nrmxl,x(l,l),1)
		x(l,l) = 1.0d0+x(l,l)
		for(j=l+1; j<=curpvt; j=j+1) {
			t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
			call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
			if (qraux(j)!=0.0d0) {
				tt = 1.0d0-(dabs(x(l,j))/qraux(j))**2
				tt = dmax1(tt,0.0d0)
				t = tt
				tt = 1.0d0+0.05d0*tt*(qraux(j)/work(j))**2
				if (tt!=1.0d0)
					qraux(j) = qraux(j)*dsqrt(t)
				else {
					qraux(j) = dnrm2(n-l,x(l+1,j),1)
					work(j) = qraux(j)
				}
			}
		}
		qraux(l) = x(l,l)
		x(l,l) = -nrmxl
		l=l+1
	}
}
rank = lup
return
end

subroutine dchdc(a,lda,p,work,jpvt,job,info)
integer lda,p,jpvt(p),job,info
double precision a(lda,p),work(p)
integer pu,pl,plp1,j,jp,jt,k,kb,km1,kp1,l,maxl
double precision temp
double precision maxdia
logical swapk,negk
pl = 1
pu = 0
info = p
if (job!=0) {
	do k = 1,p {
		swapk = jpvt(k)>0
		negk = jpvt(k)<0
		jpvt(k) = k
		if (negk)
			jpvt(k) = -jpvt(k)
		if (swapk) {
			if (k!=pl) {
				call dswap(pl-1,a(1,k),1,a(1,pl),1)
				temp = a(k,k)
				a(k,k) = a(pl,pl)
				a(pl,pl) = temp
				plp1 = pl+1
				if (p>=plp1)
					do j = plp1,p
						if (j<k) {
							temp = a(pl,j)
							a(pl,j) = a(j,k)
							a(j,k) = temp
							}
						else if (j!=k) {
							temp = a(k,j)
							a(k,j) = a(pl,j)
							a(pl,j) = temp
							}
				jpvt(k) = jpvt(pl)
				jpvt(pl) = k
				}
			pl = pl+1
			}
		}
	pu = p
	if (p>=pl)
		do kb = pl,p {
			k = p-kb+pl
			if (jpvt(k)<0) {
				jpvt(k) = -jpvt(k)
				if (pu!=k) {
					call dswap(k-1,a(1,k),1,a(1,pu),1)
					temp = a(k,k)
					a(k,k) = a(pu,pu)
					a(pu,pu) = temp
					kp1 = k+1
					if (p>=kp1)
						do j = kp1,p
							if (j<pu) {
								temp = a(k,j)
								a(k,j) = a(j,pu)
								a(j,pu) = temp
								}
							else if (j!=pu) {
								temp = a(k,j)
								a(k,j) = a(pu,j)
								a(pu,j) = temp
								}
					jt = jpvt(k)
					jpvt(k) = jpvt(pu)
					jpvt(pu) = jt
					}
				pu = pu-1
				}
			}
	}
do k = 1,p {
#        reduction loop.
	maxdia = a(k,k)
	kp1 = k+1
	maxl = k
#        determine the pivot element.
	if (k>=pl&&k<pu)
		do l = kp1,pu
			if (a(l,l)>maxdia) {
				maxdia = a(l,l)
				maxl = l
				}
#        quit if the pivot element is not positive.
	if (maxdia<=0.0d0)
		go to 10
	if (k!=maxl) {
#           start the pivoting and update jpvt.
		km1 = k-1
		call dswap(km1,a(1,k),1,a(1,maxl),1)
		a(maxl,maxl) = a(k,k)
		a(k,k) = maxdia
		jp = jpvt(maxl)
		jpvt(maxl) = jpvt(k)
		jpvt(k) = jp
		}
#        reduction step. pivoting is contained across the rows.
	work(k) = dsqrt(a(k,k))
	a(k,k) = work(k)
	if (p>=kp1)
		do j = kp1,p {
			if (k!=maxl)
				if (j<maxl) {
					temp = a(k,j)
					a(k,j) = a(j,maxl)
					a(j,maxl) = temp
					}
				else if (j!=maxl) {
					temp = a(k,j)
					a(k,j) = a(maxl,j)
					a(maxl,j) = temp
					}
			a(k,j) = a(k,j)/work(k)
			work(j) = a(k,j)
			temp = -a(k,j)
			call daxpy(j-k,temp,work(kp1),1,a(kp1,j),1)
			}
	}
return
10  info = k-1
return
end



double precision function epslon(x)
double precision x
#     estimate unit roundoff in quantities of size x.
double precision a,b,c,eps
a = 4.0d0/3.0d0
repeat {
	b = a-1.0d0
	c = b+b+b
	eps = dabs(c-1.0d0)
	}
	until(eps!=0.0d0)
epslon = eps*dabs(x)
return
end



double precision function pythag(a,b)
double precision a,b
double precision p,r,s,t,u
p = dmax1(dabs(a),dabs(b))
if (p!=0.0d0) {
	r = (dmin1(dabs(a),dabs(b))/p)**2
	repeat {
		t = 4.0d0+r
		if (t==4.0d0)
			break 1
		s = r/t
		u = 1.0d0+2.0d0*s
		p = u*p
		r = (s/u)**2*r
		}
	}
pythag = p
return
end



subroutine rg(nm,n,a,wr,wi,matz,z,iv1,fv1,ierr)
integer n,nm,is1,is2,ierr,matz
double precision a(nm,n),wr(n),wi(n),z(nm,n),fv1(n)
integer iv1(n)
if (n>nm)
	ierr = 10*n
else {
	call  balanc(nm,n,a,is1,is2,fv1)
	call  elmhes(nm,n,is1,is2,a,iv1)
	if (matz==0)
#     .......... find eigenvalues only ..........
		call  hqr(nm,n,is1,is2,a,wr,wi,ierr)
	else {
#     .......... find both eigenvalues and eigenvectors ..........
		call  eltran(nm,n,is1,is2,a,iv1,z)
		call  hqr2(nm,n,is1,is2,a,wr,wi,z,ierr)
		if (ierr==0)
			call  balbak(nm,n,is1,is2,fv1,n,z)
		}
	}
return
end

subroutine chol(a,p,work,jpvt,job,info)
integer p,jpvt(*),job,info(*)
double precision a(p,*),work(*)
integer i,j
	for(j =2; j<=p; j = j+1)
		for(i=1; i<j; i = i+1)
			if(a(i,j)!=a(j,i)){ info(1) = -1 ; return}
	call dchdc(a,p,p,work,jpvt,job,info(1))
	for(j =2; j<=p; j = j+1)
		for(i=1; i<j; i = i+1)
			a(j,i) = 0.
	return
end

#x is a real symmetric matrix
subroutine crs(x,dmx,matz,w,z,fv1,fv2,ierr)
double precision x(*),w(*),z(*),fv1(*),fv2(*)
integer dmx(2),nx,nv,ierr,matz
nx=dmx(1)
nv=dmx(2)
call rs(nx,nv,x,w,matz,z,fv1,fv2,ierr)
return
end

subroutine dqrls2(x,dx,pivot,qraux,y,dy,beta,res,qt,scrtch,eps)
integer pivot(*),dx(2),dy(2)
double precision x(*), qraux(*), y(*), beta(*),res(*),qt(*),
	scrtch(*),eps

integer n,p,q,kn,kp,k,info,rank

n=dx(1); p=dx(2); q=dy(2)
call dqrdca(x,n,n,p,qraux,pivot,scrtch,rank,eps)

kn=1; kp=1
for(k=1;k<=q;k=k+1){
	call dqrsl(x,n,n,p,qraux,y(kn),scrtch,qt(kn),beta(kp),
		res(kn),scrtch,00110,info)
	kn = kn+n; kp=kp+p
}
return
end

subroutine dsvdc1(x,dmx,job,work,e,s,u,v,info)
double precision x(*),work(*),s(*),e(*),u(*),v(*)
integer dmx(2),nx,nv,job,info
nx=dmx(1)
nv=dmx(2)
call dsvdc(x,nx,nx,nv,s,e,u,nx,v,nv,work,job,info)
return
end

subroutine balanc(nm,n,a,low,igh,scale)
integer i,j,k,l,m,n,nm,igh,low,iexc
double precision a(nm,n),scale(n)
double precision c,f,g,r,s,b2,radix
logical noconv
radix = 16.0d0
b2 = radix*radix
k = 1
l = n
repeat {
#     .......... for j=l step -1 until 1 do -- ..........
	for(j=l; j>0; j=j-1 ){
		do i = 1,l
			if (i!=j)
				if (a(j,i)!=0.0d0)
					next 2
		go to 10
		}
	go to 20
	10  m = l
	iexc = 1
	repeat {
#     .......... in-line procedure for row and
#                column exchange ..........
		scale(m) = j
		if (j!=m) {
			do i = 1,l {
				f = a(i,j)
				a(i,j) = a(i,m)
				a(i,m) = f
				}
			do i = k,n {
				f = a(j,i)
				a(j,i) = a(m,i)
				a(m,i) = f
				}
			}
		switch(iexc) {
			case 1:
#     .......... search for rows isolating an eigenvalue
#                and push them down ..........
				if (l==1)
					go to 40
				l = l-1
				break 1
			case 2:
#     .......... search for columns isolating an eigenvalue
#                and push them left ..........
				k = k+1
				20  do j = k,l {
					do i = k,l
						if (i!=j)
							if (a(i,j)!=0.0d0)
								next 2
					go to 30
					}
				break 2
				30  m = k
				iexc = 2
			}
		}
	}
#     .......... now balance the submatrix in rows k to l ..........
do i = k,l
	scale(i) = 1.0d0
repeat {
#     .......... iterative loop for norm reduction ..........
	noconv = .false.
	do i = k,l {
		c = 0.0d0
		r = 0.0d0
		do j = k,l
			if (j!=i) {
				c = c+dabs(a(j,i))
				r = r+dabs(a(i,j))
				}
#     .......... guard against zero c or r due to underflow ..........
		if (c!=0.0d0&&r!=0.0d0) {
			g = r/radix
			f = 1.0d0
			s = c+r
			while (c<g) {
				f = f*radix
				c = c*b2
				}
			g = r*radix
			while (c>=g) {
				f = f/radix
				c = c/b2
				}
#     .......... now balance ..........
			if ((c+r)/f<0.95d0*s) {
				g = 1.0d0/f
				scale(i) = scale(i)*f
				noconv = .true.
				do j = k,n
					a(i,j) = a(i,j)*g
				do j = 1,l
					a(j,i) = a(j,i)*f
				}
			}
		}
	}
	until(!noconv)
40  low = k
igh = l
return
end



subroutine balbak(nm,n,low,igh,scale,m,z)
integer i,j,k,m,n,ii,nm,igh,low
double precision scale(n),z(nm,m)
double precision s
if (m!=0) {
	if (igh!=low)
		do i = low,igh {
			s = scale(i)
#     .......... left hand eigenvectors are back transformed
#                if the foregoing statement is replaced by
#                s=1.0d0/scale(i). ..........
			do j = 1,m
				z(i,j) = z(i,j)*s
			}
#     ......... for i=low-1 step -1 until 1,
#               igh+1 step 1 until n do -- ..........
	do ii = 1,n {
		i = ii
		if (i<low||i>igh) {
			if (i<low)
				i = low-ii
			k = scale(i)
			if (k!=i)
				do j = 1,m {
					s = z(i,j)
					z(i,j) = z(k,j)
					z(k,j) = s
					}
			}
		}
	}
return
end



subroutine elmhes(nm,n,low,igh,a,int)
integer i,j,m,n,la,nm,igh,kp1,low,mm1,mp1
double precision a(nm,n)
double precision x,y
integer int(igh)
la = igh-1
kp1 = low+1
if (la>=kp1)
	do m = kp1,la {
		mm1 = m-1
		x = 0.0d0
		i = m
		do j = m,igh
			if (dabs(a(j,mm1))>dabs(x)) {
				x = a(j,mm1)
				i = j
				}
		int(m) = i
		if (i!=m) {
#     .......... interchange rows and columns of a ..........
			do j = mm1,n {
				y = a(i,j)
				a(i,j) = a(m,j)
				a(m,j) = y
				}
			do j = 1,igh {
				y = a(j,i)
				a(j,i) = a(j,m)
				a(j,m) = y
				}
			}
#     .......... end interchange ..........
		if (x!=0.0d0) {
			mp1 = m+1
			do i = mp1,igh {
				y = a(i,mm1)
				if (y!=0.0d0) {
					y = y/x
					a(i,mm1) = y
					do j = m,n
						a(i,j) = a(i,j)-y*a(m,j)
					do j = 1,igh
						a(j,m) = a(j,m)+y*a(j,i)
					}
				}
			}
		}
return
end



subroutine eltran(nm,n,low,igh,a,int,z)
integer i,j,n,kl,mp,nm,igh,low,mp1
double precision a(nm,igh),z(nm,n)
integer int(igh)
#     .......... initialize z to identity matrix ..........
do j = 1,n {
	do i = 1,n
		z(i,j) = 0.0d0
	z(j,j) = 1.0d0
	}
kl = igh-low-1
if (kl>=1)
	for(mp = igh-1; mp > low; mp = mp -1) {
		mp1 = mp+1
		do i = mp1,igh
			z(i,mp) = a(i,mp-1)
		i = int(mp)
		if (i!=mp) {
			do j = mp,igh {
				z(mp,j) = z(i,j)
				z(i,j) = 0.0d0
				}
			z(i,mp) = 1.0d0
			}
		}
return
end



subroutine hqr(nm,n,low,igh,h,wr,wi,ierr)
integer i,j,k,l,m,n,en,mm,na,nm,igh,itn,its,low,mp2,enm2,ierr
double precision h(nm,n),wr(n),wi(n)
double precision p,q,r,s,t,w,x,y,zz,norm,tst1,tst2
logical notlas
ierr = 0
norm = 0.0d0
k = 1
#     .......... store roots isolated by balanc
#                and compute matrix norm ..........
do i = 1,n {
	do j = k,n
		norm = norm+dabs(h(i,j))
	k = i
	if (i<low||i>igh) {
		wr(i) = h(i,i)
		wi(i) = 0.0d0
		}
	}
en = igh
t = 0.0d0
itn = 30*n
repeat {
#     .......... search for next eigenvalues ..........
	if (en<low)
		return
	its = 0
	na = en-1
	enm2 = na-1
	repeat {
#     .......... look for single small sub-diagonal element
		for(l=en; l > low; l = l-1){
			s = dabs(h(l-1,l-1))+dabs(h(l,l))
			if (s==0.0d0)
				s = norm
			tst1 = s
			tst2 = tst1+dabs(h(l,l-1))
			if (tst2==tst1)
				break 1
			}
#     .......... form shift ..........
		x = h(en,en)
		if (l==en)
			go to 50
		y = h(na,na)
		w = h(en,na)*h(na,en)
		if (l==na)
			break 1
		if (itn==0)
			break 2
		if (its==10||its==20) {
#     .......... form exceptional shift ..........
			t = t+x
			do i = low,en
				h(i,i) = h(i,i)-x
			s = dabs(h(en,na))+dabs(h(na,enm2))
			x = 0.75d0*s
			y = x
			w = -0.4375d0*s*s
			}
		its = its+1
		itn = itn-1
#     .......... look for two consecutive small
#                sub-diagonal elements.
#                for m=en-2 step -1 until l do -- ..........
		do mm = l,enm2 {
			m = enm2+l-mm
			zz = h(m,m)
			r = x-zz
			s = y-zz
			p = (r*s-w)/h(m+1,m)+h(m,m+1)
			q = h(m+1,m+1)-zz-r-s
			r = h(m+2,m+1)
			s = dabs(p)+dabs(q)+dabs(r)
			p = p/s
			q = q/s
			r = r/s
			if (m==l)
				break 1
			tst1 = dabs(p)*(dabs(h(m-1,m-1))+dabs(zz)+dabs(h(m+1,m+1)))
			tst2 = tst1+dabs(h(m,m-1))*(dabs(q)+dabs(r))
			if (tst2==tst1)
				break 1
			}
		mp2 = m+2
		do i = mp2,en {
			h(i,i-2) = 0.0d0
			if (i!=mp2)
				h(i,i-3) = 0.0d0
			}
#     .......... double qr step involving rows l to en and
#                columns m to en ..........
		do k = m,na {
			notlas = k!=na
			if (k!=m) {
				p = h(k,k-1)
				q = h(k+1,k-1)
				r = 0.0d0
				if (notlas)
					r = h(k+2,k-1)
				x = dabs(p)+dabs(q)+dabs(r)
				if (x==0.0d0)
					next 1
				p = p/x
				q = q/x
				r = r/x
				}
			s = dsign(dsqrt(p*p+q*q+r*r),p)
			if (k!=m)
				h(k,k-1) = -s*x
			else if (l!=m)
				h(k,k-1) = -h(k,k-1)
			p = p+s
			x = p/s
			y = q/s
			zz = r/s
			q = q/p
			r = r/p
			if (!notlas) {
#     .......... row modification ..........
				do j = k,n {
					p = h(k,j)+q*h(k+1,j)
					h(k,j) = h(k,j)-p*x
					h(k+1,j) = h(k+1,j)-p*y
					}
				j = min0(en,k+3)
#     .......... column modification ..........
				do i = 1,j {
					p = x*h(i,k)+y*h(i,k+1)
					h(i,k) = h(i,k)-p
					h(i,k+1) = h(i,k+1)-p*q
					}
				}
			else {
#     .......... row modification ..........
				do j = k,n {
					p = h(k,j)+q*h(k+1,j)+r*h(k+2,j)
					h(k,j) = h(k,j)-p*x
					h(k+1,j) = h(k+1,j)-p*y
					h(k+2,j) = h(k+2,j)-p*zz
					}
				j = min0(en,k+3)
#     .......... column modification ..........
				do i = 1,j {
					p = x*h(i,k)+y*h(i,k+1)+zz*h(i,k+2)
					h(i,k) = h(i,k)-p
					h(i,k+1) = h(i,k+1)-p*q
					h(i,k+2) = h(i,k+2)-p*r
					}
				}
			}
		}
#     .......... two roots found ..........
	p = (y-x)/2.0d0
	q = p*p+w
	zz = dsqrt(dabs(q))
	x = x+t
	if (q<0.0d0) {
#     .......... complex pair ..........
		wr(na) = x+p
		wr(en) = x+p
		wi(na) = zz
		wi(en) = -zz
		}
	else {
#     .......... real pair ..........
		zz = p+dsign(zz,p)
		wr(na) = x+zz
		wr(en) = wr(na)
		if (zz!=0.0d0)
			wr(en) = x-w/zz
		wi(na) = 0.0d0
		wi(en) = 0.0d0
		}
	en = enm2
	next 1
#     .......... one root found ..........
	50  wr(en) = x+t
	wi(en) = 0.0d0
	en = na
	}
#     .......... set error -- all eigenvalues have not
#                converged after 30*n iterations ..........
ierr = en
return
end



subroutine hqr2(nm,n,low,igh,h,wr,wi,z,ierr)
integer i,j,k,l,m,n,en,ii,jj,ll,mm,na,nm,nn,igh,itn,its,low,mp2,enm2,ierr
double precision h(nm,n),wr(n),wi(n),z(nm,n)
double precision p,q,r,s,t,w,x,y,ra,sa,vi,vr,zz,norm,tst1,tst2
logical notlas
ierr = 0
norm = 0.0d0
k = 1
#     .......... store roots isolated by balanc
#                and compute matrix norm ..........
do i = 1,n {
	do j = k,n
		norm = norm+dabs(h(i,j))
	k = i
	if (i<low||i>igh) {
		wr(i) = h(i,i)
		wi(i) = 0.0d0
		}
	}
en = igh
t = 0.0d0
itn = 30*n
repeat {
#     .......... search for next eigenvalues ..........
	if (en<low)
		go to 70
	its = 0
	na = en-1
	enm2 = na-1
	repeat {
#     .......... look for single small sub-diagonal element
#                for l=en step -1 until low do -- ..........
		do ll = low,en {
			l = en+low-ll
			if (l==low)
				break 1
			s = dabs(h(l-1,l-1))+dabs(h(l,l))
			if (s==0.0d0)
				s = norm
			tst1 = s
			tst2 = tst1+dabs(h(l,l-1))
			if (tst2==tst1)
				break 1
			}
#     .......... form shift ..........
		x = h(en,en)
		if (l==en)
			go to 60
		y = h(na,na)
		w = h(en,na)*h(na,en)
		if (l==na)
			break 1
		if (itn==0)
			break 2
		if (its==10||its==20) {
#     .......... form exceptional shift ..........
			t = t+x
			do i = low,en
				h(i,i) = h(i,i)-x
			s = dabs(h(en,na))+dabs(h(na,enm2))
			x = 0.75d0*s
			y = x
			w = -0.4375d0*s*s
			}
		its = its+1
		itn = itn-1
#     .......... look for two consecutive small
#                sub-diagonal elements.
#                for m=en-2 step -1 until l do -- ..........
		do mm = l,enm2 {
			m = enm2+l-mm
			zz = h(m,m)
			r = x-zz
			s = y-zz
			p = (r*s-w)/h(m+1,m)+h(m,m+1)
			q = h(m+1,m+1)-zz-r-s
			r = h(m+2,m+1)
			s = dabs(p)+dabs(q)+dabs(r)
			p = p/s
			q = q/s
			r = r/s
			if (m==l)
				break 1
			tst1 = dabs(p)*(dabs(h(m-1,m-1))+dabs(zz)+dabs(h(m+1,m+1)))
			tst2 = tst1+dabs(h(m,m-1))*(dabs(q)+dabs(r))
			if (tst2==tst1)
				break 1
			}
		mp2 = m+2
		do i = mp2,en {
			h(i,i-2) = 0.0d0
			if (i!=mp2)
				h(i,i-3) = 0.0d0
			}
#     .......... double qr step involving rows l to en and
#                columns m to en ..........
		do k = m,na {
			notlas = k!=na
			if (k!=m) {
				p = h(k,k-1)
				q = h(k+1,k-1)
				r = 0.0d0
				if (notlas)
					r = h(k+2,k-1)
				x = dabs(p)+dabs(q)+dabs(r)
				if (x==0.0d0)
					next 1
				p = p/x
				q = q/x
				r = r/x
				}
			s = dsign(dsqrt(p*p+q*q+r*r),p)
			if (k!=m)
				h(k,k-1) = -s*x
			else if (l!=m)
				h(k,k-1) = -h(k,k-1)
			p = p+s
			x = p/s
			y = q/s
			zz = r/s
			q = q/p
			r = r/p
			if (!notlas) {
#     .......... row modification ..........
				do j = k,n {
					p = h(k,j)+q*h(k+1,j)
					h(k,j) = h(k,j)-p*x
					h(k+1,j) = h(k+1,j)-p*y
					}
				j = min0(en,k+3)
#     .......... column modification ..........
				do i = 1,j {
					p = x*h(i,k)+y*h(i,k+1)
					h(i,k) = h(i,k)-p
					h(i,k+1) = h(i,k+1)-p*q
					}
#     .......... accumulate transformations ..........
				do i = low,igh {
					p = x*z(i,k)+y*z(i,k+1)
					z(i,k) = z(i,k)-p
					z(i,k+1) = z(i,k+1)-p*q
					}
				}
			else {
#     .......... row modification ..........
				do j = k,n {
					p = h(k,j)+q*h(k+1,j)+r*h(k+2,j)
					h(k,j) = h(k,j)-p*x
					h(k+1,j) = h(k+1,j)-p*y
					h(k+2,j) = h(k+2,j)-p*zz
					}
				j = min0(en,k+3)
#     .......... column modification ..........
				do i = 1,j {
					p = x*h(i,k)+y*h(i,k+1)+zz*h(i,k+2)
					h(i,k) = h(i,k)-p
					h(i,k+1) = h(i,k+1)-p*q
					h(i,k+2) = h(i,k+2)-p*r
					}
#     .......... accumulate transformations ..........
				do i = low,igh {
					p = x*z(i,k)+y*z(i,k+1)+zz*z(i,k+2)
					z(i,k) = z(i,k)-p
					z(i,k+1) = z(i,k+1)-p*q
					z(i,k+2) = z(i,k+2)-p*r
					}
				}
			}
		}
#     .......... two roots found ..........
	p = (y-x)/2.0d0
	q = p*p+w
	zz = dsqrt(dabs(q))
	h(en,en) = x+t
	x = h(en,en)
	h(na,na) = y+t
	if (q<0.0d0) {
#     .......... complex pair ..........
		wr(na) = x+p
		wr(en) = x+p
		wi(na) = zz
		wi(en) = -zz
		}
	else {
#     .......... real pair ..........
		zz = p+dsign(zz,p)
		wr(na) = x+zz
		wr(en) = wr(na)
		if (zz!=0.0d0)
			wr(en) = x-w/zz
		wi(na) = 0.0d0
		wi(en) = 0.0d0
		x = h(en,na)
		s = dabs(x)+dabs(zz)
		p = x/s
		q = zz/s
		r = dsqrt(p*p+q*q)
		p = p/r
		q = q/r
#     .......... row modification ..........
		do j = na,n {
			zz = h(na,j)
			h(na,j) = q*zz+p*h(en,j)
			h(en,j) = q*h(en,j)-p*zz
			}
#     .......... column modification ..........
		do i = 1,en {
			zz = h(i,na)
			h(i,na) = q*zz+p*h(i,en)
			h(i,en) = q*h(i,en)-p*zz
			}
#     .......... accumulate transformations ..........
		do i = low,igh {
			zz = z(i,na)
			z(i,na) = q*zz+p*z(i,en)
			z(i,en) = q*z(i,en)-p*zz
			}
		}
	en = enm2
	next 1
#     .......... one root found ..........
	60  h(en,en) = x+t
	wr(en) = h(en,en)
	wi(en) = 0.0d0
	en = na
	}
#     .......... set error -- all eigenvalues have not
#                converged after 30*n iterations ..........
ierr = en
return
#     .......... all roots found.  backsubstitute to find
#                vectors of upper triangular form ..........
70  if (norm!=0.0d0) {
#     .......... for en=n step -1 until 1 do -- ..........
	do nn = 1,n {
		en = n+1-nn
		p = wr(en)
		q = wi(en)
		na = en-1
		if (q<0) {
#     .......... complex vector ..........
			m = na
#     .......... last vector component chosen imaginary so that
#                eigenvector matrix is triangular ..........
			if (dabs(h(en,na))<=dabs(h(na,en)))
				call cdiv(0.0d0,-h(na,en),h(na,na)-p,q,h(na,na),h(na,en))
			else {
				h(na,na) = q/h(en,na)
				h(na,en) = -(h(en,en)-p)/h(en,na)
				}
			h(en,na) = 0.0d0
			h(en,en) = 1.0d0
			enm2 = na-1
			if (enm2!=0)
#     .......... for i=en-2 step -1 until 1 do -- ..........
				do ii = 1,enm2 {
					i = na-ii
					w = h(i,i)-p
					ra = 0.0d0
					sa = 0.0d0
					do j = m,en {
						ra = ra+h(i,j)*h(j,na)
						sa = sa+h(i,j)*h(j,en)
						}
					if (wi(i)<0.0d0) {
						zz = w
						r = ra
						s = sa
						}
					else {
						m = i
						if (wi(i)==0.0d0)
							call cdiv(-ra,-sa,w,q,h(i,na),h(i,en))
						else {
#     .......... solve complex equations ..........
							x = h(i,i+1)
							y = h(i+1,i)
							vr = (wr(i)-p)*(wr(i)-p)+wi(i)*wi(i)-q*q
							vi = (wr(i)-p)*2.0d0*q
							if (vr==0.0d0&&vi==0.0d0) {
								tst1 = norm*(dabs(w)+dabs(q)+dabs(x)+dabs(y)+dabs(zz))
								vr = tst1
								repeat {
									vr = 0.01d0*vr
									tst2 = tst1+vr
									}
									until(tst2<=tst1)
								}
							call cdiv(x*r-zz*ra+q*sa,x*s-zz*sa-q*ra,vr,vi,h(i,na),h(i,en))
							if (dabs(x)<=dabs(zz)+dabs(q)) {
								call cdiv(-r-y*h(i,na),-s-y*h(i,en),zz,q,h(i+1,na),h(i+1,en))
								}
							else {
								h(i+1,na) = (-ra-w*h(i,na)+q*h(i,en))/x
								h(i+1,en) = (-sa-w*h(i,en)-q*h(i,na))/x
								}
							}
#     .......... overflow control ..........
						t = dmax1(dabs(h(i,na)),dabs(h(i,en)))
						if (t!=0.0d0) {
							tst1 = t
							tst2 = tst1+1.0d0/tst1
							if (tst2<=tst1)
								do j = i,en {
									h(j,na) = h(j,na)/t
									h(j,en) = h(j,en)/t
									}
							}
						}
					}
			}
		else if (q==0) {
#     .......... real vector ..........
			m = en
			h(en,en) = 1.0d0
			if (na!=0)
#     .......... for i=en-1 step -1 until 1 do -- ..........
				do ii = 1,na {
					i = en-ii
					w = h(i,i)-p
					r = 0.0d0
					do j = m,en
						r = r+h(i,j)*h(j,en)
					if (wi(i)<0.0d0) {
						zz = w
						s = r
						}
					else {
						m = i
						if (wi(i)!=0.0d0) {
#     .......... solve real equations ..........
							x = h(i,i+1)
							y = h(i+1,i)
							q = (wr(i)-p)*(wr(i)-p)+wi(i)*wi(i)
							t = (x*s-zz*r)/q
							h(i,en) = t
							if (dabs(x)<=dabs(zz))
								h(i+1,en) = (-s-y*t)/zz
							else
								h(i+1,en) = (-r-w*t)/x
							}
						else {
							t = w
							if (t==0.0d0) {
								tst1 = norm
								t = tst1
								repeat {
									t = 0.01d0*t
									tst2 = norm+t
									}
									until(tst2<=tst1)
								}
							h(i,en) = -r/t
							}
#     .......... overflow control ..........
						t = dabs(h(i,en))
						if (t!=0.0d0) {
							tst1 = t
							tst2 = tst1+1.0d0/tst1
							if (tst2<=tst1)
								do j = i,en
									h(j,en) = h(j,en)/t
							}
						}
					}
			}
		}
#     .......... end back substitution.
#                vectors of isolated roots ..........
	do i = 1,n
		if (i<low||i>igh)
			do j = i,n
				z(i,j) = h(i,j)
#     .......... multiply by transformation matrix to give
#                vectors of original full matrix.
#                for j=n step -1 until low do -- ..........
	do jj = low,n {
		j = n+low-jj
		m = min0(j,igh)
		do i = low,igh {
			zz = 0.0d0
			do k = low,m
				zz = zz+z(i,k)*h(k,j)
			z(i,j) = zz
			}
		}
	}
return
end



subroutine cdiv(ar,ai,br,bi,cr,ci)
double precision ar,ai,br,bi,cr,ci
#     complex division, (cr,ci) = (ar,ai)/(br,bi)
double precision s,ars,ais,brs,bis
s = dabs(br)+dabs(bi)
ars = ar/s
ais = ai/s
brs = br/s
bis = bi/s
s = brs**2+bis**2
cr = (ars*brs+ais*bis)/s
ci = (ais*brs-ars*bis)/s
return
end



subroutine rs(nm,n,a,w,matz,z,fv1,fv2,ierr)
integer n,nm,ierr,matz
double precision a(nm,n),w(n),z(nm,n),fv1(n),fv2(n)
if (n>nm)
	ierr = 10*n
else
 if (matz!=0) {
#     .......... find both eigenvalues and eigenvectors ..........
	call  tred2(nm,n,a,w,fv1,z)
	call  tql2(nm,n,w,fv1,z,ierr)
	}
else {
#     .......... find eigenvalues only ..........
	call  tred1(nm,n,a,w,fv1,fv2)
	call  tqlrat(n,w,fv2,ierr)
	}
return
end



subroutine tql2(nm,n,d,e,z,ierr)
integer i,j,k,l,m,n,ii,l1,l2,nm,mml,ierr
double precision d(n),e(n),z(nm,n)
double precision c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2,pythag
ierr = 0
if (n!=1) {
	do i = 2,n
		e(i-1) = e(i)
	f = 0.0d0
	tst1 = 0.0d0
	e(n) = 0.0d0
	do l = 1,n {
		j = 0
		h = dabs(d(l))+dabs(e(l))
		if (tst1<h)
			tst1 = h
#     .......... look for small sub-diagonal element ..........
		do m = l,n {
			tst2 = tst1+dabs(e(m))
			if (tst2==tst1)
				break 1
			}
		if (m!=l)
			repeat {
				if (j==30)
					go to 10
				j = j+1
#     .......... form shift ..........
				l1 = l+1
				l2 = l1+1
				g = d(l)
				p = (d(l1)-g)/(2.0d0*e(l))
				r = pythag(p,1.0d0)
				d(l) = e(l)/(p+dsign(r,p))
				d(l1) = e(l)*(p+dsign(r,p))
				dl1 = d(l1)
				h = g-d(l)
				if (l2<=n)
					do i = l2,n
						d(i) = d(i)-h
				f = f+h
#     .......... ql transformation ..........
				p = d(m)
				c = 1.0d0
				c2 = c
				el1 = e(l1)
				s = 0.0d0
				mml = m-l
#     .......... for i=m-1 step -1 until l do -- ..........
				do ii = 1,mml {
					c3 = c2
					c2 = c
					s2 = s
					i = m-ii
					g = c*e(i)
					h = c*p
					r = pythag(p,e(i))
					e(i+1) = s*r
					s = e(i)/r
					c = p/r
					p = c*d(i)-s*g
					d(i+1) = h+s*(c*g+s*d(i))
#     .......... form vector ..........
					do k = 1,n {
						h = z(k,i+1)
						z(k,i+1) = s*z(k,i)+c*h
						z(k,i) = c*z(k,i)-s*h
						}
					}
				p = -s*s2*c3*el1*e(l)/dl1
				e(l) = s*p
				d(l) = c*p
				tst2 = tst1+dabs(e(l))
				}
				until(tst2<=tst1)
		d(l) = d(l)+f
		}
#     .......... order eigenvalues and eigenvectors ..........
	do ii = 2,n {
		i = ii-1
		k = i
		p = d(i)
		do j = ii,n
			if (d(j)<p) {
				k = j
				p = d(j)
				}
		if (k!=i) {
			d(k) = d(i)
			d(i) = p
			do j = 1,n {
				p = z(j,i)
				z(j,i) = z(j,k)
				z(j,k) = p
				}
			}
		}
	return
#     .......... set error -- no convergence to an
#                eigenvalue after 30 iterations ..........
	10  ierr = l
	}
return
end



subroutine tqlrat(n,d,e2,ierr)
integer i,j,l,m,n,ii,l1,mml,ierr
double precision d(n),e2(n)
double precision b,c,f,g,h,p,r,s,t,epslon,pythag
ierr = 0
if (n!=1) {
	do i = 2,n
		e2(i-1) = e2(i)
	f = 0.0d0
	t = 0.0d0
	e2(n) = 0.0d0
	do l = 1,n {
		j = 0
		h = dabs(d(l))+dsqrt(e2(l))
		if (t<=h) {
			t = h
			b = epslon(t)
			c = b*b
			}
#     .......... look for small squared sub-diagonal element ..........
		do m = l,n
			if (e2(m)<=c)
				break 1
		if (m!=l)
			repeat {
				if (j==30)
					go to 20
				j = j+1
#     .......... form shift ..........
				l1 = l+1
				s = dsqrt(e2(l))
				g = d(l)
				p = (d(l1)-g)/(2.0d0*s)
				r = pythag(p,1.0d0)
				d(l) = s/(p+dsign(r,p))
				h = g-d(l)
				do i = l1,n
					d(i) = d(i)-h
				f = f+h
#     .......... rational ql transformation ..........
				g = d(m)
				if (g==0.0d0)
					g = b
				h = g
				s = 0.0d0
				mml = m-l
#     .......... for i=m-1 step -1 until l do -- ..........
				do ii = 1,mml {
					i = m-ii
					p = g*h
					r = p+e2(i)
					e2(i+1) = s*r
					s = e2(i)/r
					d(i+1) = h+s*(h+d(i))
					g = d(i)-e2(i)/g
					if (g==0.0d0)
						g = b
					h = g*p/r
					}
				e2(l) = s*g
				d(l) = h
#     .......... guard against underflow in convergence test ..........
				if (h==0.0d0)
					break 1
				if (dabs(e2(l))<=dabs(c/h))
					break 1
				e2(l) = h*e2(l)
				}
				until(e2(l)==0.0d0)
		p = d(l)+f
#     .......... order eigenvalues ..........
		if (l!=1)
#     .......... for i=l step -1 until 2 do -- ..........
			do ii = 2,l {
				i = l+2-ii
				if (p>=d(i-1))
					go to 10
				d(i) = d(i-1)
				}
		i = 1
		10  d(i) = p
		}
	return
#     .......... set error -- no convergence to an
#                eigenvalue after 30 iterations ..........
	20  ierr = l
	}
return
end



subroutine tred1(nm,n,a,d,e,e2)
integer i,j,k,l,n,ii,nm,jp1
double precision a(nm,n),d(n),e(n),e2(n)
double precision f,g,h,scale
do i = 1,n {
	d(i) = a(n,i)
	a(n,i) = a(i,i)
	}
#     .......... for i=n step -1 until 1 do -- ..........
do ii = 1,n {
	i = n+1-ii
	l = i-1
	h = 0.0d0
	scale = 0.0d0
	if (l>=1) {
#     .......... scale row (algol tol then not needed) ..........
		do k = 1,l
			scale = scale+dabs(d(k))
		if (scale==0.0d0)
			do j = 1,l {
				d(j) = a(l,j)
				a(l,j) = a(i,j)
				a(i,j) = 0.0d0
				}
		else {
			do k = 1,l {
				d(k) = d(k)/scale
				h = h+d(k)*d(k)
				}
			e2(i) = scale*scale*h
			f = d(l)
			g = -dsign(dsqrt(h),f)
			e(i) = scale*g
			h = h-f*g
			d(l) = f-g
			if (l!=1) {
#     .......... form a*u ..........
				do j = 1,l
					e(j) = 0.0d0
				do j = 1,l {
					f = d(j)
					g = e(j)+a(j,j)*f
					jp1 = j+1
					if (l>=jp1)
						do k = jp1,l {
							g = g+a(k,j)*d(k)
							e(k) = e(k)+a(k,j)*f
							}
					e(j) = g
					}
#     .......... form p ..........
				f = 0.0d0
				do j = 1,l {
					e(j) = e(j)/h
					f = f+e(j)*d(j)
					}
				h = f/(h+h)
#     .......... form q ..........
				do j = 1,l
					e(j) = e(j)-h*d(j)
#     .......... form reduced a ..........
				do j = 1,l {
					f = d(j)
					g = e(j)
					do k = j,l
						a(k,j) = a(k,j)-f*e(k)-g*d(k)
					}
				}
			do j = 1,l {
				f = d(j)
				d(j) = a(l,j)
				a(l,j) = a(i,j)
				a(i,j) = f*scale
				}
			next 1
			}
		}
	e(i) = 0.0d0
	e2(i) = 0.0d0
	}
return
end



subroutine tred2(nm,n,a,d,e,z)
integer i,j,k,l,n,ii,nm,jp1
double precision a(nm,n),d(n),e(n),z(nm,n)
double precision f,g,h,hh,scale
do i = 1,n {
	do j = i,n
		z(j,i) = a(j,i)
	d(i) = a(n,i)
	}
if (n!=1) {
#     .......... for i=n step -1 until 2 do -- ..........
	do ii = 2,n {
		i = n+2-ii
		l = i-1
		h = 0.0d0
		scale = 0.0d0
		if (l>=2) {
#     .......... scale row (algol tol then not needed) ..........
			do k = 1,l
				scale = scale+dabs(d(k))
			if (scale!=0.0d0) {
				do k = 1,l {
					d(k) = d(k)/scale
					h = h+d(k)*d(k)
					}
				f = d(l)
				g = -dsign(dsqrt(h),f)
				e(i) = scale*g
				h = h-f*g
				d(l) = f-g
#     .......... form a*u ..........
				do j = 1,l
					e(j) = 0.0d0
				do j = 1,l {
					f = d(j)
					z(j,i) = f
					g = e(j)+z(j,j)*f
					jp1 = j+1
					if (l>=jp1)
						do k = jp1,l {
							g = g+z(k,j)*d(k)
							e(k) = e(k)+z(k,j)*f
							}
					e(j) = g
					}
#     .......... form p ..........
				f = 0.0d0
				do j = 1,l {
					e(j) = e(j)/h
					f = f+e(j)*d(j)
					}
				hh = f/(h+h)
#     .......... form q ..........
				do j = 1,l
					e(j) = e(j)-hh*d(j)
#     .......... form reduced a ..........
				do j = 1,l {
					f = d(j)
					g = e(j)
					do k = j,l
						z(k,j) = z(k,j)-f*e(k)-g*d(k)
					d(j) = z(l,j)
					z(i,j) = 0.0d0
					}
				go to 10
				}
			}
		e(i) = d(l)
		do j = 1,l {
			d(j) = z(l,j)
			z(i,j) = 0.0d0
			z(j,i) = 0.0d0
			}
		10  d(i) = h
		}
#     .......... accumulation of transformation matrices ..........
	do i = 2,n {
		l = i-1
		z(n,l) = z(l,l)
		z(l,l) = 1.0d0
		h = d(i)
		if (h!=0.0d0) {
			do k = 1,l
				d(k) = z(k,i)/h
			do j = 1,l {
				g = 0.0d0
				do k = 1,l
					g = g+z(k,i)*z(k,j)
				do k = 1,l
					z(k,j) = z(k,j)-g*d(k)
				}
			}
		do k = 1,l
			z(k,i) = 0.0d0
		}
	}
do i = 1,n {
	d(i) = z(n,i)
	z(n,i) = 0.0d0
	}
z(n,n) = 1.0d0
e(1) = 0.0d0
return
end



subroutine dmatp(x,dx,y,dy,z)
integer dx(2),dy(2)
double precision x(*), y(*),z(*),ddot

integer n,p,q,i,j

n=dx(1); p=dx(2); q=dy(2)
do i = 1,n {
	jj = 1; ij = i
	do j = 1, q {
		z(ij) = ddot(p,x(i),n,y(jj),1) # x[i,1] & y[1,j]
		if(j<q){jj = jj + p; ij = ij + n}
	}
}
return
end

subroutine dmatpt(x,dx,y,dy,z)
integer dx(2),dy(2)
double precision x(*), y(*),z(*),ddot

integer n,p,q,i,j,ii

n=dx(1); p=dx(2); q=dy(2); ii=1
do i = 1,p {
	jj = 1; ij = i
	do j = 1, q {
		z(ij) = ddot(n,x(ii),1,y(jj),1) #x[1,i] & y[1,j]
		if(j<q){jj = jj + n; ij = ij + p}
	}
	ii = ii +n
}
return
end

subroutine matpm(x,dx,mmx,mx,y,dy,mmy,my,z)
integer dx(2),dy(2)
integer  mmx(*), mmy(*)
integer mx(*), my(*)
double precision x(*), y(*),z(*),ddot

integer n,p,q,i,j

n=dx(1); p=dx(2); q=dy(2)
call rowmis(mmx,dx(1),dx(2),mx)
call colmis(mmy,dy(1),dy(2),my)
do i = 1,n {
	jj = 1; ij = i
	do j = 1, q {
		if(!(mx(i)!=0 || my(j)!=0))
			z(ij) = ddot(p,x(i),n,y(jj),1) # x[i,1] & y[1,j]
		if(j<q){jj = jj + p; ij = ij + n}
	}
}
return
end

subroutine matptm(x,dx,mmx,mx,y,dy,mmy,my,z)
integer dx(2),dy(2)
integer  mmx(*), mmy(*)
integer mx(*), my(*)
double precision x(*), y(*),z(*),ddot

integer n,p,q,i,j
call colmis(mmx,dx(1),dx(2),mx)
call colmis(mmy,dy(1),dy(2),my)

n=dx(1); p=dx(2); q=dy(2); ii=1
do i = 1,p {
	jj = 1; ij = i
	do j = 1, q {
		if(!(mx(i)!=0 || my(j)!=0))
			z(ij) = ddot(n,x(ii),1,y(jj),1) #x[1,i] & y[1,j]
		if(j<q){jj = jj + n; ij = ij + p}
	}
	ii = ii +n
}
return
end

subroutine rowmis(m,n,p,vec)
integer n,p
integer m(n,p); integer vec(*)
do i = 1,n {
	vec(i)=0
#	vec(i)=.false.
	do j = 1,p {
		if(m(i,j)!=0)vec(i) = 1
	}
}
return
end

subroutine colmis(m,n,p,vec)
integer n,p
integer m(n,p); integer vec(*)
do j = 1,p {
	vec(j)=0
	do i = 1,n {
		if(m(i,j)!=0)vec(j) = 1
	}
}
return
end

subroutine daxpy(n,da,dx,incx,dy,incy)
double precision dx(*),dy(*),da
integer i,incx,incy,m,mp1,n
if (n>0)
	if (da!=0.0d0)
		if (incx!=1||incy!=1) {
			ix = 1
			iy = 1
			if (incx<0)
				ix = (-n+1)*incx+1
			if (incy<0)
				iy = (-n+1)*incy+1
			do i = 1,n {
				dy(iy) = dy(iy)+da*dx(ix)
				ix = ix+incx
				iy = iy+incy
				}
			}
		else {
			m = mod(n,4)
			if (m!=0) {
				do i = 1,m
					dy(i) = dy(i)+da*dx(i)
				if (n<4)
					return
				}
			mp1 = m+1
			do i = mp1,n,4 {
				dy(i) = dy(i)+da*dx(i)
				dy(i+1) = dy(i+1)+da*dx(i+1)
				dy(i+2) = dy(i+2)+da*dx(i+2)
				dy(i+3) = dy(i+3)+da*dx(i+3)
				}
			}
return
end



subroutine  dcopy(n,dx,incx,dy,incy)
double precision dx(*),dy(*)
integer i,incx,incy,ix,iy,m,mp1,n
if (n>0)
	if (incx!=1||incy!=1) {
		ix = 1
		iy = 1
		if (incx<0)
			ix = (-n+1)*incx+1
		if (incy<0)
			iy = (-n+1)*incy+1
		do i = 1,n {
			dy(iy) = dx(ix)
			ix = ix+incx
			iy = iy+incy
			}
		}
	else {
		m = mod(n,7)
		if (m!=0) {
			do i = 1,m
				dy(i) = dx(i)
			if (n<7)
				return
			}
		mp1 = m+1
		do i = mp1,n,7 {
			dy(i) = dx(i)
			dy(i+1) = dx(i+1)
			dy(i+2) = dx(i+2)
			dy(i+3) = dx(i+3)
			dy(i+4) = dx(i+4)
			dy(i+5) = dx(i+5)
			dy(i+6) = dx(i+6)
			}
		}
return
end



double precision function ddot(n,dx,incx,dy,incy)
double precision dx(*),dy(*),dtemp
integer i,incx,incy,ix,iy,m,mp1,n
ddot = 0.0d0
dtemp = 0.0d0
if (n>0)
	if (incx==1&&incy==1) {
		m = mod(n,5)
		if (m!=0) {
			do i = 1,m
				dtemp = dtemp+dx(i)*dy(i)
			if (n<5)
				go to 10
			}
		mp1 = m+1
		do i = mp1,n,5
			dtemp = dtemp+dx(i)*dy(i)+dx(i+1)*dy(i+1)+dx(i+2)*dy(i+2)+dx(i+3)*dy(i+3)+dx(i+4)*dy(i+4)
		10  ddot = dtemp
		}
	else {
		ix = 1
		iy = 1
		if (incx<0)
			ix = (-n+1)*incx+1
		if (incy<0)
			iy = (-n+1)*incy+1
		do i = 1,n {
			dtemp = dtemp+dx(ix)*dy(iy)
			ix = ix+incx
			iy = iy+incy
			}
		ddot = dtemp
		}
return
end



double precision function dnrm2(n,dx,incx)
integer          nst
double precision   dx(*),cutlo,cuthi,hitest,sum,xmax,zero,one
data   zero,one/0.0d0,1.0d0/
data cutlo,cuthi/8.232d-11,1.304d19/
if (n<=0)
	dnrm2 = zero
else {
	nst = 20
	sum = zero
	nn = n*incx
	i = 1
	repeat {
                if (nst == 20) {
                    goto 20
                } else if (nst == 30) {
                    goto 30
                } else if (nst == 40) {
                    goto 40
                } else if (nst == 80) {
                    goto 80
                }
		20  if (dabs(dx(i))>cutlo)
                    go to 50
		nst = 30
		xmax = zero
		30  if (dx(i)==zero)
                    go to 100
		if (dabs(dx(i))>cutlo)
			go to 50
		nst = 40
		go to 70
		40  if (dabs(dx(i))<=cutlo)
			go to 80
		sum = (sum*xmax)*xmax
		50  hitest = cuthi/float(n)
		do j = i,nn,incx {
			if (dabs(dx(j))>=hitest)
				go to 60
			sum = sum+dx(j)**2
			}
		break 1
		60  i = j
		nst = 80
		sum = (sum/dx(i))/dx(i)
		70  xmax = dabs(dx(i))
		go to 90
		80  if (dabs(dx(i))>xmax) {
			sum = one+sum*(xmax/dx(i))**2
			xmax = dabs(dx(i))
			go to 100
			}
		90  sum = sum+(dx(i)/xmax)**2
		100  i = i+incx
		if (i>nn)
			go to 110
		}
	dnrm2 = dsqrt(sum)
	return
	110  dnrm2 = xmax*dsqrt(sum)
	}
return
end



subroutine  dscal(n,da,dx,incx)
double precision da,dx(*)
integer i,incx,m,mp1,n,nincx
if (n>0)
	if (incx!=1) {
		nincx = n*incx
		do i = 1,nincx,incx
			dx(i) = da*dx(i)
		}
	else {
		m = mod(n,5)
		if (m!=0) {
			do i = 1,m
				dx(i) = da*dx(i)
			if (n<5)
				return
			}
		mp1 = m+1
		do i = mp1,n,5 {
			dx(i) = da*dx(i)
			dx(i+1) = da*dx(i+1)
			dx(i+2) = da*dx(i+2)
			dx(i+3) = da*dx(i+3)
			dx(i+4) = da*dx(i+4)
			}
		}
return
end



subroutine  dswap(n,dx,incx,dy,incy)
double precision dx(*),dy(*),dtemp
integer i,incx,incy,ix,iy,m,mp1,n
if (n>0)
	if (incx!=1||incy!=1) {
		ix = 1
		iy = 1
		if (incx<0)
			ix = (-n+1)*incx+1
		if (incy<0)
			iy = (-n+1)*incy+1
		do i = 1,n {
			dtemp = dx(ix)
			dx(ix) = dy(iy)
			dy(iy) = dtemp
			ix = ix+incx
			iy = iy+incy
			}
		}
	else {
		m = mod(n,3)
		if (m!=0) {
			do i = 1,m {
				dtemp = dx(i)
				dx(i) = dy(i)
				dy(i) = dtemp
				}
			if (n<3)
				return
			}
		mp1 = m+1
		do i = mp1,n,3 {
			dtemp = dx(i)
			dx(i) = dy(i)
			dy(i) = dtemp
			dtemp = dx(i+1)
			dx(i+1) = dy(i+1)
			dy(i+1) = dtemp
			dtemp = dx(i+2)
			dx(i+2) = dy(i+2)
			dy(i+2) = dtemp
			}
		}
return
end



subroutine dshift(x,ldx,n,j,k)
integer ldx,n,j,k
double precision x(ldx,k),tt
integer i,jj
if (k>j)
	do i = 1,n {
		tt = x(i,j)
		do jj = j+1,k
			x(i,jj-1) = x(i,jj)
		x(i,k) = tt
		}
return
end



subroutine  rtod(dx,dy,n)
real dx(*)
double precision dy(*)
integer i,m,mp1,n
if (n>0) {
	m = mod(n,7)
	if (m!=0) {
		do i = 1,m
			dy(i) = dx(i)
		if (n<7)
			return
		}
	mp1 = m+1
	do i = mp1,n,7 {
		dy(i) = dx(i)
		dy(i+1) = dx(i+1)
		dy(i+2) = dx(i+2)
		dy(i+3) = dx(i+3)
		dy(i+4) = dx(i+4)
		dy(i+5) = dx(i+5)
		dy(i+6) = dx(i+6)
		}
	}
return
end



subroutine  dtor(dx,dy,n)
double precision dx(*)
real dy(*)
integer i,m,mp1,n
if (n>0) {
	m = mod(n,7)
	if (m!=0) {
		do i = 1,m
			dy(i) = dx(i)
		if (n<7)
			return
		}
	mp1 = m+1
	do i = mp1,n,7 {
		dy(i) = dx(i)
		dy(i+1) = dx(i+1)
		dy(i+2) = dx(i+2)
		dy(i+3) = dx(i+3)
		dy(i+4) = dx(i+4)
		dy(i+5) = dx(i+5)
		dy(i+6) = dx(i+6)
		}
	}
return
end



subroutine  drot(n,dx,incx,dy,incy,c,s)
double precision dx(*),dy(*),dtemp,c,s
integer i,incx,incy,ix,iy,n
if (n>0)
	if (incx==1&&incy==1)
		do i = 1,n {
			dtemp = c*dx(i)+s*dy(i)
			dy(i) = c*dy(i)-s*dx(i)
			dx(i) = dtemp
			}
	else {
		ix = 1
		iy = 1
		if (incx<0)
			ix = (-n+1)*incx+1
		if (incy<0)
			iy = (-n+1)*incy+1
		do i = 1,n {
			dtemp = c*dx(ix)+s*dy(iy)
			dy(iy) = c*dy(iy)-s*dx(ix)
			dx(ix) = dtemp
			ix = ix+incx
			iy = iy+incy
			}
		}
return
end



subroutine drotg(da,db,c,s)
double precision da,db,c,s,roe,scale,r,z
roe = db
if (dabs(da)>dabs(db))
	roe = da
scale = dabs(da)+dabs(db)
if (scale==0.0d0) {
	c = 1.0d0
	s = 0.0d0
	r = 0.0d0
	}
else {
	r = scale*dsqrt((da/scale)**2+(db/scale)**2)
	r = dsign(1.0d0,roe)*r
	c = da/r
	s = db/r
	}
z = 1.0d0
if (dabs(da)>dabs(db))
	z = s
if (dabs(db)>=dabs(da)&&c!=0.0d0)
	z = 1.0d0/c
da = r
db = z
return
end



subroutine dqrsl(x,ldx,n,k,qraux,y,qy,qty,b,rsd,xb,job,info)
integer ldx,n,k,job,info
double precision x(ldx,*),qraux(*),y(*),qy(*),qty(*),b(*),rsd(*),xb(*)
integer i,j,jj,ju,kp1
double precision ddot,t,temp
logical cb,cqy,cqty,cr,cxb
info = 0
cqy = job/10000!=0
cqty = mod(job,10000)!=0
cb = mod(job,1000)/100!=0
cr = mod(job,100)/10!=0
cxb = mod(job,10)!=0
ju = min0(k,n-1)
if (ju==0) {
	if (cqy)
		qy(1) = y(1)
	if (cqty)
		qty(1) = y(1)
	if (cxb)
		xb(1) = y(1)
	if (cb)
		if (x(1,1)!=0.0d0)
			b(1) = y(1)/x(1,1)
		else
			info = 1
	if (cr)
		rsd(1) = 0.0d0
	}
else {
	if (cqy)
		call dcopy(n,y,1,qy,1)
	if (cqty)
		call dcopy(n,y,1,qty,1)
	if (cqy)
		do jj = 1,ju {
			j = ju-jj+1
			if (qraux(j)!=0.0d0) {
				temp = x(j,j)
				x(j,j) = qraux(j)
				t = -ddot(n-j+1,x(j,j),1,qy(j),1)/x(j,j)
				call daxpy(n-j+1,t,x(j,j),1,qy(j),1)
				x(j,j) = temp
				}
			}
	if (cqty)
		do j = 1,ju
			if (qraux(j)!=0.0d0) {
				temp = x(j,j)
				x(j,j) = qraux(j)
				t = -ddot(n-j+1,x(j,j),1,qty(j),1)/x(j,j)
				call daxpy(n-j+1,t,x(j,j),1,qty(j),1)
				x(j,j) = temp
				}
	if (cb)
		call dcopy(k,qty,1,b,1)
	kp1 = k+1
	if (cxb)
		call dcopy(k,qty,1,xb,1)
	if (cr&&k<n)
		call dcopy(n-k,qty(kp1),1,rsd(kp1),1)
	if (cxb&&kp1<=n)
		do i = kp1,n
			xb(i) = 0.0d0
	if (cr)
		do i = 1,k
			rsd(i) = 0.0d0
	if (cb) {
		do jj = 1,k {
			j = k-jj+1
			if (x(j,j)==0.0d0)
				go to 130
			b(j) = b(j)/x(j,j)
			if (j!=1) {
				t = -b(j)
				call daxpy(j-1,t,x(1,j),1,b,1)
				}
			}
		go to 140
		130  info = j
		}
	140  if (cr||cxb)
		do jj = 1,ju {
			j = ju-jj+1
			if (qraux(j)!=0.0d0) {
				temp = x(j,j)
				x(j,j) = qraux(j)
				if (cr) {
					t = -ddot(n-j+1,x(j,j),1,rsd(j),1)/x(j,j)
					call daxpy(n-j+1,t,x(j,j),1,rsd(j),1)
					}
				if (cxb) {
					t = -ddot(n-j+1,x(j,j),1,xb(j),1)/x(j,j)
					call daxpy(n-j+1,t,x(j,j),1,xb(j),1)
					}
				x(j,j) = temp
				}
			}
	}
return
end

subroutine dsvdc(x,ldx,n,p,s,e,u,ldu,v,ldv,work,job,info)
integer ldx,n,p,ldu,ldv,job,info
double precision x(ldx,*),s(*),e(*),u(ldu,*),v(ldv,*),work(*)
integer i,iter,j,jobu,k,kase,kk,l,ll,lls,lm1,lp1,ls,lu,m,maxit,mm,mm1,mp1,nct,nctp1,ncu,nrt,nrtp1
double precision ddot,t
double precision b,c,cs,el,emm1,f,g,dnrm2,scale,shift,sl,sm,sn,smm1,t1,test,ztest
logical wantu,wantv
maxit = 30
wantu = .false.
wantv = .false.
jobu = mod(job,100)/10
ncu = n
if (jobu>1)
	ncu = min0(n,p)
if (jobu!=0)
	wantu = .true.
if (mod(job,10)!=0)
	wantv = .true.
info = 0
nct = min0(n-1,p)
nrt = max0(0,min0(p-2,n))
lu = max0(nct,nrt)
if (lu>=1)
	do l = 1,lu {
		lp1 = l+1
		if (l<=nct) {
			s(l) = dnrm2(n-l+1,x(l,l),1)
			if (s(l)!=0.0d0) {
				if (x(l,l)!=0.0d0)
					s(l) = dsign(s(l),x(l,l))
				call dscal(n-l+1,1.0d0/s(l),x(l,l),1)
				x(l,l) = 1.0d0+x(l,l)
				}
			s(l) = -s(l)
			}
		if (p>=lp1)
			do j = lp1,p {
				if (l<=nct)
					if (s(l)!=0.0d0) {
						t = -ddot(n-l+1,x(l,l),1,x(l,j),1)/x(l,l)
						call daxpy(n-l+1,t,x(l,l),1,x(l,j),1)
						}
				e(j) = x(l,j)
				}
		if (wantu&&l<=nct)
			do i = l,n
				u(i,l) = x(i,l)
		if (l<=nrt) {
			e(l) = dnrm2(p-l,e(lp1),1)
			if (e(l)!=0.0d0) {
				if (e(lp1)!=0.0d0)
					e(l) = dsign(e(l),e(lp1))
				call dscal(p-l,1.0d0/e(l),e(lp1),1)
				e(lp1) = 1.0d0+e(lp1)
				}
			e(l) = -e(l)
			if (lp1<=n&&e(l)!=0.0d0) {
				do i = lp1,n
					work(i) = 0.0d0
				do j = lp1,p
					call daxpy(n-l,e(j),x(lp1,j),1,work(lp1),1)
				do j = lp1,p
					call daxpy(n-l,-e(j)/e(lp1),work(lp1),1,x(lp1,j),1)
				}
			if (wantv)
				do i = lp1,p
					v(i,l) = e(i)
			}
		}
m = min0(p,n+1)
nctp1 = nct+1
nrtp1 = nrt+1
if (nct<p)
	s(nctp1) = x(nctp1,nctp1)
if (n<m)
	s(m) = 0.0d0
if (nrtp1<m)
	e(nrtp1) = x(nrtp1,m)
e(m) = 0.0d0
if (wantu) {
	if (ncu>=nctp1)
		do j = nctp1,ncu {
			do i = 1,n
				u(i,j) = 0.0d0
			u(j,j) = 1.0d0
			}
	if (nct>=1)
		do ll = 1,nct {
			l = nct-ll+1
			if (s(l)==0.0d0) {
				do i = 1,n
					u(i,l) = 0.0d0
				u(l,l) = 1.0d0
				}
			else {
				lp1 = l+1
				if (ncu>=lp1)
					do j = lp1,ncu {
						t = -ddot(n-l+1,u(l,l),1,u(l,j),1)/u(l,l)
						call daxpy(n-l+1,t,u(l,l),1,u(l,j),1)
						}
				call dscal(n-l+1,-1.0d0,u(l,l),1)
				u(l,l) = 1.0d0+u(l,l)
				lm1 = l-1
				if (lm1>=1)
					do i = 1,lm1
						u(i,l) = 0.0d0
				}
			}
	}
if (wantv)
	do ll = 1,p {
		l = p-ll+1
		lp1 = l+1
		if (l<=nrt)
			if (e(l)!=0.0d0)
				do j = lp1,p {
					t = -ddot(p-l,v(lp1,l),1,v(lp1,j),1)/v(lp1,l)
					call daxpy(p-l,t,v(lp1,l),1,v(lp1,j),1)
					}
		do i = 1,p
			v(i,l) = 0.0d0
		v(l,l) = 1.0d0
		}
mm = m
iter = 0
repeat {
	if (m==0)
		return
	if (iter>=maxit)
		break 1
	do ll = 1,m {
		l = m-ll
		if (l==0)
			break 1
		test = dabs(s(l))+dabs(s(l+1))
		ztest = test+dabs(e(l))
		if (ztest==test)
			go to 150
		}
	go to 160
	150  e(l) = 0.0d0
	160  if (l==m-1)
		kase = 4
	else {
		lp1 = l+1
		mp1 = m+1
		do lls = lp1,mp1 {
			ls = m-lls+lp1
			if (ls==l)
				break 1
			test = 0.0d0
			if (ls!=m)
				test = test+dabs(e(ls))
			if (ls!=l+1)
				test = test+dabs(e(ls-1))
			ztest = test+dabs(s(ls))
			if (ztest==test)
				go to 170
			}
		go to 180
		170  s(ls) = 0.0d0
		180  if (ls==l)
			kase = 3
		else if (ls==m)
			kase = 1
		else {
			kase = 2
			l = ls
			}
		}
	l = l+1
	switch(kase) {
		case 1:
			mm1 = m-1
			f = e(m-1)
			e(m-1) = 0.0d0
			do kk = l,mm1 {
				k = mm1-kk+l
				t1 = s(k)
				call drotg(t1,f,cs,sn)
				s(k) = t1
				if (k!=l) {
					f = -sn*e(k-1)
					e(k-1) = cs*e(k-1)
					}
				if (wantv)
					call drot(p,v(1,k),1,v(1,m),1,cs,sn)
				}
		case 2:
			f = e(l-1)
			e(l-1) = 0.0d0
			do k = l,m {
				t1 = s(k)
				call drotg(t1,f,cs,sn)
				s(k) = t1
				f = -sn*e(k)
				e(k) = cs*e(k)
				if (wantu)
					call drot(n,u(1,k),1,u(1,l-1),1,cs,sn)
				}
		case 3:
			scale = dmax1(dabs(s(m)),dabs(s(m-1)),dabs(e(m-1)),dabs(s(l)),dabs(e(l)))
			sm = s(m)/scale
			smm1 = s(m-1)/scale
			emm1 = e(m-1)/scale
			sl = s(l)/scale
			el = e(l)/scale
			b = ((smm1+sm)*(smm1-sm)+emm1**2)/2.0d0
			c = (sm*emm1)**2
			shift = 0.0d0
			if (b!=0.0d0||c!=0.0d0) {
				shift = dsqrt(b**2+c)
				if (b<0.0d0)
					shift = -shift
				shift = c/(b+shift)
				}
			f = (sl+sm)*(sl-sm)+shift
			g = sl*el
			mm1 = m-1
			do k = l,mm1 {
				call drotg(f,g,cs,sn)
				if (k!=l)
					e(k-1) = f
				f = cs*s(k)+sn*e(k)
				e(k) = cs*e(k)-sn*s(k)
				g = sn*s(k+1)
				s(k+1) = cs*s(k+1)
				if (wantv)
					call drot(p,v(1,k),1,v(1,k+1),1,cs,sn)
				call drotg(f,g,cs,sn)
				s(k) = f
				f = cs*e(k)+sn*s(k+1)
				s(k+1) = -sn*e(k)+cs*s(k+1)
				g = sn*e(k+1)
				e(k+1) = cs*e(k+1)
				if (wantu&&k<n)
					call drot(n,u(1,k),1,u(1,k+1),1,cs,sn)
				}
			e(m-1) = f
			iter = iter+1
		case 4:
			if (s(l)<0.0d0) {
				s(l) = -s(l)
				if (wantv)
					call dscal(p,-1.0d0,v(1,l),1)
				}
			while (l!=mm) {
				if (s(l)>=s(l+1))
					break 1
				t = s(l)
				s(l) = s(l+1)
				s(l+1) = t
				if (wantv&&l<p)
					call dswap(p,v(1,l),1,v(1,l+1),1)
				if (wantu&&l<n)
					call dswap(n,u(1,l),1,u(1,l+1),1)
				l = l+1
				}
			iter = 0
			m = m-1
		}
	}
info = m
return
end

subroutine dbksl(x,p,k,b,q,info)
integer p,k,q,info
double precision x(p,p),b(p,q)
double precision t; integer j,l
info = 0
for(j=k; j>0; j = j-1) {
	if (x(j,j)==0.0d0)
		{info = j; break}
	for(l=1; l<=q; l = l+1) {
	b(j,l) = b(j,l)/x(j,j)
	if (j!=1) {
		t = -b(j,l)
		call daxpy(j-1,t,x(1,j),1,b(1,l),1)
		}
	}
}
return
end

subroutine dtrsl(t,ldt,n,b,job,info)
integer ldt,n,job,info
double precision t(ldt,*),b(*)
double precision ddot,temp
integer which,j,jj
#        check for zero diagonal elements.
do info = 1,n
	if (t(info,info)==0.0d0)
		return
info = 0
#        determine the task and go to it.
which = 1
if (mod(job,10)!=0)
	which = 2
if (mod(job,100)/10!=0)
	which = which+2
switch(which) {
	case 1:
		b(1) = b(1)/t(1,1)
		if (n>=2)
			do j = 2,n {
				temp = -b(j-1)
				call daxpy(n-j+1,temp,t(j,j-1),1,b(j),1)
				b(j) = b(j)/t(j,j)
				}
	case 2:
		b(n) = b(n)/t(n,n)
		if (n>=2)
			do jj = 2,n {
				j = n-jj+1
				temp = -b(j+1)
				call daxpy(j,temp,t(1,j+1),1,b(1),1)
				b(j) = b(j)/t(j,j)
				}
	case 3:
		b(n) = b(n)/t(n,n)
		if (n>=2)
			do jj = 2,n {
				j = n-jj+1
				b(j) = b(j)-ddot(jj-1,t(j+1,j),1,b(j+1),1)
				b(j) = b(j)/t(j,j)
				}
	case 4:
		b(1) = b(1)/t(1,1)
		if (n>=2)
			do j = 2,n {
				b(j) = b(j)-ddot(j-1,t(1,j),1,b(1),1)
				b(j) = b(j)/t(j,j)
				}
	}
return
end
