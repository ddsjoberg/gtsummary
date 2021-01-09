subroutine sknotl(x,n,knot,k)
implicit double precision(a-h,o-z) 
double precision x(n),knot(n+6),a1,a2,a3,a4
integer     n,k,ndk,j


     # Allocate knots acording to the cutoffs given below


	# Cutoff constants

	 a1 = log(50d0)/log(2d0)  ; a2 = log(100d0)/log(2d0)
	 a3 = log(140d0)/log(2d0) ; a4 = log(200d0)/log(2d0)

	# Cutoff Criteria

        if(n<50)                    { ndk = n }
        else if (n>=50  & n<200)    { ndk = 2.**(a1+(a2-a1)*(n-50.)/150.)  }
        else if (n>=200 & n<800)    { ndk = 2.**(a2+(a3-a2)*(n-200.)/600.)  }
        else if (n>=800 & n<3200)   { ndk = 2.**(a3+(a4-a3)*(n-800.)/2400.)  }
        else if (n>=3200)           { ndk = 200. + float(n-3200)**.2 }
		 
		 k = ndk + 6

     
     # Allocate Knots  ( note no account is taken of any weighting vector )

	do j=1,3   { knot(j) = x(1) }
	do j=1,ndk { knot(j+3) = x( 1 + (j-1)*(n-1)/(ndk-1) ) }
	do j=1,3   { knot(ndk+3+j) = x(n) }

return
end
subroutine splsm(x,y,w,n,match,nef,spar,dof,smo,s0,cov,ifcov,work)
#This subroutine performs a smoothing spline fit
# This was written by Trevor Hastie in 1990
# It has been modified from the S3 version by Trevor Hastie
# in July 2004, to accommodate the modified sbart routine in R
# and also to accommodate only the gam bakfit routine.
# Note that spar has changed, and we change it here to conform with
# the smooth.spline routine in R
#All arguments are either double precision or integer
#INPUT
#
#x	double length n ; x variable for smoothing
#y	double length n	; y variable for smoothing
#w	double length n ; weights for smoothing, > 0
#n	integer length above
#match 	integer length n -- in S language x[i] == sort(unique(x)[match[i]]
#		match is produced by subroutine namat
#nef	number of unique elements in x; so match has values between 1 and nef+1
#		missing data are given the match number nef+1
#spar	double smoothing parameter -1.5 <spar<1.5; default is 1
#dof	double  equivalent degrees of freedom
#		if dof is 0, spar is used
#               if 0< dof <1, dof = 1                         
#		if dof >=1, dof is used
#		note: dof does not use the constant term
#ifcov	integer if 1, the unscaled variance information is computed
#work	double workspace of length (10+2*4)*(nef+2)+5*nef+n+15
#
#OUTPUT
#
#x,y,w,n,match,nef  are untouched
#spar	if dof > 1, then spar is that which achieves dof
#dof	the dof of the fitted smooth. Note: even if dof was given
#		as 4, it will be returned as say 3.995 which is what
#		spar produces
#smo	double length n the fitted values, with weighted average 0
#s0	double weighted mean of y
#cov	double length nef the unscaled variance elements for the NONLINEAR
#		and UNIQUE part of smo, in the order of sort(unique(x))
#		cov is lev(i)/w(i) -h(i)/w where h(i) is the hat element from
#		the simple weighted least squares fit. This is passed on
#		to bakfit and used in gamcov
#
# splsm calls (eventually after some memory management dummy calls)
# sbart, the spline routine of Finbarr O'Sullivan, slightly modified
# by Trevor Hastie, 8/2/89	

implicit double precision(a-h,o-z)
double precision x(*),y(*),w(*),spar,dof,smo(*),s0,cov(*),work(*)
integer n,match(*),nef
integer ifcov
# work should be (10+2*ld4)*nk+5*nef+n+15 double
# ld4 =4  nk<= nef+2
call splsm1(x,y,w,n,match,nef,spar,dof,smo,s0,cov,ifcov,
#	xin(nef+1),yin(nef+1), win(nef+1),   knot(n+6),
	work(1),   work(nef+2),work(2*nef+3),work(3*nef+4),
	work(3*nef+n+10))
return
end

subroutine splsm1(x,y,w,n,match,nef,spar,dof,smo,s0,lev,ifcov,
		xin,yin,win,knot,
		work)
implicit double precision(a-h,o-z)
double precision x(*),y(*),w(*),spar,dof,smo(*),s0,lev(*),work(*)
integer n,match(*),nef
integer ifcov
double precision xin(nef+1),yin(nef+1),win(nef+1),knot(nef+6)
integer nk,ldnk,ld4,k
double precision xmin,xrange
call suff(n,nef,match,x,y,w,xin,yin,win,work(1))
xmin=xin(1)
xrange=xin(nef)-xin(1)
do i=1,nef {xin(i)=(xin(i)-xmin)/xrange}
call sknotl(xin,nef,knot,k)
nk=k-4
ld4=4
ldnk=1 # p21p nd ldnk is not used
call splsm2(x,y,w,n,match,nef,spar,dof,smo,s0,lev,ifcov,
		xin,yin,win,knot,
#       coef(nk),sout(nef+1),  levout(nef+1), xwy(nk),
#	hs0(nk),           hs1(nk),           hs2(nk),
#       hs3(nk),
#	sg0(nk),           sg1(nk),           sg2(nk),
#       sg3(nk),
#	abd(ld4,nk),   p1ip(ld4,nk),
#       p2ip(ldnk,nk)
	work(1), work(nk+1),   work(nk+nef+2),work(nk+2*nef+3),
	work(2*nk+2*nef+3),work(3*nk+2*nef+3),work(4*nk+2*nef+3),
	work(5*nk+2*nef+3),
	work(6*nk+2*nef+3),work(7*nk+2*nef+3),work(8*nk+2*nef+3),
	work(9*nk+2*nef+3),
	work(10*nk+2*nef+3),work((10+ld4)*nk+2*nef+3),
	work((10+2*ld4)*nk+2*nef+3),
	ld4,ldnk,nk)

return
end
subroutine splsm2(x,y,w,n,match,nef,spar,dof,smo,s0,lev,ifcov,
		xin,yin,win,knot,
		coef,sout,levout,xwy,
		hs0,hs1,hs2,hs3,
		sg0,sg1,sg2,sg3,
		abd,p1ip,p2ip,ld4,ldnk,nk)
implicit double precision(a-h,o-z)
double precision x(*),y(*),w(*),spar,dof,smo(*),s0,lev(*)
integer n,match(*),nef
integer nk,ldnk,ld4
integer ifcov
double precision xin(nef+1),yin(nef+1),win(nef+1),knot(nk+4)
double precision coef(nk),sout(nef+1),levout(nef+1),xwy(nk),
	 	hs0(nk),hs1(nk),hs2(nk),hs3(nk),
         	sg0(nk),sg1(nk),sg2(nk),sg3(nk),
	 	abd(ld4,nk),p1ip(ld4,nk),p2ip(ldnk,*)
#    local variables
integer ispar,icrit,isetup,ier
double precision lspar,uspar,tol,penalt,
		sumwin,dofoff,crit,xbar,dsum,xsbar
double precision yssw, eps
integer maxit
# yssw is an additional parameter introduced in R version of sbart
double precision wmean
crit=0d0
# Note we only allow limited options here
if(dof <= 0d0){
# use spar  
  ispar=1
  icrit=3
  dofoff=0d0
  }
else{
  if( dof < 1d0 )dof=1d0
  ispar=0
  icrit=3
  dofoff=dof+1d0
}
#Here we set some default parameters similar to the smooth.spline in R
isetup=0
ier=1
penalt=1d0
lspar= -1.5
uspar= 2.0
tol=1d-4
eps=2d-8
maxit=200
do i=1,nef
       sout(i)=yin(i)*yin(i)
sumwin=0d0
do i=1,nef 
	sumwin=sumwin+win(i)
yssw=wmean(nef,sout,win)
s0=wmean(n,y,w)
# which should be equal to wmean(nef,yin,win)
yssw=yssw*(sumwin-s0*s0)

call sbart(penalt,dofoff,xin,yin,win,yssw,nef,knot,nk,
		  coef,sout,levout,crit,
                  icrit,spar,ispar,maxit,
                  lspar,uspar,tol,eps,
		  isetup,
		  xwy,
		  hs0,hs1,hs2,hs3,
		  sg0,sg1,sg2,sg3,
		  abd,p1ip,p2ip,ld4,ldnk,ier)
#return
#now clean up 
do i=1,nef {
	win(i)=win(i)*win(i) #we sqrted them in sbart
	}
sbar=wmean(nef,sout,win)
xbar=wmean(nef,xin,win)


#now remove the linear leverage from the leverage for the smooths
# will be altered at this stage
do i=1,nef {lev(i)=(xin(i)-xbar)*sout(i)	}
xsbar=wmean(nef,lev,win)
do i=1,nef {lev(i)=(xin(i)-xbar)**2	}
dsum=wmean(nef,lev,win)
do i=1,nef {
	if(win(i)>0d0) {
		lev(i)=levout(i)/win(i)-1d0/sumwin -lev(i)/(sumwin*dsum)
		}
	else {lev(i)=0d0}
	}
dof=0d0
do i=1,nef {dof=dof+lev(i)*win(i)}
dof=dof+1d0
do i=1,nef
	sout(i)=sout(i)-sbar -(xin(i)-xbar)*xsbar/dsum
call unpck(n,nef,match,sout,smo)
return
end

double precision function wmean(n,y,w)
integer n
double precision y(n),w(n),wtot,wsum
wtot=0d0
wsum=0d0
do i=1,n{
	wsum=wsum+y(i)*w(i)
	wtot=wtot+w(i)
}
if(wtot > 0d0) {wmean=wsum/wtot} else {wmean=0d0}
return
end
