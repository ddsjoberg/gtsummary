// Copyright (C) 2013-2016 Kasper Kristensen
// License: GPL-2

namespace atomic {

/* Workhorse version (used while sweeping - pre-allocated output) */
Eigen::MatrixXd convol2d_work(const Eigen::MatrixXd& x,
                              const Eigen::MatrixXd& K) CSKIP({
  int kr = K.rows();
  int kc = K.cols();
  Eigen::MatrixXd y(x.rows() - kr + 1, x.cols() - kc + 1);
  for (int i = 0; i < y.rows(); i++)
    for (int j = 0; j < y.cols(); j++)
      y(i, j) = (x.block(i, j, kr, kc).array() * K.array()).sum();
  return y;
})

/* Forward declare version used while taping (for AD types) */
template <class DerivedA, class DerivedB>
matrix<typename DerivedA::Scalar>
convol2d(const Eigen::MatrixBase<DerivedA>& x,
         const Eigen::MatrixBase<DerivedB>& K);

TMB_ATOMIC_VECTOR_FUNCTION(
    // ATOMIC_NAME
    convol2d,
    // OUTPUT_DIM
    (CppAD::Integer(tx[0]) - CppAD::Integer(tx[2]) + 1) *
        (CppAD::Integer(tx[1]) - CppAD::Integer(tx[3]) + 1),
    // ATOMIC_DOUBLE
    typedef TypeDefs<double>::MapMatrix MapMatrix_t;
    typedef TypeDefs<double>::ConstMapMatrix ConstMapMatrix_t;
    int nx1 = CppAD::Integer(tx[0]); int nx2 = CppAD::Integer(tx[1]);
    int nk1 = CppAD::Integer(tx[2]); int nk2 = CppAD::Integer(tx[3]);
    int ny1 = nx1 - nk1 + 1; int ny2 = nx2 - nk2 + 1;
    ConstMapMatrix_t X(&tx[4            ], nx1, nx2);
    ConstMapMatrix_t K(&tx[4 + nx1 * nx2], nk1, nk2);
    MapMatrix_t      Y(&ty[0            ], ny1, ny2);
    Y = convol2d_work(X, K);
    ,
    // ATOMIC_REVERSE
    typedef typename TypeDefs<Type>::MapMatrix MapMatrix_t;
    typedef typename TypeDefs<Type>::ConstMapMatrix ConstMapMatrix_t;
    int nx1 = CppAD::Integer(tx[0]); int nx2 = CppAD::Integer(tx[1]);
    int nk1 = CppAD::Integer(tx[2]); int nk2 = CppAD::Integer(tx[3]);
    int ny1 = nx1 - nk1 + 1; int ny2 = nx2 - nk2 + 1;
    ConstMapMatrix_t X(&tx[4            ], nx1, nx2);
    ConstMapMatrix_t K(&tx[4 + nx1 * nx2], nk1, nk2);
    ConstMapMatrix_t Y(&ty[0            ], ny1, ny2);
    ConstMapMatrix_t W(&py[0            ], ny1, ny2);
    matrix<Type> Kflip = K.reverse();
    matrix<Type> Wexpand(W.rows() + 2 * (K.rows() - 1),
                         W.cols() + 2 * (K.cols() - 1));
    Wexpand.setZero();
    Wexpand.block(K.rows() - 1, K.cols() - 1, W.rows(), W.cols()) = W;
    MapMatrix_t P0(&px[0            ],   1,   4);
    MapMatrix_t PX(&px[4            ], nx1, nx2);
    MapMatrix_t PK(&px[4 + nx1 * nx2], nk1, nk2);
    P0.setZero();
    PX = convol2d(Wexpand, Kflip);
    PK = convol2d(      X,     W);
)

/* Implementation of the forward declared version */
template <class DerivedA, class DerivedB>
matrix<typename DerivedA::Scalar> convol2d(
    const Eigen::MatrixBase<DerivedA>& x,
    const Eigen::MatrixBase<DerivedB>& K) {
  typedef typename DerivedA::Scalar Type;
  CppAD::vector<Type> arg(4 + x.size() + K.size());
  arg[0] = x.rows(); arg[1] = x.cols();
  arg[2] = K.rows(); arg[3] = K.cols();
  for (int i = 0; i < x.size(); i++) {
    arg[4 + i] = x(i);
  }
  for (int i = 0; i < K.size(); i++) {
    arg[4 + i + x.size()] = K(i);
  }
  CppAD::vector<Type> res = convol2d(arg);
  matrix<Type> y = vec2mat(res, x.rows() - K.rows() + 1, x.cols() - K.cols() + 1);
  return y;
}

}
