#ifndef STAN__MATH__MATRIX__SPARSE_MULTIPLY_HPP
#define STAN__MATH__MATRIX__SPARSE_MULTIPLY_HPP

#include <vector>

#include <stan/math/matrix/Eigen.hpp>

namespace stan {

  namespace math {


    /** Return the multiplication of the sparse matrix spefcified by
     * by values and indexing by the specified dense vector.
     *
     * The sparse matrix X of dimension m by n is represented by the
     * vector w (of values), the integer array v (containing one-based
     * row index of each value), the integer array u (containing
     * one-based indexes of where each column starts in w), and the
     * integer array z (containing the number of non-zero entries in
     * each column of w).
     *
     * @tparam T1 Type of sparse matrix entries.
     * @tparam T2 Type of dense vector entries.
     * @param m Rows in matrix.
     * @param n Columns in matrix.
     * @param v One-based row index of each value.
     * @param u one-based index of where each column starts in w.
     * @param z number of non-zer entries in each column of w.
     * @return dense vector for the product.
     * @throw ... explain error conditions briefly ...
     */
    template <typename T1, typename T2>
    inline
    Eigen::Matrix<typename boost::math::tools::promote_args<T1, T2>::type, Eigen::Dynamic, 1>
    sparse_multiply(const int& m,
                    const int& n,
                    const Eigen::Matrix<T1, Eigen::Dynamic,1>& w,
                    const std::vector<int>& v,
                    const std::vector<int>& u,
                    const std::vector<int>& z,
                    const Eigen::Matrix<T2, Eigen::Dynamic,1>& b) {

      // a whole bunch of error checking goes here, e.g., m, n > 0,
      // etc. etc.  we can't throw index out of bounds exceptions at
      // run time

      typedef typename boost::math::tools::promote_args<T1, T2>::type fun_scalar_t;
      Eigen::Matrix<fun_scalar_t, Eigen::Dynamic, 1>  y(m);
      for (int j = 0; j < n; ++j) {
        int end = u[j] + z[j] - 1;
        for (int q = u[j] - 1; q < end; ++q)
          y(v[q]) += w[q] * b[j];
      }
      return y;
    }


  }

}





