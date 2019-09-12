module mod_cublas_sgemm

  use iso_fortran_env, only: rk => real32
  use cudafor
  use cublas

  implicit none

  private

  public :: cublas_sgemm, matmul

  contains

    subroutine cublas_sgemm (a, b, c, transa, transb, alpha, beta)

      ! Input/output variables
      real(rk),     intent(in)           :: a(:, :), b(:, :)
      real(rk),     intent(inout)        :: c(:, :)
      real(rk),     intent(in), optional :: alpha, beta
      character(1), intent(in), optional :: transa, transb

      ! Local variables
      real(rk) :: o_alpha, o_beta
      integer  :: m, n, k, lda, ldb, ldc

      ! GPU variables
      type(cublasHandle)            :: handle
      real(rk), device, allocatable :: dA(:, :), dB(:, :), dC(:, :)
      integer                       :: itransa, itransb
      integer                       :: ierr, istat

      intrinsic max, present, size

      if ( present(alpha) ) then
        o_alpha = alpha
      else
        o_alpha = 1
      endif

      if ( present(beta) ) then
        o_beta = beta
      else
        o_beta = 0
      endif

      if ( present(transa) ) then
        if ( transa == 'N' .or. transa == 'n' ) then
          itransa = 0
        else
          itransa = 1
        end if
      else
          itransa = 0
      endif

      if ( present(transb) ) then
        if ( transb == 'N' .or. transb == 'n' ) then
          itransb = 0
        else
          itransb = 1
        end if
      else
          itransb = 0
      endif

      if ( itransa == 0 ) then
          k = size(a, 2)
      else
          k = size(a, 1)
      endif

      lda = max( 1, size(a, 1) )
      ldb = max( 1, size(b, 1) )
      ldc = max( 1, size(c, 1) )

      m = size(c, 1)
      n = size(c, 2)

      allocate ( dA, source = a )
      allocate ( dB, source = b )
      allocate ( dC, source = c )

      handle = cublasGetHandle ()

      istat = cublasSgemm_v2 (handle, itransa, itransb, &
                              m, n, k, o_alpha, dA, lda, dB, ldb, o_beta, dC, ldc)

      ierr = cudathreadsynchronize ()

      c = dC

    end subroutine

    function matmul (a, b) result (c)

      real(rk), intent(in)  :: a(:, :), b(:, :)
      real(rk), allocatable :: c(:, :)

      allocate ( c(size(a, 1), size(b, 2)) )

      call cublas_sgemm (a, b, c, 'N', 'N', 1.0e0, 0.0e0)

    end function

end module

program test_cublas

  use iso_fortran_env, only: rk => real32
  use mod_cublas_sgemm

  implicit none

  real(rk), allocatable :: a(:, :), b(:, :), c(:, :)
  integer i, j, m, n, k

  m = 8000
  n = 6000
  k = 5000

  allocate ( a(m, k), source = 1.0e0 )
  allocate ( b(k, n), source = 2.0e0 )
  !allocate ( c(m, n), source = 3.0e0 )

  !call cublas_sgemm (a, b, c, 'N', 'N', 1.0e0, 0.0e0)
  c = matmul(a, b)

  print *, repeat('=', 80)
  do i = 1, 5
    print '(*(f16.2))', [( c(i, j), j = 1, 5 )]
  end do
  print *, repeat('=', 80)

end
