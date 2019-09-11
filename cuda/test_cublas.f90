module mod_gemm

  use cudafor
  use cublas

  implicit none

  private

  public :: gemm

  contains

    subroutine gemm (a, b, c, transa, transb, alpha, beta)

      real(4),      intent(in)           :: a(:, :), b(:, :)
      real(4),      intent(inout)        :: c(:, :)
      real(4),      intent(in), optional :: alpha, beta
      real(4)                            :: o_alpha, o_beta
      character(1), intent(in), optional :: transa, transb
      integer                            :: m, n, k, lda, ldb, ldc

      ! GPU
      type(cublasHandle)           :: handle
      real(4), device, allocatable :: dA(:, :), dB(:, :), dC(:, :)
      integer                      :: itransa, itransb
      integer                      :: ierr, istat

      intrinsic max, present, size

      IF(PRESENT(ALPHA)) THEN
          O_ALPHA = ALPHA
      ELSE
          O_ALPHA = 1
      ENDIF

      IF(PRESENT(BETA)) THEN
          O_BETA = BETA
      ELSE
          O_BETA = 0
      ENDIF

      IF(PRESENT(TRANSA)) THEN
        if (transa == 'N' .or. transa == 'n') then
          itransa = 0
        else
          itransa = 1
        end if
      ELSE
          itransa = 0
      ENDIF

      IF(PRESENT(TRANSB)) THEN
        if (transb == 'N' .or. transb == 'n') then
          itransb = 0
        else
          itransb = 1
        end if
      ELSE
          itransb = 0
      ENDIF

      IF(itransa == 0) THEN
          K = SIZE(A,2)
      ELSE
          K = SIZE(A,1)
      ENDIF

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

end module

program test_cublas

  use mod_gemm

  implicit none

  real(4), allocatable :: a(:, :), b(:, :), c(:, :)
  integer i, j, n

  n = 8000

  allocate ( a(n, n), source = 1.0e0 )
  allocate ( b(n, n), source = 2.0e0 )
  allocate ( c(n, n), source = 3.0e0 )

  call gemm (a, b, c, 'N', 'N', 1.0e0, 0.0e0)

  print *, repeat ('=', 80)
  do i = 1, 5
    print '(*(f16.2))', [(c(i, j), j = 1, 5)]
  end do
  print *, repeat ('=', 80)

end
