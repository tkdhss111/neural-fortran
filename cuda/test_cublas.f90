program test_cublas

  use cudafor
  use cublas

  type(cublasHandle) :: h1
  real(4), device, allocatable :: dA(:, :), dB(:, :), dC(:, :)
  real(4),         allocatable :: a(:, :), b(:, :), c(:, :)
  real(4)                      :: alpha, beta
  integer i, j, k

  n     = 8000
  alpha = 1.0e0
  beta  = 0.0e0

  allocate ( a(n, n), source = 1.0e0 )
  allocate ( b(n, n), source = 2.0e0 )
  allocate ( c(n, n), source = 3.0e0 )

  allocate ( dA(n, n) )
  allocate ( dB(n, n) )
  allocate ( dC(n, n) )

  dA = a
  dB = b
  if (beta /= 0.0e0) dC = c

  h1 = cublasGetHandle ()

  istat = cublasSgemm_v2 (h1, 0, 0, n, n, n, alpha, dA, n, dB, n, beta, dC, n)

  ierr = cudathreadsynchronize ()

  c = dC

  print *, repeat ('=', 80)
  do i = 1, 5
    print '(*(f16.2))', [(c(i, j), j = 1, 5)]
  end do
  print *, repeat ('=', 80)

end
