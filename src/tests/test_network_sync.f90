program test_network_sync
  use mod_network, only: network_type
  implicit none
  type(network_type) :: net

  net = network_type([5, 3, 2])

#ifdef CUDA
  print *, 1, net % layers(1) % w
#else
  print *, this_image(), net % layers(1) % w
#endif

end program test_network_sync
