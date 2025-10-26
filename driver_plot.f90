! ============================
! Esempio d'uso
! ============================
program demo_multi
  use html_plot_mod
  implicit none
  integer :: n, m, i
  real(8), allocatable :: x(:), y(:,:)
  character(len=20), dimension(3) :: labs

  n = 50
  m = 3
  allocate(x(n), y(n,m))
  do i = 1, n
    x(i) = (i-1)*0.2d0
    y(i,1) = sin(x(i))
    y(i,2) = cos(x(i))
    y(i,3) = 0.5d0*sin(2.0d0*x(i)) + 0.2d0
  end do
  labs = ['Seno','Coseno','Doppia f.']

  call write_xy_html('plot', x, y, n, m, 'grafico_plot_multi.html', 'Tempo [s]', 'Valore', labs)
  call write_xy_html('hist', x, y, n, m, 'grafico_hist_multi.html', '','', labs)

  print *, 'Creati: grafico_plot_multi.html e grafico_hist_multi.html'
  deallocate(x,y)
end program demo_multi