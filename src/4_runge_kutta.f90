program ODE_solver 
    use func_mod
    implicit none
    real :: x0, Y0, x, dx, y

    x0 = 0
    Y0 = 1
    x = 2
    dx = 0.2

    call forth_runge_kutta(x0, Y0, x, dx, y)

    print *, 'y(2) = ', y

    contains

        subroutine forth_runge_kutta(x0, Y0, x, dx, y_last)
            integer :: i, n_max
            real :: x0, Y0, x, dx
            real :: k1, k2, k3, k4, k5
            real :: f_k1, f_k2, f_k3, f_k4
            real :: y
            real, intent(out) :: y_last
            integer :: err_status
            character(256) :: err_iomsg
            
            n_max = int((x-X0)/dx)
            
            open(10, file='data.dat', status='new', iostat=err_status, iomsg=err_iomsg)
            if (err_status /= 0) then
                write(*, *) 'Error ', trim(err_iomsg)
            end if

            write(10, *) x0,',', Y0
            
            close(10)

            y = Y0
            do i=1, n_max, 1
                call first_ODE(x0, y, f_k1)
                k1 = dx*f_k1
                call first_ODE(x0 + 0.5*dx, y + 0.5*k1, f_k2)
                k2 = dx*f_k2
                call first_ODE(x0 + 0.5*dx, y + 0.5*k2, f_k3)
                k3 = dx*f_k3
                call first_ODE(x0 + dx, y + k3, f_k4)
                k4 = dx*f_k4

                y = y + (1.0/6.0)*(k1 + 2*k2 + 2* k3 + k4)
                x0 = x0 + dx

                open(11, file='data.dat', status='old', position='append')
                write(11, *) x0,',', y
                close(11)

            end do
            
            y_last = y

        end subroutine forth_runge_kutta 

end program ODE_solver 