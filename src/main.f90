program main
    use func_mod
    use ode_solver
    implicit none

    ! Inputs:
    real :: xi, Y0, xf, dx
    real, dimension(1,2) :: Y0_vector
    ! Outputs:
    real :: y
    real, dimension(1,2) :: y_vector
    ! Intermidiate global variable
    real :: x

    xi = 0
    Y0 = 1
    !Y0_vector(1,1) = -1 
    !Y0_vector(1,2) = 1 
    xf = 2  
    dx = 0.2

    call forth_runge_kutta(xi, Y0, xf, dx, y, x)
    print *, 'y(2) = ', y

    !call high_forth_runge_kutta(x0, Y0_vector, x, dx, y_vector)

    !print *, 'y(20) = ', y_vector(1,1)
    !print *, "y'(20) = ", y_vector(1,2)

end program main
