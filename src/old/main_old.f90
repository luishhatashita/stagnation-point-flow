program main
    use func_mod
    use ode_solver
    implicit none

    ! Inputs:
    integer, parameter :: nodes=11
    real :: xi, Y0, xf
    real, dimension(1,2) :: Y0_vector
    ! Outputs:
    real, dimension(nodes) :: x, y 
    real, dimension(1,2) :: y_vector
    ! Intermidiate global variable

    xi = 0
    Y0 = 1
    !Y0_vector(1,1) = -1 
    !Y0_vector(1,2) = 1 
    xf = 2  
    dx = 0.2

    call forth_runge_kutta(xi, Y0, xf, nodes, x, y)
    print *, 'y(2) = ', y(nodes)

    !call high_forth_runge_kutta(x0, Y0_vector, x, dx, y_vector)

    !print *, 'y(20) = ', y_vector(1,1)
    !print *, "y'(20) = ", y_vector(1,2)

end program main
