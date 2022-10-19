program main
    use ode_solver_m
    implicit none
    
    !real :: x = 1, y = 1
    integer, parameter :: nodes=5
    integer :: i
    real :: xi=0, xf=5, y0=1
    real, dimension(nodes) :: domain
    real :: dx
    real :: y_zero(3), y_end(3)

    y_zero(1) = 0 
    y_zero(2) = 0 
    y_zero(3) = 1 

    !return_array = stag_flow(y_array)


    

    call create_domain(xi, xf, nodes, dx, domain)
    
    y_end = y_zero
    do i=1, nodes-1
        print *, "before 4rk", y_end(1), y_end(2), y_end(3)
        y_end = fourRK(y_end, dx)
        print *, "after 4rk", y_end(1), y_end(2), y_end(3)
    end do
end program
