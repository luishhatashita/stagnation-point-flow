program main
    use func_m!, ode_solver_m
    implicit none
    
    real :: x = 1, y = 1
    integer, parameter :: nodes=11
    real :: xi=0, xf=2, y0=1
    real, dimension(nodes) :: domain

    call create_domain(xi, xf, nodes, domain)
    
end program
