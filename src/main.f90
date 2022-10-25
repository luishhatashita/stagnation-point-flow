program main
    use ode_solver_m
    implicit none
    
    integer, parameter :: nodes=101
    integer :: i, it, max_it
    double precision :: xi=0d+0, xf=4.8d+0, y0=1d+0
    double precision, dimension(nodes) :: domain
    double precision :: dx, a, b, error, error_accept, c
    double precision :: f_a, f_b, f_c
    double precision :: y_zero(3), y_end(3)
    integer :: fid1=10, fid2=11
    integer :: err_status
    character(256) :: err_iomsg

    ! Initialising known boundary conditions:
    ! F(0) = F'(0) = 0
    ! Y1(0) = Y2(0) = 0
    ! Y3(0) is not known, it's necessary to iterate to minimize Y3(4.8)-1=0.
    y_zero(1) = 0d+0
    y_zero(2) = 0d+0  

    ! Bisection method parameters:
    ! a, b - domain bounds
    ! error_accept - maximum tolerated error
    ! it, max_t - iteration and max iteration variables, respectively.
    a = 1d+0
    b = 2d+0
    error_accept = 1.0d-5
    it = 1
    max_it = 50

    ! Function call to create uniform domain with nodes.
    ! xi, xf - domain bounds
    ! dx, domain - output variables
    call create_domain(xi, xf, nodes, dx, domain)

    ! Error variable to be achieved.
    error = ABS(a-b)

    ! Bisection method, for the following function:
    ! g(Y3(0))-1=0, where g=Y3(4.8)
    print *, "Initialize the bisection method:"
    print *, "| i |", "| F |", "| F' |", "| F'' |" 
    do while (error > error_accept .and. it <= max_it)
        ! c at the half of a and b
        c = (a+b)/2.0

        ! Determining f(a)=g(a)-1=0
        y_zero(3) = a
        y_end = y_zero
        do i=2, nodes
            y_end = fourRK(y_end, dx)
        end do
        f_a = y_end(2) - 1.0

        ! Determining f(b)=g(b)-1=0
        y_zero(3) = b 
        y_end = y_zero
        do i=2, nodes
            y_end = fourRK(y_end, dx)
        end do
        f_b = y_end(2) - 1.0

        ! Determining f(c)=g(c)-1=0
        y_zero(3) = c 
        y_end = y_zero
        do i=2, nodes
            y_end = fourRK(y_end, dx)
        end do
        print *, it , y_end(1), y_end(2), y_end(3)
        f_c = y_end(2) - 1.0

        ! Verify whether the function has a root between the intervals.
        if (f_a * f_b >= 0) then
            print *, "No root or multiple roots, algorithm crash for the mentioned condition."
            exit
        else if (f_c * f_a < 0) then
            b = c
            error = ABS(a-b)
            it = it + 1
        else if (f_c * f_b < 0) then
            a = c
            error = ABS(a-b)
            it = it + 1
        else
            print *, "Unknown error"
            it = it + 1
        end if

    end do

    ! Final root:
    print *, "Y3(0) = ", c

    ! Writing the values for the found root
    open(fid1, file='stag_flow.csv', status='new', iostat=err_status, iomsg=err_iomsg)
    if (err_status /= 0) then
        write(*, *) 'Error ', trim(err_iomsg)
    end if
    ! Write the header of the file
    write(fid1, *) 'eta,F,F_prime,F_double_prime'
    close(fid1)

    ! Iterate over the domain again with the root found and save each iteration.
    y_zero(3) = c 
    y_end = y_zero
    open(fid2, file='stag_flow.csv', status='old', position='append')
    write(fid2, *) domain(1),',', y_end(1),',', y_end(2),',', y_end(3) 
    do i=2, nodes
        y_end = fourRK(y_end, dx)
        write(fid2, *) domain(i),',', y_end(1),',', y_end(2),',', y_end(3) 
    end do
    print *, "Final iteration results: ", y_end(1), y_end(2), y_end(3)

    close(fid2)
end program
