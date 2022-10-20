program main
    use ode_solver_m
    implicit none
    
    !real :: x = 1, y = 1
    integer, parameter :: nodes=51
    integer :: i, it, max_it
    real :: xi=0, xf=4.8, y0=1
    real, dimension(nodes) :: domain
    real :: dx, a, b, error, error_accept, c
    real :: f_a, f_b, f_c
    real :: y_zero(3), y_end(3)
    integer :: fid1=10, fid2=11
    integer :: err_status
    character(256) :: err_iomsg

    y_zero(1) = 0 
    y_zero(2) = 0 

    a = 1
    b = 2
    error_accept = 0.001
    it = 1
    max_it = 50
    !y_zero(3) = 1.5

    !return_array = stag_flow(y_array)

    call create_domain(xi, xf, nodes, dx, domain)

    error = ABS(a-b)

    do while (error > error_accept .and. it <= max_it)
        c = (a+b)/2.0

        y_zero(3) = a
        y_end = y_zero
        do i=2, nodes
            !print *, "before 4rk", y_end(1), y_end(2), y_end(3)
            y_end = fourRK(y_end, dx)
        end do
        print *, "after 4rk", y_end(1), y_end(2), y_end(3)
        f_a = y_end(2) - 1

        y_zero(3) = b 
        y_end = y_zero
        do i=2, nodes
            !print *, "before 4rk", y_end(1), y_end(2), y_end(3)
            y_end = fourRK(y_end, dx)
        end do
        print *, "after 4rk", y_end(1), y_end(2), y_end(3)
        f_b = y_end(2) - 1

        y_zero(3) = c 
        y_end = y_zero
        do i=2, nodes
            !print *, "before 4rk", y_end(1), y_end(2), y_end(3)
            y_end = fourRK(y_end, dx)
        end do
        print *, "after 4rk", y_end(1), y_end(2), y_end(3)
        f_c = y_end(2) - 1

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

    print *, "root", c

    ! Writing the values for the found root
    ! Write the header of the file
    open(fid1, file='stag_flow.csv', status='new', iostat=err_status, iomsg=err_iomsg)
    if (err_status /= 0) then
        write(*, *) 'Error ', trim(err_iomsg)
    end if
    write(fid1, *) 'eta,f,f_prime,f_double_prime'
    close(fid1)

    y_zero(3) = c 
    y_end = y_zero
    open(fid2, file='stag_flow.csv', status='old', position='append')
    write(fid2, *) domain(1),',', y_end(1),',', y_end(2),',', y_end(3) 
    do i=2, nodes
        !print *, "before 4rk", y_end(1), y_end(2), y_end(3)
        y_end = fourRK(y_end, dx)
        write(fid2, *) domain(i),',', y_end(1),',', y_end(2),',', y_end(3) 
    end do
    print *, "after 4rk", y_end(1), y_end(2), y_end(3)

    close(fid2)
end program
