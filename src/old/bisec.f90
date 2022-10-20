program bisection_main
    use func_mod
    implicit none
    real :: a_user, b_user, error_accept_user, max_it_user, root_user

    a_user = -5 
    b_user = -1
    error_accept_user = 0.005
    max_it_user = 10

    call bisection(a_user, b_user, error_accept_user, max_it_user, root_user)
    print *, "Root again to access its value : ", root_user

    contains

        subroutine bisection(a, b, error_accept, max_it, root)
            real :: a, b, error_accept, max_it
            real, intent(out) :: root
            real :: c, f_a, f_b, f_c, error, it = 1 
             
            error = ABS(a-b)

            do while (error > error_accept .and. it <= max_it)
                c = (a + b)/2
                
                ! calling and assinging values to boundaries and root
                call func(a, f_a)
                call func(b, f_b)
                call func(c, f_c)

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
            
            root = c

            print *, "Final error : ", error
            print *, "Root: ", root

        end subroutine bisection

end program bisection_main
