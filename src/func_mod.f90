module func_mod 
    ! The purpose of this module is to contain the function which will be
    ! solved with the bisection method.
    
    implicit none
    real, private :: a

    public :: func 

    contains

        subroutine func(x, f_x)
            ! This function is a test to analyze the functionality of fortran
            ! modules. And provided a x, returns 2x.
            !
            ! Parameters
            ! -----------------------------
            ! :param x: defined by user, need to be entered as a real
            ! :param f_x: function's returned value of x^2 + 2*x, also a real
            real, intent(in) :: x
            real, intent(out) :: f_x

            f_x = x**2 + 2*x

            print *, "f(x) = ", f_x

        end subroutine func

end module func_mod 
