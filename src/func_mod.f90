module func_mod 
    ! The purpose of this module first is to contain the function which will be
    ! solved with the bisection method.
    ! Secondly, the ODE set up will also be placed here.
    
    implicit none
    private

    public :: func, first_ODE 

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

        subroutine first_ODE(x, Y, dYdx)
            ! This function is a first order ODE to implement the 4th order
            ! Runge-Kutta method.
            !
            ! Parameters
            ! -----------------------------
            ! :param x:    domain of the ODE
            ! :param Y:    function to be approximated, which only an initial 
            !              condition is known
            ! :param dYdx: ODE which defines the behaviour of the function Y 
            real, intent(in) :: x, Y
            real, intent(out) :: dYdx

            dYdx = (x-Y)/2

        end subroutine first_ODE

        subroutine second_ODE(x, Y, f_out)
            ! This function is a second order ODE to implement the 4th order
            ! Runge-Kutta method.
            !
            ! Parameters
            ! -----------------------------
            ! :param x:    domain of the ODE
            ! :param Y:    function to be approximated, which only an initial 
            !              condition is known, has to be an column array of
            !              1 x 2 dimension
            ! :param f_out: ODE which defines the behaviour of the function Y,
            !               also in vector form
            real :: x
            real, dimension(1,2) :: Y
            real, dimension(1,2), intent(out) :: f_out

            f_out(1,1) = Y(1,2)
            f_out(1,2) = -Y(1,1)*Y(1,2) -3*Y(1,2) + SIN(x)

        end subroutine second_ODE

end module func_mod 
