module func_mod 
    ! The purpose of this module first is to contain the function which will be
    ! solved with the bisection method.
    ! Secondly, the ODE set up will also be placed here.
    
    implicit none
    integer :: i_fm
    public :: func, first_ODE, create_domain 

    contains

        real function func(x) result(f_x)
            ! This function is a test to analyze the functionality of fortran
            ! modules. And provided a x, returns 2x.
            !
            ! Parameters
            ! -----------------------------
            ! :param x: defined by user, need to be entered as a real
            ! :param f_x: function's returned value of x^2 + 2*x, also a real
            real, intent(in) :: x

            f_x = x**2 + 2*x

            print *, "f(x) = ", f_x

        end function func

        real function first_ODE(x, Y) result(dYdx)
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

            dYdx = (x-Y)/2
        end function first_ODE

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

        subroutine create_domain(x_init, x_end, n, domain)
            implicit none
            real, intent(in) :: x_init, x_end
            integer, parameter, intent(in) :: n
            real, dimension(n), intent(out) :: domain
            real :: dx 

            dx = (x_end-x_init)/(n-1)

            domain(1) = x_init
            domain(n) = x_end

            do i_fm=2, n-1
                domain(i_fm) = domain(i_fm-1) + (i_fm-1)*dx
            end do

            do i_fm=1, n
                print *, domain(i_fm)
            end do
        end subroutine create_domain
end module func_mod 
