module ode_solver 
    use func_mod
    implicit none
    private

    public :: forth_runge_kutta!, high_forth_runge_kutta

    integer :: i_om, n_max
    !real :: x0, Y0, x, dx
    integer :: err_status
    character(256) :: err_iomsg

    contains

        subroutine forth_runge_kutta(xi_1, Y0_1, xf_1, n_1, x_1, y_1)
            implicit none
            integer, intent(in) :: n_1
            real, intent(in) :: xi_1, Y0_1, xf_1
            real, dimension(:),  intent(out) :: x_1, y_1
            real :: dx_4RK
            real :: k1, k2, k3, k4            
            !real :: f_k1, f_k2, f_k3, f_k4
            !real :: y
            !integer :: err_status
            !character(256) :: err_iomsg
            
            call create_domain(xi_1, xf_1, n_1, x_1)
            !n_max = int((x-X0)/dx)
            
            !open(10, file='data.dat', status='new', iostat=err_status, iomsg=err_iomsg)
            !if (err_status /= 0) then
            !    write(*, *) 'Error ', trim(err_iomsg)
            !end if

            !write(10, *) x0,',', Y0
            !
            !close(10)

            y_1(1) = Y0_1
            do i_om=2, n_1
                !call first_ODE(x0, y, f_k1)
                k1 = dx*first_ODE(x_1(i_om-1), y_1(i_om-1))
                !call first_ODE(x0 + 0.5*dx, y + 0.5*k1, f_k2)
                k2 = dx*first_ODE(x_1(i_om-1) + 0.5*dx, y_1(i_om-1) + 0.5*k1)
                !call first_ODE(x0 + 0.5*dx, y + 0.5*k2, f_k3)
                k3 = dx*first_ODE(x_1(i_om-1) + 0.5*dx, y_1(i_om-1) + 0.5*k2)
                !call first_ODE(x0 + dx, y + k3, f_k4)
                k4 = dx*first_ODE(x_1(i_om-1) + dx, y_1(i_om-1) + k3)

                y_1(i_om) = y(i_om-1) + (1.0/6.0)*(k1 + 2*k2 + 2* k3 + k4)

                !open(11, file='data.dat', status='old', position='append')
                !write(11, *) x0,',', y
                !close(11)

            end do
            
            !y_last = y

        end subroutine forth_runge_kutta 

        ! subroutine high_forth_runge_kutta(xi, aY0, xf, dx, ay_last)
        !     ! This function is a test to analyze the functionality of fortran
        !     ! modules. And provided a x, returns 2x.
        !     !
        !     ! Parameters
        !     ! -----------------------------
        !     ! :param x: defined by user, need to be entered as a real
        !     ! :param f_x: function's returned value of x^2 + 2*x, also a real
        !     !integer :: i, n_max
        !     real, intent(in) :: xi, xf, dx
        !     real, dimension(2) :: aY0
        !     real, dimension(2) :: ak1, ak2, ak3, ak4, ak5
        !     real, dimension(2) :: af_k1, af_k2, af_k3, af_k4
        !     real, dimension(2) :: ay
        !     real, dimension(2), intent(out) :: ay_last
        !     !integer :: err_status
        !     !character(256) :: err_iomsg
        !     
        !     n_max = int((xf-xi)/dx)
        !     
        !     open(10, file='second_data.csv', status='new', iostat=err_status, iomsg=err_iomsg)
        !     if (err_status /= 0) then
        !         write(*, *) 'Error ', trim(err_iomsg)
        !     end if

        !     write(10, *) xi,',', aY0(1),',', aY0(1)
        !     
        !     close(10)

        !     !ay(1,1) = aY0(1,1)
        !     !ay(1,2) = aY0(1,2)
        !     x = xi
        !     ay = aY0
        !     do i=1, n_max, 1
        !         call second_ODE(x, ay, af_k1)
        !         ak1 = dx_1*af_k1
        !         call second_ODE(x + 0.5*dx, ay + 0.5*ak1, af_k2)
        !         ak2 = dx_1*af_k2
        !         call second_ODE(x + 0.5*dx, ay + 0.5*ak2, af_k3)
        !         ak3 = dx_1*af_k3
        !         call second_ODE(x + dx, ay + ak3, af_k4)
        !         ak4 = dx*af_k4

        !         ay = ay + (1.0/6.0)*(ak1 + 2*ak2 + 2* ak3 + ak4)
        !         x = x + dx

        !         open(11, file='second_data.csv', status='old', position='append')
        !         write(11, *) x,',', ay(1),',', ay(2)
        !         close(11)

        !     end do
        !     
        !     ay_last = ay

        ! end subroutine high_forth_runge_kutta

end module ode_solver 
