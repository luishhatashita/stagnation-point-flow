module func_m
    implicit none

    integer :: i_fm

    public

    contains
        real function f_ode(x_f_ode_fm, y_f_ode_fm) result(f_f_ode_fm)
            implicit none
            real, intent(in) :: x_f_ode_fm, y_f_ode_fm

            f_f_ode_fm = (x_f_ode_fm-y_f_ode_fm)/2
        end function f_ode

        subroutine create_domain(xi_cd_fm, xf_cd_fm, n_cd_fm, a_cd_fm)
            implicit none
            integer, intent(in) :: n_cd_fm
            real, intent(in) :: xi_cd_fm, xf_cd_fm
            real, dimension(:), intent(out) :: a_cd_fm
            real :: dx_cd_fm

            dx_cd_fm = (xf_cd_fm-xi_cd_fm)/(n_cd_fm-1)

            a_cd_fm(1) = xi_cd_fm
            a_cd_fm(n_cd_fm) = xf_cd_fm
            do i_fm=2, n_cd_fm-1
                a_cd_fm(i_fm) = a_cd_fm(i_fm-1) + dx_cd_fm
            end do

        end subroutine create_domain
end module func_m
