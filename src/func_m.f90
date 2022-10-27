module func_m
    implicit none

    integer :: i_fm

    public

    contains

        subroutine create_domain(xi_cd_fm, xf_cd_fm, n_cd_fm, dx_cd_fm, a_cd_fm)
            ! Function to create the integration domain. Given beginning and end
            ! of the domain. The interval will be divided into nodes-1 cells.
            ! 
            ! Returns the domain as an array and its respective integration 
            ! step, which will be later an input for the 4RK.
            !
            ! Parameters
            ! -----------------------------
            ! :param xi_cd_fm: defined by user, beginning of the domain interval
            ! :param xf_cd_fm: defined by user, end of the domain interval
            ! :param n_cd_fm: defined by user, number of nodes of the domain, 
            ! which is equal to the number of cells plus 1
            ! :param dx_cd_fm: output value, equal to the integration step size
            ! :param a_cd_fm: output value, equal to the discretized domain, in
            ! array form with n_cd_fm nodes
            implicit none
            integer, intent(in) :: n_cd_fm
            double precision, intent(in) :: xi_cd_fm, xf_cd_fm
            double precision, intent(out) :: dx_cd_fm
            double precision, dimension(:), intent(out) :: a_cd_fm

            dx_cd_fm = (xf_cd_fm-xi_cd_fm)/(n_cd_fm-1)

            a_cd_fm(1) = xi_cd_fm
            a_cd_fm(n_cd_fm) = xf_cd_fm
            do i_fm=2, n_cd_fm-1
                a_cd_fm(i_fm) = a_cd_fm(i_fm-1) + dx_cd_fm
            end do

        end subroutine create_domain

end module func_m
