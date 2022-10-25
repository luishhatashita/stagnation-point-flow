module ode_solver_m
    use func_m
    implicit none

    integer :: i_osm

    contains 

        function fourRK(y_4rk_osm, dx_4rk_osm)
            ! Fourth order Runge Kutta integration scheme for the stagnation 
            ! point flow ODE. Each 4RK step calls the stag_flow function with
            ! the updated Y vector, which defines the derivatives of F.
            !
            ! Returns the final updated Y vector after 4 steps.
            !
            ! Parameters
            ! -----------------------------
            ! :param dx_4rk_osm: defined by user, domain step of 4rk integration
            ! scheme
            ! :param y_4rk_osm: defined by user, entered as a 1 dimension vector
            ! with 3 components
            ! :param fourRK: function's returned value of 4RK scheme as the 1 
            ! dimension vector with 3 correspondent components
            implicit none
            double precision, intent(in) :: dx_4rk_osm 
            double precision, dimension(3), intent(in) :: y_4rk_osm
            double precision, dimension(3) :: k1, k2, k3, k4
            double precision, dimension(3) :: fourRK
            
            k1 = dx_4rk_osm*stag_flow(y_4rk_osm)
            k2 = dx_4rk_osm*stag_flow(y_4rk_osm+0.5*k1)
            k3 = dx_4rk_osm*stag_flow(y_4rk_osm+0.5*k2)
            k4 = dx_4rk_osm*stag_flow(y_4rk_osm+k3)

            fourRK = y_4rk_osm + (1.0/6.0)*(k1 + 2*k2 + 2*k3 + k4)

        end function fourRK

        function stag_flow(y_sf_fm) 
            ! This function is a test to analyze the functionality of fortran
            ! modules. And provided a x, returns 2x.
            !
            ! Parameters
            ! -----------------------------
            ! :param x: defined by user, need to be entered as a real
            ! :param f_x: function's returned value of x^2 + 2*x, also a real
            implicit none
            double precision, dimension(3), intent(in) :: y_sf_fm
            double precision, dimension(3) :: stag_flow 
            
            stag_flow(1) = y_sf_fm(2)
            stag_flow(2) = y_sf_fm(3)
            stag_flow(3) = -y_sf_fm(1)*y_sf_fm(3) - 1 + y_sf_fm(2)*y_sf_fm(2)
        end function stag_flow

end module ode_solver_m
