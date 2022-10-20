module ode_solver_m
    use func_m
    implicit none

    integer :: i_osm

    contains 

        function fourRK(y_4rk_osm, dx_4rk_osm)
            implicit none
            real, intent(in) :: dx_4rk_osm 
            real, dimension(3), intent(in) :: y_4rk_osm
            real, dimension(3) :: k1, k2, k3, k4
            real, dimension(3) :: fourRK
            
            !print *, "inside 4rk"
            k1 = dx_4rk_osm*stag_flow(y_4rk_osm)
            !print *, k1
            k2 = dx_4rk_osm*stag_flow(y_4rk_osm+0.5*k1)
            !print *, k2
            k3 = dx_4rk_osm*stag_flow(y_4rk_osm+0.5*k2)
            !print *, k3
            k4 = dx_4rk_osm*stag_flow(y_4rk_osm+k3)
            !print *, k4

            fourRK = y_4rk_osm + (1.0/6.0)*(k1 + 2*k2 + 2*k3 + k4)
            !print *, "4rk", fourRK
            !print *, "inside after 4rk", fourRK(1), fourRK(2), fourRK(3)

        end function fourRK

        function stag_flow(y_sf_fm) 
            implicit none
            real, dimension(3), intent(in) :: y_sf_fm
            real, dimension(3) :: stag_flow 
            
            !print *, "inside stag"
            stag_flow(1) = y_sf_fm(2)
            stag_flow(2) = y_sf_fm(3)
            stag_flow(3) = -y_sf_fm(1)*y_sf_fm(3) - 1 + y_sf_fm(2)*y_sf_fm(2)
            !print *, "after stag", stag_flow(1), stag_flow(2), stag_flow(3)
        end function stag_flow

end module ode_solver_m
