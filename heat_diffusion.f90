program heat_diffusion
    implicit none

    ! Parameters
    integer, parameter :: nx = 50, ny = 50
    integer, parameter :: nt = 1000
    real, parameter :: alpha = 0.01
    real, parameter :: dx = 1.0, dy = 1.0, dt = 0.1
    real, dimension(nx, ny) :: u, u_new
    integer :: i, j, n

    ! Initial condition
    u = 0.0
    u(nx/2, ny/2) = 100.0 ! Initial heat source

    ! Time stepping
    do n = 1, nt
        ! Update the temperature field
        do i = 2, nx-1
            do j = 2, ny-1
                u_new(i, j) = u(i, j) + alpha * dt * (
     &                (u(i+1, j) - 2*u(i, j) + u(i-1, j)) / dx**2 + 
     &                (u(i, j+1) - 2*u(i, j) + u(i, j-1)) / dy**2)
            end do
        end do
        
        ! Apply boundary conditions (e.g., fixed temperature at the edges)
        u_new(1, :) = 0.0
        u_new(nx, :) = 0.0
        u_new(:, 1) = 0.0
        u_new(:, ny) = 0.0

        ! Update the temperature field
        u = u_new

        ! Optionally write the data to a file for visualization
        if (mod(n, 100) == 0) then
            call write_to_file(u, n)
        end if
    end do
end program heat_diffusion

subroutine write_to_file(u, step)
    implicit none
    real, dimension(:, :), intent(in) :: u
    integer, intent(in) :: step
    character(len=20) :: filename
    integer :: i, j
    open(unit=10, file=filename)
    write(filename, '("data_", I4.4, ".txt")') step
    do i = 1, size(u, 1)
        do j = 1, size(u, 2)
            write(10, '(F8.2, 1X)', advance="no") u(i, j)
        end do
        write(10, *)
    end do
    close(10)
end subroutine write_to_file
