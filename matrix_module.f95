!!! These functions and subroutines help one to generate different
!!! kind of matrices, for example identity matrix and also contains
!!! useful matrix functions, for example trace calculating function.

module matrix_module
    use random_numbers
    implicit none

    contains

        !!! Subroutine to print a given matrix.
        subroutine print_matrix(matrix)
            implicit none

            double precision, dimension(:,:) :: matrix
            integer :: dim1, dim2, i ,j
            intrinsic size

            dim1 = size(matrix, dim = 1)
            dim2 = size(matrix, dim = 2)

            do i = 1, dim1
                do j = 1, dim2
                    write(*, '(f10.6)', advance = 'no') matrix(i,j)
                end do
                write(*,*)
            end do

        end subroutine print_matrix

        !!! Function "constant_diagonal" creates a diagonal matrix
        !!! with constant diagonal value in double precision.    
        function constant_diagonal(dim, constant) result(matrix)
            implicit none

            !!! Variables
            integer :: i
            integer, intent(in) :: dim
            double precision, dimension(:,:), allocatable :: matrix
            double precision, intent(in) :: constant

            allocate (matrix(dim, dim))

            do i = 1, dim
                matrix(i,i) = constant
            end do

        end function constant_diagonal

        !!! Function that creates a symmetric 3 by 3 matrix, and saves
        !!! its non-diagonal entries x, y and z.
        subroutine symmetric3by3(lower, upper, xyz, matrix)
            implicit none

            !!! Variables
            integer :: dim = 3
            double precision, intent(in) :: upper, lower
            double precision :: constant = 2.0
            double precision, dimension(1:3), intent(out) :: xyz
            double precision, dimension(3,3), intent(out) :: matrix

            call get_xyz(lower, upper, xyz)

            matrix = constant_diagonal(dim, constant)

            matrix(1,2) = xyz(1)
            matrix(2,1) = xyz(1)
            matrix(1,3) = xyz(2)
            matrix(3,1) = xyz(2)
            matrix(2,3) = xyz(3)
            matrix(3,2) = xyz(3)

            
        end subroutine symmetric3by3

        !!! Function that calculates the trace of a given matrix.
        !!! IMPORTANT: Trace can only be calculated for square
        !!! matrices so function checks that and gives error
        !!! message if given matrix is not square.
        function trace(matrix)
            implicit none

            !!! Variables
            integer :: i, dim1, dim2
            double precision, dimension(:,:), intent(in) :: matrix
            double precision :: trace
            
            dim1 = size(matrix, dim = 1)
            dim2 = size(matrix, dim = 2)

            if (dim1 == dim2) then
                do i = 1, dim1
                    trace = trace + matrix(i,i)
                end do
            else
                print*, 'Given matrix is not square matrix!'
                stop
            end if

        end function trace
    
end module matrix_module