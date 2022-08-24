program semidefinite
    use random_numbers
    use matrix_module
    implicit none

    !!! Variables
    integer :: i, repeats = 10000, n = 3, lda = 3, info
    integer, parameter :: lwork = 3*3 - 1
    !integer, parameter :: lwmax = 1000
    double precision :: upper = 2.0, lower = -2.0
    double precision, dimension(1:3) :: xyz, w
    double precision, dimension(lwork) :: work
    double precision, dimension(3,3) :: A
    character(len=1) :: jobz = 'N', uplo = 'U'
    logical :: T, F

    external dsyev
    intrinsic min, int, all, any

    call initialize

    open(10, file = 'eigenvalues.txt', status = 'replace')

    do i = 1, repeats
        call symmetric3by3(lower, upper, xyz, A)

        !call print_matrix(A)

        call dsyev(jobz, uplo, n, A, lda, w, work, lwork, info)

        !print*, info

        !write(*, '((A), 3f15.8)') 'Eigenvalues are: ', w(1), w(2), w(3)
        !write(*, '((A), 3f15.8)') 'Coordinates are: ', xyz
        !print*, any(w <= 0) .eqv. T

        if (any(w <= 0) .eqv. F) then 
            open(10, file = 'eigenvalues.txt')
            write(10, *) xyz(1), xyz(2), xyz(3), ' '
        end if

    end do

end program semidefinite

!Compiling:
!
!gfortran -c random_numbers.f95 matrix_module.f95 semidefinite.f95
!gfortran random_numbers.o matrix_module.o semidefinite.o -L/usr/local/bin/libblas.a -lblas -L/usr/local/bin/liblapack.a -llapack
!./a.out
