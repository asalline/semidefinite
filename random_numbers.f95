!!! This module contains random number procedures

module random_numbers
    implicit none

    contains

        !!! Subroutine that initializes random seed by the values
        !!! of the intrinsic subroutine "date_and_time".
        !!! Taken from the book:
        !!! "Fortran 95/2003 by J. Haataja, J. Rahola & J. Ruokolainen
        subroutine initialize
            implicit none

            !!! Variables
            integer :: size1, status1
            integer, dimension(8) :: time1
            integer, dimension(:), allocatable :: seed1

            call random_seed(size = size1)

            allocate(seed1(size1), stat = status1)
            if (status1 > 0) stop 'Allocation did not work as expected!'
            
            call date_and_time(values = time1)
            seed1 = 100 * time1(7) + time1(8) / 10
            !seed1 = 20180815
            call random_seed(put = seed1)

        end subroutine initialize

        !!! Subroutine to get three random variables "x, y, z" 
        !!! from some real interval (eg. [-1, 1]). Inputs are
        !!! upper and lower bound and output is vector "xyz"
        !!! which contains the random variables such that:
        !!! x = xyz(1), y = xyz(2) and z = xyz(3).
        subroutine get_xyz(lower, upper, xyz)
            implicit none

            !!! Variables
            double precision :: x, y, z
            double precision, intent(in) :: upper, lower
            double precision, dimension(1:3), intent(out) :: xyz

            !call initialize
            call random_number(x)
            call random_number(y)
            call random_number(z)

            xyz(1) = lower + (upper - lower) * x
            xyz(2) = lower + (upper - lower) * y
            xyz(3) = lower + (upper - lower) * z
            
        end subroutine get_xyz


        !!! With this subroutine one can check if the initialization
        !!! is done right. One can adjust "repeats" variable to loop
        !!! wanted amount of random numbers and see if they really
        !!! change iteration by iteration.
        subroutine random_num_check
        implicit none

        integer :: i, repeats = 20
        double precision :: rand, old
    
        call initialize
        rand = 0.5
        do i = 1, repeats
            old = rand
            call random_number(rand)
    
            write(*, '((A),i6,3f15.7)') '#Iteration, Random number, Difference:', i, rand, rand-old
        end do

        end subroutine random_num_check



end module random_numbers