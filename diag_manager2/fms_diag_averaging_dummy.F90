!! \brief Contains miscellanous temporary and experimental routines

! \description Contents of this file are temporary and experimental and
!! may be deleted without notice. This file is used
!! to support the development of module diag_manager_omp_aux module and its test program

module fms_diag_averaging_dummy
    use omp_lib
    implicit none

    contains

    !! Set result to argument xv if its present, otherwise to the alternate
    function pr_or_alt(xv, alternate) result (xv_result)
        integer , optional, intent (in) :: xv
        integer , intent (in) :: alternate
        integer  ::  xv_result

        if(present(xv)) then
            xv_result = xv
        else
            xv_result = alternate
        end if
    end function pr_or_alt


      subroutine get_average_5D_R( the_data, the_average)
        real (kind = 8),  intent(in) :: the_data (:,:,:,:,:)
        real(kind=8), intent(in out) :: the_average(:,:,:,:)
        integer :: NT,i

        NT = size (the_data,1)

        the_average = 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do i = 1, NT
            the_average(:,:,:,:) = the_average(:,:,:,:) + the_data(i,:,:,:,:)
        end do

        the_average = the_average / NT

    end subroutine get_average_5D_R


    !This is a test subroutine.
    !Returns the average of an input vector.
    subroutine get_average_v1( data, the_average, numThreads)
        real (kind = 8), intent(in) :: data (:)
        integer (kind=4), intent(in) :: numThreads
        real(kind=8), intent (in out) :: the_average
        integer (kind=4) N, iter_chunk
        real(kind=8) the_sum, local_sum, t1, t2;
        real(kind=8) elapsed(0:3)
        integer :: i

        print *, "Entering subroutine getAverage"

        N = size (data)
        the_sum = 0;
        iter_chunk = 5  !TODO: find rational amount and method of determining.

        call omp_set_num_threads( numThreads )

        !compare with simple parallel for and parallel for reduction
        ! each thread gets local copy of local_sum ...
        !$omp parallel private(local_sum, t1, t2) shared(the_sum)
        local_sum = 0
        t1 = omp_get_wtime()

        !the array is distributed statically between threads. Also try dynamic?
        !$omp do schedule(static,iter_chunk)
        do i = 1, N
            local_sum = local_sum +  data(i)
        end do

        t2 = omp_get_wtime();

        !each thread calculated its local_sum. ALl threads have to add to
        !the global sum. It is critical that this operation is atomic.

        !$omp critical
        the_sum = the_sum +  local_sum
        !$omp end critical

        elapsed(omp_get_thread_num()) = t2 - t1
        !$omp end parallel

        do i = 0, numThreads - 1
            print *, "Elapsed time for thread " ,i, " = ",elapsed(i)
        end do

        the_average = the_sum / N
    end subroutine get_average_v1


end module fms_diag_averaging_dummy
