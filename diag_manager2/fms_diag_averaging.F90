!! \brief Contains OpenMP-based averaging routines
!!
!! \description This module for performssome statistical (e.g. average) operations
!! on data arrays. Typically, With the input array A being N-dimensioanl, an
!! (N-1)-dimensional array is calculated containing the averages for the Nth (righmost) dimension.
!! TODO: 1) Test the actual offloading. See "omp declare target" statement in the
!!         1D version and copy to tothers if works.
!! TODO 2) Test on alternate compiler and GPU
!! TODO 3) Determine loggin error policy (related to array allocation if any)
!! TODO 4) Delete all experimental subroutines

module fms_diag_averaging_mod
    use omp_lib
    implicit none
    private get_average_1D, get_average_2D, get_average_3D, get_average_4D, &
    get_average_5D, alloc_subarray_3D, alloc_subarray_4D

    !!Interface for calculating the averages.
    interface get_average
        procedure get_average_1D, get_average_2D, get_average_3D, &
            get_average_4D, get_average_5D
    end interface

    !Interface for allocating an array of one less dimention than the argument array.
    !It is assumed the RHS dimention will is the one that will not be in the newly
    !allocated (sub) array
    interface alloc_subarray
        procedure alloc_subarray_3D, alloc_subarray_4D
    end interface

    contains


 !! Calculate the scalar average of the input 1D array
!! the_data: the input data
!! the_average : Scalar variable where the average is to be stored.
!! The averages that will be  calculated are the_average(si_o : ei_o, sj_o : ej_o) if the
!! array index arguments are provided. Otherwise the indices will be determined from the
!! upper and lower bounds of the corresponding dimension of the array.

    subroutine get_average_1D( the_data, the_average, st, et)
        integer , optional, intent (in) :: st, et
        real (kind = 8),  intent(in) :: the_data (:)
        real(kind=8), intent(in out) :: the_average
        integer  :: st_, et_, it

        et_ = pr_or_alt(et, ubound (the_data,1))
        st_ = pr_or_alt(st, lbound (the_data,1))

        the_average= 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do it = st_, et_
            the_average = the_average + the_data( it)
        end do

       the_average = the_average / (et_ - st_ + 1)

    end subroutine get_average_1D


!! Calculate the 1D array containing averages of the input 2D array
!! the_data: the input data
!! the_average : array where the averages are stored. Should have one dimension less the the_data.
!! The averages that will be  calculated are the_average(si_o : ei_o, sj_o : ej_o) if the
!! array index arguments are provided. Otherwise the indices will be determined from the
!! upper and lower bounds of the corresponding dimension of the array.

    subroutine get_average_2D( the_data, the_average, si, ei, st, et)
        integer , optional, intent (in) :: si, ei, st, et
        real (kind = 8),  intent(in) :: the_data (:,:)
        real(kind=8), intent(in out) :: the_average(:)
        integer  :: si_, ei_, st_, et_, it

        et_ = pr_or_alt(et, ubound (the_data,2))
        st_ = pr_or_alt(st, lbound (the_data,2))
        ei_ = pr_or_alt(ei, ubound (the_data,1))
        si_ = pr_or_alt(si, lbound (the_data,1))

        the_average(si_:ei_)= 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do it = st_, et_
            the_average(si_:ei_) = the_average(si_:ei_) + the_data(si_:ei_, it)
        end do

       the_average(si_:ei_)=  the_average(si_:ei_)/ (et_ - st_ + 1)

    end subroutine get_average_2D



!! Calculate the 2D array containing averages of the input 3D array
!! the_data: the input data
!! the_average : array where the averages are stored. Should have one dimension less the the_data.
!! The averages that will be  calculated are the_average(si_o : ei_o, sj_o : ej_o) if the
!! array index arguments are provided. Otherwise the indices will be determined from the
!! upper and lower bounds of the corresponding dimension of the array.

    subroutine get_average_3D( the_data, the_average, si, ei, sj, ej, st, et)
        integer , optional, intent (in) :: si, ei, sj, ej, st, et
        real (kind = 8),  intent(in) :: the_data (:,:,:)
        real(kind=8), intent(in out) :: the_average(:,:)
        integer  :: si_, ei_, sj_, ej_, st_, et_, it

        et_ = pr_or_alt(et, ubound (the_data,3))
        st_ = pr_or_alt(st, lbound (the_data,3))
        ej_ = pr_or_alt(ej, ubound (the_data,2))
        sj_ = pr_or_alt(sj, lbound (the_data,2))
        ei_ = pr_or_alt(ei, ubound (the_data,1))
        si_ = pr_or_alt(si, lbound (the_data,1))

        the_average(si_:ei_,sj_:ej_)= 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do it = st_, et_
            the_average(si_:ei_,sj_:ej_) = the_average(si_:ei_,sj_:ej_) + the_data(si_:ei_,sj_:ej_, it)
        end do

       the_average(si_:ei_,sj_:ej_)=  the_average(si_:ei_,sj_:ej_)/ (et_ - st_ + 1)

    end subroutine get_average_3D


    !! Calculate the 3D array contaning averages of the input 4D array
    !! the_data: the input data
    !! the_average : array where the averages are stored. Should have one dimension less the the_data.
    !! The averages that will be  calculated are the_average(si_o : ei_o, sj_o : ej_o) if the
    !! array index arguments are provided. Otherwise the indices will be determined from the
    !! upper and lower bounds of the corresponding dimension of the array.


    subroutine get_average_4D( the_data, the_average, si, ei, sj, ej, sk, ek, st, et)
        integer , optional, intent (in) :: si, ei, sj, ej, sk, ek, st, et
        real (kind = 8),  intent(in) :: the_data (:,:,:,:)
        real(kind=8), intent(in out) :: the_average(:,:,:)
        integer  :: si_, ei_, sj_, ej_, sk_, ek_, st_, et_, it

        et_ = pr_or_alt(et, ubound (the_data,4))
        st_ = pr_or_alt(st, lbound (the_data,4))
        ek_ = pr_or_alt(ek, ubound (the_data,3))
        sk_ = pr_or_alt(sk, lbound (the_data,3))
        ej_ = pr_or_alt(ej, ubound (the_data,2))
        sj_ = pr_or_alt(sj, lbound (the_data,2))
        ei_ = pr_or_alt(ei, ubound (the_data,1))
        si_ = pr_or_alt(si, lbound (the_data,1))

        the_average(si_:ei_,sj_:ej_,sk_:ek_)= 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do it = st_, et_
            the_average(si_:ei_,sj_:ej_,sk_:ek_) = the_average(si_:ei_,sj_:ej_,sk_:ek_)  &
            +  the_data(si_:ei_,sj_:ej_,sk_:ek_, it)
        end do

       the_average(si_:ei_,sj_:ej_,sk_:ek_)= the_average(si_:ei_,sj_:ej_,sk_:ek_)/ (et_ - st_ + 1)

    end subroutine get_average_4D

   subroutine get_average_5D( the_data, the_average, si, ei, sj, ej, sk, ek, sl, el, st, et)
        integer , optional, intent (in) :: si, ei, sj, ej, sk, ek, sl, el, st, et
        real (kind = 8),  intent(in) :: the_data (:,:,:,:,:)
        real(kind=8), intent(in out) :: the_average(:,:,:,:)
        integer  :: si_, ei_, sj_, ej_, sk_, ek_, sl_, el_, st_, et_, it

        et_ = pr_or_alt(et, ubound (the_data,5))
        st_ = pr_or_alt(st, lbound (the_data,5))
        el_ = pr_or_alt(el, ubound (the_data,4))
        sl_ = pr_or_alt(sl, lbound (the_data,4))
        ek_ = pr_or_alt(ek, ubound (the_data,3))
        sk_ = pr_or_alt(sk, lbound (the_data,3))
        ej_ = pr_or_alt(ej, ubound (the_data,2))
        sj_ = pr_or_alt(sj, lbound (the_data,2))
        ei_ = pr_or_alt(ei, ubound (the_data,1))
        si_ = pr_or_alt(si, lbound (the_data,1))

        the_average(si_:ei_,sj_:ej_,sk_:ek_,sl_:el_)= 0.0d0

        !$opm declare target

        !$omp parallel do reduction(+ : the_average)
        do it = st_, et_
            the_average(si_:ei_,sj_:ej_,sk_:ek_,sl_:el_) = the_average(si_:ei_,sj_:ej_,sk_:ek_,sl_:el_)  &
            +  the_data(si_:ei_,sj_:ej_,sk_:ek_,sl_:el_, it)
        end do

       the_average(si_:ei_,sj_:ej_,sk_:ek_,sl_:el_)= the_average(si_:ei_,sj_:ej_,sk_:ek_,sl_:el_)/ (et_ - st_ + 1)

    end subroutine get_average_5D

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



!!Determine the number of threads used durring offloading to GPU,
!!If a gpu is not available, return the number of cpu_threads to use
!!WARNIG: This function needs to be called within an "!$omp target ..."
function num_offloading_threads(cpu_num_threads) result (gpu_num_threads)
       integer , intent(in) :: cpu_num_threads
       logical  (kind = 4) :: initial_device
       integer  :: gpu_num_threads
       integer  :: max_gpu_num_threads

       gpu_num_threads = cpu_num_threads

       max_gpu_num_threads = 1024

       initial_device = omp_is_initial_device()
       !!initial_device = .True.

       if ((cpu_num_threads .ne. 0) .and. (initial_device .eqv. .False.)) then
          gpu_num_threads = max_gpu_num_threads
       end if
end function num_offloading_threads


    subroutine alloc_subarray_3D( the_data, the_average)
        real (kind = 8),  intent(in) :: the_data (:,:,:)
        real(kind=8), allocatable, intent(in out) :: the_average(:,:)
        allocate (the_average(1:size(the_data,1), 1:size(the_data,2)))
     !TODO: Determine allocation status
     end subroutine alloc_subarray_3D

      subroutine alloc_subarray_4D( the_data, the_average)
        real (kind = 8),  intent(in) :: the_data (:,:,:,:)
        real(kind=8), allocatable, intent(in out) :: the_average(:,:,:)
        allocate (the_average(1: size(the_data,1), 1: size(the_data,2), 1: size(the_data,3)))
     !TODO: Determine allocation status
     end subroutine alloc_subarray_4D


end module fms_diag_averaging_mod

