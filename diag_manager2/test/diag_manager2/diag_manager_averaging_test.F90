!! \brief Contains a program for testing module fms_diag_omp_aux

! \description
!! Contains a program for testing module fms_diag_omp_aux
program diag_manager_averaging_test
    use omp_lib
    use fms_diag_averaging_mod, only: get_average, alloc_subarray, num_offloading_threads
    use fms_diag_averaging_dummy
    implicit none

    integer, parameter:: dp=kind(0.d0)

    integer(kind = 4) :: num_threads_cpu
    real (dp), allocatable ::  data1D(:)
    real (dp), allocatable ::  data2D(:,:)
    real (dp), allocatable ::  data3D(:,:,:)
    real (dp), allocatable ::  data4D(:,:,:,:)
    real (dp), allocatable ::  data5D(:,:,:,:,:)
    real (dp), allocatable ::  data5D_R(:,:,:,:,:)
    real (dp) ::  average0D, expected1D

    real (dp), allocatable :: average1D(:)
    real (dp), allocatable :: average2D(:,:)
    real (dp), allocatable :: average3D(:,:,:)
    real (dp), allocatable :: average4D(:,:,:,:)
    real (dp), allocatable :: average4D_R(:,:,:,:)


    real (dp), allocatable :: a_average2D(:,:)
    real (dp), allocatable :: a_average3D(:, :, :)

    integer :: max_x, max_y, max_z, max_h, max_nt
    integer :: nx,ny,nz,nh, nt, target_row
    real (dp) k2, k3, k4, k5

    real :: start_t, end_t

    print *, "Starting OMP_FF program"
    max_x = 5
    max_y = 6
    max_z = 7
    max_h = 8
    max_nt = 1000
    !!Allocate the larger arrays
    allocate (data1D (max_nt))
    allocate (data2D (max_x,max_nt))
    allocate (data3D (max_x,max_y,max_nt))
    allocate (data4D (max_x,max_y,max_z,max_nt))
    allocate (data5D (max_x,max_y,max_z,max_h, max_nt))
    allocate (data5D_R (max_nt, max_x,max_y,max_z,max_h))

    allocate (average1D (max_x))
    allocate (average2D (max_x,max_y))
    allocate (average3D (max_x,max_y,max_z))
    allocate (average4D (max_x,max_y,max_z,max_h))
    allocate (average4D_R (max_x,max_y,max_z,max_h))


    !Make some data
    k2 = 2.2d0
    k3 = 3.3_dp
    k4 = 4.4_dp
    k5 = 5.432_dp

    !Initialize more test data
    do nt = 1, max_nt
        data1D(nt) =  nt
        do nx = 1,max_x
             data2D(nx, nt ) =  nt * k2
            do ny = 1, max_y
                data3D(nx, ny, nt ) =  nt * k3
                do nz = 1, max_z
                     data4D(nx, ny, nz, nt ) =  nt * k4
                     do nh = 1, max_h
                        data5D(nx, ny, nz, nh, nt ) =  nt * k5
                        data5D_R(nt, nx, ny, nz, nh ) =  nt * k5
                    end do
                end do
            end do
        end do
    end do

    expected1D = sum(data1D) / max_nt

    !! expected 1D ave = (1/max_nt) * [(max_nt) * (max_nt + 1) / 2] == (max_nt+1)/2
    !! expectd 2D SUM ave = (1D ave) (max_x)(max_x+1)/2 = (1D ave)*15
    !! 3D >> 1D * 21   4D >> 1D  * 28
    !!

    num_threads_cpu = 1
    call omp_set_num_threads( num_threads_cpu )

    print *, "*** OMP_FF CALLS get_average KD subsets:"

    call get_average(data3D , average2D, 1, 5, 1, 6, 1, 10)

    average2D = 0.0d0

    call get_average(data3D , average2D, 1,4 , 3)

    do nx = 1,5
        print "(6(E8.2, TR2))", AVERAGE2d(nx,:)
    END DO


    average4D = 0.0d0
    call get_average(data5D , average4D, 3,4, 5,5,6,6,7,8, 11,12)
    print *,"sum(average4D subset)= ",sum(average4D), "expected =", &
        (4 * ((11 + 12)/2.0d0) *  k5 )


    average3D = 0.0d0
    call get_average(data4D , average3D, 3,4, 4,5,6,6,11,14)
    print *,"sum(average3D subset)= ",sum(average3D), "expected =", &
        (4 * ((11 + 12 + 13 + 14)/4.0d0) *  k4 )

    average2D = 0.0d0
    call get_average(data3D , average2D, 3,4, 1,3 ,11,14)
    print *,"sum(average2D subset)= ",sum(average2D), "expected =", &
        (2 * 3* ((11 + 12 + 13 + 14)/4.0d0) *  k3 )

   average1D = 0.0d0
    call get_average(data2D , average1D, 1,2, 11,14)
    print *,"sum(average1D subset)= ",sum(average1D), "expected =", &
        (2 * ((11 + 12 + 13 + 14)/4.0d0) *  k2 )

    average0D = 0.0d0
    call get_average(data1D , average0D, 1,99)
    print *,"average0D subset= ",average0D, "expected =", 50 !!!(100 * 99/ 2) /99



    print *, "\n*** OMP_FF CALLS get_average (1D):"

    call get_average( data1D, average0D)

    print *,"average0D= ",average0D, "expected=",expected1D

    print *, "*** OMP_FF CALLS get_average (2D):"

    call get_average(data2D , average1D)

    !!print *,"average1D(1:2)= ",average1D(1:2)  !! just print the first two

    print *,"sum(average1D)= ",sum(average1D), "expected =",(expected1D * max_x* k2 )

    print *, "*** OMP_FF CALLS get_average (3D):"

    call get_average(data3D , average2D)

    !!print *,"average1D(1,1:3)= ",average2D(1,1:3)  !! just print the first three

    print *,"sum(average2D)= ",sum(average2D), "expected =",(expected1D * (max_y * max_x ) * k3)

    print *, "*** OMP_FF CALLS get_average (4D):"

    call get_average(data4D , average3D)

    !!print *,"average3D(1,1,1:3)= ",average3D(1,1,1:3)  !! just print the first three

    print *,"sum(average3D)= ",sum(average3D), "expected =", &
        (expected1D * (max_x * max_y * max_z) * k4 )

    print *, "*** OMP_FF CALLS get_average (5D):"

    call get_average(data5D , average4D)

    !!print *,"average3D(1,1,1:3)= ",average3D(1,1,1:3)  !! just print the first three

    print *,"sum(average4D)= ",sum(average4D), "expected =", &
        (expected1D * (max_x * max_y * max_z * max_h) * k5 )

    print *, "OMP_FF CALLS get_average_MN:"

    !!target_row = 3
    !!call get_average_MN( data2D, target_row ,average0D,  num_threads_cpu)

    !!print *,"average0D= ",average0D, "expected=",(target_row * expected1D)

    !!print *, "OMP_FF Ave from Matrix column V2"

    print *, "OMP_FF CALLS allocating subarrays:"

    print *, "is allocated (a_average2D):", allocated (a_average2D)
    call alloc_subarray(data3D,a_average2D)
    print *, "is allocated (a_average2D):", allocated (a_average2D)
    a_average2D(1,1) = 1.11_dp
    print *,"a_average2D(1,1:3)= ",a_average2D(1,1:3)  !! just print the first three

    print *, "is allocated ? (a_average3D):", allocated (a_average3D)
    call alloc_subarray(data4D, a_average3D)
    print *, "is allocated ? (a_average3D):", allocated (a_average3D)
    a_average3D(1,1,1) = 2.23_dp
    print *,"a_average3D(1,1,1:3)= ",a_average3D(1,1,1:3)  !! just print the first three

    stop

    !!Some timing tests
    call cpu_time(start_t)
    call get_average(data5D , average4D)
    call cpu_time(end_t)

    print *,"sum(average4D)= ",sum(average4D), "expected =", &
        (expected1D * (max_x * max_y * max_z * max_h) * k5 )
    print *, "delta time=", (end_t - start_t)

    call cpu_time(start_t)
    call get_average_5D_R(data5D_R , average4D_R)
    call cpu_time(end_t)

    print *,"sum(average4D_R)= ",sum(average4D_R), "expected =", &
        (expected1D * (max_x * max_y * max_z * max_h) * k5 )
    print *, "delta time=", (end_t - start_t)

    write(*,*) 'stopping program - end of row vs column major order timing test'

    call print_device_info()


end program diag_manager_averaging_test

!!TODO convention for index starting array.
!! Array multi dim

 !This is a test subroutine.
    !Prints some info helpful in device offloading
    subroutine print_device_info()
        use omp_lib
        use fms_diag_averaging_mod, only: num_offloading_threads
        implicit none

        integer :: num_threads, tid, num_gpu_threads,n_gpu_thread
        logical :: inital_dev

        print *, "In print_device_info()"

        !$opm declare target


         inital_dev = omp_is_initial_device()
         print *, "Post declare target; pre parallel sec., inital_dev=", inital_dev

        !!OMP may be optimizing this for thread zero to thread to repeat
        !$omp parallel private(tid, n_gpu_thread)
            tid = omp_get_thread_num()

            if (tid == 0) then
                num_threads = omp_get_num_threads()
                print *, "Hello from third id=", tid
                print *, "In parallel section:"
                print *, "Number of threads available  =", num_threads
                print *, "Number of processors available = ", omp_get_num_procs ( )
                print *, "Max threads available = ", omp_get_max_threads ( )
                print *, "Number of devices = ", omp_get_num_devices()
            else
                n_gpu_thread = num_offloading_threads(num_threads)
                print *, "Hello from third id=", tid, "n_gpu_thread=", n_gpu_thread
            end if
        !$omp end parallel

        print *, "Outside parallel section:"
        print *, "Number of threads available=", omp_get_num_threads()
        print *, "Number of devices = ", omp_get_num_devices()

        num_gpu_threads = num_offloading_threads(num_threads)
        if ( num_gpu_threads ==num_threads) then
            print *, "Probably cannot offload?"
        else
            print *, "Probably can offload?"
        endif

        print *, "Leaving print_device_info()"


    end subroutine print_device_info




