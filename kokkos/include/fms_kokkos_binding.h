/*
 * Apache License 2.0
 *
 * This file is part of the GFDL Flexible Modeling System (FMS).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * FMS is distributed in the hope that it will be useful, but WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the License for the specific language
 * governing permissions and limitations under the License.
 */

/**
 * @file fms_kokkos_binding.h
 * @brief C++ to Fortran bindings for Kokkos integration with FMS
 *
 * This header provides C++ implementations of Kokkos functionality that are
 * callable from Fortran code via C interoperability. It includes support for:
 * - Kokkos initialization and finalization
 * - Execution space and device queries
 * - View creation and management
 * - Parallel operations (for, reduce)
 */

#ifndef FMS_KOKKOS_BINDING_H
#define FMS_KOKKOS_BINDING_H

#ifdef ENABLE_KOKKOS
#include <Kokkos_Core.hpp>
#endif

/**
 * @brief Enum for Kokkos execution spaces
 */
enum KokkosExecutionSpace {
  KOKKOS_HOST = 0,
  KOKKOS_CUDA = 1,
  KOKKOS_OPENMP = 2,
  KOKKOS_HIP = 3,
  KOKKOS_SYCL = 4
};

/**
 * @brief Enum for Kokkos memory spaces
 */
enum KokkosMemorySpace {
  KOKKOS_HOST_MEMORY = 0,
  KOKKOS_CUDA_MEMORY = 1,
  KOKKOS_DEVICE_MEMORY = 2
};

#ifdef __cplusplus
extern "C" {
#endif

  /**
   * @brief Initialize the Kokkos runtime
   *
   * Must be called once before any Kokkos operations.
   */
  void kokkos_initialize(void);

  /**
   * @brief Finalize the Kokkos runtime
   *
   * Should be called once at the end of the program.
   */
  void kokkos_finalize(void);

  /**
   * @brief Get the current Kokkos execution space
   *
   * @return Integer identifier for the execution space (KokkosExecutionSpace enum)
   */
  int kokkos_get_execution_space(void);

  /**
   * @brief Get the number of Kokkos threads
   *
   * @return Number of threads available in the current execution space
   */
  int kokkos_get_num_threads(void);

  /**
   * @brief Print Kokkos configuration
   *
   * Prints information about the current Kokkos configuration to stdout.
   */
  void kokkos_print_configuration(void);

  /**
   * @brief Synchronize Kokkos execution
   *
   * Ensures all pending Kokkos operations are complete.
   */
  void kokkos_synchronize(void);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // FMS_KOKKOS_BINDING_H
