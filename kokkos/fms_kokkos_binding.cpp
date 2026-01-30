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
 * @file fms_kokkos_binding.cpp
 * @brief C++ implementation of Kokkos bindings for FMS
 */

#ifdef ENABLE_KOKKOS
#include <Kokkos_Core.hpp>
#endif

#include "fms_kokkos_binding.h"
#include <iostream>

// Global state for Kokkos initialization
static bool kokkos_initialized = false;

#ifdef ENABLE_KOKKOS

/**
 * @brief Initialize the Kokkos runtime
 */
void kokkos_initialize(void) {
  if (!kokkos_initialized) {
    Kokkos::initialize();
    kokkos_initialized = true;
  }
}

/**
 * @brief Finalize the Kokkos runtime
 */
void kokkos_finalize(void) {
  if (kokkos_initialized) {
    Kokkos::finalize();
    kokkos_initialized = false;
  }
}

/**
 * @brief Get the current Kokkos execution space
 */
int kokkos_get_execution_space(void) {
#ifdef KOKKOS_ENABLE_CUDA
  if (std::is_same<Kokkos::DefaultExecutionSpace, Kokkos::Cuda>::value) {
    return KOKKOS_CUDA;
  }
#endif

#ifdef KOKKOS_ENABLE_OPENMP
  if (std::is_same<Kokkos::DefaultExecutionSpace, Kokkos::OpenMP>::value) {
    return KOKKOS_OPENMP;
  }
#endif

#ifdef KOKKOS_ENABLE_HIP
  if (std::is_same<Kokkos::DefaultExecutionSpace, Kokkos::HIP>::value) {
    return KOKKOS_HIP;
  }
#endif

#ifdef KOKKOS_ENABLE_SYCL
  if (std::is_same<Kokkos::DefaultExecutionSpace, Kokkos::SYCL>::value) {
    return KOKKOS_SYCL;
  }
#endif

  return KOKKOS_HOST;
}

/**
 * @brief Get the number of Kokkos threads
 */
int kokkos_get_num_threads(void) {
  return Kokkos::DefaultExecutionSpace().concurrency();
}

/**
 * @brief Print Kokkos configuration
 */
void kokkos_print_configuration(void) {
  std::cout << "=== Kokkos Configuration ===" << std::endl;
  Kokkos::print_configuration(std::cout);
  std::cout << "============================" << std::endl;
}

/**
 * @brief Synchronize Kokkos execution
 */
void kokkos_synchronize(void) {
  Kokkos::fence();
}

#else  // ENABLE_KOKKOS not defined

/**
 * @brief Stub: Initialize the Kokkos runtime
 */
void kokkos_initialize(void) {
  // No-op when Kokkos is not enabled
}

/**
 * @brief Stub: Finalize the Kokkos runtime
 */
void kokkos_finalize(void) {
  // No-op when Kokkos is not enabled
}

/**
 * @brief Stub: Get the current Kokkos execution space
 */
int kokkos_get_execution_space(void) {
  return KOKKOS_HOST;
}

/**
 * @brief Stub: Get the number of Kokkos threads
 */
int kokkos_get_num_threads(void) {
  return 1;
}

/**
 * @brief Stub: Print Kokkos configuration
 */
void kokkos_print_configuration(void) {
  std::cout << "Kokkos support not enabled at build time" << std::endl;
}

/**
 * @brief Stub: Synchronize Kokkos execution
 */
void kokkos_synchronize(void) {
  // No-op when Kokkos is not enabled
}

#endif  // ENABLE_KOKKOS
