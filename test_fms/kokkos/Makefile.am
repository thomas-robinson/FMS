#***********************************************************************
#*                             Apache License 2.0
#*
#* This file is part of the GFDL Flexible Modeling System (FMS).
#*
#* Licensed under the Apache License, Version 2.0 (the "License");
#* you may not use this file except in compliance with the License.
#* You may obtain a copy of the License at
#*
#*     http://www.apache.org/licenses/LICENSE-2.0
#*
#* FMS is distributed in the hope that it will be useful, but WITHOUT
#* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied;
#* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#* PARTICULAR PURPOSE. See the License for the specific language
#* governing permissions and limitations under the License.
#***********************************************************************

# This is the autotools file for the Kokkos test directory.

# Include compiler flags
AM_FCFLAGS = $(FC_MODINC). $(FC_MODOUT)./include
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/kokkos/include

# If building with Kokkos, enable the define
if ENABLE_KOKKOS
  AM_FCFLAGS += -DENABLE_KOKKOS
  AM_CXXFLAGS = $(KOKKOS_CXXFLAGS)
endif

# Build test programs
check_PROGRAMS = test_fms_kokkos test_fms_kokkos_views

# Test source files
test_fms_kokkos_SOURCES = test_fms_kokkos.F90
test_fms_kokkos_LDADD = $(top_builddir)/libFMS.la

test_fms_kokkos_views_SOURCES = test_fms_kokkos_views.F90
test_fms_kokkos_views_LDADD = $(top_builddir)/libFMS.la

if ENABLE_KOKKOS
  test_fms_kokkos_LDFLAGS = $(KOKKOS_LDFLAGS)
  test_fms_kokkos_LDADD += $(KOKKOS_LIBS)
  test_fms_kokkos_views_LDFLAGS = $(KOKKOS_LDFLAGS)
  test_fms_kokkos_views_LDADD += $(KOKKOS_LIBS)
endif

# Link with MPI if needed
test_fms_kokkos_LDADD += $(MPI_FC_LIB)
test_fms_kokkos_views_LDADD += $(MPI_FC_LIB)

# Shell test scripts
check_SCRIPTS = test_fms_kokkos.sh test_fms_kokkos_views.sh
TESTS_ENVIRONMENT = 
TESTS = test_fms_kokkos.sh test_fms_kokkos_views.sh

# Distribute test files
EXTRA_DIST = test_fms_kokkos.sh test_fms_kokkos_views.sh
