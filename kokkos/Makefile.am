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

if ENABLE_KOKKOS
  AM_FCFLAGS += -DENABLE_KOKKOS
  AM_CXXFLAGS += -DENABLE_KOKKOS
endif

SUBDIRS = include

noinst_LTLIBRARIES = libkokkos.la

libkokkos_la_SOURCES = \
	fms_kokkos.F90 \
	fms_kokkos_binding.cpp

libkokkos_la_CPPFLAGS = -I$(top_srcdir)/kokkos/include

if ENABLE_KOKKOS
  libkokkos_la_CXXFLAGS = $(KOKKOS_CXXFLAGS)
  libkokkos_la_LDFLAGS = $(KOKKOS_LDFLAGS)
  libkokkos_la_LIBADD = $(KOKKOS_LIBS)
endif
