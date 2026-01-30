#!/bin/bash
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

# Test script for FMS Kokkos module

# Get the directory where the test script is located
TESTDIR=$(cd "$(dirname "$0")" && pwd)

# Run the test
$TESTDIR/test_fms_kokkos
exit_code=$?

# Report results
if [ $exit_code -eq 0 ]; then
  echo "test_fms_kokkos: PASSED"
  exit 0
else
  echo "test_fms_kokkos: FAILED"
  exit 1
fi
