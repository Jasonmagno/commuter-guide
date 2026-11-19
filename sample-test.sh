#!/bin/sh
# Test the COBOL program alone: Aircon bus, 0 km -> 14 km, 2 students, 1 traffic report
echo "T BUSAC 0 14 2 STUDENT 1" | ./cobol/farecalc
