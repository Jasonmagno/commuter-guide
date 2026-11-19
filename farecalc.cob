>>SOURCE FORMAT FREE
*> ============================================================
*> FARECALC - Commuter Guide (PUV-TA) fare and ETA engine (COBOL)
*>
*> Reads ONE trip record from STANDARD INPUT:
*>   T <VEHICLE> <FROM-KM> <TO-KM> <PASSENGERS> <PASSENGER-TYPE> <TRAFFIC-REPORTS>
*>   VEHICLE        : JEEP | MODERN | BUS | BUSAC | UV
*>   PASSENGER-TYPE : REGULAR | STUDENT | SENIOR | PWD
*>
*> Writes ONE result line, pipe-delimited:
*>   R|status|distanceKm|regularFare|discountPerPax|farePerPax|total|etaMin|delayMin|traffic
*>
*> Business rules (sample rates for school project):
*>   - Fare = base fare (first N km) + extra km x per-km rate
*>   - Rounded to nearest PHP 0.25
*>   - STUDENT / SENIOR / PWD get 20% off
*>   - 1..10 passengers, distance must be > 0 and <= 60 km
*>   - ETA = travel time (by vehicle speed) + 0.5 min/km stops
*>           + 3 min per active traffic/accident report (max 30)
*>   - Traffic level: 0 = LIGHT, 1-2 = MODERATE, 3+ = HEAVY
*> ============================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. FARECALC.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT TRIP-IN ASSIGN TO KEYBOARD
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD TRIP-IN.
01 TRIP-REC             PIC X(120).

WORKING-STORAGE SECTION.
*> ---- constants ----
01 WS-MAX-PAX           PIC 9(2)     VALUE 10.
01 WS-MAX-KM            PIC 9(2)     VALUE 60.
01 WS-DISC-RATE         PIC 9V99     VALUE 0.20.
01 WS-STOP-MIN-PER-KM   PIC 9V9      VALUE 0.5.
01 WS-DELAY-PER-REPORT  PIC 9        VALUE 3.
01 WS-MAX-DELAY         PIC 99       VALUE 30.

*> ---- input ----
01 WS-EOF               PIC X        VALUE "N".
   88 END-OF-INPUT                    VALUE "Y".
01 WS-TYPE              PIC X(1).
01 WS-F1                PIC X(20).
01 WS-F2                PIC X(20).
01 WS-F3                PIC X(20).
01 WS-F4                PIC X(20).
01 WS-F5                PIC X(20).
01 WS-F6                PIC X(20).
01 WS-VEHICLE           PIC X(10).
01 WS-PTYPE             PIC X(10).
01 WS-FROM              PIC S9(3)V99 VALUE 0.
01 WS-TO                PIC S9(3)V99 VALUE 0.
01 WS-PAX               PIC S9(5)    VALUE 0.
01 WS-REPORTS           PIC S9(5)    VALUE 0.

*> ---- vehicle rates ----
01 WS-BASE-FARE         PIC 9(3)V99  VALUE 0.
01 WS-BASE-KM           PIC 9(2)V9   VALUE 0.
01 WS-PER-KM            PIC 9V99     VALUE 0.
01 WS-SPEED             PIC 9(2)     VALUE 0.

*> ---- results ----
01 WS-DIST              PIC 9(3)V99  VALUE 0.
01 WS-EXTRA-KM          PIC 9(3)V99  VALUE 0.
01 WS-FARE              PIC 9(5)V99  VALUE 0.
01 WS-NET-FARE          PIC 9(5)V99  VALUE 0.
01 WS-DISCOUNT          PIC 9(5)V99  VALUE 0.
01 WS-QUARTERS          PIC 9(7)     VALUE 0.
01 WS-TOTAL             PIC 9(7)V99  VALUE 0.
01 WS-DELAY             PIC 9(3)     VALUE 0.
01 WS-ETA               PIC 9(4)     VALUE 0.
01 WS-TRAFFIC           PIC X(8)     VALUE "LIGHT".
01 WS-STATUS            PIC X(12)    VALUE "OK".
01 WS-GOT-TRIP          PIC X        VALUE "N".

*> ---- edited output ----
01 ED-DIST              PIC 9(3).99.
01 ED-A                 PIC 9(5).99.
01 ED-B                 PIC 9(5).99.
01 ED-C                 PIC 9(5).99.
01 ED-TOTAL             PIC 9(7).99.
01 ED-ETA               PIC 9(4).
01 ED-DELAY             PIC 9(3).

PROCEDURE DIVISION.
MAIN-PARA.
    OPEN INPUT TRIP-IN
    PERFORM UNTIL END-OF-INPUT
        READ TRIP-IN
            AT END
                SET END-OF-INPUT TO TRUE
            NOT AT END
                PERFORM PROCESS-RECORD
        END-READ
    END-PERFORM
    CLOSE TRIP-IN
    IF WS-GOT-TRIP = "N"
        MOVE "NO_TRIP" TO WS-STATUS
    END-IF
    PERFORM PRINT-RESULT
    STOP RUN.

PROCESS-RECORD.
    MOVE SPACES TO WS-TYPE WS-F1 WS-F2 WS-F3 WS-F4 WS-F5 WS-F6
    UNSTRING TRIP-REC DELIMITED BY ALL SPACE
        INTO WS-TYPE WS-F1 WS-F2 WS-F3 WS-F4 WS-F5 WS-F6
    END-UNSTRING
    IF WS-TYPE = "T"
        MOVE "Y" TO WS-GOT-TRIP
        PERFORM PROCESS-TRIP
    END-IF.

PROCESS-TRIP.
    MOVE FUNCTION UPPER-CASE(WS-F1) TO WS-VEHICLE
    MOVE FUNCTION UPPER-CASE(WS-F5) TO WS-PTYPE
    COMPUTE WS-FROM    = FUNCTION NUMVAL(WS-F2)
    COMPUTE WS-TO      = FUNCTION NUMVAL(WS-F3)
    COMPUTE WS-PAX     = FUNCTION NUMVAL(WS-F4)
    COMPUTE WS-REPORTS = FUNCTION NUMVAL(WS-F6)
    IF WS-REPORTS < 0
        MOVE 0 TO WS-REPORTS
    END-IF
    MOVE "OK" TO WS-STATUS

    PERFORM SET-VEHICLE-RATES
    IF WS-STATUS = "OK"
        PERFORM VALIDATE-TRIP
    END-IF
    IF WS-STATUS = "OK"
        PERFORM COMPUTE-FARE
        PERFORM COMPUTE-ETA
    END-IF.

SET-VEHICLE-RATES.
    EVALUATE WS-VEHICLE
        WHEN "JEEP"
            MOVE 14.00 TO WS-BASE-FARE
            MOVE 4     TO WS-BASE-KM
            MOVE 1.80  TO WS-PER-KM
            MOVE 16    TO WS-SPEED
        WHEN "MODERN"
            MOVE 15.00 TO WS-BASE-FARE
            MOVE 4     TO WS-BASE-KM
            MOVE 2.00  TO WS-PER-KM
            MOVE 20    TO WS-SPEED
        WHEN "BUS"
            MOVE 15.00 TO WS-BASE-FARE
            MOVE 5     TO WS-BASE-KM
            MOVE 2.65  TO WS-PER-KM
            MOVE 22    TO WS-SPEED
        WHEN "BUSAC"
            MOVE 18.00 TO WS-BASE-FARE
            MOVE 5     TO WS-BASE-KM
            MOVE 2.85  TO WS-PER-KM
            MOVE 24    TO WS-SPEED
        WHEN "UV"
            MOVE 25.00 TO WS-BASE-FARE
            MOVE 5     TO WS-BASE-KM
            MOVE 3.50  TO WS-PER-KM
            MOVE 28    TO WS-SPEED
        WHEN OTHER
            MOVE "BAD_VEHICLE" TO WS-STATUS
    END-EVALUATE.

VALIDATE-TRIP.
    IF WS-PTYPE NOT = "REGULAR" AND WS-PTYPE NOT = "STUDENT"
       AND WS-PTYPE NOT = "SENIOR" AND WS-PTYPE NOT = "PWD"
        MOVE "BAD_TYPE" TO WS-STATUS
    END-IF
    IF WS-STATUS = "OK"
        IF WS-PAX < 1 OR WS-PAX > WS-MAX-PAX
            MOVE "BAD_PAX" TO WS-STATUS
        END-IF
    END-IF
    IF WS-STATUS = "OK"
        IF WS-FROM < 0 OR WS-TO < 0
            MOVE "BAD_STOP" TO WS-STATUS
        END-IF
    END-IF
    IF WS-STATUS = "OK"
        IF WS-FROM > WS-TO
            COMPUTE WS-DIST = WS-FROM - WS-TO
        ELSE
            COMPUTE WS-DIST = WS-TO - WS-FROM
        END-IF
        EVALUATE TRUE
            WHEN WS-DIST = 0
                MOVE "SAME_STOP" TO WS-STATUS
            WHEN WS-DIST > WS-MAX-KM
                MOVE "TOO_FAR" TO WS-STATUS
        END-EVALUATE
    END-IF.

COMPUTE-FARE.
    IF WS-DIST <= WS-BASE-KM
        MOVE WS-BASE-FARE TO WS-FARE
    ELSE
        COMPUTE WS-EXTRA-KM = WS-DIST - WS-BASE-KM
        COMPUTE WS-FARE = WS-BASE-FARE + (WS-EXTRA-KM * WS-PER-KM)
    END-IF
    *> round the regular fare to the nearest 0.25
    COMPUTE WS-QUARTERS ROUNDED = WS-FARE * 4
    COMPUTE WS-FARE = WS-QUARTERS / 4

    *> 20% discount for students, seniors and PWD
    IF WS-PTYPE = "REGULAR"
        MOVE WS-FARE TO WS-NET-FARE
    ELSE
        COMPUTE WS-NET-FARE = WS-FARE - (WS-FARE * WS-DISC-RATE)
        COMPUTE WS-QUARTERS ROUNDED = WS-NET-FARE * 4
        COMPUTE WS-NET-FARE = WS-QUARTERS / 4
    END-IF
    COMPUTE WS-DISCOUNT = WS-FARE - WS-NET-FARE
    COMPUTE WS-TOTAL = WS-NET-FARE * WS-PAX.

COMPUTE-ETA.
    COMPUTE WS-DELAY = WS-REPORTS * WS-DELAY-PER-REPORT
    IF WS-DELAY > WS-MAX-DELAY
        MOVE WS-MAX-DELAY TO WS-DELAY
    END-IF
    COMPUTE WS-ETA ROUNDED =
            (WS-DIST / WS-SPEED * 60)
          + (WS-DIST * WS-STOP-MIN-PER-KM)
          + WS-DELAY
    EVALUATE TRUE
        WHEN WS-REPORTS = 0
            MOVE "LIGHT" TO WS-TRAFFIC
        WHEN WS-REPORTS < 3
            MOVE "MODERATE" TO WS-TRAFFIC
        WHEN OTHER
            MOVE "HEAVY" TO WS-TRAFFIC
    END-EVALUATE.

PRINT-RESULT.
    MOVE WS-DIST     TO ED-DIST
    MOVE WS-FARE     TO ED-A
    MOVE WS-DISCOUNT TO ED-B
    MOVE WS-NET-FARE TO ED-C
    MOVE WS-TOTAL    TO ED-TOTAL
    MOVE WS-ETA      TO ED-ETA
    MOVE WS-DELAY    TO ED-DELAY
    DISPLAY "R|" FUNCTION TRIM(WS-STATUS) "|" ED-DIST "|" ED-A "|" ED-B
            "|" ED-C "|" ED-TOTAL "|" ED-ETA "|" ED-DELAY
            "|" FUNCTION TRIM(WS-TRAFFIC).
