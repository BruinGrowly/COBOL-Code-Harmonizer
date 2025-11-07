       IDENTIFICATION DIVISION.
       PROGRAM-ID. PremiumCalculator.
      *INSURANCE - PREMIUM CALCULATION ENGINE
      *Calculates insurance premiums based on risk factors

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *Customer risk profile
       01  WS-CUSTOMER-PROFILE.
           05  WS-AGE                  PIC 9(3).
           05  WS-GENDER               PIC X.
           05  WS-ZIP-CODE             PIC X(5).
           05  WS-CREDIT-SCORE         PIC 9(3).
           05  WS-YEARS-INSURED        PIC 9(2).
           05  WS-CLAIMS-3YEARS        PIC 9(2).

      *Vehicle data (for auto insurance)
       01  WS-VEHICLE-DATA.
           05  WS-VEHICLE-YEAR         PIC 9(4).
           05  WS-VEHICLE-MAKE         PIC X(20).
           05  WS-VEHICLE-MODEL        PIC X(20).
           05  WS-VEHICLE-VALUE        PIC 9(7)V99.
           05  WS-ANNUAL-MILEAGE       PIC 9(6).
           05  WS-SAFETY-RATING        PIC 9.
           05  WS-THEFT-RATING         PIC 9.

      *Property data (for home insurance)
       01  WS-PROPERTY-DATA.
           05  WS-CONSTRUCTION-TYPE    PIC X(15).
           05  WS-CONSTRUCTION-YEAR    PIC 9(4).
           05  WS-SQUARE-FOOTAGE       PIC 9(5).
           05  WS-PROPERTY-VALUE       PIC 9(9)V99.
           05  WS-DISTANCE-FIRESTATION PIC 9(2)V9.
           05  WS-ALARM-SYSTEM         PIC X.
           05  WS-SPRINKLER-SYSTEM     PIC X.

      *Coverage selections
       01  WS-COVERAGE-SELECTIONS.
           05  WS-LIABILITY-LIMIT      PIC 9(9)V99.
           05  WS-COLLISION-COVERAGE   PIC X.
           05  WS-COMPREHENSIVE-CVG    PIC X.
           05  WS-DEDUCTIBLE           PIC 9(5).

      *Premium calculation
       01  WS-PREMIUM-CALCULATION.
           05  WS-BASE-PREMIUM         PIC 9(7)V99.
           05  WS-AGE-FACTOR           PIC 9V9999.
           05  WS-LOCATION-FACTOR      PIC 9V9999.
           05  WS-CREDIT-FACTOR        PIC 9V9999.
           05  WS-VEHICLE-FACTOR       PIC 9V9999.
           05  WS-CLAIMS-FACTOR        PIC 9V9999.
           05  WS-DISCOUNT-FACTOR      PIC 9V9999.
           05  WS-TOTAL-PREMIUM        PIC 9(7)V99.

      *Discounts
       01  WS-DISCOUNTS.
           05  WS-MULTICAR-DISCOUNT    PIC 9V99.
           05  WS-MULTIPOLICY-DISCOUNT PIC 9V99.
           05  WS-SAFE-DRIVER-DISCOUNT PIC 9V99.
           05  WS-LOYALTY-DISCOUNT     PIC 9V99.
           05  WS-SAFETY-FEATURE-DISC  PIC 9V99.
           05  WS-TOTAL-DISCOUNT-PCT   PIC 9(2)V99.

      *Display variables
       01  WS-DISPLAY-AMOUNT           PIC $$$,$$$,$$9.99.
       01  WS-DISPLAY-FACTOR           PIC 9.9999.
       01  WS-DISPLAY-PERCENT          PIC ZZ9.99.

       01  WS-SCENARIO-NUM             PIC 9 VALUE 1.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM DISPLAY-SYSTEM-HEADER.
           PERFORM CALCULATE-AUTO-SCENARIO-1.
           PERFORM CALCULATE-AUTO-SCENARIO-2.
           PERFORM CALCULATE-HOME-SCENARIO-1.
           DISPLAY 'Premium calculations complete.'.
           STOP RUN.

       DISPLAY-SYSTEM-HEADER SECTION.
       SHOW-HEADER.
           DISPLAY '========================================'.
           DISPLAY 'Insurance Premium Calculator'.
           DISPLAY 'Risk Assessment Engine'.
           DISPLAY '========================================'.
           DISPLAY SPACE.

       CALCULATE-AUTO-SCENARIO-1 SECTION.
       CALC-AUTO-1.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Auto Insurance - Young Driver'.
           DISPLAY '--------------------------------------------'.

      *Set customer profile
           MOVE 22 TO WS-AGE.
           MOVE 'M' TO WS-GENDER.
           MOVE '10001' TO WS-ZIP-CODE.
           MOVE 680 TO WS-CREDIT-SCORE.
           MOVE 2 TO WS-YEARS-INSURED.
           MOVE 1 TO WS-CLAIMS-3YEARS.

      *Set vehicle data
           MOVE 2022 TO WS-VEHICLE-YEAR.
           MOVE 'Honda' TO WS-VEHICLE-MAKE.
           MOVE 'Civic' TO WS-VEHICLE-MODEL.
           MOVE 25000.00 TO WS-VEHICLE-VALUE.
           MOVE 15000 TO WS-ANNUAL-MILEAGE.
           MOVE 5 TO WS-SAFETY-RATING.
           MOVE 3 TO WS-THEFT-RATING.

      *Set coverage
           MOVE 100000.00 TO WS-LIABILITY-LIMIT.
           MOVE 'Y' TO WS-COLLISION-COVERAGE.
           MOVE 'Y' TO WS-COMPREHENSIVE-CVG.
           MOVE 500 TO WS-DEDUCTIBLE.

           PERFORM CALCULATE-AUTO-PREMIUM.
           PERFORM DISPLAY-PREMIUM-BREAKDOWN.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-AUTO-SCENARIO-2 SECTION.
       CALC-AUTO-2.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Auto Insurance - Experienced Driver'.
           DISPLAY '--------------------------------------------'.

      *Set customer profile
           MOVE 45 TO WS-AGE.
           MOVE 'F' TO WS-GENDER.
           MOVE '10002' TO WS-ZIP-CODE.
           MOVE 780 TO WS-CREDIT-SCORE.
           MOVE 15 TO WS-YEARS-INSURED.
           MOVE 0 TO WS-CLAIMS-3YEARS.

      *Set vehicle data
           MOVE 2023 TO WS-VEHICLE-YEAR.
           MOVE 'Toyota' TO WS-VEHICLE-MAKE.
           MOVE 'Camry' TO WS-VEHICLE-MODEL.
           MOVE 32000.00 TO WS-VEHICLE-VALUE.
           MOVE 12000 TO WS-ANNUAL-MILEAGE.
           MOVE 5 TO WS-SAFETY-RATING.
           MOVE 2 TO WS-THEFT-RATING.

      *Set coverage
           MOVE 250000.00 TO WS-LIABILITY-LIMIT.
           MOVE 'Y' TO WS-COLLISION-COVERAGE.
           MOVE 'Y' TO WS-COMPREHENSIVE-CVG.
           MOVE 1000 TO WS-DEDUCTIBLE.

           PERFORM CALCULATE-AUTO-PREMIUM.
           PERFORM DISPLAY-PREMIUM-BREAKDOWN.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-HOME-SCENARIO-1 SECTION.
       CALC-HOME-1.
           DISPLAY 'Scenario ' WS-SCENARIO-NUM
                   ': Homeowner Insurance'.
           DISPLAY '--------------------------------------------'.

      *Set customer profile
           MOVE 38 TO WS-AGE.
           MOVE 'M' TO WS-GENDER.
           MOVE '10003' TO WS-ZIP-CODE.
           MOVE 750 TO WS-CREDIT-SCORE.
           MOVE 8 TO WS-YEARS-INSURED.
           MOVE 0 TO WS-CLAIMS-3YEARS.

      *Set property data
           MOVE 'BRICK' TO WS-CONSTRUCTION-TYPE.
           MOVE 1998 TO WS-CONSTRUCTION-YEAR.
           MOVE 2500 TO WS-SQUARE-FOOTAGE.
           MOVE 450000.00 TO WS-PROPERTY-VALUE.
           MOVE 2.5 TO WS-DISTANCE-FIRESTATION.
           MOVE 'Y' TO WS-ALARM-SYSTEM.
           MOVE 'Y' TO WS-SPRINKLER-SYSTEM.

           PERFORM CALCULATE-HOME-PREMIUM.
           PERFORM DISPLAY-PREMIUM-BREAKDOWN.

           ADD 1 TO WS-SCENARIO-NUM.
           DISPLAY SPACE.

       CALCULATE-AUTO-PREMIUM SECTION.
       CALC-AUTO-PREM.
      *Base premium from liability limit
           COMPUTE WS-BASE-PREMIUM =
               (WS-LIABILITY-LIMIT / 100000) * 350.

      *Age factor
           EVALUATE TRUE
               WHEN WS-AGE < 25
                   MOVE 1.8 TO WS-AGE-FACTOR
               WHEN WS-AGE < 30
                   MOVE 1.4 TO WS-AGE-FACTOR
               WHEN WS-AGE < 50
                   MOVE 1.0 TO WS-AGE-FACTOR
               WHEN WS-AGE < 65
                   MOVE 1.1 TO WS-AGE-FACTOR
               WHEN OTHER
                   MOVE 1.3 TO WS-AGE-FACTOR
           END-EVALUATE.

      *Location factor (simplified by ZIP)
           IF WS-ZIP-CODE(1:2) = '10'
               MOVE 1.25 TO WS-LOCATION-FACTOR
           ELSE
               MOVE 1.0 TO WS-LOCATION-FACTOR
           END-IF.

      *Credit score factor
           EVALUATE TRUE
               WHEN WS-CREDIT-SCORE >= 750
                   MOVE 0.85 TO WS-CREDIT-FACTOR
               WHEN WS-CREDIT-SCORE >= 700
                   MOVE 0.95 TO WS-CREDIT-FACTOR
               WHEN WS-CREDIT-SCORE >= 650
                   MOVE 1.05 TO WS-CREDIT-FACTOR
               WHEN OTHER
                   MOVE 1.25 TO WS-CREDIT-FACTOR
           END-EVALUATE.

      *Vehicle factor
           COMPUTE WS-VEHICLE-FACTOR =
               1.0 + ((10 - WS-SAFETY-RATING) * 0.05) +
               (WS-THEFT-RATING * 0.03).

      *Claims factor
           IF WS-CLAIMS-3YEARS = 0
               MOVE 0.9 TO WS-CLAIMS-FACTOR
           ELSE
               COMPUTE WS-CLAIMS-FACTOR =
                   1.0 + (WS-CLAIMS-3YEARS * 0.25)
           END-IF.

      *Calculate discounts
           PERFORM CALCULATE-AUTO-DISCOUNTS.

      *Calculate total premium
           COMPUTE WS-TOTAL-PREMIUM =
               WS-BASE-PREMIUM *
               WS-AGE-FACTOR *
               WS-LOCATION-FACTOR *
               WS-CREDIT-FACTOR *
               WS-VEHICLE-FACTOR *
               WS-CLAIMS-FACTOR *
               (1 - (WS-TOTAL-DISCOUNT-PCT / 100))
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.

       CALCULATE-AUTO-DISCOUNTS SECTION.
       CALC-AUTO-DISC.
           MOVE ZERO TO WS-DISCOUNTS.

      *Safe driver discount
           IF WS-CLAIMS-3YEARS = 0
               MOVE 10.00 TO WS-SAFE-DRIVER-DISCOUNT
           END-IF.

      *Loyalty discount
           IF WS-YEARS-INSURED >= 10
               MOVE 15.00 TO WS-LOYALTY-DISCOUNT
           ELSE
               IF WS-YEARS-INSURED >= 5
                   MOVE 10.00 TO WS-LOYALTY-DISCOUNT
               ELSE
                   IF WS-YEARS-INSURED >= 3
                       MOVE 5.00 TO WS-LOYALTY-DISCOUNT
                   END-IF
               END-IF
           END-IF.

      *Safety features discount
           IF WS-SAFETY-RATING >= 5
               MOVE 5.00 TO WS-SAFETY-FEATURE-DISC
           END-IF.

      *Calculate total discount
           COMPUTE WS-TOTAL-DISCOUNT-PCT =
               WS-MULTICAR-DISCOUNT +
               WS-MULTIPOLICY-DISCOUNT +
               WS-SAFE-DRIVER-DISCOUNT +
               WS-LOYALTY-DISCOUNT +
               WS-SAFETY-FEATURE-DISC.

       CALCULATE-HOME-PREMIUM SECTION.
       CALC-HOME-PREM.
      *Base premium from property value
           COMPUTE WS-BASE-PREMIUM =
               (WS-PROPERTY-VALUE / 1000) * 0.50.

      *Construction type factor
           EVALUATE WS-CONSTRUCTION-TYPE
               WHEN 'BRICK'
                   MOVE 0.9 TO WS-VEHICLE-FACTOR
               WHEN 'FRAME'
                   MOVE 1.1 TO WS-VEHICLE-FACTOR
               WHEN 'MIXED'
                   MOVE 1.0 TO WS-VEHICLE-FACTOR
               WHEN OTHER
                   MOVE 1.05 TO WS-VEHICLE-FACTOR
           END-EVALUATE.

      *Age of home factor
           COMPUTE WS-AGE-FACTOR =
               1.0 + ((2024 - WS-CONSTRUCTION-YEAR) * 0.005).

      *Fire station distance factor
           IF WS-DISTANCE-FIRESTATION < 2.0
               MOVE 0.95 TO WS-LOCATION-FACTOR
           ELSE
               IF WS-DISTANCE-FIRESTATION < 5.0
                   MOVE 1.0 TO WS-LOCATION-FACTOR
               ELSE
                   MOVE 1.15 TO WS-LOCATION-FACTOR
               END-IF
           END-IF.

      *Credit score factor (same as auto)
           EVALUATE TRUE
               WHEN WS-CREDIT-SCORE >= 750
                   MOVE 0.85 TO WS-CREDIT-FACTOR
               WHEN WS-CREDIT-SCORE >= 700
                   MOVE 0.95 TO WS-CREDIT-FACTOR
               WHEN OTHER
                   MOVE 1.10 TO WS-CREDIT-FACTOR
           END-EVALUATE.

      *Claims factor
           IF WS-CLAIMS-3YEARS = 0
               MOVE 0.9 TO WS-CLAIMS-FACTOR
           ELSE
               COMPUTE WS-CLAIMS-FACTOR =
                   1.0 + (WS-CLAIMS-3YEARS * 0.30)
           END-IF.

      *Calculate discounts
           PERFORM CALCULATE-HOME-DISCOUNTS.

      *Calculate total premium
           COMPUTE WS-TOTAL-PREMIUM =
               WS-BASE-PREMIUM *
               WS-AGE-FACTOR *
               WS-LOCATION-FACTOR *
               WS-CREDIT-FACTOR *
               WS-VEHICLE-FACTOR *
               WS-CLAIMS-FACTOR *
               (1 - (WS-TOTAL-DISCOUNT-PCT / 100))
               ROUNDED MODE IS NEAREST-TOWARD-ZERO.

       CALCULATE-HOME-DISCOUNTS SECTION.
       CALC-HOME-DISC.
           MOVE ZERO TO WS-DISCOUNTS.

      *Alarm system discount
           IF WS-ALARM-SYSTEM = 'Y'
               MOVE 10.00 TO WS-SAFE-DRIVER-DISCOUNT
           END-IF.

      *Sprinkler system discount
           IF WS-SPRINKLER-SYSTEM = 'Y'
               MOVE 15.00 TO WS-SAFETY-FEATURE-DISC
           END-IF.

      *Loyalty discount
           IF WS-YEARS-INSURED >= 10
               MOVE 15.00 TO WS-LOYALTY-DISCOUNT
           ELSE
               IF WS-YEARS-INSURED >= 5
                   MOVE 10.00 TO WS-LOYALTY-DISCOUNT
               END-IF
           END-IF.

      *Calculate total discount
           COMPUTE WS-TOTAL-DISCOUNT-PCT =
               WS-MULTIPOLICY-DISCOUNT +
               WS-SAFE-DRIVER-DISCOUNT +
               WS-LOYALTY-DISCOUNT +
               WS-SAFETY-FEATURE-DISC.

       DISPLAY-PREMIUM-BREAKDOWN SECTION.
       SHOW-BREAKDOWN.
           DISPLAY 'Customer Profile:'.
           DISPLAY '  Age: ' WS-AGE.
           DISPLAY '  Credit Score: ' WS-CREDIT-SCORE.
           DISPLAY '  Years Insured: ' WS-YEARS-INSURED.
           DISPLAY '  Claims (3yr): ' WS-CLAIMS-3YEARS.
           DISPLAY SPACE.

           DISPLAY 'Premium Calculation:'.
           MOVE WS-BASE-PREMIUM TO WS-DISPLAY-AMOUNT.
           DISPLAY '  Base Premium: ' WS-DISPLAY-AMOUNT.

           MOVE WS-AGE-FACTOR TO WS-DISPLAY-FACTOR.
           DISPLAY '  Age Factor: ' WS-DISPLAY-FACTOR.

           MOVE WS-LOCATION-FACTOR TO WS-DISPLAY-FACTOR.
           DISPLAY '  Location Factor: ' WS-DISPLAY-FACTOR.

           MOVE WS-CREDIT-FACTOR TO WS-DISPLAY-FACTOR.
           DISPLAY '  Credit Factor: ' WS-DISPLAY-FACTOR.

           MOVE WS-CLAIMS-FACTOR TO WS-DISPLAY-FACTOR.
           DISPLAY '  Claims Factor: ' WS-DISPLAY-FACTOR.

           IF WS-TOTAL-DISCOUNT-PCT > ZERO
               MOVE WS-TOTAL-DISCOUNT-PCT TO WS-DISPLAY-PERCENT
               DISPLAY '  Total Discount: ' WS-DISPLAY-PERCENT '%'
           END-IF.

           DISPLAY SPACE.
           MOVE WS-TOTAL-PREMIUM TO WS-DISPLAY-AMOUNT.
           DISPLAY 'ANNUAL PREMIUM: ' WS-DISPLAY-AMOUNT.
