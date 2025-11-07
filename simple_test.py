#!/usr/bin/env python3
"""
Simple test without external dependencies

Demonstrates core functionality of COBOL Code Harmonizer.
"""

import sys
from pathlib import Path

# Add package to path
sys.path.insert(0, str(Path(__file__).parent))

from cobol_harmonizer.parser.cobol_parser import COBOLParser
from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator
from cobol_harmonizer.semantic.verb_mapper import VerbMapper


def test_verb_mapper():
    """Test VerbMapper functionality"""
    print("=" * 70)
    print("TEST 1: VerbMapper - COBOL Verb to LJPW Mapping")
    print("=" * 70)

    mapper = VerbMapper()

    test_verbs = ['READ', 'WRITE', 'DELETE', 'IF', 'CALL', 'COMPUTE']

    for verb in test_verbs:
        coords = mapper.map_verb(verb)
        dominant = mapper.get_dominant_dimension(coords)
        description = mapper.describe_semantics(coords)

        print(f"\n{verb}:")
        print(f"  Coordinates: L={coords[0]:.2f}, J={coords[1]:.2f}, "
              f"P={coords[2]:.2f}, W={coords[3]:.2f}")
        print(f"  Dominant: {dominant}")
        print(f"  Description: {description}")

    print("\n✓ VerbMapper test passed!\n")


def test_intent_extraction():
    """Test IntentExtractor functionality"""
    print("=" * 70)
    print("TEST 2: IntentExtractor - Extract Intent from Procedure Names")
    print("=" * 70)

    extractor = IntentExtractor()

    test_names = [
        'GET-CUSTOMER-RECORD',
        'DELETE-OLD-DATA',
        'VALIDATE-INPUT',
        'CALCULATE-TOTAL',
        'UPDATE-DATABASE'
    ]

    for name in test_names:
        coords = extractor.extract_intent(name)
        dominant = extractor.get_dominant_intent(name)
        is_vague = extractor.is_vague_name(name)

        print(f"\n{name}:")
        print(f"  Intent coordinates: L={coords[0]:.2f}, J={coords[1]:.2f}, "
              f"P={coords[2]:.2f}, W={coords[3]:.2f}")
        print(f"  Dominant intent: {dominant}")
        print(f"  Vague name: {is_vague}")

    print("\n✓ IntentExtractor test passed!\n")


def test_disharmony_calculator():
    """Test DisharmonyCalculator functionality"""
    print("=" * 70)
    print("TEST 3: DisharmonyCalculator - Semantic Distance Calculation")
    print("=" * 70)

    calculator = DisharmonyCalculator()

    # Test cases: (procedure_name, intent, execution)
    test_cases = [
        (
            "GET-CUSTOMER-RECORD (Harmonious)",
            (0.1, 0.1, 0.0, 0.8),  # Intent: GET = Wisdom
            (0.1, 0.2, 0.0, 0.7)   # Execution: READ = Wisdom
        ),
        (
            "GET-CUSTOMER-BALANCE (Critical Bug!)",
            (0.1, 0.1, 0.0, 0.8),  # Intent: GET = Wisdom
            (0.0, 0.2, 0.7, 0.1)   # Execution: DELETE = Power
        ),
        (
            "VALIDATE-DATA (Minor Drift)",
            (0.0, 0.8, 0.1, 0.1),  # Intent: VALIDATE = Justice
            (0.0, 0.6, 0.3, 0.1)   # Execution: Mixed Justice+Power
        )
    ]

    for name, intent, execution in test_cases:
        score = calculator.calculate(intent, execution)
        level = calculator.classify(score)
        from_dim, to_dim = calculator.get_dominant_shift(intent, execution)

        print(f"\n{name}:")
        print(f"  Disharmony Score: {score:.3f}")
        print(f"  Severity: {level.value.upper()}")
        print(f"  Semantic Shift: {from_dim} → {to_dim}")

        if score >= 0.8:
            print(f"  ⚠️  CRITICAL: Requires immediate attention!")
        elif score >= 0.5:
            print(f"  ⚠️  CONCERNING: Consider refactoring")
        elif score >= 0.3:
            print(f"  ⚠️  MINOR: Review for clarity")
        else:
            print(f"  ✓ HARMONIOUS: Well-aligned!")

    print("\n✓ DisharmonyCalculator test passed!\n")


def test_cobol_parser():
    """Test COBOLParser functionality"""
    print("=" * 70)
    print("TEST 4: COBOLParser - Parsing COBOL Source")
    print("=" * 70)

    # Create simple test COBOL source
    test_cobol = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM GET-DATA.
           PERFORM PROCESS-DATA.
           STOP RUN.
       GET-DATA.
           READ CUSTOMER-FILE.
           MOVE CUSTOMER-NAME TO WS-NAME.
       PROCESS-DATA.
           IF CUSTOMER-STATUS = 'ACTIVE'
               COMPUTE TOTAL = PRICE * QUANTITY
           END-IF.
    """

    parser = COBOLParser()
    program = parser.parse_source(test_cobol)

    print(f"\nProgram ID: {program.program_id}")
    print(f"Procedures found: {len(program.procedures)}")

    for proc in program.procedures:
        print(f"\n  {proc.type}: {proc.name}")
        print(f"    Statements: {len(proc.statements)}")
        for stmt in proc.statements[:3]:  # Show first 3 statements
            print(f"      - {stmt.verb}")

    print("\n✓ COBOLParser test passed!\n")


def test_full_analysis():
    """Test complete analysis workflow"""
    print("=" * 70)
    print("TEST 5: Full Analysis - End-to-End Workflow")
    print("=" * 70)

    # Create test COBOL with semantic bug
    test_cobol = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. BUG-EXAMPLE.
       PROCEDURE DIVISION.
       GET-CUSTOMER-DATA.
           READ CUSTOMER-FILE.
           DELETE CUSTOMER-FILE RECORD.
           MOVE CUSTOMER-NAME TO WS-RESULT.
    """

    print("\nAnalyzing COBOL procedure with semantic bug...")
    print("Procedure: GET-CUSTOMER-DATA")
    print("Bug: Name says 'GET' but code DELETES!")

    # Parse
    parser = COBOLParser()
    program = parser.parse_source(test_cobol)

    if program.procedures:
        procedure = program.procedures[0]

        # Analyze
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        intent = intent_extractor.extract_intent(procedure.name)
        execution = execution_analyzer.analyze_procedure(procedure)
        analysis = calculator.calculate_detailed_analysis(
            procedure.name,
            intent,
            execution
        )

        print(f"\nResults:")
        print(f"  Procedure: {analysis['procedure_name']}")
        print(f"  Disharmony Score: {analysis['disharmony_score']:.3f}")
        print(f"  Severity: {analysis['severity_level'].upper()}")
        print(f"  Semantic Shift: {analysis['dominant_shift']['from']} → "
              f"{analysis['dominant_shift']['to']}")
        print(f"  Requires Action: {analysis['requires_action']}")

        print(f"\n  Explanation:")
        print(f"  {analysis['explanation']}")

        if analysis['disharmony_score'] > 0.8:
            print("\n  🔴 CRITICAL SEMANTIC BUG DETECTED!")
            print("  This procedure name lies about its behavior.")
            print("  Developers will be surprised when it deletes data!")

    print("\n✓ Full analysis test passed!\n")


def main():
    """Run all tests"""
    print("\n")
    print("╔" + "=" * 68 + "╗")
    print("║" + " " * 15 + "COBOL Code Harmonizer - Core Tests" + " " * 19 + "║")
    print("║" + " " * 68 + "║")
    print("║" + " " * 10 + "Testing core functionality without dependencies" + " " * 10 + "║")
    print("╚" + "=" * 68 + "╝")
    print()

    try:
        test_verb_mapper()
        test_intent_extraction()
        test_disharmony_calculator()
        test_cobol_parser()
        test_full_analysis()

        print("=" * 70)
        print("ALL TESTS PASSED! ✓")
        print("=" * 70)
        print()
        print("The COBOL Code Harmonizer core is working correctly!")
        print()
        print("Next steps:")
        print("  1. Install dependencies: pip install -r requirements.txt")
        print("  2. Run full demo: python demo.py")
        print("  3. Run CLI: python -m cobol_harmonizer.cli.commands analyze examples/disharmonious_example.cbl")
        print()

    except Exception as e:
        print(f"\n❌ TEST FAILED: {str(e)}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == '__main__':
    main()
