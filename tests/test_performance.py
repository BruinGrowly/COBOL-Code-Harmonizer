"""
Performance Benchmarks for COBOL Code Harmonizer

Tests enterprise-scale performance to ensure the tool can handle
large codebases efficiently.

Run with: pytest tests/test_performance.py --benchmark-only

NOTE: These tests require pytest-benchmark. If not installed, they will be skipped.
Install with: pip install pytest-benchmark
"""

import pytest
import tempfile
import os
from pathlib import Path

from cobol_harmonizer.parser.cobol_parser import COBOLParser
from cobol_harmonizer.semantic.intent_extractor import IntentExtractor
from cobol_harmonizer.semantic.execution_analyzer import ExecutionAnalyzer
from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator
from cobol_harmonizer.compliance import ComplianceTagger, ComplianceRiskAssessor

# Check if pytest-benchmark is available
try:
    import pytest_benchmark

    BENCHMARK_AVAILABLE = True
except ImportError:
    BENCHMARK_AVAILABLE = False

# Skip all tests in this module if pytest-benchmark is not available
pytestmark = pytest.mark.skipif(
    not BENCHMARK_AVAILABLE,
    reason="pytest-benchmark not installed. Install with: pip install pytest-benchmark",
)


# Sample COBOL programs of varying sizes
SMALL_COBOL_PROGRAM = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMALL-PROG.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Hello World".
           STOP RUN.
"""

MEDIUM_COBOL_PROGRAM = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MEDIUM-PROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-ID      PIC 9(10).
       01 CUSTOMER-NAME    PIC X(50).
       01 ACCOUNT-BALANCE  PIC 9(10)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-CUSTOMER-DATA.
           PERFORM VALIDATE-CUSTOMER-DATA.
           PERFORM PROCESS-CUSTOMER-ACCOUNT.
           STOP RUN.

       READ-CUSTOMER-DATA.
           MOVE 123456 TO CUSTOMER-ID.
           MOVE "JOHN DOE" TO CUSTOMER-NAME.
           MOVE 1000.50 TO ACCOUNT-BALANCE.

       VALIDATE-CUSTOMER-DATA.
           IF CUSTOMER-ID = 0
               DISPLAY "Invalid Customer ID"
           END-IF.
           IF ACCOUNT-BALANCE < 0
               DISPLAY "Negative Balance"
           END-IF.

       PROCESS-CUSTOMER-ACCOUNT.
           COMPUTE ACCOUNT-BALANCE = ACCOUNT-BALANCE * 1.05.
           DISPLAY "Processed: " CUSTOMER-NAME.
"""

LARGE_COBOL_PROGRAM = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-RECORD.
          05 CUSTOMER-ID        PIC 9(10).
          05 CUSTOMER-NAME      PIC X(50).
          05 CUSTOMER-ADDRESS   PIC X(100).
          05 ACCOUNT-BALANCE    PIC 9(10)V99.
          05 LAST-TRANSACTION   PIC X(10).

       01 TRANSACTION-RECORD.
          05 TRANS-ID           PIC 9(15).
          05 TRANS-TYPE         PIC X(10).
          05 TRANS-AMOUNT       PIC 9(10)V99.
          05 TRANS-DATE         PIC X(10).

       01 WS-COUNTERS.
          05 RECORD-COUNT       PIC 9(8) VALUE 0.
          05 ERROR-COUNT        PIC 9(8) VALUE 0.
          05 SUCCESS-COUNT      PIC 9(8) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SYSTEM.
           PERFORM READ-CUSTOMER-FILE.
           PERFORM PROCESS-TRANSACTIONS.
           PERFORM GENERATE-REPORTS.
           PERFORM CLEANUP-RESOURCES.
           STOP RUN.

       INITIALIZE-SYSTEM.
           MOVE 0 TO RECORD-COUNT.
           MOVE 0 TO ERROR-COUNT.
           MOVE 0 TO SUCCESS-COUNT.
           DISPLAY "System Initialized".

       READ-CUSTOMER-FILE.
           MOVE 123456 TO CUSTOMER-ID.
           MOVE "JOHN DOE" TO CUSTOMER-NAME.
           MOVE "123 MAIN ST" TO CUSTOMER-ADDRESS.
           MOVE 5000.00 TO ACCOUNT-BALANCE.
           ADD 1 TO RECORD-COUNT.

       VALIDATE-CUSTOMER-RECORD.
           IF CUSTOMER-ID = 0
               ADD 1 TO ERROR-COUNT
               DISPLAY "Error: Invalid Customer ID"
           ELSE
               IF ACCOUNT-BALANCE < 0
                   ADD 1 TO ERROR-COUNT
                   DISPLAY "Error: Negative Balance"
               ELSE
                   ADD 1 TO SUCCESS-COUNT
               END-IF
           END-IF.

       PROCESS-TRANSACTIONS.
           PERFORM PROCESS-DEPOSIT.
           PERFORM PROCESS-WITHDRAWAL.
           PERFORM UPDATE-BALANCE.

       PROCESS-DEPOSIT.
           MOVE 1001 TO TRANS-ID.
           MOVE "DEPOSIT" TO TRANS-TYPE.
           MOVE 500.00 TO TRANS-AMOUNT.
           ADD TRANS-AMOUNT TO ACCOUNT-BALANCE.

       PROCESS-WITHDRAWAL.
           MOVE 1002 TO TRANS-ID.
           MOVE "WITHDRAWAL" TO TRANS-TYPE.
           MOVE 200.00 TO TRANS-AMOUNT.
           IF ACCOUNT-BALANCE >= TRANS-AMOUNT
               SUBTRACT TRANS-AMOUNT FROM ACCOUNT-BALANCE
           ELSE
               DISPLAY "Insufficient Funds"
               ADD 1 TO ERROR-COUNT
           END-IF.

       UPDATE-BALANCE.
           DISPLAY "Updated Balance: " ACCOUNT-BALANCE.

       CALCULATE-INTEREST.
           COMPUTE ACCOUNT-BALANCE = ACCOUNT-BALANCE * 1.02.

       GENERATE-REPORTS.
           DISPLAY "Total Records: " RECORD-COUNT.
           DISPLAY "Successful: " SUCCESS-COUNT.
           DISPLAY "Errors: " ERROR-COUNT.

       CLEANUP-RESOURCES.
           DISPLAY "Cleanup Complete".
"""


class TestParserPerformance:
    """Benchmark COBOL parser performance"""

    def test_parse_small_program(self, benchmark):
        """Benchmark parsing a small COBOL program (10 LOC)"""
        parser = COBOLParser()
        result = benchmark(parser.parse_source, SMALL_COBOL_PROGRAM)
        assert result is not None
        assert len(result.procedures) >= 1

    def test_parse_medium_program(self, benchmark):
        """Benchmark parsing a medium COBOL program (50 LOC)"""
        parser = COBOLParser()
        result = benchmark(parser.parse_source, MEDIUM_COBOL_PROGRAM)
        assert result is not None
        assert len(result.procedures) >= 3

    def test_parse_large_program(self, benchmark):
        """Benchmark parsing a large COBOL program (100+ LOC)"""
        parser = COBOLParser()
        result = benchmark(parser.parse_source, LARGE_COBOL_PROGRAM)
        assert result is not None
        assert len(result.procedures) >= 10

    def test_parse_1000_loc_program(self, benchmark):
        """Benchmark parsing 1,000 LOC program"""
        # Generate a 1K LOC program
        program = LARGE_COBOL_PROGRAM * 10
        parser = COBOLParser()
        result = benchmark(parser.parse_source, program)
        assert result is not None


class TestSemanticAnalysisPerformance:
    """Benchmark semantic analysis performance"""

    def test_intent_extraction(self, benchmark):
        """Benchmark intent extraction from procedure names"""
        extractor = IntentExtractor()
        result = benchmark(extractor.extract_intent, "VALIDATE-CUSTOMER-RECORD")
        assert result is not None
        assert len(result) == 4  # LJPW coords

    def test_execution_analysis(self, benchmark):
        """Benchmark execution analysis"""
        parser = COBOLParser()
        program = parser.parse_source(MEDIUM_COBOL_PROGRAM)
        analyzer = ExecutionAnalyzer()

        # Benchmark analyzing first procedure
        procedure = program.procedures[0]
        result = benchmark(analyzer.analyze_procedure, procedure)
        assert result is not None

    def test_disharmony_calculation(self, benchmark):
        """Benchmark disharmony score calculation"""
        calculator = DisharmonyCalculator()
        intent = (0.2, 0.7, 0.1, 0.0)  # Justice-dominant
        execution = (0.0, 0.1, 0.8, 0.1)  # Power-dominant

        result = benchmark(calculator.calculate, intent, execution)
        assert result > 0

    def test_full_semantic_pipeline(self, benchmark):
        """Benchmark complete semantic analysis pipeline"""
        parser = COBOLParser()
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        def analyze_procedure_complete():
            program = parser.parse_source(MEDIUM_COBOL_PROGRAM)
            procedure = program.procedures[1]  # VALIDATE procedure

            intent = intent_extractor.extract_intent(procedure.name)
            execution = execution_analyzer.analyze_procedure(procedure)
            score = calculator.calculate(intent, execution)
            return score

        result = benchmark(analyze_procedure_complete)
        assert result >= 0


class TestCompliancePerformance:
    """Benchmark compliance analysis performance"""

    def test_compliance_tagging(self, benchmark):
        """Benchmark compliance tagging"""
        tagger = ComplianceTagger()
        result = benchmark(
            tagger.tag_procedure,
            "PROCESS-PAYMENT-TRANSACTION",
            verbs=["MOVE", "WRITE", "UPDATE"],
        )
        assert result is not None

    def test_risk_assessment(self, benchmark):
        """Benchmark risk assessment"""
        tagger = ComplianceTagger()
        assessor = ComplianceRiskAssessor(tagger)

        result = benchmark(
            assessor.assess_procedure,
            procedure_name="VALIDATE-CUSTOMER-ACCOUNT",
            file_path="banking.cbl",
            disharmony_score=0.75,
            disharmony_level="concerning",
            verbs=["MOVE", "UPDATE", "WRITE"],
            fan_in=5,
        )
        assert result is not None
        assert result.risk_score >= 0


class TestBatchProcessingPerformance:
    """Benchmark batch processing performance"""

    def test_analyze_10_files(self, benchmark):
        """Benchmark analyzing 10 COBOL files"""
        parser = COBOLParser()

        def analyze_batch():
            results = []
            for i in range(10):
                result = parser.parse_source(MEDIUM_COBOL_PROGRAM)
                results.append(result)
            return results

        results = benchmark(analyze_batch)
        assert len(results) == 10

    def test_analyze_100_procedures(self, benchmark):
        """Benchmark analyzing 100 procedures"""
        parser = COBOLParser()
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        # Parse once, reuse
        program = parser.parse_source(LARGE_COBOL_PROGRAM)
        procedures = program.procedures * 10  # Replicate to get ~100

        def analyze_all_procedures():
            scores = []
            for proc in procedures[:100]:
                intent = intent_extractor.extract_intent(proc.name)
                execution = execution_analyzer.analyze_procedure(proc)
                score = calculator.calculate(intent, execution)
                scores.append(score)
            return scores

        results = benchmark(analyze_all_procedures)
        assert len(results) <= 100


class TestRealWorldScenarios:
    """Benchmark real-world usage scenarios"""

    def test_ci_cd_pipeline_simulation(self, benchmark):
        """Simulate CI/CD pipeline: analyze changed file"""
        parser = COBOLParser()
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        def ci_cd_check():
            """Typical CI/CD: parse file, analyze, check thresholds"""
            program = parser.parse_source(LARGE_COBOL_PROGRAM)

            findings = []
            for proc in program.procedures:
                intent = intent_extractor.extract_intent(proc.name)
                execution = execution_analyzer.analyze_procedure(proc)
                score = calculator.calculate(intent, execution)

                if score > 0.5:  # Threshold check
                    findings.append({"procedure": proc.name, "score": score})

            return findings

        findings = benchmark(ci_cd_check)
        # Should complete in under 100ms for CI/CD
        assert isinstance(findings, list)

    def test_nightly_scan_simulation(self, benchmark):
        """Simulate nightly scan: analyze multiple files"""
        parser = COBOLParser()
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        programs = [SMALL_COBOL_PROGRAM, MEDIUM_COBOL_PROGRAM, LARGE_COBOL_PROGRAM]

        def nightly_scan():
            """Typical nightly: batch analyze 50 files"""
            all_findings = []
            for _ in range(50):
                for source in programs:
                    program = parser.parse_source(source)
                    for proc in program.procedures:
                        intent = intent_extractor.extract_intent(proc.name)
                        execution = execution_analyzer.analyze_procedure(proc)
                        score = calculator.calculate(intent, execution)
                        if score > 0.5:
                            all_findings.append(score)
            return all_findings

        findings = benchmark(nightly_scan)
        # Nightly scans can take longer, but should be reasonable
        assert isinstance(findings, list)


class TestScalabilityBenchmarks:
    """Test scalability with increasing dataset sizes"""

    @pytest.mark.parametrize("multiplier", [1, 10, 100])
    def test_scaling_analysis(self, benchmark, multiplier):
        """Test how performance scales with codebase size"""
        parser = COBOLParser()
        intent_extractor = IntentExtractor()
        execution_analyzer = ExecutionAnalyzer()
        calculator = DisharmonyCalculator()

        # Scale up the program
        program_text = LARGE_COBOL_PROGRAM * multiplier

        def scaled_analysis():
            program = parser.parse_source(program_text)
            scores = []
            for proc in program.procedures:
                intent = intent_extractor.extract_intent(proc.name)
                execution = execution_analyzer.analyze_procedure(proc)
                score = calculator.calculate(intent, execution)
                scores.append(score)
            return scores

        results = benchmark(scaled_analysis)
        assert len(results) >= multiplier


# Performance targets (assertions to ensure we meet SLAs)
def test_performance_targets():
    """Assert performance targets are met"""
    import time

    parser = COBOLParser()
    intent_extractor = IntentExtractor()
    execution_analyzer = ExecutionAnalyzer()
    calculator = DisharmonyCalculator()

    # Target: Parse 1K LOC in < 100ms
    start = time.perf_counter()
    program = parser.parse_source(LARGE_COBOL_PROGRAM * 10)
    parse_time = (time.perf_counter() - start) * 1000
    assert parse_time < 100, f"Parsing too slow: {parse_time}ms (target: <100ms)"

    # Target: Analyze procedure in < 5ms (adjusted for CI environment variability)
    start = time.perf_counter()
    proc = program.procedures[0]
    intent = intent_extractor.extract_intent(proc.name)
    execution = execution_analyzer.analyze_procedure(proc)
    score = calculator.calculate(intent, execution)
    analyze_time = (time.perf_counter() - start) * 1000
    assert analyze_time < 5, f"Analysis too slow: {analyze_time}ms (target: <5ms)"

    # Target: Batch 100 procedures in < 100ms
    start = time.perf_counter()
    for _ in range(100):
        intent = intent_extractor.extract_intent("TEST-PROCEDURE")
        calculator.calculate(intent, intent)
    batch_time = (time.perf_counter() - start) * 1000
    assert batch_time < 100, f"Batch too slow: {batch_time}ms (target: <100ms)"
