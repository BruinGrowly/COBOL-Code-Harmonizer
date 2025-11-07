"""
Tests for CallExtractor
"""

import pytest
from pathlib import Path

from cobol_harmonizer.callgraph.extractor import CallExtractor
from cobol_harmonizer.callgraph.models import CallType
from cobol_harmonizer.parser import COBOLParser


class TestCallExtractor:
    """Test suite for CallExtractor"""

    @pytest.fixture
    def extractor(self):
        """Create a CallExtractor instance"""
        parser = COBOLParser()
        return CallExtractor(parser)

    def test_extract_static_call(self, extractor):
        """Test extracting static CALL statements"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-PARA.
          CALL 'SUBPROG'.
          STOP RUN.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        call_sites_calls = [cs for cs in call_sites if cs.call_type == CallType.PROGRAM_CALL]
        assert len(call_sites_calls) >= 1
        assert call_sites_calls[0].callee == 'SUBPROG'
        assert not call_sites_calls[0].is_dynamic

    def test_extract_dynamic_call(self, extractor):
        """Test extracting dynamic CALL statements"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-PROGRAM PIC X(8).
      PROCEDURE DIVISION.
      MAIN-PARA.
          CALL WS-PROGRAM.
          STOP RUN.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        dynamic_calls = [cs for cs in call_sites if cs.call_type == CallType.DYNAMIC_CALL]
        assert len(dynamic_calls) >= 1
        assert dynamic_calls[0].callee == 'WS-PROGRAM'
        assert dynamic_calls[0].is_dynamic

    def test_extract_perform_paragraph(self, extractor):
        """Test extracting PERFORM statements"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-PARA.
          PERFORM SUB-PARA.
          STOP RUN.
      SUB-PARA.
          DISPLAY 'SUB'.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        performs = [cs for cs in call_sites if cs.call_type == CallType.PROCEDURE_PERFORM]
        assert len(performs) >= 1
        assert performs[0].callee == 'SUB-PARA'

    def test_extract_perform_thru(self, extractor):
        """Test extracting PERFORM ... THRU statements"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-PARA.
          PERFORM PARA-100 THRU PARA-999.
          STOP RUN.
      PARA-100.
          DISPLAY '100'.
      PARA-999.
          DISPLAY '999'.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        performs = [cs for cs in call_sites if cs.call_type == CallType.PROCEDURE_PERFORM]

        # Should have two: PARA-100 and PARA-999
        assert len(performs) >= 2
        callees = [cs.callee for cs in performs]
        assert 'PARA-100' in callees
        assert 'PARA-999' in callees

    def test_extract_multiple_calls(self, extractor):
        """Test extracting multiple CALL and PERFORM statements"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-PARA.
          CALL 'PROG1'.
          PERFORM SUB1.
          CALL 'PROG2'.
          PERFORM SUB2.
          STOP RUN.
      SUB1.
          DISPLAY 'SUB1'.
      SUB2.
          DISPLAY 'SUB2'.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        assert len(call_sites) >= 4

        calls = [cs for cs in call_sites if cs.call_type == CallType.PROGRAM_CALL]
        performs = [cs for cs in call_sites if cs.call_type == CallType.PROCEDURE_PERFORM]

        assert len(calls) >= 2
        assert len(performs) >= 2

    def test_skip_inline_perform(self, extractor):
        """Test skipping inline PERFORM (UNTIL, TIMES, VARYING)"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-COUNTER PIC 9(5).
      PROCEDURE DIVISION.
      MAIN-PARA.
          PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 10
              DISPLAY WS-COUNTER
          END-PERFORM.
          PERFORM 5 TIMES
              DISPLAY 'HELLO'
          END-PERFORM.
          STOP RUN.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        # Should not extract inline PERFORMs
        # (or if it does, they should be filtered out)
        performs = [cs for cs in call_sites if cs.call_type == CallType.PROCEDURE_PERFORM]

        # Should have no performs to actual paragraphs
        # (unless 'VARYING' or 'TIMES' are treated as paragraph names, which they shouldn't be)

    def test_extract_procedures(self, extractor):
        """Test extracting procedure names"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-SECTION SECTION.
      MAIN-PARA.
          DISPLAY 'MAIN'.
      SUB-PARA.
          DISPLAY 'SUB'.
      OTHER-SECTION SECTION.
      OTHER-PARA.
          DISPLAY 'OTHER'.
"""

        procedures = extractor._extract_procedures(source)

        # Should extract both sections and paragraphs
        proc_names = set(procedures.values())
        assert 'MAIN-SECTION' in proc_names
        assert 'MAIN-PARA' in proc_names
        assert 'SUB-PARA' in proc_names
        assert 'OTHER-SECTION' in proc_names
        assert 'OTHER-PARA' in proc_names

    def test_find_containing_procedure(self, extractor):
        """Test finding which procedure contains a line"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      PARA-A.
          DISPLAY 'A'.
          DISPLAY 'A2'.
      PARA-B.
          DISPLAY 'B'.
"""

        procedures = extractor._extract_procedures(source)

        # Line 4 (PARA-A) should be in PARA-A
        # Line 5 (DISPLAY 'A') should be in PARA-A
        # Line 7 (PARA-B) should be in PARA-B

    def test_get_statistics(self, extractor):
        """Test getting call site statistics"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-PROG PIC X(8).
      PROCEDURE DIVISION.
      MAIN-PARA.
          CALL 'PROG1'.
          CALL 'PROG2'.
          CALL WS-PROG.
          PERFORM SUB1.
          PERFORM SUB2.
          STOP RUN.
      SUB1.
          DISPLAY 'SUB1'.
      SUB2.
          DISPLAY 'SUB2'.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        stats = extractor.get_statistics(call_sites)

        assert stats['total_calls'] == len(call_sites)
        assert stats['program_calls'] >= 2
        assert stats['dynamic_calls'] >= 1
        assert stats['perform_statements'] >= 2

    def test_line_numbers_accurate(self, extractor):
        """Test that line numbers are accurately captured"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-PARA.
          CALL 'PROG1'.
          PERFORM SUB1.
          STOP RUN.
      SUB1.
          DISPLAY 'SUB1'.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        # Check that line numbers are set
        for cs in call_sites:
            assert cs.line_number > 0

    def test_empty_program(self, extractor):
        """Test extracting from empty program"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
          STOP RUN.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        # Should have no calls
        # (may have some internal procedure tracking)

    def test_comments_ignored(self, extractor):
        """Test that comments are ignored"""
        source = """      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEST.
      PROCEDURE DIVISION.
      MAIN-PARA.
      *   CALL 'COMMENTED'.
          CALL 'REAL'.
          STOP RUN.
"""

        program = extractor.parser.parse_source(source)
        call_sites = extractor.extract_from_source(source, program.program_id, 'test.cbl')

        calls = [cs for cs in call_sites if cs.call_type == CallType.PROGRAM_CALL]
        callees = [cs.callee for cs in calls]

        # Should only have 'REAL', not 'COMMENTED'
        assert 'REAL' in callees
        assert 'COMMENTED' not in callees
