"""
Execution Analyzer

Analyzes COBOL procedure bodies to determine actual semantic behavior.
"""

from typing import List
from cobol_harmonizer.parser.cobol_parser import Procedure, COBOLStatement
from cobol_harmonizer.semantic.verb_mapper import VerbMapper, LJPWCoords


class ExecutionAnalyzer:
    """Analyze procedure execution semantics"""

    def __init__(self):
        self.verb_mapper = VerbMapper()

    def analyze_procedure(self, procedure: Procedure) -> LJPWCoords:
        """
        Analyze procedure body to extract execution semantics.

        Strategy:
        1. Map each verb in procedure to LJPW coordinates
        2. Calculate centroid (weighted average)
        3. Return execution coordinates

        Args:
            procedure: Parsed Procedure object

        Returns:
            LJPW coordinates representing actual execution
        """
        if not procedure.statements:
            # Empty procedure - neutral coordinates
            return (0.25, 0.25, 0.25, 0.25)

        # Collect coordinates for each statement
        statement_coords = []

        for statement in procedure.statements:
            coords = self.verb_mapper.map_verb(statement.verb)
            statement_coords.append(coords)

        # Calculate centroid (simple average for now)
        return self._calculate_centroid(statement_coords)

    def _calculate_centroid(self, coords_list: List[LJPWCoords]) -> LJPWCoords:
        """
        Calculate centroid (average) of multiple coordinate sets.

        Args:
            coords_list: List of LJPW coordinates

        Returns:
            Centroid coordinates
        """
        if not coords_list:
            return (0.25, 0.25, 0.25, 0.25)

        n = len(coords_list)
        l_avg = sum(c[0] for c in coords_list) / n
        j_avg = sum(c[1] for c in coords_list) / n
        p_avg = sum(c[2] for c in coords_list) / n
        w_avg = sum(c[3] for c in coords_list) / n

        return (l_avg, j_avg, p_avg, w_avg)

    def get_verb_distribution(self, procedure: Procedure) -> dict:
        """
        Get distribution of verbs by semantic dimension.

        Args:
            procedure: Parsed Procedure

        Returns:
            Dictionary with dimension counts
        """
        distribution = {
            'Love': 0,
            'Justice': 0,
            'Power': 0,
            'Wisdom': 0
        }

        for statement in procedure.statements:
            coords = self.verb_mapper.map_verb(statement.verb)
            dominant = self.verb_mapper.get_dominant_dimension(coords)
            distribution[dominant] += 1

        return distribution

    def identify_semantic_patterns(self, procedure: Procedure) -> List[str]:
        """
        Identify common semantic patterns in procedure.

        Args:
            procedure: Parsed Procedure

        Returns:
            List of pattern descriptions
        """
        patterns = []

        verbs = [s.verb for s in procedure.statements]

        # Pattern 1: File I/O sequence
        if 'OPEN' in verbs and 'READ' in verbs and 'CLOSE' in verbs:
            patterns.append("File I/O sequence (OPEN-READ-CLOSE)")

        # Pattern 2: Update pattern
        if 'READ' in verbs and ('REWRITE' in verbs or 'DELETE' in verbs):
            patterns.append("Update pattern (READ followed by modification)")

        # Pattern 3: Validation sequence
        if verbs.count('IF') >= 2 and 'MOVE' in verbs:
            patterns.append("Validation sequence (multiple IF statements)")

        # Pattern 4: Calculation sequence
        if 'COMPUTE' in verbs or 'ADD' in verbs or 'MULTIPLY' in verbs:
            patterns.append("Calculation sequence")

        # Pattern 5: Database operation (SQL)
        if any(v in verbs for v in ['SELECT', 'INSERT', 'UPDATE', 'DELETE']):
            patterns.append("Database operation (embedded SQL)")

        # Pattern 6: Procedure chain
        if verbs.count('PERFORM') >= 3:
            patterns.append("Procedure chain (multiple PERFORM statements)")

        # Pattern 7: Display/Report
        if verbs.count('DISPLAY') >= 2:
            patterns.append("Display/Report generation")

        return patterns

    def calculate_complexity(self, procedure: Procedure) -> int:
        """
        Calculate cyclomatic complexity of procedure.

        Args:
            procedure: Parsed Procedure

        Returns:
            Complexity score
        """
        complexity = 1  # Base complexity

        # Add complexity for each decision point
        for statement in procedure.statements:
            if statement.verb in ['IF', 'EVALUATE', 'SEARCH', 'PERFORM']:
                complexity += 1

        return complexity

    def is_pure_function(self, procedure: Procedure) -> bool:
        """
        Check if procedure is "pure" (no side effects).

        A pure procedure only reads data and performs calculations,
        without modifying state.

        Args:
            procedure: Parsed Procedure

        Returns:
            True if procedure appears to be pure
        """
        modifying_verbs = {
            'WRITE', 'REWRITE', 'DELETE', 'MOVE', 'SET',
            'INITIALIZE', 'OPEN', 'CLOSE', 'CALL',
            'INSERT', 'UPDATE', 'DELETE'  # SQL verbs
        }

        verbs = {s.verb for s in procedure.statements}

        # If any modifying verb is present, not pure
        return not bool(verbs & modifying_verbs)

    def get_execution_summary(self, procedure: Procedure) -> dict:
        """
        Get comprehensive execution summary.

        Args:
            procedure: Parsed Procedure

        Returns:
            Dictionary with execution analysis
        """
        coords = self.analyze_procedure(procedure)
        distribution = self.get_verb_distribution(procedure)
        patterns = self.identify_semantic_patterns(procedure)
        complexity = self.calculate_complexity(procedure)
        is_pure = self.is_pure_function(procedure)

        return {
            'name': procedure.name,
            'type': procedure.type,
            'execution_coords': {
                'love': round(coords[0], 3),
                'justice': round(coords[1], 3),
                'power': round(coords[2], 3),
                'wisdom': round(coords[3], 3),
            },
            'dominant_dimension': self.verb_mapper.get_dominant_dimension(coords),
            'statement_count': len(procedure.statements),
            'verb_distribution': distribution,
            'semantic_patterns': patterns,
            'cyclomatic_complexity': complexity,
            'is_pure': is_pure,
            'performed_procedures': procedure.performed_procedures,
        }
