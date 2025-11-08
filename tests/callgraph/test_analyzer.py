"""
Tests for CallGraphAnalyzer
"""

import pytest

from cobol_harmonizer.callgraph.builder import CallGraphBuilder
from cobol_harmonizer.callgraph.analyzer import CallGraphAnalyzer
from cobol_harmonizer.callgraph.models import CallSite, CallType


class TestCallGraphAnalyzer:
    """Test suite for CallGraphAnalyzer"""

    @pytest.fixture
    def sample_graph(self):
        """Create a sample call graph for testing"""
        call_sites = [
            CallSite("PROG.MAIN", "SUB1", CallType.PROCEDURE_PERFORM, 10, "prog.cbl", False),
            CallSite("PROG.MAIN", "SUB2", CallType.PROCEDURE_PERFORM, 11, "prog.cbl", False),
            CallSite("PROG.SUB1", "SUB3", CallType.PROCEDURE_PERFORM, 20, "prog.cbl", False),
            CallSite("PROG.SUB2", "SUB3", CallType.PROCEDURE_PERFORM, 30, "prog.cbl", False),
            CallSite("PROG.SUB3", "SUB4", CallType.PROCEDURE_PERFORM, 40, "prog.cbl", False),
        ]

        builder = CallGraphBuilder()
        return builder.build(call_sites)

    @pytest.fixture
    def analyzer(self, sample_graph):
        """Create analyzer with sample graph"""
        return CallGraphAnalyzer(sample_graph)

    def test_analyze_impact_basic(self, analyzer):
        """Test basic impact analysis"""
        analysis = analyzer.analyze_impact("PROG.SUB3")

        assert analysis is not None
        assert analysis.target_node == "PROG.SUB3"
        assert analysis.total_impact > 0

    def test_direct_impact(self, analyzer):
        """Test identifying direct callers"""
        analysis = analyzer.analyze_impact("PROG.SUB3")

        # SUB3 is called by SUB1 and SUB2
        assert len(analysis.directly_affected) == 2
        assert "PROG.SUB1" in analysis.directly_affected
        assert "PROG.SUB2" in analysis.directly_affected

    def test_transitive_impact(self, analyzer):
        """Test identifying transitive callers"""
        analysis = analyzer.analyze_impact("PROG.SUB3")

        # MAIN calls SUB1 and SUB2, which call SUB3
        # So MAIN is transitively affected
        assert "PROG.MAIN" in analysis.transitively_affected

    def test_risk_scoring(self, analyzer):
        """Test risk score calculation"""
        analysis = analyzer.analyze_impact("PROG.SUB3")

        # Risk score should be between 0 and 100
        assert 0 <= analysis.risk_score <= 100

        # SUB3 has fan-in of 2, so should have some risk
        assert analysis.risk_score > 0

    def test_risk_levels(self, analyzer):
        """Test risk level classification"""
        analysis = analyzer.analyze_impact("PROG.SUB3")

        # Risk level should be one of the valid levels
        assert analysis.risk_level in ["LOW", "MEDIUM", "HIGH", "CRITICAL"]

    def test_high_fan_in_warning(self):
        """Test warning for high fan-in nodes"""
        # Create a node with many callers
        call_sites = []
        for i in range(15):
            call_sites.append(
                CallSite(
                    f"PROG.CALLER{i}",
                    "TARGET",
                    CallType.PROCEDURE_PERFORM,
                    10 + i,
                    "prog.cbl",
                    False,
                )
            )

        builder = CallGraphBuilder()
        graph = builder.build(call_sites)
        analyzer = CallGraphAnalyzer(graph)

        analysis = analyzer.analyze_impact("PROG.TARGET")

        # Should have warnings about high fan-in
        assert len(analysis.warnings) > 0
        assert any("called by" in w for w in analysis.warnings)

    def test_entry_point_warning(self, analyzer):
        """Test warning for entry points"""
        analysis = analyzer.analyze_impact("PROG.MAIN")

        # MAIN is an entry point
        assert any("entry point" in w.lower() for w in analysis.warnings)

    def test_recommendations(self, analyzer):
        """Test that recommendations are generated"""
        analysis = analyzer.analyze_impact("PROG.SUB3")

        # Should have some recommendations
        assert len(analysis.recommendations) > 0

    def test_find_dead_code(self):
        """Test finding dead code"""
        call_sites = [
            CallSite("PROG.MAIN", "SUB1", CallType.PROCEDURE_PERFORM, 10, "prog.cbl", False),
            # SUB2 is never called - dead code
        ]

        # Manually add SUB2 as a caller to simulate orphaned node
        call_sites.append(
            CallSite("PROG.SUB2", "SUB3", CallType.PROCEDURE_PERFORM, 20, "prog.cbl", False)
        )

        builder = CallGraphBuilder()
        graph = builder.build(call_sites)
        analyzer = CallGraphAnalyzer(graph)

        dead_code = analyzer.find_dead_code()

        # SUB2 and SUB3 should be dead code
        assert "PROG.SUB2" in dead_code
        assert "PROG.SUB3" in dead_code

    def test_find_circular_dependencies(self):
        """Test finding circular dependencies"""
        call_sites = [
            CallSite("PROG.A", "B", CallType.PROCEDURE_PERFORM, 10, "prog.cbl", False),
            CallSite("PROG.B", "C", CallType.PROCEDURE_PERFORM, 20, "prog.cbl", False),
            CallSite("PROG.C", "A", CallType.PROCEDURE_PERFORM, 30, "prog.cbl", False),
        ]

        builder = CallGraphBuilder()
        graph = builder.build(call_sites)
        analyzer = CallGraphAnalyzer(graph)

        cycles = analyzer.find_circular_dependencies()

        # Should find the A -> B -> C -> A cycle
        assert len(cycles) > 0
        # Check that the cycle contains all three nodes
        assert any(
            "PROG.A" in cycle and "PROG.B" in cycle and "PROG.C" in cycle for cycle in cycles
        )

    def test_find_hot_spots(self, analyzer):
        """Test finding hot spots (most called nodes)"""
        hot_spots = analyzer.find_hot_spots(top_n=3)

        assert len(hot_spots) <= 3
        # Should be sorted by fan-in (descending)
        if len(hot_spots) > 1:
            assert hot_spots[0].metrics.fan_in >= hot_spots[1].metrics.fan_in

    def test_find_critical_paths(self, analyzer):
        """Test finding critical paths"""
        paths = analyzer.find_critical_paths()

        # Should have at least one path from entry point
        assert len(paths) > 0

        # First path should be longest
        if len(paths) > 1:
            assert len(paths[0]) >= len(paths[1])

    def test_get_node_dependencies(self, analyzer):
        """Test getting node dependencies"""
        deps = analyzer.get_node_dependencies("PROG.SUB3")

        assert "callers" in deps
        assert "callees" in deps
        assert "transitive_callers" in deps
        assert "transitive_callees" in deps

        # SUB3 is called by SUB1 and SUB2
        assert len(deps["callers"]) == 2

        # SUB3 calls SUB4
        assert len(deps["callees"]) == 1

    def test_impact_analysis_nonexistent_node(self, analyzer):
        """Test impact analysis on nonexistent node"""
        with pytest.raises(ValueError):
            analyzer.analyze_impact("NONEXISTENT")

    def test_zero_impact_leaf_node(self):
        """Test impact analysis on leaf node (no callers)"""
        call_sites = [
            CallSite("PROG.MAIN", "LEAF", CallType.PROCEDURE_PERFORM, 10, "prog.cbl", False),
        ]

        builder = CallGraphBuilder()
        graph = builder.build(call_sites)
        analyzer = CallGraphAnalyzer(graph)

        # MAIN has no callers (entry point)
        analysis = analyzer.analyze_impact("PROG.MAIN")

        assert analysis.total_impact == 0
        assert len(analysis.directly_affected) == 0
        assert len(analysis.transitively_affected) == 0

    def test_complex_transitive_impact(self):
        """Test complex transitive impact calculation"""
        call_sites = [
            # MAIN -> A -> B -> C -> TARGET
            CallSite("PROG.MAIN", "A", CallType.PROCEDURE_PERFORM, 10, "prog.cbl", False),
            CallSite("PROG.A", "B", CallType.PROCEDURE_PERFORM, 20, "prog.cbl", False),
            CallSite("PROG.B", "C", CallType.PROCEDURE_PERFORM, 30, "prog.cbl", False),
            CallSite("PROG.C", "TARGET", CallType.PROCEDURE_PERFORM, 40, "prog.cbl", False),
        ]

        builder = CallGraphBuilder()
        graph = builder.build(call_sites)
        analyzer = CallGraphAnalyzer(graph)

        analysis = analyzer.analyze_impact("PROG.TARGET")

        # Direct: C
        assert "PROG.C" in analysis.directly_affected

        # Transitive: B, A, MAIN
        transitive = analysis.transitively_affected
        assert "PROG.B" in transitive
        assert "PROG.A" in transitive
        assert "PROG.MAIN" in transitive
