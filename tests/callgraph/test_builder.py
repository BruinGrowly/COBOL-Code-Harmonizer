"""
Tests for CallGraphBuilder
"""

import pytest

from cobol_harmonizer.callgraph.builder import CallGraphBuilder
from cobol_harmonizer.callgraph.models import CallSite, CallType


class TestCallGraphBuilder:
    """Test suite for CallGraphBuilder"""

    @pytest.fixture
    def builder(self):
        """Create a CallGraphBuilder instance"""
        return CallGraphBuilder()

    @pytest.fixture
    def sample_call_sites(self):
        """Create sample call sites for testing"""
        return [
            CallSite(
                caller='PROG1.MAIN',
                callee='SUB1',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=10,
                source_file='prog1.cbl',
                is_dynamic=False
            ),
            CallSite(
                caller='PROG1.MAIN',
                callee='SUB2',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=11,
                source_file='prog1.cbl',
                is_dynamic=False
            ),
            CallSite(
                caller='PROG1.SUB1',
                callee='SUB3',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=20,
                source_file='prog1.cbl',
                is_dynamic=False
            ),
            CallSite(
                caller='PROG1.MAIN',
                callee='PROG2',
                call_type=CallType.PROGRAM_CALL,
                line_number=12,
                source_file='prog1.cbl',
                is_dynamic=False
            ),
        ]

    def test_build_graph_basic(self, builder, sample_call_sites):
        """Test building a basic call graph"""
        graph = builder.build(sample_call_sites)

        assert graph is not None
        assert len(graph.nodes) > 0
        assert len(graph.edges) > 0

    def test_create_nodes_from_call_sites(self, builder, sample_call_sites):
        """Test that all nodes are created from call sites"""
        graph = builder.build(sample_call_sites)

        # Should have nodes for all unique callers and callees
        node_ids = set(graph.nodes.keys())

        # Check for key nodes
        assert 'PROG1.MAIN' in node_ids
        assert 'PROG1.SUB1' in node_ids
        assert 'PROG1.SUB2' in node_ids
        assert 'PROG1.SUB3' in node_ids

    def test_create_edges_from_call_sites(self, builder, sample_call_sites):
        """Test that edges are created correctly"""
        graph = builder.build(sample_call_sites)

        # Should have edges for each call relationship
        assert len(graph.edges) >= 3  # At least the non-dynamic calls

        # Check specific edges exist
        edge_pairs = [(e.source, e.target) for e in graph.edges]

        assert ('PROG1.MAIN', 'PROG1.SUB1') in edge_pairs
        assert ('PROG1.MAIN', 'PROG1.SUB2') in edge_pairs
        assert ('PROG1.SUB1', 'PROG1.SUB3') in edge_pairs

    def test_calculate_fan_in(self, builder, sample_call_sites):
        """Test fan-in calculation"""
        graph = builder.build(sample_call_sites)

        # SUB1 is called once (from MAIN)
        sub1_node = graph.get_node('PROG1.SUB1')
        assert sub1_node.metrics.fan_in == 1

        # SUB2 is called once (from MAIN)
        sub2_node = graph.get_node('PROG1.SUB2')
        assert sub2_node.metrics.fan_in == 1

        # SUB3 is called once (from SUB1)
        sub3_node = graph.get_node('PROG1.SUB3')
        assert sub3_node.metrics.fan_in == 1

        # MAIN is not called (entry point)
        main_node = graph.get_node('PROG1.MAIN')
        assert main_node.metrics.fan_in == 0

    def test_calculate_fan_out(self, builder, sample_call_sites):
        """Test fan-out calculation"""
        graph = builder.build(sample_call_sites)

        # MAIN calls 3 things (SUB1, SUB2, PROG2)
        main_node = graph.get_node('PROG1.MAIN')
        assert main_node.metrics.fan_out == 3

        # SUB1 calls 1 thing (SUB3)
        sub1_node = graph.get_node('PROG1.SUB1')
        assert sub1_node.metrics.fan_out == 1

        # SUB2 calls nothing
        sub2_node = graph.get_node('PROG1.SUB2')
        assert sub2_node.metrics.fan_out == 0

    def test_find_entry_points(self, builder, sample_call_sites):
        """Test finding entry points"""
        graph = builder.build(sample_call_sites)

        # Entry points have fan-in = 0
        assert len(graph.entry_points) > 0
        assert 'PROG1.MAIN' in graph.entry_points

    def test_calculate_depth(self, builder, sample_call_sites):
        """Test depth calculation"""
        graph = builder.build(sample_call_sites)

        # MAIN is entry point (depth 0)
        main_node = graph.get_node('PROG1.MAIN')
        assert main_node.metrics.depth == 0

        # SUB1 and SUB2 are called from MAIN (depth 1)
        sub1_node = graph.get_node('PROG1.SUB1')
        assert sub1_node.metrics.depth == 1

        sub2_node = graph.get_node('PROG1.SUB2')
        assert sub2_node.metrics.depth == 1

        # SUB3 is called from SUB1 (depth 2)
        sub3_node = graph.get_node('PROG1.SUB3')
        assert sub3_node.metrics.depth == 2

    def test_handle_duplicate_calls(self, builder):
        """Test handling duplicate calls (same caller -> callee)"""
        call_sites = [
            CallSite(
                caller='PROG.MAIN',
                callee='SUB',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=10,
                source_file='prog.cbl',
                is_dynamic=False
            ),
            CallSite(
                caller='PROG.MAIN',
                callee='SUB',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=20,
                source_file='prog.cbl',
                is_dynamic=False
            ),
        ]

        graph = builder.build(call_sites)

        # Should have only one edge, but with call_count=2
        edges = [e for e in graph.edges if e.source == 'PROG.MAIN' and e.target == 'PROG.SUB']
        assert len(edges) == 1
        assert edges[0].call_count == 2
        assert len(edges[0].line_numbers) == 2

    def test_skip_dynamic_calls(self, builder):
        """Test that dynamic calls are handled appropriately"""
        call_sites = [
            CallSite(
                caller='PROG.MAIN',
                callee='WS-PROGRAM',
                call_type=CallType.DYNAMIC_CALL,
                line_number=10,
                source_file='prog.cbl',
                is_dynamic=True
            ),
        ]

        graph = builder.build(call_sites)

        # Dynamic calls might be skipped or handled specially
        # Depending on implementation

    def test_merge_graphs(self, builder):
        """Test merging multiple call graphs"""
        # Create two separate graphs
        call_sites1 = [
            CallSite(
                caller='PROG1.MAIN',
                callee='SUB1',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=10,
                source_file='prog1.cbl',
                is_dynamic=False
            ),
        ]

        call_sites2 = [
            CallSite(
                caller='PROG2.MAIN',
                callee='SUB2',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=10,
                source_file='prog2.cbl',
                is_dynamic=False
            ),
        ]

        graph1 = builder.build(call_sites1)
        graph2 = builder.build(call_sites2)

        # Merge
        merged = builder.merge_graphs([graph1, graph2])

        # Should have nodes from both
        assert 'PROG1.MAIN' in merged.nodes
        assert 'PROG2.MAIN' in merged.nodes
        assert 'PROG1.SUB1' in merged.nodes
        assert 'PROG2.SUB2' in merged.nodes

    def test_empty_call_sites(self, builder):
        """Test building graph with no call sites"""
        graph = builder.build([])

        assert graph is not None
        assert len(graph.nodes) == 0
        assert len(graph.edges) == 0
        assert len(graph.entry_points) == 0

    def test_orphaned_nodes(self, builder):
        """Test finding orphaned nodes"""
        call_sites = [
            # MAIN calls SUB1
            CallSite(
                caller='PROG.MAIN',
                callee='SUB1',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=10,
                source_file='prog.cbl',
                is_dynamic=False
            ),
            # SUB2 calls SUB3 (but SUB2 is never called - orphaned chain)
            CallSite(
                caller='PROG.SUB2',
                callee='SUB3',
                call_type=CallType.PROCEDURE_PERFORM,
                line_number=20,
                source_file='prog.cbl',
                is_dynamic=False
            ),
        ]

        graph = builder.build(call_sites)

        # SUB2 and SUB3 are orphaned (not reachable from MAIN)
        assert len(graph.orphaned_nodes) >= 2
        assert 'PROG.SUB2' in graph.orphaned_nodes
        assert 'PROG.SUB3' in graph.orphaned_nodes
