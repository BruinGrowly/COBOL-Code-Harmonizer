"""
Call Graph Builder

Constructs call graphs from extracted call sites
"""

from typing import List, Dict, Set, Optional
from collections import defaultdict

from .models import (
    CallSite,
    CallGraph,
    GraphNode,
    GraphEdge,
    NodeType,
    NodeMetrics,
    CallGraphStats,
)


class CallGraphBuilder:
    """
    Builds call graphs from call sites

    Constructs:
    - Graph nodes (programs, procedures, sections)
    - Graph edges (call relationships)
    - Metrics (fan-in, fan-out, depth)
    - Entry points and orphaned nodes
    """

    def __init__(self):
        """Initialize builder"""
        pass

    def build(self, call_sites: List[CallSite]) -> CallGraph:
        """
        Build a complete call graph from call sites

        Args:
            call_sites: List of extracted call sites

        Returns:
            Complete CallGraph object
        """
        graph = CallGraph()

        # Step 1: Create nodes from call sites
        self._create_nodes(graph, call_sites)

        # Step 2: Create edges from call relationships
        self._create_edges(graph, call_sites)

        # Step 3: Calculate metrics
        self._calculate_metrics(graph)

        # Step 4: Find entry points
        self._find_entry_points(graph)

        # Step 5: Find orphaned nodes
        self._find_orphaned_nodes(graph)

        # Step 6: Calculate graph statistics
        self._calculate_graph_stats(graph)

        return graph

    def _create_nodes(self, graph: CallGraph, call_sites: List[CallSite]):
        """Create graph nodes from call sites"""

        # Collect all unique nodes (callers and callees)
        node_names = set()

        for call_site in call_sites:
            # Add caller
            node_names.add(call_site.caller)

            # Add callee (if not dynamic)
            if not call_site.is_dynamic:
                # Determine full name for callee
                if '.' in call_site.caller:
                    # Intra-program call - callee is in same program
                    program_prefix = call_site.caller.split('.')[0]
                    callee_full = f"{program_prefix}.{call_site.callee}"
                else:
                    # Inter-program call - callee is a program
                    callee_full = call_site.callee

                node_names.add(callee_full)

        # Create nodes
        for node_name in node_names:
            node = self._create_node(node_name, call_sites)
            graph.add_node(node)

    def _create_node(self, node_name: str, call_sites: List[CallSite]) -> GraphNode:
        """
        Create a graph node

        Args:
            node_name: Fully qualified node name (e.g., "PROGRAM.PARAGRAPH")
            call_sites: All call sites (for finding source location)

        Returns:
            GraphNode object
        """
        # Determine node type
        if '.' in node_name:
            # Format: PROGRAM.PROCEDURE
            parts = node_name.split('.')
            program_name = parts[0]
            procedure_name = parts[1]

            # Check if it's a section or paragraph
            # (In real implementation, would check source)
            node_type = NodeType.PARAGRAPH  # Default to paragraph

            display_name = procedure_name
        else:
            # Top-level program
            node_type = NodeType.PROGRAM
            program_name = node_name
            display_name = node_name

        # Find source location from call sites
        file_path = None
        start_line = 0
        for call_site in call_sites:
            if call_site.caller == node_name or call_site.callee == node_name:
                file_path = call_site.source_file
                start_line = call_site.line_number
                break

        return GraphNode(
            id=node_name,
            name=display_name,
            node_type=node_type,
            file_path=file_path,
            start_line=start_line,
            end_line=0,  # Would need source analysis to determine
            metrics=NodeMetrics()
        )

    def _create_edges(self, graph: CallGraph, call_sites: List[CallSite]):
        """Create graph edges from call relationships"""

        # Group call sites by (caller, callee) pair
        edge_map = defaultdict(list)

        for call_site in call_sites:
            # Determine target node ID
            if call_site.is_dynamic:
                # Dynamic calls - skip for now (could add placeholder node)
                continue

            # Build full callee name
            if '.' in call_site.caller:
                # Intra-program call
                program_prefix = call_site.caller.split('.')[0]
                callee_id = f"{program_prefix}.{call_site.callee}"
            else:
                # Inter-program call
                callee_id = call_site.callee

            # Check if target node exists
            if callee_id not in graph.nodes:
                # Target not in graph (external program or undefined)
                continue

            key = (call_site.caller, callee_id)
            edge_map[key].append(call_site)

        # Create edges
        for (caller_id, callee_id), sites in edge_map.items():
            edge = GraphEdge(
                source=caller_id,
                target=callee_id,
                call_type=sites[0].call_type,  # Use first call type
                call_count=len(sites),
                line_numbers=[site.line_number for site in sites]
            )
            graph.add_edge(edge)

    def _calculate_metrics(self, graph: CallGraph):
        """Calculate metrics for all nodes"""

        # Calculate fan-in and fan-out
        for edge in graph.edges:
            # Fan-out: how many nodes this calls
            source_node = graph.get_node(edge.source)
            if source_node:
                source_node.metrics.fan_out += 1

            # Fan-in: how many nodes call this
            target_node = graph.get_node(edge.target)
            if target_node:
                target_node.metrics.fan_in += 1

        # Calculate depth from entry points (will be set after entry points found)

    def _find_entry_points(self, graph: CallGraph):
        """
        Find entry points (nodes with no callers)

        Entry points are typically:
        - Main programs
        - Top-level procedures never called
        """
        graph.entry_points = []

        for node_id, node in graph.nodes.items():
            # Entry point if fan-in is 0 (no one calls it)
            if node.metrics.fan_in == 0:
                graph.entry_points.append(node_id)

                # Entry points have depth 0
                node.metrics.depth = 0

        # Calculate depth for other nodes using BFS
        self._calculate_depth(graph)

    def _calculate_depth(self, graph: CallGraph):
        """Calculate depth for all nodes using breadth-first search"""

        # Start from entry points
        visited = set()
        queue = [(ep, 0) for ep in graph.entry_points]

        while queue:
            node_id, depth = queue.pop(0)

            if node_id in visited:
                continue

            visited.add(node_id)

            # Update node depth
            node = graph.get_node(node_id)
            if node:
                node.metrics.depth = max(node.metrics.depth, depth)

                # Update max depth
                graph.max_depth = max(graph.max_depth, depth)

                # Add callees to queue
                for callee_id in graph.get_callees(node_id):
                    if callee_id not in visited:
                        queue.append((callee_id, depth + 1))

    def _find_orphaned_nodes(self, graph: CallGraph):
        """
        Find orphaned nodes (unreachable from entry points)

        These are dead code or unreferenced procedures
        """
        graph.orphaned_nodes = []

        # Find all reachable nodes from entry points
        reachable = set()

        def dfs(node_id: str):
            if node_id in reachable:
                return
            reachable.add(node_id)

            for callee_id in graph.get_callees(node_id):
                dfs(callee_id)

        # DFS from each entry point
        for entry_point in graph.entry_points:
            dfs(entry_point)

        # Orphaned nodes are those not reachable
        for node_id in graph.nodes:
            if node_id not in reachable:
                graph.orphaned_nodes.append(node_id)

    def _calculate_graph_stats(self, graph: CallGraph):
        """Calculate overall graph statistics"""

        stats = CallGraphStats()

        stats.total_nodes = len(graph.nodes)
        stats.total_edges = len(graph.edges)
        stats.entry_points = len(graph.entry_points)
        stats.orphaned_nodes = len(graph.orphaned_nodes)

        # Count node types
        for node in graph.nodes.values():
            if node.node_type == NodeType.PROGRAM:
                stats.total_programs += 1
            elif node.node_type == NodeType.PROCEDURE:
                stats.total_procedures += 1
            elif node.node_type == NodeType.SECTION:
                stats.total_sections += 1
            elif node.node_type == NodeType.PARAGRAPH:
                stats.total_procedures += 1  # Count paragraphs as procedures

        # Calculate max/avg metrics
        if graph.nodes:
            fan_ins = [node.metrics.fan_in for node in graph.nodes.values()]
            fan_outs = [node.metrics.fan_out for node in graph.nodes.values()]

            stats.max_fan_in = max(fan_ins) if fan_ins else 0
            stats.max_fan_out = max(fan_outs) if fan_outs else 0
            stats.avg_fan_in = sum(fan_ins) / len(fan_ins) if fan_ins else 0
            stats.avg_fan_out = sum(fan_outs) / len(fan_outs) if fan_outs else 0

        stats.max_depth = graph.max_depth

        # Store stats in graph (add to CallGraph model if needed)
        # For now, return it
        return stats

    def merge_graphs(self, graphs: List[CallGraph]) -> CallGraph:
        """
        Merge multiple call graphs into one

        Useful for analyzing entire codebases

        Args:
            graphs: List of CallGraph objects

        Returns:
            Merged CallGraph
        """
        merged = CallGraph()

        # Merge all nodes
        for graph in graphs:
            for node_id, node in graph.nodes.items():
                if node_id not in merged.nodes:
                    merged.add_node(node)

        # Merge all edges
        edge_map = {}  # (source, target) -> edge
        for graph in graphs:
            for edge in graph.edges:
                key = (edge.source, edge.target)
                if key in edge_map:
                    # Merge with existing edge
                    existing = edge_map[key]
                    existing.call_count += edge.call_count
                    existing.line_numbers.extend(edge.line_numbers)
                else:
                    # Add new edge
                    edge_map[key] = edge
                    merged.add_edge(edge)

        # Recalculate metrics
        self._calculate_metrics(merged)
        self._find_entry_points(merged)
        self._find_orphaned_nodes(merged)

        return merged
