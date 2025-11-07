"""
Call Graph Analyzer

Provides analysis capabilities for call graphs:
- Impact analysis (what's affected by changes)
- Risk assessment
- Dead code detection
- Circular dependency detection
"""

from typing import List, Set, Dict, Optional
from collections import defaultdict

from .models import (
    CallGraph,
    GraphNode,
    ImpactAnalysis,
    NodeType,
)


class CallGraphAnalyzer:
    """
    Analyzes call graphs to provide insights

    Features:
    - Impact analysis: What's affected if a node changes?
    - Risk assessment: How risky is a change?
    - Dead code detection: What's never called?
    - Circular dependencies: Are there cycles?
    - Critical path analysis: What's the longest call chain?
    """

    def __init__(self, graph: CallGraph):
        """
        Initialize analyzer with a call graph

        Args:
            graph: CallGraph to analyze
        """
        self.graph = graph

    def analyze_impact(self, target_node_id: str) -> ImpactAnalysis:
        """
        Analyze impact of changing a target node

        Shows all nodes that would be affected (directly and transitively)

        Args:
            target_node_id: Node to analyze

        Returns:
            ImpactAnalysis object
        """
        target_node = self.graph.get_node(target_node_id)
        if not target_node:
            raise ValueError(f"Node not found: {target_node_id}")

        analysis = ImpactAnalysis(
            target_node=target_node_id,
            target_name=target_node.name
        )

        # Find directly affected nodes (direct callers)
        analysis.directly_affected = self.graph.get_callers(target_node_id)

        # Find transitively affected nodes (indirect callers)
        analysis.transitively_affected = self._find_transitive_callers(
            target_node_id,
            exclude=set(analysis.directly_affected)
        )

        # Calculate total impact
        analysis.total_impact = len(analysis.directly_affected) + len(analysis.transitively_affected)

        # Calculate risk score
        analysis.risk_score = self._calculate_risk_score(target_node, analysis)

        # Determine risk level
        analysis.risk_level = self._determine_risk_level(analysis.risk_score)

        # Generate warnings
        analysis.warnings = self._generate_warnings(target_node, analysis)

        # Generate recommendations
        analysis.recommendations = self._generate_recommendations(target_node, analysis)

        return analysis

    def _find_transitive_callers(self, node_id: str, exclude: Set[str]) -> List[str]:
        """
        Find all nodes that transitively call this node

        Args:
            node_id: Target node
            exclude: Nodes to exclude (direct callers)

        Returns:
            List of node IDs
        """
        visited = set()
        transitive = set()

        def dfs_reverse(current_id: str):
            """DFS in reverse (following callers)"""
            if current_id in visited:
                return

            visited.add(current_id)

            for caller_id in self.graph.get_callers(current_id):
                if caller_id not in exclude and caller_id != node_id:
                    transitive.add(caller_id)
                    dfs_reverse(caller_id)

        # Start from direct callers
        for caller_id in self.graph.get_callers(node_id):
            dfs_reverse(caller_id)

        return list(transitive)

    def _calculate_risk_score(self, target_node: GraphNode, analysis: ImpactAnalysis) -> float:
        """
        Calculate risk score (0-100)

        Factors:
        - Number of affected nodes (fan-in)
        - Depth in call graph
        - Complexity of node
        - Harmony/disharmony scores (if available)

        Args:
            target_node: Node being changed
            analysis: Impact analysis

        Returns:
            Risk score 0-100
        """
        score = 0.0

        # Factor 1: Fan-in (how many nodes call this) - 40 points max
        fan_in = target_node.metrics.fan_in
        if fan_in > 0:
            # Logarithmic scale: 1 caller = 10, 10 callers = 30, 100+ callers = 40
            score += min(40, 10 + (10 * (fan_in ** 0.5)))

        # Factor 2: Total impact (direct + transitive) - 30 points max
        if analysis.total_impact > 0:
            # Logarithmic scale for impact
            score += min(30, 10 + (5 * (analysis.total_impact ** 0.5)))

        # Factor 3: Depth (how deep in call graph) - 10 points max
        # Deeper nodes are riskier (harder to test)
        depth = target_node.metrics.depth
        score += min(10, depth * 2)

        # Factor 4: Complexity - 10 points max
        complexity = target_node.metrics.complexity
        if complexity > 0:
            score += min(10, complexity / 2)

        # Factor 5: Disharmony score - 10 points max
        if target_node.disharmony_score:
            score += min(10, target_node.disharmony_score * 10)

        return min(100, score)

    def _determine_risk_level(self, risk_score: float) -> str:
        """
        Determine risk level from score

        Args:
            risk_score: Risk score 0-100

        Returns:
            "LOW", "MEDIUM", "HIGH", or "CRITICAL"
        """
        if risk_score < 25:
            return "LOW"
        elif risk_score < 50:
            return "MEDIUM"
        elif risk_score < 75:
            return "HIGH"
        else:
            return "CRITICAL"

    def _generate_warnings(self, target_node: GraphNode, analysis: ImpactAnalysis) -> List[str]:
        """Generate warnings about the change"""
        warnings = []

        # High fan-in warning
        if target_node.metrics.fan_in >= 10:
            warnings.append(
                f"⚠️  This node is called by {target_node.metrics.fan_in} different locations"
            )

        # High total impact warning
        if analysis.total_impact >= 20:
            warnings.append(
                f"⚠️  Changes will affect {analysis.total_impact} nodes across the codebase"
            )

        # Entry point warning
        if target_node.id in self.graph.entry_points:
            warnings.append("⚠️  This is an entry point - external callers may be affected")

        # Circular dependency warning
        if self._has_circular_dependency(target_node.id):
            warnings.append("⚠️  This node is part of a circular call chain")

        return warnings

    def _generate_recommendations(self, target_node: GraphNode, analysis: ImpactAnalysis) -> List[str]:
        """Generate recommendations for safe changes"""
        recommendations = []

        # High-risk recommendations
        if analysis.risk_level in ["HIGH", "CRITICAL"]:
            recommendations.append("✓ Add comprehensive unit tests before making changes")
            recommendations.append("✓ Consider breaking this into smaller, testable components")

            if analysis.total_impact > 10:
                recommendations.append("✓ Perform incremental refactoring instead of large changes")

        # Medium-risk recommendations
        if analysis.risk_level in ["MEDIUM", "HIGH", "CRITICAL"]:
            recommendations.append("✓ Test all directly affected callers")
            recommendations.append("✓ Review integration test coverage")

        # Always recommend
        if analysis.directly_affected:
            recommendations.append(
                f"✓ Test these {len(analysis.directly_affected)} direct callers: "
                f"{', '.join(analysis.directly_affected[:3])}"
                + ("..." if len(analysis.directly_affected) > 3 else "")
            )

        return recommendations

    def find_dead_code(self) -> List[str]:
        """
        Find dead code (unreachable nodes)

        Returns:
            List of node IDs that are never called
        """
        # Orphaned nodes are dead code
        return self.graph.orphaned_nodes.copy()

    def find_circular_dependencies(self) -> List[List[str]]:
        """
        Find circular dependencies (cycles in call graph)

        Returns:
            List of cycles, where each cycle is a list of node IDs
        """
        cycles = []
        visited = set()
        rec_stack = []

        def dfs(node_id: str, path: List[str]):
            """DFS to detect cycles"""
            if node_id in rec_stack:
                # Found a cycle - extract it
                cycle_start = rec_stack.index(node_id)
                cycle = rec_stack[cycle_start:] + [node_id]
                cycles.append(cycle)
                return

            if node_id in visited:
                return

            visited.add(node_id)
            rec_stack.append(node_id)

            # Visit callees
            for callee_id in self.graph.get_callees(node_id):
                dfs(callee_id, path + [callee_id])

            rec_stack.pop()

        # Check from all nodes
        for node_id in self.graph.nodes:
            if node_id not in visited:
                dfs(node_id, [node_id])

        return cycles

    def _has_circular_dependency(self, node_id: str) -> bool:
        """Check if a node is part of a circular dependency"""
        visited = set()
        rec_stack = set()

        def dfs(current_id: str) -> bool:
            if current_id in rec_stack:
                return True  # Cycle detected

            if current_id in visited:
                return False

            visited.add(current_id)
            rec_stack.add(current_id)

            for callee_id in self.graph.get_callees(current_id):
                if dfs(callee_id):
                    return True

            rec_stack.remove(current_id)
            return False

        return dfs(node_id)

    def find_critical_paths(self) -> List[List[str]]:
        """
        Find critical paths (longest call chains)

        Returns:
            List of paths, where each path is a list of node IDs
        """
        paths = []

        def dfs(node_id: str, current_path: List[str]):
            """DFS to find all paths"""
            callees = self.graph.get_callees(node_id)

            if not callees:
                # Leaf node - end of path
                paths.append(current_path.copy())
                return

            for callee_id in callees:
                if callee_id not in current_path:  # Avoid cycles
                    dfs(callee_id, current_path + [callee_id])

        # Start from entry points
        for entry_point in self.graph.entry_points:
            dfs(entry_point, [entry_point])

        # Sort by length
        paths.sort(key=len, reverse=True)

        return paths

    def find_hot_spots(self, top_n: int = 10) -> List[GraphNode]:
        """
        Find hot spots (most frequently called nodes)

        Args:
            top_n: Number of hot spots to return

        Returns:
            List of nodes sorted by fan-in
        """
        nodes = list(self.graph.nodes.values())
        nodes.sort(key=lambda n: n.metrics.fan_in, reverse=True)

        return nodes[:top_n]

    def find_complexity_hot_spots(self, top_n: int = 10) -> List[GraphNode]:
        """
        Find complexity hot spots (high complexity nodes)

        Args:
            top_n: Number to return

        Returns:
            List of nodes sorted by complexity
        """
        nodes = list(self.graph.nodes.values())
        nodes.sort(key=lambda n: n.metrics.complexity, reverse=True)

        return nodes[:top_n]

    def get_node_dependencies(self, node_id: str) -> Dict[str, List[str]]:
        """
        Get all dependencies for a node

        Args:
            node_id: Node to analyze

        Returns:
            Dictionary with 'callers' and 'callees' lists
        """
        return {
            'callers': self.graph.get_callers(node_id),
            'callees': self.graph.get_callees(node_id),
            'transitive_callers': self._find_transitive_callers(node_id, set()),
            'transitive_callees': self._find_transitive_callees(node_id),
        }

    def _find_transitive_callees(self, node_id: str) -> List[str]:
        """Find all nodes transitively called by this node"""
        visited = set()
        transitive = set()

        def dfs(current_id: str):
            if current_id in visited:
                return

            visited.add(current_id)

            for callee_id in self.graph.get_callees(current_id):
                if callee_id != node_id:
                    transitive.add(callee_id)
                    dfs(callee_id)

        dfs(node_id)
        return list(transitive)
